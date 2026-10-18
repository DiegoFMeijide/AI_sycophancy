#!/usr/bin/env Rscript
# Public, offline adaptation of the audited 17 October 2026 analysis.
# Same model specification and numerical calculations; private operational gates removed.
# Models were chosen after fieldwork: binary LOGIT + nominal MULTINOMIAL are primary;
# LPM estimates are sensitivity analyses. Run from repository root.
suppressPackageStartupMessages({library(jsonlite);library(dplyr);library(tidyr);library(digest);library(sandwich)})
options(stringsAsFactors=FALSE,scipen=999)
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==0L||identical(args,"--check"));check_only<-identical(args,"--check")
script<-"code/R/02_analyze_14400.R"
source_csv<-"data/analysis_14400.csv"
sha<-function(p)digest(file=p,algo="sha256",serialize=FALSE)
readj<-function(p)fromJSON(p,simplifyVector=FALSE)
json<-function(x)as.character(toJSON(x,auto_unbox=TRUE,dataframe="rows",na="null",null="null",digits=15,pretty=TRUE))
manifest_path <- "data/provenance.json"
manifest <- readj(manifest_path)
expected_source_hash <- manifest$output_sha256[["analysis_14400.csv"]]
stopifnot(is.character(expected_source_hash), length(expected_source_hash)==1L,
  sha(source_csv)==expected_source_hash)
d<-read.csv(source_csv,check.names=FALSE,colClasses=c(call_id="character",source_call_id="character",r="character"))
stopifnot(nrow(d)==14400,!anyDuplicated(d$call_id),all(d$derived_accepted),
 all(d$returned_model=="gpt-6-luna"),all(d$arg+d$balanced+d$gbr==1),
 all(table(d$scale_id,d$location)==1800),sum(d$cohort=="GEOGRAPHY" & !d$raw_accepted)==2,
 all(d$search_used==as.integer(d$search_regime=="WEB_AUTO")))
# Row order, treatment fields and recorded block membership are retained in public data.
d$environment<-d$location
d$cluster_id<-ifelse(d$cohort=="GEOGRAPHY",paste("GEO",d$scale_id,d$source_block_number,sep="_"),
 ifelse(d$scale_id=="ORDINAL5",paste("HIST_ORDINAL",d$arm,d$source_block_number,sep="_"),paste("HIST_BINARY",d$source_block_number,sep="_")))
stopifnot(n_distinct(d$cluster_id[d$scale_id=="ORDINAL5"])==150,n_distinct(d$cluster_id[d$scale_id=="BINARY2"])==100,
 all(table(d$choice_id[d$scale_id=="ORDINAL5"])[c("ARG_SOME","BALANCED","GBR_SOME")]==c(1036,2532,3632)),
 !any(d$choice_id %in% c("ARG_CLEAR","GBR_CLEAR")))
cat("GATE_OK:14,400 official selected cases; public source hash; 150/100 real clusters.\n")

labels<-list(scale=c(ORDINAL5="Escala de cinco opciones",BINARY2="Elección entre dos países"),
 environment=c(HIST_NO_WEB="Histórico · sin búsqueda",HIST_DEFAULT="Histórico · búsqueda, US por defecto",AR="Nueva tanda · búsqueda, Argentina",GB="Nueva tanda · búsqueda, Reino Unido"),
 arm=c(CONTROL="Sin mitigación",MITIGATION="Con mitigación",ALL="Ambos brazos"),
 language_level=c(ES_AR="Español argentino",ES_NEUTRAL="Español neutro",EN="Inglés"),
 toponym_id=c(MALVINAS="Malvinas",FALKLANDS="Falkland"),origin_id=c(NONE="Sin declaración",BUENOS_AIRES="Buenos Aires",LONDON="Londres"),
 search_regime=c(WEB_AUTO="Búsqueda disponible",NO_WEB="Sin búsqueda"),orientation=c(ARG_FIRST="Argentina primero",GBR_FIRST="Reino Unido primero"))
counts<-function(z,groups){z |> group_by(across(all_of(groups))) |> summarise(n=n(),arg_n=sum(arg),balanced_n=sum(balanced),gbr_n=sum(gbr),
 ARG_CLEAR=sum(choice_id=="ARG_CLEAR"),ARG_SOME=sum(choice_id=="ARG_SOME"),BALANCED=sum(choice_id=="BALANCED"),
 GBR_SOME=sum(choice_id=="GBR_SOME"),GBR_CLEAR=sum(choice_id=="GBR_CLEAR"),
 arg_pct=100*mean(arg),balanced_pct=100*mean(balanced),gbr_pct=100*mean(gbr),
 mean_score=if(all(is.na(score)))NA_real_ else mean(score),search_pct=100*mean(search_used),
 recovered_n=sum(recovered),metadata_flagged_n=sum(metadata_flagged),.groups="drop")}
overall<-counts(d,"scale_id")
context_summary<-bind_rows(counts(d,c("scale_id","environment","arm")),counts(d,c("scale_id","environment")) |> mutate(arm="ALL"))
factor_vars<-c("language_level","toponym_id","origin_id","search_regime","environment","arm","orientation")
factor_aggregates<-bind_rows(lapply(factor_vars,function(f){
 ans<-counts(d,c("scale_id",f));ans$level<-ans[[f]];ans[[f]]<-NULL
 ans$factor<-f;ans$level_label<-unname(labels[[f]][ans$level]);ans$weighting<-"Pool de cuatro entornos de igual tamaño; no contraste causal único";ans}))
factor_context<-bind_rows(lapply(c("language_level","toponym_id","origin_id","orientation"),function(f){
 ans<-counts(d,c("scale_id","environment","arm",f));ans$level<-ans[[f]];ans[[f]]<-NULL
 ans$factor<-f;ans$level_label<-unname(labels[[f]][ans$level]);ans}))
conditions288<-counts(d,c("scale_id","environment","arm","language_level","toponym_id","origin_id"))
stopifnot(nrow(conditions288)==288,all(conditions288$n==50))
toponym_percentages<-bind_rows(
 filter(factor_aggregates,factor=="toponym_id") |> mutate(environment="ALL",arm="ALL",aggregation="Cuatro entornos y ambos brazos"),
 filter(factor_context,factor=="toponym_id") |> mutate(aggregation="Entorno y brazo específicos"))
outcome_distribution<-d |> count(scale_id,environment,arm,choice_id,name="n") |>
 complete(nesting(scale_id,environment,arm),choice_id=c("ARG_CLEAR","ARG_SOME","BALANCED","GBR_SOME","GBR_CLEAR","ARG","GBR"),fill=list(n=0)) |>
 filter((scale_id=="ORDINAL5" & !choice_id %in% c("ARG","GBR")) | (scale_id=="BINARY2" & choice_id %in% c("ARG","GBR"))) |>
 group_by(scale_id,environment,arm) |> mutate(percentage=100*n/sum(n)) |> ungroup()
chronology<-d |> group_by(scale_id,environment,arm,run_id) |> summarise(n=n(),start_utc=min(request_started_at_utc),end_utc=max(request_completed_at_utc),recovered_n=sum(recovered),.groups="drop")
clusters<-d |> group_by(scale_id,cluster_id) |> summarise(n=n(),environments=paste(sort(unique(environment)),collapse="|"),
 arms=paste(sort(unique(arm)),collapse="|"),source_block_number=first(source_block_number),recovered_n=sum(recovered),.groups="drop")
parameters<-list(requested_model="gpt-6-luna",independent_user_message=TRUE,strict_json=TRUE,
 r_enum=list(ORDINAL5=as.character(1:5),BINARY2=as.character(1:2)),explanation="e, una o dos oraciones",
 max_output_tokens=8192,max_tool_calls=4,tool_choice_web="auto",tool_choice_no_web="none",
 explicit_locations=c("AR","GB"),historical_location="Omitida en request, US devuelto por API; no es un tercer brazo concurrente",
 defaults_reported_not_explicitly_sent=list(list(reasoning="medium",reasoning_mode="standard",
 temperature=1,top_p=0.98,verbosity="medium",search_context="medium",model="gpt-6-luna")))

# Linear probability sensitivity and primary binary logistic model. X49 allows each environment×arm
# tenga sus propias asociaciones de idioma, topónimo y origen; no incluye outcomes
# postratamiento (búsqueda efectiva, fuentes, citas o longitud de explicación).
formula_rhs<-~environment*arm*(language_level+toponym_id+origin_id)+orientation
factor_levels<-list(environment=c("HIST_NO_WEB","HIST_DEFAULT","AR","GB"),arm=c("CONTROL","MITIGATION"),
 language_level=c("EN","ES_NEUTRAL","ES_AR"),toponym_id=c("FALKLANDS","MALVINAS"),
 origin_id=c("NONE","BUENOS_AIRES","LONDON"),orientation=c("ARG_FIRST","GBR_FIRST"))
as_design<-function(z){for(f in names(factor_levels))z[[f]]<-factor(z[[f]],levels=factor_levels[[f]]);z}
grid<-as_design(expand.grid(factor_levels,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE))
stopifnot(nrow(grid)==288)
factor_defs<-data.frame(contrast_id=c("ES_AR_vs_EN","ES_NEUTRAL_vs_EN","ES_AR_vs_ES_NEUTRAL","MALVINAS_vs_FALKLANDS","BUENOS_AIRES_vs_LONDON","BUENOS_AIRES_vs_NONE","LONDON_vs_NONE"),
 factor=c(rep("language_level",3),"toponym_id",rep("origin_id",3)),
 left=c("ES_AR","ES_NEUTRAL","ES_AR","MALVINAS","BUENOS_AIRES","BUENOS_AIRES","LONDON"),
 right=c("EN","EN","ES_NEUTRAL","FALKLANDS","LONDON","NONE","NONE"),
 label=c("Español argentino − inglés","Español neutro − inglés","Español argentino − neutro","Malvinas − Falkland","Buenos Aires − Londres","Buenos Aires − sin declaración","Londres − sin declaración"))
contrast_specs<-list();j<-0L
for(i in seq_len(nrow(factor_defs))){
 scopes<-bind_rows(data.frame(environment="ALL",arm="ALL"),expand.grid(environment=factor_levels$environment,arm=factor_levels$arm,stringsAsFactors=FALSE))
 for(k in seq_len(nrow(scopes))){j<-j+1;contrast_specs[[j]]<-cbind(factor_defs[i,],scopes[k,],comparison_class="PROMPT_FACTOR")}
}
for(env in c("ALL",factor_levels$environment)){
 j<-j+1;contrast_specs[[j]]<-data.frame(contrast_id="MITIGATION_vs_CONTROL",factor="arm",left="MITIGATION",right="CONTROL",
 label="Con mitigación − sin mitigación",environment=env,arm="ALL",comparison_class=if(env %in% c("HIST_NO_WEB","HIST_DEFAULT"))"MITIGATION_HISTORICAL" else if(env=="ALL")"MIXED_CONTEXT_AVERAGE" else "MITIGATION_CONCURRENT")
}
env_defs<-data.frame(contrast_id=c("AR_vs_GB","HIST_WEB_vs_NO_WEB","AR_vs_HIST_DEFAULT","GB_vs_HIST_DEFAULT"),
 left=c("AR","HIST_DEFAULT","AR","GB"),right=c("GB","HIST_NO_WEB","HIST_DEFAULT","HIST_DEFAULT"),
 label=c("Ubicación Argentina − Reino Unido","Búsqueda disponible − sin búsqueda, tandas históricas","Argentina nueva − US histórico","Reino Unido nuevo − US histórico"),
 comparison_class=c("GEOGRAPHY_CONCURRENT","SEARCH_HISTORICAL_RANDOMIZED","BETWEEN_WAVES_DESCRIPTIVE","BETWEEN_WAVES_DESCRIPTIVE"))
for(i in seq_len(nrow(env_defs)))for(arm in c("ALL",factor_levels$arm)){
 j<-j+1;contrast_specs[[j]]<-data.frame(contrast_id=env_defs$contrast_id[i],factor="environment",left=env_defs$left[i],right=env_defs$right[i],
 label=env_defs$label[i],environment="ALL",arm=arm,comparison_class=env_defs$comparison_class[i])
}
j<-j+1;contrast_specs[[j]]<-data.frame(contrast_id="ARG_FIRST_vs_GBR_FIRST",factor="orientation",left="ARG_FIRST",right="GBR_FIRST",
 label="Argentina primero − Reino Unido primero",environment="ALL",arm="ALL",comparison_class="ORDER_ADDITIVE_ASSUMPTION")
specs<-bind_rows(contrast_specs);stopifnot(nrow(specs)==81)
effect_rows<-list();interaction_rows<-list();prob_rows<-list();coef_rows<-list();diagnostic_rows<-list();prediction_rows<-list();fits<-list();idx<-0L
effect_ci<-function(est,grad,V){se<-sqrt(max(0,drop(t(grad)%*%V%*%grad)));data.frame(estimate=est,se=se,
 lower=est-qt(.975,49)*se,upper=est+qt(.975,49)*se,p=if(se==0)if(est==0)1 else 0 else 2*pt(-abs(est/se),49),df=49)}
counterfactual<-function(g,f,level){g[[f]]<-factor(rep(level,nrow(g)),levels=factor_levels[[f]]);g}
for(scale in c("ORDINAL5","BINARY2"))for(model in if(scale=="BINARY2")c("LPM","LOGIT") else "LPM")for(outcome in if(scale=="ORDINAL5")c("ARG","BALANCED","GBR") else "ARG"){
 z<-as_design(filter(d,scale_id==scale));z$y<-z[[c(ARG="arg",BALANCED="balanced",GBR="gbr")[[outcome]]]]
 form<-as.formula(paste("y",paste(deparse(formula_rhs),collapse=" ")))
 fit<-if(model=="LPM")lm(form,data=z) else glm(form,data=z,family=binomial(link="logit"),control=glm.control(maxit=100,epsilon=1e-10))
 X<-model.matrix(fit);b<-coef(fit);V<-vcovCL(fit,cluster=z$cluster_id,type="HC1",cadjust=TRUE)
 stopifnot(nrow(X)==7200,ncol(X)==49,qr(X)$rank==49,all(is.finite(b)),all(is.finite(V)),
  max(abs(V-t(V)))<1e-8,all(diag(V)>=0),model=="LPM"||isTRUE(fit$converged))
 makeX<-function(g)model.matrix(delete.response(terms(fit)),g,contrasts.arg=fit$contrasts,xlev=fit$xlevels)
 predgrad<-function(g){x<-makeX(g);eta<-drop(x%*%b)
  if(model=="LPM")list(estimate=100*mean(eta),grad=100*colMeans(x)) else {
   p<-plogis(eta);list(estimate=100*mean(p),grad=100*colMeans(x*as.numeric(p*(1-p))))}}
 idx<-idx+1;fit_key<-paste(scale,model,outcome,sep="_");fits[[fit_key]]<-list(coefficients=b,vcov=V)
 cr<-data.frame(term=names(b),coefficient=unname(b),se=sqrt(diag(V)),row.names=NULL)
 cr$lower<-cr$coefficient-qt(.975,49)*cr$se;cr$upper<-cr$coefficient+qt(.975,49)*cr$se
 cr$p<-2*pt(-abs(cr$coefficient/cr$se),49);cr$scale_id<-scale;cr$model<-model;cr$outcome<-outcome
 coef_rows[[idx]]<-cr
 predicted<-if(model=="LPM")as.numeric(fitted(fit)) else as.numeric(fitted(fit))
 invalid<-predicted<0|predicted>1
 diagnostic_rows[[idx]]<-data.frame(scale_id=scale,model=model,outcome=outcome,n=7200,n_parameters=49,
  rank=qr(X)$rank,n_clusters=n_distinct(z$cluster_id),ci_df=49,converged=if(model=="LOGIT")fit$converged else TRUE,
  iterations=if(model=="LOGIT")fit$iter else NA_integer_,r_squared=if(model=="LPM")summary(fit)$r.squared else NA_real_,
  adjusted_r_squared=if(model=="LPM")summary(fit)$adj.r.squared else NA_real_,
  null_deviance=if(model=="LOGIT")fit$null.deviance else NA_real_,residual_deviance=if(model=="LOGIT")fit$deviance else NA_real_,
  min_prediction=min(predicted),max_prediction=max(predicted),outside_unit_interval_n=sum(invalid),outside_unit_interval_pct=100*mean(invalid),
  mean_prediction=mean(predicted),observed_mean=mean(z$y),max_hat=max(hatvalues(fit)),
  condition_number=kappa(X,exact=TRUE),residual_rmse=sqrt(mean((z$y-predicted)^2)))
 prediction_rows[[idx]]<-z |> mutate(prediction=predicted,invalid=invalid) |> group_by(environment,arm) |>
  summarise(n=n(),min_prediction=min(prediction),max_prediction=max(prediction),outside_n=sum(invalid),outside_pct=100*mean(invalid),.groups="drop") |>
  mutate(scale_id=scale,model=model,outcome=outcome)
 ef<-vector("list",nrow(specs))
 for(si in seq_len(nrow(specs))){sp<-specs[si,];g<-grid
  if(sp$environment!="ALL")g<-g[as.character(g$environment)==sp$environment,]
  if(sp$arm!="ALL")g<-g[as.character(g$arm)==sp$arm,]
  left<-predgrad(counterfactual(g,sp$factor,sp$left));right<-predgrad(counterfactual(g,sp$factor,sp$right))
  ei<-effect_ci(left$estimate-right$estimate,left$grad-right$grad,V)
  ef[[si]]<-cbind(data.frame(scale_id=scale,model=model,outcome=outcome),sp,
   data.frame(adjusted_left_pct=left$estimate,adjusted_right_pct=right$estimate,n_clusters=n_distinct(z$cluster_id),standardization_rows=nrow(g)),ei)
 }
 effect_rows[[idx]]<-bind_rows(ef)
 # Cambio directo de una brecha al aplicar mitigación: diferencia de efectos,
 # no comparación entre significación/no significación de dos brazos.
 ir<-list();ii<-0L
 interaction_specs<-bind_rows(factor_defs |> mutate(comparison_class="PROMPT_GAP_CHANGE"),
  env_defs[1:2,] |> mutate(factor="environment"))
 for(si in seq_len(nrow(interaction_specs))){sp<-interaction_specs[si,]
  contexts<-if(sp$factor=="environment")"ALL" else c("ALL",factor_levels$environment)
  for(env in contexts){g<-grid
   if(env!="ALL")g<-g[as.character(g$environment)==env,]
   diffs<-list()
   for(a in factor_levels$arm){ga<-counterfactual(g,"arm",a)
    left<-predgrad(counterfactual(ga,sp$factor,sp$left));right<-predgrad(counterfactual(ga,sp$factor,sp$right))
    diffs[[a]]<-list(estimate=left$estimate-right$estimate,grad=left$grad-right$grad)}
   ii<-ii+1;ir[[ii]]<-cbind(data.frame(scale_id=scale,model=model,outcome=outcome,environment=env,
    contrast_id=sp$contrast_id,factor=sp$factor,label=paste("Cambio con mitigación:",sp$label),
    comparison_class=sp$comparison_class,control_gap=diffs$CONTROL$estimate,mitigation_gap=diffs$MITIGATION$estimate,n_clusters=n_distinct(z$cluster_id)),
    effect_ci(diffs$MITIGATION$estimate-diffs$CONTROL$estimate,diffs$MITIGATION$grad-diffs$CONTROL$grad,V))
  }
 }
 interaction_rows[[idx]]<-bind_rows(ir)
 pr<-list();pi<-0L
 for(env in factor_levels$environment)for(arm in factor_levels$arm){
  g<-grid[as.character(grid$environment)==env & as.character(grid$arm)==arm,];pg<-predgrad(g);pi<-pi+1
  pr[[pi]]<-cbind(data.frame(scale_id=scale,model=model,outcome=outcome,environment=env,arm=arm,n_standardization_rows=nrow(g)),effect_ci(pg$estimate,pg$grad,V))
 }
 prob_rows[[idx]]<-bind_rows(pr)
 cat("Modelo",fit_key,":49parámetros;",n_distinct(z$cluster_id),"clusters; prediccionesfuera[0,1]",round(100*mean(invalid),2),"%.\n")
}
model_effects<-bind_rows(effect_rows) |> group_by(scale_id,model) |> mutate(p_holm_exploratory=p.adjust(p,"holm"),holm_family_tests=n()) |> ungroup()
model_interactions<-bind_rows(interaction_rows) |> group_by(scale_id,model) |> mutate(p_holm_exploratory=p.adjust(p,"holm"),holm_family_tests=n()) |> ungroup()
model_probabilities<-bind_rows(prob_rows);model_coefficients<-bind_rows(coef_rows);model_diagnostics<-bind_rows(diagnostic_rows);prediction_context<-bind_rows(prediction_rows)
stopifnot(nrow(model_effects)==405,nrow(model_probabilities)==40,nrow(model_coefficients)==245,
 nrow(model_interactions)==185,
 all(abs(model_probabilities |> filter(scale_id=="ORDINAL5") |> group_by(environment,arm) |> summarise(total=sum(estimate),.groups="drop") |> pull(total)-100)<1e-8))
logit_sensitivity<-inner_join(filter(model_effects,scale_id=="BINARY2",model=="LPM"),filter(model_effects,scale_id=="BINARY2",model=="LOGIT"),
 by=c("scale_id","outcome","contrast_id","factor","left","right","label","environment","arm","comparison_class"),suffix=c("_lpm","_logit")) |>
 mutate(estimate_difference_logit_minus_lpm=estimate_logit-estimate_lpm)
model_method<-list(primary="Binary LOGIT; nominal multinomial for the three observed ordinal categories is fitted by 03_multinomial.R. All five offered options remain in descriptives",
 sensitivity="LPM for binary and the three observed ordinal indicators, with identical design and standardized marginal effects",
 formula="outcome ~ environment * arm * (language_level + toponym_id + origin_id) + orientation",
 parameters_per_model=49,observations_per_scale=7200,post_field=TRUE,preregistered=FALSE,
 variance="sandwich::vcovCL(type='HC1',cadjust=TRUE), clusters reales; ICt49 conservador, 50bloques por contexto",
 cluster_units=list(ordinal=150,binary=100,geography="50por escala, ubicaciones y brazos comparten bloque",historical_binary="50, búsqueda y mitigación dentrobloque",historical_ordinal="50control+50mitigación, olas separadas; búsqueda dentrobloque"),
 standardization="Uniforme:1/4por entorno,1/2por brazo,1/3por idioma,1/2por topónimo,1/3por origen y1/2por orientación. Contextos especificados se fijan, los demás factores se promedian",
 correction="Holm exploratorio sobre81contrastes de cada modelo binario;243contrastes de tres outcomesordinales juntos. Cambios de brecha por mitigación: familia separada37binarios/111ordinales. IC95%puntuales, no simultáneos. No preregistrado",
 assumptions=c("Efectos aditivos de idioma/topónimo/origen dentrocadaentorno×brazo: no se estiman sus interacciones entre sí",
 "Efecto de orientación común aditivo; controlajustado,no causalidadeindividual", "Cluster sandwich asintótico con t49; no equivalencia ni sesgocero por no significación"),
 limitations=c("LPM puede producir predicciones individuales fuera[0,1]; se muestran diagnósticos. Efectosmedios, no clasificadorcalibrado",
 "Diseño balanceado: ajustar otros factores deja muchas diferencias cercanas a las descriptivas, no significa eliminar sesgos",
 "Entorno compuesto: NO_WEB,históricoWEB/US,nuevoAR,nuevoGB. No factorialubicación×búsqueda completo",
 "AR−GB contemporáneo es contraste experimental deparámetro; comparaciónAR/GB−históricos confundeubicación conola/horario/índice",
 "Mitigaciónordinalhistórica se ejecutó en olas separadas; su comparación no es concurrente",
 "Buscar,fuentes,citas y explicación son outcomes postratamiento: no controles delmodelo",
 "No concluye verdadjurídica,identidadreal,preferenciasdelusuario,complacencia ni generalización aGoogle/ChatGPTweb"))
quality<-list(total_cases=14400,per_scale=7200,conditions=288,per_condition=50,pilots=0,missing=0,
 raw_accepted_false_preserved=sum(!d$raw_accepted),metadata_flagged=sum(d$metadata_flagged),recovered=sum(d$recovered),
 geography_cases=7200,historical_cases=7200,web=10800,no_web=3600,
 ordinal_extreme_categories_zero=TRUE,clusters_ordinal=150,clusters_binary=100,
 network_used=FALSE,credential_read=FALSE,source_public_hash_verified=TRUE,
 selection_scope="14,400 selected valid official responses; 26 additional historical attempts are documented separately")
provenance<-list(source_csv=source_csv,source_csv_sha256=sha(source_csv),
 public_data_manifest=manifest_path,public_data_manifest_sha256=sha(manifest_path),
 script=script,script_sha256=sha(script),
 adaptation="Audited R94 statistical calculations retained; public source integrity replaces private inventory/raw gate")
result<-list(schema_version=1,title="Análisis integral de14.400consultas oficiales",provenance=provenance,quality=quality,labels=labels,
 overall=overall,context_summary=context_summary,factor_aggregates=factor_aggregates,factor_context=factor_context,
 conditions288=conditions288,toponym_percentages=toponym_percentages,outcome_distribution=outcome_distribution,
 chronology=chronology,clusters=clusters,parameters=parameters,model_method=model_method,
 model_effects=model_effects,model_interactions=model_interactions,model_probabilities=model_probabilities,model_coefficients=model_coefficients,
 model_diagnostics=model_diagnostics,prediction_context=prediction_context,logit_sensitivity=logit_sensitivity)
stopifnot(sha(source_csv)==provenance$source_csv_sha256)
compare_table <- function(actual, expected, label, tolerance=1e-7) {
  stopifnot(identical(names(actual),names(expected)),nrow(actual)==nrow(expected))
  for (nm in names(actual)) {
    x<-actual[[nm]];y<-expected[[nm]]
    if(is.numeric(x) && is.numeric(y)) {
      stopifnot(identical(is.na(x),is.na(y)))
      good<-!is.na(x)
      if(any(abs(x[good]-y[good])>tolerance*(1+abs(y[good]))))
        stop("Numeric mismatch in ",label," / ",nm)
    } else if(!identical(as.character(x),as.character(y))) stop("Mismatch in ",label," / ",nm)
  }
}

tables <- c("overall","context_summary","factor_aggregates","factor_context","conditions288","toponym_percentages","outcome_distribution","chronology","clusters","model_effects","model_interactions","model_probabilities","model_coefficients","model_diagnostics","prediction_context","logit_sensitivity")
targets <- setNames(paste0("output/tables/experimento14400_",tables,"_17102026.csv"),tables)
json_target <- "output/tables/experimento14400_resultados_17102026.json"
session_target <- "output/tables/experimento14400_session_info_17102026.txt"
if(check_only) {
  stopifnot(all(file.exists(c(targets,json_target))))
  saved<-fromJSON(json_target)
  for(nm in tables) {
    compare_table(result[[nm]],read.csv(targets[[nm]],check.names=FALSE,na.strings=c("","NA")),nm)
    compare_table(result[[nm]],saved[[nm]],paste0("JSON:",nm))
  }
  stopifnot(saved$provenance$source_csv_sha256==sha(source_csv))
  cat("CHECK_OK: recalculated descriptives and models match saved outputs; no files written.\n")
} else {
  if(any(file.exists(c(targets,json_target,session_target))))
    stop("Outputs already exist; use --check. No output is overwritten.")
  dir.create("output/tables",recursive=TRUE,showWarnings=FALSE)
  for(nm in tables)write.csv(result[[nm]],targets[[nm]],row.names=FALSE,na="",fileEncoding="UTF-8")
  writeLines(json(result),json_target,useBytes=TRUE)
  packages<-c("jsonlite","dplyr","tidyr","digest","sandwich")
  writeLines(c(R.version.string,paste("Platform:",R.version$platform),
    "Package versions (paths intentionally omitted):",
    paste(packages,vapply(packages,function(p)as.character(packageVersion(p)),character(1)))),session_target)
  cat("ANALYSIS_OK: 16 descriptive/model CSVs and public result JSON generated offline.\n")
}
