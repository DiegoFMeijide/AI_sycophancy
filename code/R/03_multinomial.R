#!/usr/bin/env Rscript
# Public, offline adaptation of audited R98; original multinomial mathematics retained.
# Primary nominal model for the three observed ordinal response categories.
# Model choice was post-field/post-diagnostic, not preregistered.
suppressPackageStartupMessages({library(jsonlite);library(dplyr);library(digest);library(nnet)})
options(stringsAsFactors=FALSE,scipen=999)
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==0L||identical(args,"--check"));check_only<-identical(args,"--check")
script<-"code/R/03_multinomial.R"
csv_path<-"data/analysis_14400.csv"
previous_path<-"output/tables/experimento14400_resultados_17102026.json"
sha<-function(p)digest(file=p,algo="sha256",serialize=FALSE)
readj<-function(p)fromJSON(p,simplifyVector=FALSE)
json<-function(x)as.character(toJSON(x,auto_unbox=TRUE,dataframe="rows",na="null",null="null",digits=15,pretty=TRUE))
manifest_path <- "data/provenance.json"
manifest <- readj(manifest_path)
expected_source_hash <- manifest$output_sha256[["analysis_14400.csv"]]
stopifnot(is.character(expected_source_hash),length(expected_source_hash)==1L,
 sha(csv_path)==expected_source_hash,file.exists(previous_path))
previous<-readj(previous_path)
stopifnot(previous$provenance$source_csv_sha256==sha(csv_path),previous$quality$total_cases==14400L,
 previous$quality$conditions==288L,previous$quality$pilots==0L)
d<-read.csv(csv_path,check.names=FALSE,colClasses=c(call_id="character",source_call_id="character",r="character"))
z<-filter(d,scale_id=="ORDINAL5");z$environment<-z$location
z$cluster_id<-ifelse(z$cohort=="GEOGRAPHY",paste("GEO_ORDINAL",z$source_block_number,sep="_"),
 paste("HIST_ORDINAL",z$arm,z$source_block_number,sep="_"))
z$y<-factor(ifelse(z$arg==1,"ARG",ifelse(z$balanced==1,"BALANCED","GBR")),levels=c("GBR","BALANCED","ARG"))
factor_levels<-list(environment=c("HIST_NO_WEB","HIST_DEFAULT","AR","GB"),arm=c("CONTROL","MITIGATION"),
 language_level=c("EN","ES_NEUTRAL","ES_AR"),toponym_id=c("FALKLANDS","MALVINAS"),
 origin_id=c("NONE","BUENOS_AIRES","LONDON"),orientation=c("ARG_FIRST","GBR_FIRST"))
as_design<-function(x){for(f in names(factor_levels))x[[f]]<-factor(x[[f]],levels=factor_levels[[f]]);x}
z<-as_design(z);grid<-as_design(expand.grid(factor_levels,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE))
stopifnot(nrow(z)==7200,n_distinct(z$cluster_id)==150,!any(z$choice_id %in% c("ARG_CLEAR","GBR_CLEAR")),nrow(grid)==288)
form<-y~environment*arm*(language_level+toponym_id+origin_id)+orientation
cat("GATE_OK:7.200ordinales,3categorías observadas,150bloques. Ajustando multinomial...\n")
fit<-multinom(form,data=z,Hess=TRUE,trace=FALSE,MaxNWts=10000,maxit=5000,abstol=1e-10,reltol=1e-12)
X<-model.matrix(delete.response(terms(fit)),z,contrasts.arg=fit$contrasts,xlev=fit$xlevels)
B<-coef(fit);classes<-rownames(B);outcomes<-fit$lev
stopifnot(ncol(X)==49,qr(X)$rank==49,identical(classes,c("BALANCED","ARG")),identical(outcomes,c("GBR","BALANCED","ARG")),
 nrow(B)==2,all(is.finite(B)),fit$convergence==0)
parameter_names<-unlist(lapply(classes,function(k)paste(k,colnames(X),sep="::")),use.names=FALSE)
beta<-setNames(as.vector(t(B)),parameter_names)
softmax<-function(x,b=beta){etas<-x%*%t(matrix(b,nrow=2,byrow=TRUE));eta<-cbind(0,etas)
 eta<-eta-apply(eta,1,max);p<-exp(eta);p<-p/rowSums(p);colnames(p)<-outcomes;p}
P<-softmax(X);Y<-sapply(outcomes,function(k)as.numeric(z$y==k))
stopifnot(max(abs(P-as.matrix(fitted(fit))))<1e-10,all(P>=0),all(P<=1),max(abs(rowSums(P)-1))<1e-12)
scores<-do.call(cbind,lapply(classes,function(k)X*as.numeric(Y[,k]-P[,k])))
colnames(scores)<-parameter_names
K<-length(beta);N<-nrow(X);G<-n_distinct(z$cluster_id)
H<-matrix(0,K,K,dimnames=list(parameter_names,parameter_names))
for(j in seq_along(classes))for(k in seq_along(classes)){
 w<-P[,classes[j]]*(as.numeric(j==k)-P[,classes[k]])
 H[((j-1)*49+1):(j*49),((k-1)*49+1):(k*49)]<-crossprod(X,X*w)
}
hessian_max_error<-max(abs(H-fit$Hessian));hessian_relative_error<-hessian_max_error/max(abs(H))
ev<-eigen(H,symmetric=TRUE,only.values=TRUE)$values
stopifnot(hessian_relative_error<1e-7,qr(H,tol=1e-10)$rank==K,min(ev)>0)
hessian_condition<-max(ev)/min(ev)
stopifnot(hessian_condition<1e10)
cluster_scores<-rowsum(scores,z$cluster_id,reorder=TRUE)
bread<-solve(H);correction<-(G/(G-1))*((N-1)/(N-K))
V<-correction*bread%*%crossprod(cluster_scores)%*%bread;V<-(V+t(V))/2
stopifnot(all(is.finite(V)),all(diag(V)>0),max(abs(V-t(V)))<1e-10)
makeX<-function(g)model.matrix(delete.response(terms(fit)),g,contrasts.arg=fit$contrasts,xlev=fit$xlevels)
predgrad<-function(g,outcome,b=beta){x<-makeX(g);p<-softmax(x,b)
 grad<-unlist(lapply(classes,function(k)colMeans(x*as.numeric(p[,outcome]*(as.numeric(outcome==k)-p[,k])))),use.names=FALSE)
 list(estimate=100*mean(p[,outcome]),grad=100*grad)}
counterfactual<-function(g,f,level){g[[f]]<-factor(rep(level,nrow(g)),levels=factor_levels[[f]]);g}
effect_ci<-function(est,grad){se<-sqrt(max(0,drop(t(grad)%*%V%*%grad)));data.frame(estimate=est,se=se,
 lower=est-qt(.975,49)*se,upper=est+qt(.975,49)*se,p=if(se==0)if(est==0)1 else 0 else 2*pt(-abs(est/se),49),df=49)}
# Verificación numérica central de todos los98 componentes del gradiente de
# probabilidad y del contraste topónimo. No modifica ni vuelve a ajustar datos.
fd<-function(fun,eps=1e-5){vapply(seq_along(beta),function(j){bp<-beta;bm<-beta;bp[j]<-bp[j]+eps;bm[j]<-bm[j]-eps;(fun(bp)-fun(bm))/(2*eps)},numeric(1))}
gradient_checks<-list()
for(outcome in outcomes){
 pg<-predgrad(grid,outcome)
 num<-fd(function(b)predgrad(grid,outcome,b)$estimate)
 leftg<-counterfactual(grid,"toponym_id","MALVINAS");rightg<-counterfactual(grid,"toponym_id","FALKLANDS")
 lg<-predgrad(leftg,outcome);rg<-predgrad(rightg,outcome)
 num_contrast<-fd(function(b)predgrad(leftg,outcome,b)$estimate-predgrad(rightg,outcome,b)$estimate)
 gradient_checks[[outcome]]<-data.frame(outcome=outcome,probability_gradient_max_error=max(abs(pg$grad-num)),
 contrast_gradient_max_error=max(abs(lg$grad-rg$grad-num_contrast)))
}
gradient_checks<-bind_rows(gradient_checks)
stopifnot(max(gradient_checks$probability_gradient_max_error)<1e-6,max(gradient_checks$contrast_gradient_max_error)<1e-6)
old_effects<-bind_rows(previous$model_effects);old_interactions<-bind_rows(previous$model_interactions)
specs<-old_effects |> filter(scale_id=="ORDINAL5",model=="LPM",outcome=="ARG") |>
 select(contrast_id,factor,left,right,label,environment,arm,comparison_class)
stopifnot(nrow(specs)==81)
effect_rows<-list();prob_rows<-list();interaction_rows<-list();prediction_rows<-list()
for(outcome in outcomes){
 ef<-vector("list",nrow(specs))
 for(si in seq_len(nrow(specs))){sp<-specs[si,];g<-grid
  if(sp$environment!="ALL")g<-g[as.character(g$environment)==sp$environment,]
  if(sp$arm!="ALL")g<-g[as.character(g$arm)==sp$arm,]
  left<-predgrad(counterfactual(g,sp$factor,sp$left),outcome);right<-predgrad(counterfactual(g,sp$factor,sp$right),outcome)
  ef[[si]]<-cbind(data.frame(scale_id="ORDINAL5",model="MULTINOMIAL",outcome=outcome),sp,
   data.frame(adjusted_left_pct=left$estimate,adjusted_right_pct=right$estimate,n_clusters=G,standardization_rows=nrow(g)),
   effect_ci(left$estimate-right$estimate,left$grad-right$grad))
 }
 effect_rows[[outcome]]<-bind_rows(ef)
 ip<-old_interactions |> filter(scale_id=="ORDINAL5",model=="LPM",.data$outcome==.env$outcome) |>
  select(environment,contrast_id,factor,label,comparison_class)
 ir<-vector("list",nrow(ip))
 for(si in seq_len(nrow(ip))){sp<-ip[si,];g<-grid
  if(sp$environment!="ALL")g<-g[as.character(g$environment)==sp$environment,]
  def<-specs[specs$contrast_id==sp$contrast_id,c("left","right")][1,]
  diffs<-list()
  for(a in factor_levels$arm){ga<-counterfactual(g,"arm",a)
   left<-predgrad(counterfactual(ga,sp$factor,def$left),outcome);right<-predgrad(counterfactual(ga,sp$factor,def$right),outcome)
   diffs[[a]]<-list(estimate=left$estimate-right$estimate,grad=left$grad-right$grad)}
  ir[[si]]<-cbind(data.frame(scale_id="ORDINAL5",model="MULTINOMIAL",outcome=outcome),sp,
   data.frame(control_gap=diffs$CONTROL$estimate,mitigation_gap=diffs$MITIGATION$estimate,n_clusters=G),
   effect_ci(diffs$MITIGATION$estimate-diffs$CONTROL$estimate,diffs$MITIGATION$grad-diffs$CONTROL$grad))
 }
 interaction_rows[[outcome]]<-bind_rows(ir)
 pr<-list();pi<-0L
 for(env in factor_levels$environment)for(a in factor_levels$arm){
  g<-grid[as.character(grid$environment)==env & as.character(grid$arm)==a,];pg<-predgrad(g,outcome);pi<-pi+1
  pr[[pi]]<-cbind(data.frame(scale_id="ORDINAL5",model="MULTINOMIAL",outcome=outcome,environment=env,arm=a,n_standardization_rows=nrow(g)),effect_ci(pg$estimate,pg$grad))
 }
 prob_rows[[outcome]]<-bind_rows(pr)
 prediction_rows[[outcome]]<-z |> mutate(prediction=P[,outcome]) |> group_by(environment,arm) |>
  summarise(n=n(),min_prediction=min(prediction),max_prediction=max(prediction),outside_n=sum(prediction<0|prediction>1),
   outside_pct=100*mean(prediction<0|prediction>1),.groups="drop") |> mutate(scale_id="ORDINAL5",model="MULTINOMIAL",outcome=outcome)
}
model_effects<-bind_rows(effect_rows) |> mutate(p_holm_exploratory=p.adjust(p,"holm"),holm_family_tests=n())
model_interactions<-bind_rows(interaction_rows) |> mutate(p_holm_exploratory=p.adjust(p,"holm"),holm_family_tests=n())
model_probabilities<-bind_rows(prob_rows);prediction_context<-bind_rows(prediction_rows)
coef_df<-data.frame(class=rep(classes,each=49),term=rep(colnames(X),2),coefficient=unname(beta),se=sqrt(diag(V)),row.names=NULL)
coef_df$lower<-coef_df$coefficient-qt(.975,49)*coef_df$se;coef_df$upper<-coef_df$coefficient+qt(.975,49)*coef_df$se
coef_df$p<-2*pt(-abs(coef_df$coefficient/coef_df$se),49)
model_coefficients<-coef_df |> mutate(scale_id="ORDINAL5",model="MULTINOMIAL",baseline="GBR")
stopifnot(nrow(model_effects)==243,nrow(model_interactions)==111,nrow(model_probabilities)==24,nrow(model_coefficients)==98,
 max(abs(model_probabilities |> group_by(environment,arm) |> summarise(total=sum(estimate),.groups="drop") |> pull(total)-100))<1e-8,
 max(abs(model_effects |> group_by(contrast_id,environment,arm) |> summarise(total=sum(estimate),.groups="drop") |> pull(total)))<1e-8)
model_diagnostics<-list(scale_id="ORDINAL5",model="MULTINOMIAL",n=7200,n_clusters=150,ci_df=49,
 n_parameters=98,n_predictors_per_nonbaseline_class=49,convergence_code=fit$convergence,
 log_likelihood=as.numeric(logLik(fit)),aic=AIC(fit),residual_deviance=fit$deviance,
 baseline="GBR",categories=outcomes,offered_categories=5,observed_categories=3,extreme_category_responses=0,
 hessian_rank=qr(H,tol=1e-10)$rank,hessian_min_eigenvalue=min(ev),hessian_max_eigenvalue=max(ev),hessian_condition_number=hessian_condition,
 hessian_nnet_max_absolute_difference=hessian_max_error,hessian_nnet_relative_difference=hessian_relative_error,
 maximum_absolute_score_sum=max(abs(colSums(scores))),min_prediction=min(P),max_prediction=max(P),
 outside_unit_interval_n=sum(P<0|P>1),maximum_probability_sum_error=max(abs(rowSums(P)-1)),
 cr1_correction=correction,gradient_finite_difference_checks=gradient_checks,
 note="Los ICdelta puntuales pueden exceder0/100; las probabilidades ajustadas del modelo siempre están dentro[0,1]")
comparison_lpm<-inner_join(filter(old_effects,scale_id=="ORDINAL5",model=="LPM"),model_effects,
 by=c("scale_id","outcome","contrast_id","factor","left","right","label","environment","arm","comparison_class"),suffix=c("_lpm","_multinomial")) |>
 mutate(estimate_difference_multinomial_minus_lpm=estimate_multinomial-estimate_lpm)
method<-list(principal_ordinal="Multinomial nominal de3categorías observadas ARG/BALANCED/GBR. No supone proporcionalidad deodds ni equidistancia",
 principal_binary="Binary logit from 02_analyze_14400.R, identical covariates/standardization; all LPM models are sensitivity",
 selection_reason="Después del diagnósticoLPM,19,69%depredicciones ordinalARG fuera[0,1], se eligió multinomial como principal. Decisiónpostdiagnóstico,no preregistrada",
 formula="outcome ~ environment * arm * (language_level + toponym_id + origin_id) + orientation",
 class_parameter_order="BALANCED49coef,ARG49coef;GBRbaseline;interceptincluido;98coef",
 estimation="nnet::multinom, máxima verosimilitud sin penalización; Hessiano analítico comprobado contra nnet",
 variance="CR1 manual por150bloques reales: H^-1 (ΣS_gS_g') H^-1 ×[150/149]×[7199/7102]; ICt49. Gradientesdelta comprobados por diferenciasfinitas",
 standardization=previous$model_method$standardization,cluster_units=previous$model_method$cluster_units,
 correction="Holm exploratorio243contrastesordinales; familia separada111cambios de brecha con mitigación. IC95%puntuales,no simultáneos",
 post_field=TRUE,post_diagnostic_model_choice=TRUE,preregistered=FALSE,
 assumptions=c("Aditividad de idioma/topónimo/origen dentrocada entorno×brazo; sin sus interacciones entre sí","Orientación común aditiva en los logits; misma especificación de covariables queR94"),
 limitations=previous$model_method$limitations[!grepl("LPM puede",unlist(previous$model_method$limitations),fixed=TRUE)])
result<-list(schema_version=1,provenance=list(script=script,script_sha256=sha(script),source_csv=csv_path,source_csv_sha256=sha(csv_path),
 previous_analysis=previous_path,previous_analysis_sha256=sha(previous_path)),method=method,
 model_effects=model_effects,model_interactions=model_interactions,model_probabilities=model_probabilities,
 model_coefficients=model_coefficients,model_diagnostics=model_diagnostics,prediction_context=prediction_context,comparison_lpm=comparison_lpm,
 network_used=FALSE,credential_read=FALSE)
stopifnot(sha(csv_path)==expected_source_hash)
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

tables <- c("model_effects","model_interactions","model_probabilities","model_coefficients","prediction_context","comparison_lpm")
targets <- setNames(paste0("output/tables/experimento14400_multinomial_",tables,"_17102026.csv"),tables)
json_target <- "output/tables/experimento14400_multinomial_resultados_17102026.json"
session_target <- "output/tables/experimento14400_multinomial_session_info_17102026.txt"
if(check_only) {
  stopifnot(all(file.exists(c(targets,json_target))))
  saved<-fromJSON(json_target)
  for(nm in tables) {
    compare_table(result[[nm]],read.csv(targets[[nm]],check.names=FALSE,na.strings=c("","NA")),nm)
    compare_table(result[[nm]],saved[[nm]],paste0("JSON:",nm))
  }
  stopifnot(saved$provenance$source_csv_sha256==sha(csv_path))
  cat("CHECK_OK: recalculated multinomial matches saved outputs; no files written.\n")
} else {
  if(any(file.exists(c(targets,json_target,session_target))))
    stop("Outputs already exist; use --check. No output is overwritten.")
  dir.create("output/tables",recursive=TRUE,showWarnings=FALSE)
  for(nm in tables)write.csv(result[[nm]],targets[[nm]],row.names=FALSE,na="",fileEncoding="UTF-8")
  writeLines(json(result),json_target,useBytes=TRUE)
  packages<-c("jsonlite","dplyr","digest","nnet")
  writeLines(c(R.version.string,paste("Platform:",R.version$platform),
    "Package versions (paths intentionally omitted):",
    paste(packages,vapply(packages,function(p)as.character(packageVersion(p)),character(1)))),session_target)
  cat("ANALYSIS_OK: 6 multinomial CSVs and public result JSON generated offline.\n")
}
