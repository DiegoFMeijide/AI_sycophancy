#!/usr/bin/env Rscript
# Public offline descriptive replication for the complete 14,400-response exercise.
# Counts, percentages, treatment cells, chronology and block membership only.
# Run from the repository root; --check does not write files.
suppressPackageStartupMessages({library(jsonlite);library(dplyr);library(tidyr);library(digest)})
options(stringsAsFactors=FALSE,scipen=999)
args<-commandArgs(trailingOnly=TRUE);stopifnot(length(args)==0L||identical(args,"--check"));check_only<-identical(args,"--check")
script<-"code/R/02_descriptives.R"
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
result<-list(overall=overall,context_summary=context_summary,factor_aggregates=factor_aggregates,
 factor_context=factor_context,conditions288=conditions288,toponym_percentages=toponym_percentages,
 outcome_distribution=outcome_distribution,chronology=chronology,clusters=clusters)
stopifnot(sha(source_csv)==expected_source_hash)
compare_table<-function(actual,expected,label,tolerance=1e-10){
 stopifnot(identical(names(actual),names(expected)),nrow(actual)==nrow(expected))
 for(nm in names(actual)){
  x<-actual[[nm]];y<-expected[[nm]]
  if(!identical(is.na(x),is.na(y)))stop("Missingness mismatch in ",label," / ",nm)
  if(is.numeric(x)&&is.numeric(y)){
   keep<-!is.na(x)
   if(any(abs(x[keep]-y[keep])>tolerance*(1+abs(y[keep]))))stop("Numeric mismatch in ",label," / ",nm)
  }else if(!identical(as.character(x),as.character(y)))stop("Value mismatch in ",label," / ",nm)
 }
}
tables<-c("overall","context_summary","factor_aggregates","factor_context","conditions288","toponym_percentages","outcome_distribution","chronology","clusters")
targets<-setNames(paste0("output/tables/experimento14400_",tables,"_17102026.csv"),tables)
json_target<-"output/tables/experimento14400_descriptivos_17102026.json"
session_target<-"output/tables/experimento14400_descriptivos_session_info_17102026.txt"
# Existing identical descriptive CSVs may be retained; no file is overwritten.
for(nm in tables)if(file.exists(targets[[nm]]))
 compare_table(result[[nm]],read.csv(targets[[nm]],check.names=FALSE,na.strings=c("","NA")),nm)
if(check_only){
 stopifnot(all(file.exists(c(targets,json_target,session_target))))
 saved<-fromJSON(json_target,simplifyVector=FALSE)
 stopifnot(saved$source_sha256==sha(source_csv),saved$script_sha256==sha(script),
  saved$total_responses==14400L,saved$conditions==288L)
 for(path in names(saved$output_sha256))stopifnot(sha(path)==saved$output_sha256[[path]])
 cat("CHECK_OK: nine descriptive tables recalculated; no files written.\n")
}else{
 if(any(file.exists(c(json_target,session_target))))stop("Descriptive metadata already exists; use --check.")
 dir.create("output/tables",recursive=TRUE,showWarnings=FALSE)
 for(nm in tables)if(!file.exists(targets[[nm]]))
  write.csv(result[[nm]],targets[[nm]],row.names=FALSE,na="",fileEncoding="UTF-8")
 metadata<-list(source=source_csv,source_sha256=sha(source_csv),script=script,script_sha256=sha(script),
  total_responses=14400,per_scale=7200,conditions=288,per_condition=50,
  output_sha256=as.list(setNames(vapply(targets,sha,character(1)),targets)))
 writeLines(json(metadata),json_target,useBytes=TRUE)
 packages<-c("jsonlite","dplyr","tidyr","digest")
 writeLines(c(R.version.string,paste("Platform:",R.version$platform),
  "Package versions (paths intentionally omitted):",
  paste(packages,vapply(packages,function(p)as.character(packageVersion(p)),character(1)))),session_target)
 cat("DESCRIPTIVES_OK: nine CSVs and descriptive-only metadata; no files overwritten.\n")
}
