
library(LinkEDA)
folder <- commandArgs(TRUE)[[1L]];dir.create(folder,recursive=TRUE,showWarnings=FALSE)
build <- function(data,group,id,rows,split,mode) {
  group <- ls_register_dataset(group,data);ds <- LinkEDA:::.rls_dataset_record(group)
  h <- LinkEDA:::.rls_nested_contingency_record(group,rows,split,id,display_mode=mode)
  r <- LinkEDA:::.rls_table1_record(h)
  writeLines(LinkEDA:::.rls_table1_native_payload(r),
    file.path(folder,paste0(id,".payload")),useBytes=TRUE)
  writeLines(c("REGISTER_DATASET",group,LinkEDA:::.rls_variable_payload(ds$data,ds$variable_metadata),
    LinkEDA:::.rls_dataframe_payload(ds$data,ds$variable_metadata,dataset_record=ds)),
    file.path(folder,paste0(id,"-dataset.payload")),useBytes=TRUE)
}
d <- data.frame(group=factor(rep(c("A","B"),each=3)),sex=factor(c("F","M","F","M","M","F")),grade=rep(1:3,2))
build(d,"demo","nested_demo",c("group","sex"),"grade","count")
m <- data.frame(cyl=rep(c("4","4","4","6","6","8","8"),c(1,3,7,3,4,12,2)),
  vs=rep(c("0","1","1","0","1","0","0"),c(1,3,7,3,4,12,2)),am=rep(c("1","0","1","1","0","0","1"),c(1,3,7,3,4,12,2)))
m[] <- lapply(m,factor)
build(m,"mtcars_like","nested_mtcars",c("cyl","vs"),"am","count")
build(m,"mtcars_like","nested_mtcars_count_percent",c("cyl","vs"),"am","count_percent")
missing <- data.frame(row=factor(c("A","A","B","B",NA)),column=factor(c("X",NA,"X","Y","Y")))
build(missing,"missing_table","missing_contingency","row","column","count")
