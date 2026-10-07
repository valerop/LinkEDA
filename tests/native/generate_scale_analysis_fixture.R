library(LinkEDA)
set.seed(405); z<-rnorm(80);w<-rnorm(80)
d<-data.frame(item_one=z+rnorm(80),item_two=z+rnorm(80),item_three=w+rnorm(80),item_four=w+rnorm(80),item_five=z+rnorm(80))
d$item_one[seq(1,80,5)]<-NA
imp<-mice::mice(d,m=3,maxit=2,printFlag=FALSE)
id<-ls_import_mice(imp,name='scale_visual_check',make_active=FALSE)
a<-ls_new_scale_analysis(id,items=names(d),dimensionality=TRUE,factors=2,parallel_iterations=2,name='scale_visual',native=FALSE)
r<-LinkEDA:::.rls_dataset_record(id)
writeLines(c('REGISTER_DATASET',id,LinkEDA:::.rls_variable_payload(r$data,r$variable_metadata),LinkEDA:::.rls_dataframe_payload(r$data,r$variable_metadata,dataset_record=r)),'/tmp/scale-dataset.payload')
local({testthat::local_mocked_bindings(.rls_send=function(lines){writeLines(lines,'/tmp/scale-result.payload');TRUE},.package='LinkEDA');LinkEDA:::.rls_scale_sync_native(LinkEDA:::.rls_scale_record(a),open=TRUE)})

# A separate ordinary-data fixture verifies that MI labels never leak into it.
id_plain <- ls_register_dataset("scale_plain_check", d)
a_plain <- ls_new_scale_analysis(id_plain, items=names(d), dimensionality=TRUE,
  factors=2, parallel_iterations=2, name="scale_plain", native=FALSE)
r_plain <- LinkEDA:::.rls_dataset_record(id_plain)
writeLines(c('REGISTER_DATASET',id_plain,LinkEDA:::.rls_variable_payload(r_plain$data,r_plain$variable_metadata),LinkEDA:::.rls_dataframe_payload(r_plain$data,r_plain$variable_metadata,dataset_record=r_plain)), '/tmp/scale-plain-dataset.payload')
local({testthat::local_mocked_bindings(.rls_send=function(lines){writeLines(lines,'/tmp/scale-plain-result.payload');TRUE},.package='LinkEDA');LinkEDA:::.rls_scale_sync_native(LinkEDA:::.rls_scale_record(a_plain),open=TRUE)})
saveRDS(d, "/tmp/scale-verification.rds")
