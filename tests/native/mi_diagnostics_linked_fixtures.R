# Run from the repository root before mi_diagnostics_menu_smoke with its
# linked-fixtures argument. Outputs are isolated fixtures under /tmp.
pkgload::load_all('.',quiet=TRUE,compile=FALSE,export_all=FALSE)
completed <- lapply(seq_len(3),function(i)data.frame(y=c(5,7,9,11,13,15,17,19)+i*c(.2,0,.1,0,.2,0,.1,0),
  z=c(4,8,7,12,14,13,20,17)+i/10,
  g=factor(rep(c('Control','Treatment'),each=4)),country=factor(rep(c('Finland','Greece'),4))))
id <- ls_register_dataset('mi-linked-fixture',completed[[1]])
data <- LinkEDA:::.rls_dataset_record(id)
data$dataset_type <- 'multiple_imputation';data$completed_datasets <- completed;data$imputation_count <- 3L
data$original_data <- completed[[1]];data$original_row_ids <- 1:8
LinkEDA:::.rls_set_dataset_record(data)
write <- function(payload,name)writeLines(payload,paste0('/tmp/linkeda-',name,'.payload'),useBytes=TRUE)
for(i in 1:2) {
  tab <- ls_new_table1(id,variables=if(i==1)'y' else c('y','z'),group='g',name='linked-table1')
  write(LinkEDA:::.rls_table1_native_payload(LinkEDA:::.rls_table1_record(tab)),paste0('linked-table1-',i))
  ids <- ls_new_one_sample_t_test(id,response='y',mu=if(i==1)5 else 10)
  results <- lapply(ids,LinkEDA:::.rls_compare_means_record)
  write(LinkEDA:::.rls_compare_means_batch_native_payload(results,run_id='linked-means'),paste0('linked-means-',i))
}
cross <- LinkEDA:::.rls_mi_contingency_record(id,c('g','country'),'','linked-cross',display_mode='percent')
write(LinkEDA:::.rls_table1_native_payload(cross),'linked-cross')
