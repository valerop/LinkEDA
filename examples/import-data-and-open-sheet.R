library(LinkEDA)

# SPSS:
# ls_import_data("/path/to/file.sav")
#
# Excel:
# ls_import_data("/path/to/file.xlsx")

ls_register_dataset("cars", mtcars)
ls_set_active_dataset("cars")
ls_data_sheet("cars")
