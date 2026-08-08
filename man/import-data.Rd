\name{ls_import_data}
\alias{ls_import_data}
\alias{ls_import_spss}
\alias{ls_import_stata}
\alias{ls_import_sas}
\alias{ls_import_excel}
\alias{ls_import_csv}
\alias{ls_import_tsv}
\alias{ls_import_rds}
\alias{ls_import_rdata}
\title{Import external data into the LinkEDA registry}
\usage{
ls_import_data(path = NULL, name = NULL, sheet = NULL, make_active = TRUE)
ls_import_spss(path, name = NULL, make_active = TRUE)
ls_import_stata(path, name = NULL, make_active = TRUE)
ls_import_sas(path, name = NULL, make_active = TRUE)
ls_import_excel(path, sheet = NULL, name = NULL, make_active = TRUE)
ls_import_csv(path, name = NULL, make_active = TRUE, ...)
ls_import_tsv(path, name = NULL, make_active = TRUE, ...)
ls_import_rds(path, name = NULL, make_active = TRUE)
ls_import_rdata(path, name = NULL, make_active = TRUE)
}
\arguments{
\item{path}{Path to a supported data file.}
\item{name}{Optional dataset name.}
\item{sheet}{Excel sheet name or index. If omitted, the first sheet is used.}
\item{make_active}{Logical. If \code{TRUE}, the imported dataset becomes active.}
\item{...}{Additional arguments passed to base text importers.}
}
\description{
Imports \file{.sav}/\file{.zsav} files with \pkg{haven},
\file{.dta}, \file{.sas7bdat}, and \file{.xpt} files with \pkg{haven},
\file{.xls}/\file{.xlsx} files with \pkg{readxl}, \file{.csv},
\file{.txt}/\file{.tsv}, \file{.rds}, \file{.rda}, and \file{.RData}
files, then registers the result in the LinkEDA dataset registry.
}
\value{
Invisibly returns the registered dataset name.
}
