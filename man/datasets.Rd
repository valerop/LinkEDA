\name{ls_datasets}
\alias{ls_datasets}
\alias{ls_register_dataset}
\alias{ls_unregister_dataset}
\alias{ls_set_active_dataset}
\alias{ls_active_dataset}
\alias{ls_get_active_dataset}
\alias{ls_refresh_r_dataframes}
\title{Register, list, and activate LinkEDA datasets}
\usage{
ls_datasets()
ls_register_dataset(name, data)
ls_unregister_dataset(name)
ls_set_active_dataset(name)
ls_active_dataset()
ls_get_active_dataset()
ls_refresh_r_dataframes(envir = .GlobalEnv)
}
\arguments{
\item{name}{Dataset name and default linked brushing group id.}
\item{data}{A data frame.}
\item{envir}{Environment scanned for data frames.}
}
\description{
Maintains the R-side dataset registry used by workbench menu commands and
registered plotting workflows.
}
\value{
\code{ls_datasets()} returns a data frame. \code{ls_get_active_dataset()}
returns the active data frame. Other functions invisibly return the affected
dataset name, \code{TRUE}, or the refreshed registry.
}
