\name{ls_data_sheet}
\alias{ls_data_sheet}
\alias{ls_open_data_sheet}
\alias{ls_close_data_sheet}
\alias{ls_show_data_sheet}
\alias{ls_data_sheet_active_dataset}
\title{Open or return a registered data sheet}
\usage{
ls_data_sheet(group = NULL)
ls_open_data_sheet(group = NULL)
ls_close_data_sheet(group = NULL)
ls_show_data_sheet(show = TRUE)
ls_data_sheet_active_dataset()
}
\arguments{
\item{group}{Dataset/group name. If omitted, the active dataset is used.}
\item{show}{Logical; show or hide the data sheet.}
}
\description{
Opens or updates the native read-only data sheet for the active registered
dataset. The data sheet shares row selection with linked plots.
}
\value{
\code{ls_data_sheet()} invisibly returns the registered data frame.
}
