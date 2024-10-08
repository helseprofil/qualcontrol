% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_formats.R
\name{tab_output}
\alias{tab_output}
\title{tab_output}
\usage{
tab_output(
  table,
  nosearchcolumns = NULL,
  filter = "top",
  rownames = FALSE,
  dom = "ltpi",
  scrollX = TRUE
)
}
\arguments{
\item{table}{output table}

\item{nosearchcolumns}{vector of columns to disable searching}

\item{filter}{Where to put the filter, defaults to "top", but can be "bottom" or "none"}

\item{rownames}{Should rownames be printed? Defaults to FALSE}

\item{dom}{Control the table's layout (e.g., 'lfrtip' for length, filter, records,
table, info, pagination). Defaults to "ltpi"}

\item{scrollX}{Enable vertical scrolling, defaults to TRUE}
}
\value{
datatable
}
\description{
Formats output tables using \code{\link[DT:datatable]{DT::datatable()}}.By default, all
columns is searchable unless mentioned as a nosearchcolumn.
}
\keyword{internal}
