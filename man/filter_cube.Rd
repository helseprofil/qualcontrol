% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{filter_cube}
\alias{filter_cube}
\title{filter_cube}
\usage{
filter_cube(cube.new, cube.old, dimtable, filter = c("new", "old"))
}
\arguments{
\item{cube.new}{new file}

\item{cube.old}{old file}

\item{dimtable}{table generated with \code{\link[=compare_dimensions]{compare_dimensions()}}}

\item{filter}{"new" or "old", indicating whether the file to filter is the new or old file}
}
\description{
Filters out new and expired levels in new and old file, respectively, for comparison.

If filter = "new", rows with new levels are removed from cube.new.
If filter = "old", rows with expired levels are removed from cube.old
}
