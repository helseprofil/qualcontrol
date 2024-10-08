% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_all_combinations}
\alias{get_all_combinations}
\title{get_all_combinations}
\usage{
get_all_combinations(dt, columns)
}
\arguments{
\item{dt}{a data.table}

\item{columns}{character vector of the columns to identify combinations}
}
\value{
data.table containing all possible combinations of selected variables
}
\description{
Find all combinations of unique levels of selected columns in a dataset.
Can be used for rectangularization, as it returns a data.table with all possible
combinations.
}
\examples{
# get_all_combinations(newcube, c("KJONN", "ALDER", "UTDANN"))
}
\keyword{internal}
