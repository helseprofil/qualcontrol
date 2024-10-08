% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barometer.R
\name{check_barometer}
\alias{check_barometer}
\title{check_barometer}
\usage{
check_barometer(
  type = c("FHP", "OVP"),
  year = NULL,
  geo = c("fylke", "kommune", "bydel"),
  indikator = NULL,
  barometer = NULL
)
}
\arguments{
\item{type}{"FHP" or "OVP"}

\item{year}{production year}

\item{geo}{one of "fylke", "kommune", "bydel"}

\item{indikator}{file path indicator}

\item{barometer}{file path barometer}
}
\description{
Checks whether identical values have received a colored point marker in the barometer.
#'
}
\author{
Yusman Kamaleri
}
