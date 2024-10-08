% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_timeseries.R
\name{plot_timeseries}
\alias{plot_timeseries}
\title{plot_timeseries}
\usage{
plot_timeseries(dt = newcube_flag, onlynew = TRUE, change = FALSE, save = TRUE)
}
\arguments{
\item{dt}{flagged data file, default to newcube_flag}

\item{onlynew}{should only new outliers be plotted? Default = TRUE}

\item{change}{Should year-to-year changes be plotted? Default = FALSE}

\item{save}{Should plots be saved to the default folder? Default = T}
}
\value{
saved plots
}
\description{
Generate timeseries plots indicating outliers (values outside q25-1.5IQR, q75+1.5IQR).
}
