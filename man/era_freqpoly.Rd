% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{era_freqpoly}
\alias{era_freqpoly}
\title{Prequel vs current binned over time}
\usage{
era_freqpoly(
  pubs,
  col_use,
  since_first = FALSE,
  binwidth = 365,
  preprints = TRUE,
  do_smooth = FALSE,
  smooth_method = NULL,
  smooth_formula = NULL
)
}
\arguments{
\item{pubs}{A data frame with at least these columns:
\describe{
  \item{journal}{Name of the journal of the paper.}
  \item{year}{Year when the paper was published.}
}
There must be one row per publication or per method or species for each title
if faceting by those. If facetting, then a column whose name is the value in
`facet_by` must be present.}

\item{col_use}{Which logical variable to plot. Tidyeval is supported.}

\item{since_first}{Logical. Whether to plot days after the first publication
appeared.}

\item{binwidth}{Width of bins for the histogram in days.}

\item{preprints}{Logical, whether preprints should be included. Defaults to
`TRUE` to include preprints.}

\item{do_smooth}{Logical. Whether to plot smoothed curve for the trend rather
than freqpoly.}

\item{smooth_method}{Method of smoothing, passed to \code{geom_smooth}.}

\item{smooth_formula}{Formula of smoothing, passed to \code{geom_smooth}.}
}
\value{
A ggplot2 object
}
\description{
Plots freqpoly for prequel vs current, with an option to start both at the
date when the first publication in the category appeared. The point here is
not to compare to the distribution of everything, like in hist_bool, or to
compare when things started, like when I plotted a histogram of different
methods over time, but to compare the height of the histograms and how steeply
they rise and fall. So I think freqpoly may be better than blocky histograms
for this purposes.
}
