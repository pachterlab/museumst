% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_manipulation.R
\name{unnest_cat}
\alias{unnest_cat}
\title{Transform the sheet to have one species, method, or language per row}
\usage{
unnest_cat(sheet, col_use, other_cols = NULL)
}
\arguments{
\item{sheet}{The tibble for the spreadsheet.}

\item{col_use}{Column to unnest. Tidyeval is supported.}

\item{other_cols}{Character vector of other columns to include.}
}
\value{
A data frame with one row per category of interest. NAs are removed.
}
\description{
When entering species, method, and programming languages into the spreadsheet,
sometimes one paper or dataset has multiple of those, and those are separated
by a semicolon. This function unnests that and returns a data frame with one
row per category of interest.
}
