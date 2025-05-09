% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tribbelise.R
\name{tribbelise}
\alias{tribbelise}
\title{Recreate a data frame as a tribble}
\usage{
tribbelise(df)
}
\arguments{
\item{df}{A data frame.}
}
\description{
Prints a representation of \code{df} using \code{\link[tibble:tribble]{tribble()}} for
constructing examples or configuration data.
}
\examples{
quitte_example_data |>
    head() |>
    print() |>
    tribbelise()
}
