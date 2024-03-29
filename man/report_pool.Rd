% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_pool.R
\name{report_pool}
\alias{report_pool}
\title{Generate a report on seismic pool status}
\usage{
report_pool(
  filename,
  browser = TRUE,
  sourcefiles,
  interactive = TRUE,
  include_external = FALSE
)
}
\arguments{
\item{filename}{\code{Character} value, name of the output file (without
extension). Default is \code{"report"}, saved in a temporary directory.
Please note that no ~ signs are allowed as path flags.}

\item{browser}{\code{Logical} value, option to open report in browser.
Default is \code{TRUE}.}

\item{sourcefiles}{\code{Character} value, path to the directory that
contains the source files of the data base. See details for further
information.}

\item{interactive}{\code{Logical} value, option to use an interactive map
instead of a static world map. Default is \code{TRUE}.}

\item{include_external}{\code{Logical} value, option to activate/suppress
inclusion of pool-external instruments to the report. Default is
\code{FALSE}.}
}
\value{
HTML document and data frame with requested information.
}
\description{
Details, later.
}
\examples{

\dontrun{
report_pool()
}

}
\author{
Michael Dietze
}
\keyword{eseis}
