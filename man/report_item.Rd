% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_item.R
\name{report_item}
\alias{report_item}
\title{Generate a report on a selected item of the seismic pool}
\usage{
report_item(
  item = "station",
  ID,
  filename = "report",
  browser = TRUE,
  zoom = 15,
  sourcefiles = ""
)
}
\arguments{
\item{item}{\code{Character} value, item about which to report, one
out of \code{station}, \code{sensor}, \code{logger}.}

\item{ID}{\code{Character} value, ID of the item to report on.}

\item{filename}{\code{Character} value, name of the output file (without
extension). Default is \code{"report"}.}

\item{browser}{\code{Logical} value, option to open report in broser.
Default is \code{TRUE}.}

\item{zoom}{\code{Numeric} value, zoom level for map of item location.
Default is 15.}

\item{sourcefiles}{\code{Character} value, path to the directory that
contains the source files of the data base. See details for further
information.}
}
\value{
HTML document and data frame with requested information.
}
\description{
Details, later.
}
\examples{

\dontrun{
report_item(item = "station", ID = "HID01")
}

}
\author{
Michael Dietze
}
\keyword{eseis}
