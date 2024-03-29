% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_uid.R
\name{create_uid}
\alias{create_uid}
\title{Create a UID (unique identifier) for a seismic pool item.}
\usage{
create_uid(
  label = TRUE,
  path,
  file,
  line1 = "Uni Goettingen, Geogr. Inst.",
  line2 = "Geophysical Device Pool",
  line3,
  line4
)
}
\arguments{
\item{label}{\code{Logical} value, option to create PDF with device
information and QR code to be taped onto the registered item. Default
is \code{TRUE}}

\item{path}{\code{Character} value, optional path to
PDF document with label. If omitted, the file will be created in the
current working directory.}

\item{file}{\code{Character} value, optional file name of the PDF
document to create. If omitted, the ID will be used as file name.}

\item{line1}{\code{Character} value, content of first label line, should
be host institution name. Default is \code{"Uni Goettingen, Geogr. Inst."}.}

\item{line2}{\code{Character} value, content of second label line, should
be organisation unit. Default is \code{"Geophysical Device Pool"}.}

\item{line3}{\code{Character} value, content of third label line, should
be item name. If omitted, a dummy entry will be added:
\code{Item: unknown}.}

\item{line4}{\code{Character} value, content of fourth label line, should
be ID in plane text. If omitted, the ID will be added automatically.}
}
\description{
The function creates a UDI based on the current time stamp and
a 6 digit random number. This UID is supposed to be added to the
base data of each item.
}
\examples{

\dontrun{

create_uid(line3 = "Item: Geophone")

}

}
\author{
Michael Dietze
}
\keyword{eseis}
