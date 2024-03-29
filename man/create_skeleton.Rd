% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_skeleton.R
\name{create_skeleton}
\alias{create_skeleton}
\title{Create a system file skeleton in the specified path}
\usage{
create_skeleton(path, overwrite = FALSE)
}
\arguments{
\item{path}{\code{Character} value, path to the directory where the
system files for the package will be installed.}

\item{overwrite}{\code{Logical} value, option to overwrite existing
system files in the target directory. This is basically a prevention
measure against unwanted data loss. Default is \code{FALSE}.}
}
\description{
The function creates a set of empty ASCII files which will be used
by the package for further workflows.
}
\examples{

\dontrun{

create_skeleton(path = "/home/username/systemfiles/eseisManager/")

}

}
\author{
Michael Dietze
}
\keyword{eseis}
