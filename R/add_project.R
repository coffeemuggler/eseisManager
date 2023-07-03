#' Add a project to the data base
#'
#' The function adds project with meta information to the data base system
#' text file.
#'
#' @param name \code{Character} value, short name or acronym of the project
#' to add
#'
#' @param title \code{Character} value, one line summary of the project,
#' ideally expressing where and what the project is about.
#'
#' @param lat \code{Numeric} value, latitude of the project areas centroid
#'
#' @param lon \code{Numeric} value, longitude of the project areas centroid
#'
#' @param start \code{POSIXct} value, start time of the project. A character
#' string will be (attempted to be) converted to the POSIXct format. It is
#' sufficient to provide year, month and day.
#'
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#'
#' \dontrun{
#'
#' add_project(name = "Earthworms",
#'             title = "Seismic earthworm tracking in Patagonia",
#'             lat = -45
#'             lon = -72,
#'             start = as.POSIXct("1921-04-01"))
#'
#' }
#'
#' @export add_project

add_project <- function(

  name,
  title,
  lat,
  lon,
  start

) {

  ## check/set input parameters
  if(missing(lat)) {lat <- NA}
  if(missing(lon)) {lon <- NA}
  if(missing(start)) {start <- NA}

  ## try to convert time string
  if(class(start)[1] == "POSIXct") {

    start <- format(x = start, format = "%Y-%m-%d")
  }

  ## define path to system files
  path_files <- readLines(paste0(con = system.file("extdata",
                                                   package = "eseisManager"),
                                 "/data/path.txt"))[1]

  ## read system file
  x <- readLines(con = paste0(path_files, "projects.txt"))

  ## check for duplicates
  if(any(grepl(x = x, pattern = name, fixed = TRUE))) {
    stop("Name already exists!")
  }

  ## add project
  x <- c(x, paste0(name, ",", title, ",", lat, ",", lon, ",", start))

  ## save updated data set
  writeLines(text = x, con = paste0(path_files, "projects.txt"))
}
