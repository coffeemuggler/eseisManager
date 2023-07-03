#' Add an item to the pool of sensors, loggers, batteries
#'
#' The function adds an entry to the ASCII files which will be used
#' by the package for further workflows.
#'
#' @param item \code{Character} value, item to be added. Must be one of
#' \code{"sensor"}, \code{"logger"}, \code{"battery"}.
#'
#' @param ID \code{Character} value, optional manufacturer ID or label of the
#' item to add. IDs will not be used by 'eseisManager' for object
#' identification but only be shown in reports.
#'
#' @param UID \code{Character} value, mandatory unique identifier of the item
#' to add. UIDs will be used by 'eseisManager' to identify objects.
#'
#' @param type \code{Character} value, mandatory information on the type of
#' sensor, logger or battery. Entry will be used in pull down menu of the
#' GUI. Hence, the types of items should be named consistently.
#'
#' @param date \code{POSIXct} or \code{Character} value, date when
#' the item was added to the pool. Character strings will be converted to
#' POSIXct format if possible. It is sufficient to define year, month and
#' day.
#'
#' @param capacity \code{Character} value, depending on the type of item,
#' this information can have different meanings. For \code{item = "sensor"}
#' capacity will be ignored. For \code{item = "logger"} it will imply the
#' logger's storage capacity (e.g. \code{"64 GB"}). For \code{item = "battery"}
#' it will imply the battery's capacity (e.g. \code{"200 Ah"}).
#'
#' @param comment \code{Character} string, optional comment line which will
#' be stored in the system text file but not be tracked or used by the
#' package reports.
#'
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#'
#' \dontrun{
#'
#' add_item(item = "battery",
#'          ID = "PowerCell",
#'          UID = "1999-01-01-08154712",
#'          type = "AGM",
#'          date = "1999-01-01",
#'          capacity = "55 Ah",
#'          comment = "dummy entry")
#'
#' }
#'
#' @export add_item

add_item <- function(

  item,
  ID,
  UID,
  type,
  date,
  capacity,
  comment

) {

  ## check item keyword
  if(item %in% c("sensor", "logger", "battery") == FALSE) {

    stop("Keyword for item not supported!")
  }

  ## check combination of sensor and capacity attributes
  if(item == "sensor" & missing(capacity) == FALSE) {

    warning("Information on capacity will be ignored for sensors!")

    capacity <- "NA"
  } else if(missing(capacity) == TRUE) {

    capacity <- "NA"
  }

  ## add empty comment input if item is missing
  if(missing(comment) == TRUE) {

    comment <- NULL
  }

  ## format possible POSIXct data
  if(class(date)[1] == "POSIXct") {

    date <- format(x = date, format = "%Y-%m-%d")
  }

  ## define path to system files
  path_files <- readLines(paste0(con = system.file("extdata",
                                                    package = "eseisManager"),
                                  "/data/path.txt"))

  ## read system file, build and add new line entry
  if(item == "sensor") {
    x <- readLines(con = paste0(path_files, "sensors.txt"))

    if(any(grepl(x = x, pattern = UID, fixed = TRUE))) {
      stop("UID already exists!")
    }

    x <- c(x, paste0(type, ",", ID, ",", UID, ",", date, ",", comment))

    writeLines(text = x, con = paste0(path_files, "sensors.txt"))

  } else if(item == "logger") {
    x <- readLines(con = paste0(path_files, "loggers.txt"))

    if(any(grepl(x = x, pattern = UID, fixed = TRUE))) {
      stop("UID already exists!")
    }

    x <- c(x, paste0(type, ",", ID, ",", UID, ",", date,
                     ",", capacity, ",", comment))

    writeLines(text = x, con = paste0(path_files, "loggers.txt"))

  } else if(item == "battery") {
    x <- readLines(con = paste0(path_files, "batteries.txt"))

    if(any(grepl(x = x, pattern = UID, fixed = TRUE))) {
      stop("UID already exists!")
    }

    x <- c(x, paste0(type, ",", ID, ",", UID, ",", date,
                     ",", capacity, ",", comment))

    writeLines(text = x, con = paste0(path_files, "batteries.txt"))
  }
}

