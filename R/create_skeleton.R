#' Create a system file skeleton in the specified path
#'
#' The function creates a set of empty ASCII files which will be used
#' by the package for further workflows.
#'
#' @param path \code{Character} value, path to the directory where the
#' system files for the package will be installed.
#'
#' @param overwrite \code{Logical} value, option to overwrite existing
#' system files in the target directory. This is basically a prevention
#' measure against unwanted data loss. Default is \code{FALSE}.
#'
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#'
#' \dontrun{
#'
#' create_skeleton(path = "/home/username/systemfiles/eseisManager/")
#'
#' }
#'
#' @export create_skeleton

create_skeleton <- function(

  path,
  overwrite = FALSE

) {

  ## check if target directory exists
  if(dir.exists(paths = path) == FALSE) {

    stop("Directory for system files does not exist!")
  }

  ## create batteries file
  if(overwrite == FALSE & file.exists(paste0(path, "/batteries.txt"))) {

    stop("Cannot overwrite batteries.txt")
  } else {

    writeLines(text = "type,ID,UID,date_purchased,capacity,comment",
               con = paste0(path, "/batteries.txt"))
  }

  ## create commits file
  if(overwrite == FALSE & file.exists(paste0(path, "/commits.txt"))) {

    stop("Cannot overwrite commits.txt")
  } else {

    writeLines(text = c(paste0("ID\tdate\tname\tproject\tdate_start\tdate_stop",
                            "\ttype_station\tid_station\tuid_sensor\tuid_logg",
                            "er\ttype_sensor\ttype_logger\tBOB\tuid_battery\t",
                            "check_include"),
                        paste0("00_hello\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA",
                               "\tNA\tNA\tNA\tNA\tNA")),
               con = paste0(path, "/commits.txt"))
  }

  ## create loggers file
  if(overwrite == FALSE & file.exists(paste0(path, "/loggers.txt"))) {

    stop("Cannot overwrite loggers.txt")
  } else {

    writeLines(text = c(paste0("type,ID,UID,date_purchased,capacity,comment"),
                        paste0("NA,NA,NA,NA,NA,")),
               con = paste0(path, "/loggers.txt"))
  }

  ## create names file
  if(overwrite == FALSE & file.exists(paste0(path, "/names.txt"))) {

    stop("Cannot overwrite names.txt")
  } else {

    writeLines(text = paste0(""),
               con = paste0(path, "/names.txt"))
  }

  ## create projects file
  if(overwrite == FALSE & file.exists(paste0(path, "/projects.txt"))) {

    stop("Cannot overwrite projects.txt")
  } else {

    writeLines(text = c(paste0("name,running_title,lat,lon, startdate"),
                        paste0("Storage,Items in storage room,",
                               "51.556459,9.948074,0000-01-01,"),
                        paste0("Repair,Items send in for repair,",
                               "51.536333,9.925588,0000-01-01,"),
                        paste0("Graveyard,Items no longer functioning,",
                               "51.550815,9.922997,0000-01-01")),
               con = paste0(path, "/projects.txt"))
  }

  ## create sensors file
  if(overwrite == FALSE & file.exists(paste0(path, "/sensors.txt"))) {

    stop("Cannot overwrite sensors.txt")
  } else {

    writeLines(text = c(paste0("type,ID,UID,date_purchased,comment"),
                        paste0("NA,NA,NA,NA,")),
               con = paste0(path, "/sensors.txt"))
  }
}

