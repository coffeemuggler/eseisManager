#' Check activity commits for validity
#'
#' The function screens a commit log for potential validity violations. The
#' identified issues are returned.
#'
#' The following tests are performed:
#'
#' @param commits \code{Character} value, file name of the commit log file
#' to screen.
#'
#' @return \code{Character} vector with identified issues.
#'
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#'
#' \dontrun{
#' # Start the GUI
#' check_validity(commits = "/path/to/sourcefiles/commits.txt")
#' }
#'
#' @export check_validity

check_validity <- function(

  commits
) {

  ## check/set commits file
  if(missing(commits) == TRUE) {

    commits <- "commits.txt"

    if(file.exists(commits) == FALSE) {

      stop("No commits file found! Check path and file name.")
    }
  }

  ## import commit log file
  commits <- read.table(file = commits,
                        header = TRUE,
                        sep = "\t",
                        stringsAsFactors = FALSE)

  ## initiate new log file
  log <- character(length = 0)

  ## identify doublettes
  entry_duplicate <- duplicated(x = commits[,-1])

  ## update log file
  if(sum(entry_duplicate) > 0) {

    log <- c(log, paste0("Entry ",
                         commits$ID[entry_duplicate],
                         ": duplicated entry"))
  }

  ## check for new commits dummy entry ----------------------------------------
  if(commits[1,1] == "00_hello") {
    commits <- commits[-1,]
  }

  ## process data set station-wise --------------------------------------------

  ## get unique station IDs
  station_unique <- unique(commits$id_station)

  ## remove empty station IDs
  station_unique <- station_unique[nchar(station_unique) > 0]

  ## loop through all stations
  for(i in 1:length(station_unique)) {

    ## isolate station of focus
    station_i <- commits[which(commits$id_station == station_unique[i]),]

    ## sort entries by start date
    station_i_sort <- station_i[order(station_i$date_start),]

    ## identify potential time flips
    station_i_sort_flip <- station_i$date_start > station_i$date_stop

    ## update log file
    if(sum(station_i_sort_flip) > 0) {

      log <- c(log, paste0("Entry ",
                           station_i_sort$ID[station_i_sort_flip],
                           ": start and stop time mismatch (station-wise)"))
    }

    ## get gaps between entries
    if(nrow(station_i_sort) > 1) {

      station_i_sort_gaps <-
        difftime(time1 = as.Date(station_i_sort$date_start[-1]),
                 time2 = as.Date(station_i_sort$date_stop[-nrow(station_i_sort)]),
                 units = "days")

      ## update log file
      if(sum(station_i_sort_gaps) > 0) {

        log <- c(log, paste0("Entry ",
                             station_i_sort$ID[c(FALSE, station_i_sort_gaps > 0)],
                             ": positive gap between stop and subsequent start time (station-wise)"))
      }

      if(sum(station_i_sort_gaps) < 0) {

        log <- c(log, paste0("Entry ",
                             station_i_sort$ID[c(FALSE, station_i_sort_gaps < 0)],
                             ": negative gap between stop and subsequent start time (station-wise)"))
      }
    }
  }

  ## process data set logger-wise --------------------------------------------

  ## get unique station IDs
  logger_unique <- unique(commits$uid_logger)

  ## loop through all loggers
  for(i in 1:length(logger_unique)) {

    ## isolate logger of focus
    logger_i <- commits[which(commits$uid_logger == logger_unique[i]),]

    ## sort entries by start date
    logger_i_sort <- logger_i[order(logger_i$date_start),]

    ## identify potential time flips
    logger_i_sort_flip <- logger_i$date_start > logger_i$date_stop

    ## update log file
    if(sum(logger_i_sort_flip) > 0) {

      log <- c(log, paste0("Entry ",
                           logger_i_sort$UID[logger_i_sort_flip],
                           ": start and stop time mismatch (logger-wise)"))
    }

    ## get gaps between entries
    if(nrow(logger_i_sort) > 1) {

      logger_i_sort_gaps <-
        difftime(time1 = as.Date(logger_i_sort$date_start[-1]),
                 time2 = as.Date(logger_i_sort$date_stop[-nrow(logger_i_sort)]),
                 units = "days")

      ## update log file
      if(sum(logger_i_sort_gaps) > 0) {

        log <- c(log, paste0("Entry ",
                             logger_i_sort$UID[c(FALSE, logger_i_sort_gaps > 0)],
                             ": positive gap between stop and subsequent start time (logger-wise)"))
      }

      if(sum(logger_i_sort_gaps) < 0) {

        log <- c(log, paste0("Entry ",
                             logger_i_sort$UID[c(FALSE, logger_i_sort_gaps < 0)],
                             ": negative gap between stop and subsequent start time (logger-wise)"))
      }
    }
  }

  ## generate screen message
  if(length(log) == 0) {

    print("No issues identified.")
  } else {

    ## extract entry IDs
    entry_nr <- do.call(c, lapply(X = log, FUN = function(x) {

      y <- strsplit(x = x, split = ":")[[1]][1]
      y <- strsplit(x = y, split = " ")[[1]][2]
      return(as.numeric(y))
    }))

    ## sort log file by entry IDs
    log <- log[order(entry_nr)]

    ## remove excluded items
    entry_omit <- unique(commits$ID[commits$check_include == FALSE])

    if(length(entry_omit) > 0) {

      for(i in 1:length(entry_omit)) {

        log[grepl(pattern = entry_omit[i], x = log)] <- character(length = 1)
      }
    }

    if(sum(nchar(log)) < 1) {

      log = character(length = 0)

      print("No issues identified.")
    } else {

      warning(paste0(length(log), " issues identified! See log file."))
    }

  }

  ## return output
  return(return(log))
}
