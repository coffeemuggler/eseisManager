#' Create source files backup
#'
#' The function creates a compressed backup version of the source
#' files.
#'
#' It is recommended to run this function after new commits have been
#' added.
#'
#' @param path_save \code{Character} value, path where the backup archive
#' will be saved. By default, the data is saved in
#' \code{Documents/projects/Environmental_seismology/00_stationtracker/backup}
#'
#' @param sourcefiles \code{Character} value, path to the directory that
#' contains the source files of the data base. If omitted, it is assumed
#' that the source files are present in the extdata directory of the R
#' package.
#'
#' @return A zip file with source files backup
#'
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#'
#' \dontrun{
#' create_backup()
#' }
#'
#' @export create_backup

create_backup <- function(

  path_save,
  sourcefiles
) {

  ## check/set commits file
  if(missing(path_save) == TRUE) {

    path_save <- paste0("~/Downloads")
  }

  ## check/set sourcefiles
  if(missing(sourcefiles) == TRUE) {

    ## find path to source files
    sourcefiles <- readLines(paste0(con = system.file(
      "extdata", package = "eseisManager"), "/data/path.txt"))

    print(paste("Using default sourcefile directory at:", sourcefiles))
  }

  ## list files
  files_backup <- list.files(path = sourcefiles, full.names = TRUE)

  ## create backup archive
  utils::zip(zipfile = paste0(path_save, "/",
                       format(Sys.time(), "%Y-%m-%d_%H:%M:%S"),
                       "_backup_eseisManager_sourcefiles.zip"),
      files = files_backup,
      extras = '-j')
}

