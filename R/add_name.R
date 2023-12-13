#' Add a researcher name to the data base
#'
#' The function adds a name of a potential responsible researcher, an
#' information needed for every new commit.
#'
#' @param name \code{Character} value, given name and family name of the
#' person to add, e.g. \code{"Michael Dietze"}.
#'
#' @param sort \code{Logical} value, option to update the order of names
#' in the list of researchers. Default is \code{TRUE}.
#'
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#'
#' \dontrun{
#'
#' add_name(name = "Heinz Ketchup")
#'
#' }
#'
#' @export add_name

add_name <- function(

  name,
  sort = TRUE

) {

  ## define path to system files
  path_files <- readLines(paste0(con = system.file("extdata",
                                                   package = "eseisManager"),
                                 "/data/path.txt"))

  ## read system file
  x <- readLines(con = paste0(path_files, "names.txt"))

  ## check for duplicates
  if(any(grepl(x = x, pattern = name, fixed = TRUE))) {
    stop("Name already exists!")
  }

  ## add name
  x <- c(x, name)

  ## optionally update name order
  if(sort == TRUE) {

    x <- sort(x)
  }

  ## save updated data set
  writeLines(text = x, con = paste0(path_files, "names.txt"))
}
