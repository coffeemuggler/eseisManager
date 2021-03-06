#' User interface to add new activities to the pool lifeline
#'
#' The function opens a graphical user interface to conveniently update
#' changes to the seismic pool items. The function must be called from
#' the directory which contains the management source files (see details).
#'
#' The function requires a set of predefined source files. These source
#' files must be located in the same directory. The following files are
#' required:
#'
#' \itemize{
#'   \item \code{commits.txt}, ASCII file, tab-separated, containing at
#'   least two lines of (dummy) data. The following column names must be
#'   present: \code{ID}, \code{date}, \code{name}, \code{project},
#'   \code{date_start}, \code{date_stop}, \code{type_station},
#'   \code{id_station}, \code{id_sensor}, \code{id_logger}, \code{type_sensor},
#'   \code{type_logger}, \code{BOB}, \code{type_battery}.
#'   \item \code{projects.txt}, ASCII file, comma separated, containing
#'   information about projects to choose from. The following column names
#'   must be present: \code{name}, \code{running_title}, \code{lat},
#'   \code{lon}.
#'   \item \code{names.txt}, ASCII file with scientist names to choose from,
#'   one per line.
#'   \item \code{sensors.txt},
#'   \item \code{loggers.txt},
#'   \item \code{batteries.txt},
#' }
#'
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#'
#' \dontrun{
#' # Start the GUI
#' gui_input()
#' }
#'
#' @export gui_input

gui_input <- function(

) {

  ## start Shiny app
  app <- runApp(appDir = system.file("shiny/input",
                                     package = "eseisManager"),
                launch.browser = TRUE)
}
