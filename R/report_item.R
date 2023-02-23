#' Generate a report on a selected item of the seismic pool
#'
#' By default, the source files are stored in the extdata
#' directory of the installed R package.
#'
#' @param item \code{Character} value, item about which to report, one
#' out of \code{station}, \code{sensor}, \code{logger}.
#'
#' @param ID \code{Character} value, ID of the item to report on.
#'
#' @param filename \code{Character} value, name of the output file (without
#' extension).Default is \code{"report"}, saved in current working directory.
#' Please note that no ~ signs are allowed as path flags.
#'
#' @param browser \code{Logical} value, option to open report in browser.
#' Default is \code{TRUE}.
#'
#' @param extent \code{Numeric} vector, extent of the output map. Must be
#' in the order xmin, xmax, ymin, ymax. If omitted, global map will be
#' used.
#'
#' @param sourcefiles \code{Character} value, path to the directory that
#' contains the source files of the data base. See details for further
#' information.
#'
#' @return HTML document and data frame with requested information.
#'
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#'
#' \dontrun{
#' report_item(item = "station", ID = "HID01")
#' }
#'
#' @export report_item

report_item <- function(

  item = "station",
  ID,
  filename,
  browser = TRUE,
  extent,
  sourcefiles

) {

  if(missing(filename) == TRUE) {

    filename <- paste0(getwd(), "/report")
  }

  ## check/set sourcefiles
  if(missing(sourcefiles) == TRUE) {

    sourcefiles <- paste0(system.file("extdata",
                                      package = "eseisManager"),
                          "/data/")

    print(paste("Using default sourcefile directory at:", sourcefiles))
  }

  ## read source files
  names <- readLines(con = paste0(sourcefiles,
                                  "names.txt"),
                     warn = FALSE)
  projects <- utils::read.table(file = paste0(sourcefiles,
                                              "projects.txt"),
                                header = TRUE,
                                sep = ",",
                                stringsAsFactors = FALSE)
  sensors <- utils::read.table(file = paste0(sourcefiles,
                                             "sensors.txt"),
                               header = TRUE,
                               sep = ",",
                               stringsAsFactors = FALSE)
  loggers <- utils::read.table(file = paste0(sourcefiles,
                                             "loggers.txt"),
                               header = TRUE,
                               sep = ",",
                               stringsAsFactors = FALSE)
  batteries <- readLines(con = paste0(sourcefiles,
                                      "batteries.txt"),
                         warn = FALSE)
  commits <- utils::read.table(file = paste0(sourcefiles,
                                             "commits.txt"),
                               header = TRUE,
                               sep = "\t",
                               stringsAsFactors = FALSE)

  ## report on station --------------------------------------------------------
  if(item == "station") {

    ## check if station exists
    if(sum(commits$id_station == ID, na.rm = TRUE) == 0) {

      stop("Station ID not found in commits file!")
    }

    ## isolate station of interest
    commits_focus <- commits[commits$id_station == ID,]

    ## remove NA cases
    commits_focus <- commits_focus[!is.na(commits_focus$id_station),]

    ## sort commits by date
    commits_focus <- commits_focus[order(as.Date(commits_focus$date_start)),]

    ## extract report table
    table_output <- commits_focus[nrow(commits_focus):1,-c(1, 2, 3, 15)]

    ## reorganise columns
    table_output_1 <- table_output[,c(1, 2, 3, 4)]
    table_output_2 <- table_output[,c(5, 6, 7, 8, 9, 10)]

    ## get days in use
    uptime_range <- range(c(as.Date(table_output$date_start),
                            as.Date(table_output$date_stop)))
    uptime_sum <- as.numeric(difftime(time1 = uptime_range[2],
                                      time2 = uptime_range[1],
                                      units = "days"))

    ## make map of deployment
    project_focus <- unique(commits_focus$project)[1]
    project_focus_data <- projects[projects$name == project_focus,]

    ## create station location map
    jpeg(filename = paste0(filename, "_map.jpeg"),
         width = 500,
         height = 500,
         res = 300)

    ## load world map
    map <- try(raster::brick(x = paste0(system.file("extdata",
                                                    package = "eseisManager"),
                                        "/map/blue_marble.tif")),
               silent = TRUE)

    ## update extent
    map_ext <- raster::extent(map)

    if(missing(extent) == FALSE) {

      map_ext[1] <- extent[1]
      map_ext[2] <- extent[2]
      map_ext[3] <- extent[3]
      map_ext[4] <- extent[4]
    }

    ## crop map to new extent
    map <- raster::crop(x = map, y = map_ext)

    ## plot world map and add station points and labels
    raster::plotRGB(map)
    lines(x = project_focus_data$lon,
          y = project_focus_data$lat,
          lwd = 1,
          col = "grey80")
    points(x = project_focus_data$lon,
           y = project_focus_data$lat,
           pch = 20,
           col = grDevices::heat.colors(n = nrow(project_focus_data)),
           cex = 1)

    dev.off()
  }

  ## report on logger ---------------------------------------------------------
  if(item == "logger") {

    ## check if station exists
    if(sum(commits$id_logger == ID, na.rm = TRUE) == 0) {

      stop("Logger ID not found in commits file!")
    }

    ## isolate station of interest
    commits_focus <- commits[commits$id_logger == ID,]

    ## remove NA cases
    commits_focus <- commits_focus[!is.na(commits_focus$id_logger),]

    ## sort commits by date
    commits_focus <- commits_focus[order(as.Date(commits_focus$date_start)),]

    ## extract report table
    table_output <- commits_focus[nrow(commits_focus):1,-c(1, 2, 3, 15)]

    ## reorganise columns
    table_output_1 <- table_output[,c(1, 2, 3, 4)]
    table_output_2 <- table_output[,c(5, 6, 7, 8, 9, 10)]

    ## get days in use
    uptime_range <- range(c(as.Date(table_output$date_start),
                            as.Date(table_output$date_stop)))
    uptime_sum <- as.numeric(difftime(time1 = uptime_range[2],
                                      time2 = uptime_range[1],
                                      units = "days"))

    ## make map of deployment
    project_focus <- unique(commits_focus$project)

    ## get project details from project list
    project_focus_data <- vector(mode = "list",
                                 length = length(project_focus))

    for(i in 1:length(project_focus_data)) {

      project_focus_data[[i]] <- projects[projects$name == project_focus[i],]
    }

    project_focus_data <- do.call(rbind, project_focus_data)

    ## create station location map
    jpeg(filename = paste0(filename, "_map.jpeg"),
         width = 500,
         height = 500,
         res = 300)

    ## load world map
    map <- try(raster::brick(x = paste0(system.file("extdata",
                                                    package = "eseisManager"),
                                        "/map/blue_marble.tif")),
               silent = TRUE)

    ## update extent
    map_ext <- raster::extent(map)

    if(missing(extent) == FALSE) {

      map_ext[1] <- extent[1]
      map_ext[2] <- extent[2]
      map_ext[3] <- extent[3]
      map_ext[4] <- extent[4]
    }

    ## crop map to new extent
    map <- raster::crop(x = map, y = map_ext)

    ## plot world map and add station points and labels
    raster::plotRGB(map)
    points(x = project_focus_data$lon,
         y = project_focus_data$lat,
         pch = 20,
         col = grDevices::heat.colors(n = nrow(project_focus_data)),
         cex = 1)

    dev.off()
  }

  ## report on sensor ---------------------------------------------------------
  if(item == "sensor") {

    ## check if sensor exists
    if(sum(commits$id_sensor == ID, na.rm = TRUE) == 0) {

      stop("Sensor ID not found in commits file!")
    }

    ## isolate station of interest
    commits_focus <- commits[commits$id_sensor == ID,]

    ## remove NA cases
    commits_focus <- commits_focus[!is.na(commits_focus$id_sensor),]

    ## sort commits by date
    commits_focus <- commits_focus[order(as.Date(commits_focus$date_start)),]

    ## extract report table
    table_output <- commits_focus[nrow(commits_focus):1,-c(1, 2, 3, 15)]

    ## reorganise columns
    table_output_1 <- table_output[,c(1, 2, 3, 4)]
    table_output_2 <- table_output[,c(5, 6, 7, 8, 9, 10)]

    ## get days in use
    uptime_range <- range(c(as.Date(table_output$date_start),
                            as.Date(table_output$date_stop)))
    uptime_sum <- as.numeric(difftime(time1 = uptime_range[2],
                                      time2 = uptime_range[1],
                                      units = "days"))

    ## make map of deployment
    project_focus <- unique(commits_focus$project)

    ## get project details from project list
    project_focus_data <- vector(mode = "list",
                                 length = length(project_focus))

    for(i in 1:length(project_focus_data)) {

      project_focus_data[[i]] <- projects[projects$name == project_focus[i],]
    }

    project_focus_data <- do.call(rbind, project_focus_data)

    ## create station location map
    jpeg(filename = paste0(filename, "_map.jpeg"),
         width = 500,
         height = 500,
         res = 300)

    ## load world map
    map <- try(raster::brick(x = paste0(system.file("extdata",
                                                    package = "eseisManager"),
                                        "/map/blue_marble.tif")),
               silent = TRUE)

    ## update extent
    map_ext <- raster::extent(map)

    if(missing(extent) == FALSE) {

      map_ext[1] <- extent[1]
      map_ext[2] <- extent[2]
      map_ext[3] <- extent[3]
      map_ext[4] <- extent[4]
    }

    ## crop map to new extent
    map <- raster::crop(x = map, y = map_ext)

    ## plot world map and add station points and labels
    raster::plotRGB(map)
    points(x = project_focus_data$lon,
           y = project_focus_data$lat,
           pch = 20,
           col = grDevices::heat.colors(n = nrow(project_focus_data)),
           cex = 1)

    dev.off()
  }

  ## generate report document -------------------------------------------------

  ## set default css options
  css_content <- list(font_family = "arial",
                      headings_size = "166%",
                      content_color = "#a72925")

  ## define rmd and html file names
  file.rmd <- paste(filename,
                    ".Rmd",
                    sep = "")

  file.html <- paste(filename,
                     ".html",
                     sep = "")

  ## Create and open the file
  file.create(file.rmd)

  print(paste0(filename,
               ".html created in working directory ",
               "or used defined path"))

  tmp <- file(file.rmd,
              open = "w")

  ## write Rmd basic header information
  writeLines("---", con = tmp)
  writeLines("output:", con = tmp)
  writeLines("  html_document:", con = tmp)
  writeLines("    mathjax: null", con = tmp)
  writeLines("    title: eseisManager.Report", con = tmp)
  writeLines(paste("    theme:", "cerulean"), con = tmp)
  writeLines(paste("    highlight:", "haddock"), con = tmp)
  writeLines("    md_extensions: -autolink_bare_uris", con = tmp)
  writeLines("---", con = tmp)
  writeLines(paste0(
    "<style>",
    paste0("h1, h2, h3, h4, h5, h6 { font-size:",
           css_content$headings_size," } \n"),
    paste0("#root { color: ", css_content$content_color," } \n"),
    paste0("BODY { font-family:", css_content$font_family, " } \n"),
    "</style>"
  ),
  con = tmp)

  ## Write report title
  writeLines(paste("<div align='center'><h1>",
                   ID, "-", item, "report",
                   "</h1></div>\n\n<hr>"),
             con = tmp)

  ## add general info
  writeLines(paste("<div align='center'><h3>",
                   "Overview",
                   "</h3></div>\n"),
             con = tmp)

  writeLines(paste("<p>",
                   "Time covered: ",
                   as.character(uptime_range[1]),
                   " - ", as.character(uptime_range[2]), " (",
                   as.character(uptime_sum), " days)", "<br>",
                   "Current project: ", project_focus_data$name[1], "<br>",
                   "Involved sensors: ",
                   paste(unique(commits_focus$type_sensor), collapse = ", "),
                   "<br>",
                   "Involved loggers: ",
                   paste(unique(commits_focus$type_logger), collapse = ", "),
                   "<br>",
                   "</p>\n",
                   sep = ""),
             con = tmp)

  ## add map
  writeLines(paste("<div align='center'><h3>",
                   "Location map (yellow circle most recent location)",
                   "</h3></div>\n"),
             con = tmp)
  writeLines(paste0("<center>\n",
                    "![](", filename, "_map.jpeg)\n",
                    "</center>\n"),
             con = tmp)

  ## add table contents
  writeLines(pander::pander_return(table_output_1),
             con = tmp)

  writeLines(pander::pander_return(table_output_2),
             con = tmp)

  ## write summary information
  writeLines(paste0("</pre>",
                    "\n\n"),
             con = tmp)

  ## close rmd file
  close(tmp)

  ## render html file
  try(rmarkdown::render(file.rmd,
                        clean = TRUE,
                        quiet = TRUE), silent = TRUE)

  ## remove rmd file
  try(invisible(unlink(file.rmd,
                       recursive = TRUE)))
  try(invisible(unlink(paste0(filename, "_map.jpeg"),
                       recursive = TRUE)))

  ## optionally open file in browser
  if (browser == TRUE) {
    try(utils::browseURL(file.html))
  }

  ## return tabular output
  return(table_output)
}
