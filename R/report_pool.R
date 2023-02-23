#' Generate a report on seismic pool status
#'
#' Details, later.
#'
#' @param filename \code{Character} value, name of the output file (without
#' extension). Default is \code{"report"}, saved in current working directory.
#' Please note that no ~ signs are allowed as path flags.
#'
#' @param browser \code{Logical} value, option to open report in broser.
#' Default is \code{TRUE}.
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
#' report_pool()
#' }
#'
#' @export report_pool

report_pool <- function(

  filename,
  browser = TRUE,
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

  ## get total numbers of available items
  types_sensors <- na.exclude(unique(sensors$Type))
  types_sensors <- types_sensors[!grepl(x = types_sensors, pattern = "GIPP_")]
  n_sensors <- lapply(X = types_sensors, FUN = function(type, sensors) {

    sum(type == sensors$Type, na.rm = TRUE)
  }, sensors)
  names(n_sensors) <- types_sensors

  types_loggers <- na.exclude(unique(loggers$Type))
  types_loggers <- types_loggers[!grepl(x = types_loggers, pattern = "GIPP_")]
  n_loggers <- lapply(X = types_loggers, FUN = function(type, loggers) {

    sum(type == loggers$Type, na.rm = TRUE)
  }, loggers)
  names(n_loggers) <- types_loggers

  ## get number of items by project -------------------------------------------
  projects_active <-
    lapply(X = projects$name, FUN = function(project, commits) {

      ## isolate commits by project
      commits_i <- commits[commits$project == project,]

      ## get latest commits by station ID
      id_unique <- unique(commits_i$id_station)
      commits_focus_recent <-
        lapply(X = id_unique, FUN = function(id_unique, commits_i) {

          tail(commits_i[commits_i$id_station == id_unique,], n = 1)
        }, commits_i)

      ## check if any sensor or logger appears somewhere else since then
      active <-
        lapply(X = commits_focus_recent, FUN = function(i, commits) {

          ## get sensor and logger ID
          sensor_i <- i$id_sensor
          logger_i <- i$id_logger

          ## compare dates
          if(is.na(sensor_i) == FALSE) {

            commits_sensor <- commits$date_start[commits$id_sensor == sensor_i]
            commits_sensor <- max(as.Date(na.exclude(commits_sensor)))

            if(commits_sensor > as.Date(i$date_stop)) {

              out_sensor <- FALSE
            } else {

              out_sensor <- TRUE
            }
          } else {

            out_sensor <- TRUE
          }

          if(is.na(logger_i) == FALSE) {

            commits_logger <- commits$date_start[commits$id_logger == logger_i]
            commits_logger <- max(as.Date(na.exclude(commits_logger)))

            if(commits_logger > as.Date(i$date_stop)) {

              out_logger <- FALSE
            } else {

              out_logger <- TRUE
            }
          } else {

            out_logger <- TRUE
          }

          return(out_sensor & out_logger)

        }, commits)
      active <- do.call(c, active)

      ## reduce list to active stations
      commits_focus_active <- do.call(rbind, commits_focus_recent[active])

      ## return output
      return(commits_focus_active)

    }, commits)

  ## remove empty data sets
  i_ok <- do.call(c, lapply(X = projects_active, FUN = function(x) {

    !is.null(x)
  }))
  projects_active <- projects_active[i_ok]

  ## remove NA data sets
  i_ok <- do.call(c, lapply(X = projects_active, FUN = function(x) {

    !is.na(sum(x$check_include))
  }))
  projects_active <- projects_active[i_ok]

  ## add project names
  projects_active_names <- lapply(X = projects_active, FUN = function(x) {

    unique(x$project)[1]
  })
  names(projects_active) <- do.call(c, projects_active_names)

  ## get number of stations by project ----------------------------------------
  project_n_station <- lapply(X = projects_active, FUN = function(x) {

    nrow(x)
  })
  project_n_station <- cbind(names(project_n_station),
                            do.call(rbind, project_n_station))
  colnames(project_n_station) <- c("project", "n_station")
  rownames(project_n_station) <- 1:nrow(project_n_station)
  project_n_station <- as.data.frame(project_n_station)
  for(i in 2:ncol(project_n_station)) {
    project_n_station[,i] <- as.numeric(project_n_station[,i])
  }

  ## get coordinates and number of stations for active projects
  projects_active_summary <-
    lapply(X = projects_active_names, FUN = function(name, projects, n) {

      ## isolate project
      y <- projects[projects$name == name,]
      y <- cbind(y, n[n$project == name, 2])
      colnames(y)[5] <- "n_stations"

      return(y)

    }, projects, n = project_n_station)
  projects_active_summary <- do.call(rbind, projects_active_summary)

  ## get number of sensors by project and sensor type -------------------------
  project_n_sensor <- lapply(X = projects_active, FUN = function(x, sensors) {

    ## get available sensor types
    sensors_unique <- na.exclude(unique(sensors$Type))

    ## get sensor types of project
    sensors_project <- x$type_sensor

    ## count sensors by type
    n_sensors <-
      lapply(X = sensors_unique, FUN = function(y, sensors_project) {

        sum(sensors_project == y)
    }, sensors_project)

    n_sensors <- do.call(c, n_sensors)
    names(n_sensors) <- sensors_unique

    return(n_sensors)

  }, sensors)
  project_n_sensor <- cbind(names(project_n_sensor),
                            do.call(rbind, project_n_sensor))
  colnames(project_n_sensor)[1] <- "project"
  rownames(project_n_sensor) <- 1:nrow(project_n_sensor)
  project_n_sensor <- as.data.frame(project_n_sensor)
  for(i in 2:ncol(project_n_sensor)) {
    project_n_sensor[,i] <- as.numeric(project_n_sensor[,i])
  }

  ## get number of loggers by project and sensor type -------------------------
  project_n_logger <- lapply(X = projects_active, FUN = function(x, loggers) {

    ## get available sensor types
    loggers_unique <- na.exclude(unique(loggers$Type))

    ## get sensor types of project
    loggers_project <- x$type_logger

    ## count sensors by type
    n_loggers <-
      lapply(X = loggers_unique, FUN = function(y, loggers_project) {

        sum(loggers_project == y)
      }, loggers_project)

    n_loggers <- do.call(c, n_loggers)
    names(n_loggers) <- loggers_unique

    return(n_loggers)

  }, loggers)
  project_n_logger <- cbind(names(project_n_logger),
                            do.call(rbind, project_n_logger))
  colnames(project_n_logger)[1] <- "project"
  rownames(project_n_logger) <- 1:nrow(project_n_logger)
  project_n_logger <- as.data.frame(project_n_logger)
  for(i in 2:ncol(project_n_logger)) {
    project_n_logger[,i] <- as.numeric(project_n_logger[,i])
  }

  ## get sensors in storage ---------------------------------------------------

  ## get all sensors with storage flag
  sensors_storage <- commits[commits$project == "Storage",]
  sensors_storage <- sensors_storage[is.na(sensors_storage$type_logger),]

  ## remove sensors that were out of storage at a later time
  if(nrow(sensors_storage) > 0) {

    i_ok <- rep(FALSE, nrow(sensors_storage))
    for(i in 1:length(i_ok)) {

      ## get date of sensor
      sensor_date <- as.Date(sensors_storage$date_stop[i])

      ## get commit dates for sensor
      commits_clean <- commits[!is.na(commits$id_sensor),]
      commits_date <- as.Date(commits_clean$date_start[
        commits_clean$id_sensor == sensors_storage$id_sensor[i]])

      ## test if commit date is younger than storage stop date
      i_ok[i] <- sensor_date > max(commits_date)
    }
    sensors_storage <- sensors_storage[i_ok,]
    mode(sensors_storage$id_sensor) <- "character"
  }

  ## get loggers in storage ---------------------------------------------------

  ## get all loggers with storage flag
  loggers_storage <- commits[commits$project == "Storage",]
  loggers_storage <- loggers_storage[is.na(loggers_storage$type_sensor),]

  ## remove sensors that were out of storage at a later time
  if(nrow(loggers_storage) > 0) {

    i_ok <- rep(FALSE, nrow(loggers_storage))
    for(i in 1:length(i_ok)) {

      ## get date of logger
      logger_date <- as.Date(loggers_storage$date_stop[i])

      ## get commit dates for logger
      commits_clean <- commits[!is.na(commits$id_logger),]
      commits_date <- as.Date(commits_clean$date_start[
        commits_clean$id_logger == loggers_storage$id_logger[i]])

      ## test if commit date is younger than storage stop date
      i_ok[i] <- logger_date > max(commits_date)
    }
    loggers_storage <- loggers_storage[i_ok,]
    mode(loggers_storage$id_logger) <- "character"
  }

  ## get sensors in repair ----------------------------------------------------

  ## get all sensors with repair flag
  sensors_repair <- commits[commits$project == "Repair",]
  sensors_repair <- sensors_repair[is.na(sensors_repair$type_logger),]

  ## remove sensors that were out of repair at a later time
  if(nrow(sensors_repair) > 0) {

    i_ok <- rep(FALSE, nrow(sensors_repair))
    for(i in 1:length(i_ok)) {

      ## get date of sensor
      sensor_date <- as.Date(sensors_repair$date_stop[i])

      ## get commit dates for sensor
      commits_clean <- commits[!is.na(commits$id_sensor),]
      commits_date <- as.Date(commits_clean$date_start[
        commits_clean$id_sensor == sensors_repair$id_sensor[i]])

      ## test if commit date is younger than repair stop date
      i_ok[i] <- sensor_date > max(commits_date)
    }
    sensors_repair <- sensors_repair[i_ok,]
  }

  ## get loggers in repair ----------------------------------------------------

  ## get all loggers with repair flag
  loggers_repair <- commits[commits$project == "Repair",]
  loggers_repair <- loggers_repair[is.na(loggers_repair$type_sensor),]

  ## remove sensors that were out of repair at a later time
  if(nrow(loggers_repair) > 0) {

    i_ok <- rep(FALSE, nrow(loggers_repair))
    for(i in 1:length(i_ok)) {

      ## get date of logger
      logger_date <- as.Date(loggers_repair$date_stop[i])

      ## get commit dates for logger
      commits_clean <- commits[!is.na(commits$id_logger),]
      commits_date <- as.Date(commits_clean$date_start[
        commits_clean$id_logger == loggers_repair$id_logger[i]])

      ## test if commit date is younger than repair stop date
      i_ok[i] <- logger_date > max(commits_date)
    }
    loggers_repair <- loggers_repair[i_ok,]
  }

  ## convert structure of n_sensors and n_loggers
  n_sensors <- do.call(cbind, n_sensors)
  n_loggers <- do.call(cbind, n_loggers)

  ## group sensors in storage by type
  n_sensors_storage <-
    lapply(X = types_sensors, FUN = function(x, sensors_storage) {

      sum(sensors_storage$type_sensor == x, na.rm = TRUE)
    }, sensors_storage)
  n_sensors_storage <- do.call(c, n_sensors_storage)
  names(n_sensors_storage) <- types_sensors

  ## group loggers in storage by type
  n_loggers_storage <-
    lapply(X = types_loggers, FUN = function(x, loggers_storage) {

      sum(loggers_storage$type_logger == x, na.rm = TRUE)
    }, loggers_storage)
  n_loggers_storage <- do.call(c, n_loggers_storage)
  names(n_loggers_storage) <- types_loggers

  ## group sensors in repair by type
  n_sensors_repair <-
    lapply(X = types_sensors, FUN = function(x, sensors_repair) {

      sum(sensors_repair$type_sensor == x, na.rm = TRUE)
    }, sensors_repair)
  n_sensors_repair <- do.call(c, n_sensors_repair)
  names(n_sensors_repair) <- types_sensors

  ## group loggers in repair by type
  n_loggers_repair <-
    lapply(X = types_loggers, FUN = function(x, loggers_repair) {

      sum(loggers_repair$type_logger == x, na.rm = TRUE)
    }, loggers_repair)
  n_loggers_repair <- do.call(c, n_loggers_repair)
  names(n_loggers_repair) <- types_loggers

  ## calculate occupied sensors
  n_sensors_occupied <- n_sensors - n_sensors_storage - n_sensors_repair

  ## calculate occupied loggers
  n_loggers_occupied <- n_loggers - n_loggers_storage - n_loggers_repair

  ## build bar chart data set
  n_sensors_barplot <- rbind(n_sensors_occupied, n_sensors_storage, n_sensors_repair)
  n_loggers_barplot <- rbind(n_loggers_occupied, n_loggers_storage, n_loggers_repair)

  ## split projects by activity state
  date_running <- do.call(c, lapply(X = projects_active, FUN = function(x) {

    max(x$date_stop)
  }))

  projects_active <- projects_active[order(date_running)]
  date_running <- sort(date_running)

  is_running <- date_running > Sys.time()

  projects_running <- projects_active[is_running]
  projects_done <- projects_active[!is_running]

  ## create station location map
  jpeg(filename = paste0(filename, "_map.jpeg"),
       width = 1000,
       height = 600,
       res = 100)

  par(pty="s")

  ## classify point size by number of stations
  pt_size <- as.numeric(cut(x = projects_active_summary$n_stations,
                            breaks = c(0, 1, 4, 8, 16, 30))) / 5 * 3

  ## load world map
  map <- try(raster::brick(x = paste0(system.file("extdata",
                                                  package = "eseisManager"),
                                      "/map/blue_marble.tif")),
             silent = TRUE)

  ## plot world map and add station points and labels
  raster::plotRGB(map)
  points(x = projects_active_summary$lon,
         y = projects_active_summary$lat,
         pch = 20,
         col = "yellow",
         cex = pt_size)
  text(x = projects_active_summary$lon,
       y = projects_active_summary$lat,
       labels = projects_active_summary$name,
       cex = 0.8,
       col = "yellow",
       adj = c(0, 0))
  dev.off()

  ## create sensor availability plot
  jpeg(filename = paste0(filename, "_sensors.jpeg"),
       width = 800,
       height = 400,
       res = 100)
  barplot(n_sensors_barplot,
          col = c("grey", "darkgreen", "orange"),
          ylim = c(0, max(colSums(n_sensors_barplot)) * 1.1))
  box()
  axis(side = 3,
       at = seq(from = 0.5,
                to = length(n_sensors_storage) + 0.7,
                length.out = length(n_sensors_storage)),
       labels = n_sensors_storage,
       col = "darkgreen")
  dev.off()

  ## create logger availability plot
  jpeg(filename = paste0(filename, "_loggers.jpeg"),
       width = 800,
       height = 400,
       res = 100)
  barplot(n_loggers_barplot,
          col = c("grey", "darkgreen", "orange"),
          ylim = c(0, max(colSums(n_loggers_barplot)) * 1.1))
  box()
  axis(side = 3,
       at = seq(from = 0.5,
                to = length(n_loggers_storage) + 0.7,
                length.out = length(n_loggers_storage)),
       labels = n_loggers_storage,
       col = "darkgreen")
  dev.off()

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
  writeLines(paste0("<div align='center'><h1>",
                   "Seismic pool report (", Sys.time(), ")",
                   "</h1></div>\n\n<hr>"),
             con = tmp)

  ## add general info
  writeLines(paste("<div align='center'><h3>",
                   "Overview",
                   "</h3></div>\n"),
             con = tmp)

  writeLines(paste("<p>", "The total fleet:", "</p>\n", sep = ""),
             con = tmp)

  ## add table of sensors
  writeLines(pander::pander_return(n_sensors),
             con = tmp)

  ## add table of loggers
  writeLines(pander::pander_return(n_loggers),
             con = tmp)

  writeLines(paste("<p>", "Sensors in storage (", nrow(sensors_storage), "):</p>\n", sep = ""),
             con = tmp)

  ## add table of sensors
  writeLines(pander::pander_return(sensors_storage[,c(11, 9, 6)]),
             con = tmp)

  writeLines(paste("<p>", "Loggers in storage (", nrow(loggers_storage), "):</p>\n", sep = ""),
             con = tmp)

  ## add table of sensors
  writeLines(pander::pander_return(loggers_storage[,c(12, 10, 6)]),
             con = tmp)

  writeLines(paste("<p>", "Sensors in repair (", nrow(sensors_repair), "):</p>\n", sep = ""),
             con = tmp)

  ## add table of sensors
  writeLines(pander::pander_return(sensors_repair[,c(11, 9, 6)]),
             con = tmp)

  writeLines(paste("<p>", "Loggers in repair (", nrow(loggers_repair), "):</p>\n", sep = ""),
             con = tmp)

  ## add table of sensors
  writeLines(pander::pander_return(loggers_repair[,c(12, 10, 6)]),
             con = tmp)

  ## add map
  writeLines(paste("<div align='center'><h3>",
                   "Location map (Circle size depicts number of stations (0 | 1 | 4 | 8 | 16 | 30))",
                   "</h3></div>\n"),
             con = tmp)
  writeLines(paste0("<center>\n",
                    "![](", filename, "_map.jpeg)\n",
                    "</center>\n"),
             con = tmp)

  ## add sensor availability
  writeLines(paste("<br><br><div align='center'><h3>",
                   "Sensor availability (grey - occupied, green - free, orange - repair)",
                   "</h3></div>\n"),
             con = tmp)
  writeLines(paste0("<center>\n",
                    "![](", filename, "_sensors.jpeg)\n",
                    "</center>\n"),
             con = tmp)

  ## add logger availability
  writeLines(paste("<br><br><div align='center'><h3>",
                   "Logger availability (grey - occupied, green - free, orange - repair)",
                   "</h3></div>\n"),
             con = tmp)
  writeLines(paste0("<center>\n",
                    "![](", filename, "_loggers.jpeg)\n",
                    "</center>\n"),
             con = tmp)



  ## add running project summaries
  writeLines(paste("<br><br><div align='center'><h3>",
                   "Running projects",
                   "</h3></div>\n"),
             con = tmp)

  for(i in 1:length(projects_running)) {

    writeLines(paste("<br><br><div align='center'><h3>",
                     "Project summary:", names(projects_running)[i],
                     "(Stop date: ", max(projects_running[[i]]$date_stop), ")",
                     "</h3></div>\n"),
               con = tmp)

    writeLines(pander::pander_return(projects_running[[i]][,c(8, 9, 10, 11, 12)]),
               con = tmp)
  }

  ## add finished project summaries
  writeLines(paste("<br><br><div align='center'><h3>",
                   "Finished projects",
                   "</h3></div>\n"),
             con = tmp)

  for(i in 1:length(projects_done)) {

    writeLines(paste("<br><br><div align='center'><h3>",
                     "Project summary:", names(projects_done)[i],
                     " (Stop date: ", max(projects_done[[i]]$date_stop), ")",
                     "</h3></div>\n"),
               con = tmp)

    # writeLines(pander::pander_return(projects_done[[i]][,c(8, 9, 10, 11, 12)]),
    #            con = tmp)
  }

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
  try(invisible(unlink(paste0(filename, "_sensors.jpeg"),
                       recursive = TRUE)))
  try(invisible(unlink(paste0(filename, "_loggers.jpeg"),
                       recursive = TRUE)))

  ## optionally open file in browser
  if (browser == TRUE) {
    try(utils::browseURL(file.html))
  }
}