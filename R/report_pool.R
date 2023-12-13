#' Generate a report on seismic pool status
#'
#' Details, later.
#'
#' @param filename \code{Character} value, name of the output file (without
#' extension). Default is \code{"report"}, saved in a temporary directory.
#' Please note that no ~ signs are allowed as path flags.
#'
#' @param browser \code{Logical} value, option to open report in browser.
#' Default is \code{TRUE}.
#'
#' @param sourcefiles \code{Character} value, path to the directory that
#' contains the source files of the data base. See details for further
#' information.
#'
#' @param interactive \code{Logical} value, option to use an interactive map
#' instead of a static world map. Default is \code{TRUE}.
#'
#' @param include_external \code{Logical} value, option to activate/suppress
#' inclusion of pool-external instruments to the report. Default is
#' \code{FALSE}.
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
  sourcefiles,
  interactive = TRUE,
  include_external = FALSE

) {

  if(missing(filename) == TRUE) {

    filename <- tempdir()
    html_out <- paste0(filename, "/report.html")
  } else {

    html_out <- paste0(filename, ".html")
  }

  ## check/set sourcefiles
  if(missing(sourcefiles) == TRUE) {

    ## find path to source files
    sourcefiles <- readLines(paste0(con = system.file(
      "extdata",
      package = "eseisManager"),
      "/data/path.txt"))

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
  types_sensors <- na.exclude(unique(sensors$type))
  types_sensors_show <- types_sensors

  if(include_external == FALSE) {

    types_sensors_show <- types_sensors_show[!grepl(x = types_sensors_show,
                                                    pattern = "GIPP_")]
    types_sensors_show <- types_sensors_show[!grepl(x = types_sensors_show,
                                                    pattern = "Sec46_")]
  }

  n_sensors <- lapply(X = types_sensors, FUN = function(type, sensors) {

    sum(type == sensors$type, na.rm = TRUE)
  }, sensors)
  names(n_sensors) <- types_sensors


  types_loggers <- na.exclude(unique(loggers$type))
  types_loggers_show <- types_loggers

  if(include_external == FALSE) {

    types_loggers_show <- types_loggers_show[!grepl(x = types_loggers_show,
                                                    pattern = "GIPP_")]
    types_loggers_show <- types_loggers_show[!grepl(x = types_loggers_show,
                                                    pattern = "Sec46_")]
  }

  n_loggers <- lapply(X = types_loggers, FUN = function(type, loggers) {

    sum(type == loggers$type, na.rm = TRUE)
  }, loggers)
  names(n_loggers) <- types_loggers

  ## get number of items by project -------------------------------------------
  projects <- projects[projects$name != "Storage",]

  projects_active <-
    lapply(X = projects$name, FUN = function(project, commits) {

      ## isolate commits by project
      commits_i <- commits[commits$project == project,]

      ## get unique station IDs
      id_unique <- unique(commits_i$id_station)

      ## proceed if ID exists in commit file
      if(length(id_unique) > 0) {

        ## set dummy ID in case of missing station ID
        if(is.na(id_unique)[1]) {

          id_unique <- paste0("ID", floor(stats::runif(n = 1,
                                                       min = 100,
                                                       max = 999)))
          commits_i$id_station <- id_unique
        }

        ## get latest commits by station ID
        commits_focus_recent <-
          lapply(X = id_unique, FUN = function(id_unique, commits_i) {

            tail(commits_i[commits_i$id_station == id_unique,], n = 1)
          }, commits_i)

        ## check if any sensor or logger appears somewhere else since then
        active <-
          lapply(X = commits_focus_recent, FUN = function(i, commits) {

            ## get sensor and logger ID
            sensor_i <- i$uid_sensor
            logger_i <- i$uid_logger

            ## compare dates
            if(is.na(sensor_i) == FALSE) {

              commits_sensor <-
                commits$date_start[commits$uid_sensor == sensor_i]
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

              commits_logger <-
                commits$date_start[commits$uid_logger == logger_i]
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

      } else {

        commits_focus_active <- NULL
      }

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
      n_stations <- n[n$project == name, 2]
      y <- cbind(y, n_stations)

      return(y)

    }, projects, n = project_n_station)
  projects_active_summary <- do.call(rbind, projects_active_summary)

  ## get number of sensors by project and sensor type -------------------------
  project_n_sensor <- lapply(X = projects_active, FUN = function(x, sensors) {

    ## get available sensor types
    sensors_unique <- na.exclude(unique(sensors$type))

    ## get sensor types of project
    sensors_project <- na.exclude(x$type_sensor)

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
    loggers_unique <- na.exclude(unique(loggers$type))

    ## get sensor types of project
    loggers_project <- na.exclude(x$type_logger)

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
      commits_clean <- commits[!is.na(commits$uid_sensor),]
      commits_date <- as.Date(commits_clean$date_start[
        commits_clean$uid_sensor == sensors_storage$uid_sensor[i]])

      ## test if commit date is younger than storage stop date
      i_ok[i] <- sensor_date > max(commits_date)
    }
    sensors_storage <- sensors_storage[i_ok,]
    mode(sensors_storage$uid_sensor) <- "character"
  }

  ## optionally remove external sensors
  sensors_storage_show <-
    sensors_storage[sensors_storage$type_sensor %in% types_sensors_show,]

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
      commits_clean <- commits[!is.na(commits$uid_logger),]
      commits_date <- as.Date(commits_clean$date_start[
        commits_clean$uid_logger == loggers_storage$uid_logger[i]])

      ## test if commit date is younger than storage stop date
      i_ok[i] <- logger_date > max(commits_date)
    }
    loggers_storage <- loggers_storage[i_ok,]
    mode(loggers_storage$uid_logger) <- "character"
  }

  ## optionally remove external sensors
  loggers_storage_show <-
    loggers_storage[loggers_storage$type_logger %in% types_loggers_show,]

  ## add Cube ID for output table
  id_cube <- lapply(X = loggers_storage_show$uid_logger, FUN = function(x) {
    loggers$ID[loggers$UID == x][1]
  })
  id_cube <- do.call(c, id_cube)

  loggers_storage_show <- cbind(loggers_storage_show, id_cube)

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
      commits_clean <- commits[!is.na(commits$uid_sensor),]
      commits_date <- as.Date(commits_clean$date_start[
        commits_clean$uid_sensor == sensors_repair$uid_sensor[i]])

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
      commits_clean <- commits[!is.na(commits$uid_logger),]
      commits_date <- as.Date(commits_clean$date_start[
        commits_clean$uid_logger == loggers_repair$uid_logger[i]])

      ## test if commit date is younger than repair stop date
      i_ok[i] <- logger_date > max(commits_date)
    }
    loggers_repair <- loggers_repair[i_ok,]
  }

  ## convert structure of n_sensors and n_loggers
  n_sensors <- do.call(cbind, n_sensors)
  n_loggers <- do.call(cbind, n_loggers)

  ## optionally remove external instruments
  n_sensors_show <- n_sensors[,colnames(n_sensors) %in% types_sensors_show]
  n_loggers_show <- n_loggers[,colnames(n_loggers) %in% types_loggers_show]

  ## group sensors in storage by type
  n_sensors_storage <-
    lapply(X = types_sensors, FUN = function(x, sensors_storage) {

      sum(sensors_storage$type_sensor == x, na.rm = TRUE)
    }, sensors_storage)
  n_sensors_storage <- do.call(c, n_sensors_storage)
  names(n_sensors_storage) <- types_sensors

  ## optionally remove external sensors
  n_sensors_storage <-
    n_sensors_storage[names(n_sensors_storage) %in% types_sensors_show]

  ## group loggers in storage by type
  n_loggers_storage <-
    lapply(X = types_loggers, FUN = function(x, loggers_storage) {

      sum(loggers_storage$type_logger == x, na.rm = TRUE)
    }, loggers_storage)
  n_loggers_storage <- do.call(c, n_loggers_storage)
  names(n_loggers_storage) <- types_loggers

  ## optionally remove external loggers
  n_loggers_storage <-
    n_loggers_storage[names(n_loggers_storage) %in% types_loggers_show]

  ## group sensors in repair by type
  n_sensors_repair <-
    lapply(X = types_sensors, FUN = function(x, sensors_repair) {

      sum(sensors_repair$type_sensor == x, na.rm = TRUE)
    }, sensors_repair)
  n_sensors_repair <- do.call(c, n_sensors_repair)
  names(n_sensors_repair) <- types_sensors

  ## optionally remove external sensors
  n_sensors_repair <-
    n_sensors_repair[names(n_sensors_repair) %in% types_sensors_show]

  ## group loggers in repair by type
  n_loggers_repair <-
    lapply(X = types_loggers, FUN = function(x, loggers_repair) {

      sum(loggers_repair$type_logger == x, na.rm = TRUE)
    }, loggers_repair)
  n_loggers_repair <- do.call(c, n_loggers_repair)
  names(n_loggers_repair) <- types_loggers

  ## optionally remove external sensors
  n_loggers_repair <-
    n_loggers_repair[names(n_loggers_repair) %in% types_loggers_show]

  ## calculate occupied sensors
  n_sensors_occupied <- n_sensors_show - n_sensors_storage - n_sensors_repair

  ## calculate occupied loggers
  n_loggers_occupied <- n_loggers_show - n_loggers_storage - n_loggers_repair

  ## build bar chart data set
  n_sensors_barplot <- rbind(n_sensors_occupied,
                             n_sensors_storage,
                             n_sensors_repair)
  n_loggers_barplot <- rbind(n_loggers_occupied,
                             n_loggers_storage,
                             n_loggers_repair)

  ## split projects by activity state
  date_running <- do.call(c, lapply(X = projects_active, FUN = function(x) {

    max(x$date_stop)
  }))

  projects_active <- projects_active[order(date_running)]
  date_running <- sort(date_running)

  is_running <- date_running > Sys.time()

  projects_running <- projects_active[is_running]
  projects_done <- projects_active[!is_running]

  ## remove past projects from active ones
  projects_active_summary_clean <-
    projects_active_summary[projects_active_summary$name %in%
                              names(projects_running),]

  ## assign done project information
  projects_done_summary_clean <-
    projects_active_summary[!(projects_active_summary$name %in%
                                names(projects_running)),]

  ## create station location map
  if(interactive == TRUE) {

    ## create plot data for running projetcs
    xyz_plot <- projects_active_summary_clean
    xyz_plot$size <-
      as.numeric(cut(x = projects_active_summary_clean$n_stations,
                     breaks = c(0, 1, 4, 8, 16, 30))) / 5 * 3
    xyz_plot$label <- paste0(xyz_plot$name, " (", xyz_plot$n_stations, ")")

    ## create plot data for past projects
    xyz_done_plot <- projects_done_summary_clean
    xyz_done_plot$size <-
      as.numeric(cut(x = projects_done_summary_clean$n_stations,
                     breaks = c(0, 1, 4, 8, 16, 30))) / 5 * 3
    xyz_done_plot$label <- paste0(xyz_done_plot$name, " (",
                                  xyz_done_plot$n_stations, ")")

    geo <- list(projection = list(type = 'orthographic',
                                  rotation = list(lon = 13,
                                                  lat = 51,
                                                  roll = 0)),
                showland = TRUE,
                landcolor = plotly::toRGB("gray25"),
                oceancolor = plotly::toRGB("grey5"),
                countrycolor = plotly::toRGB("gray35"),
                showcountries = TRUE,
                showocean = TRUE)

    map <- plotly::plot_geo(color = I("yellow"))

    ## add running project data
    map <- plotly::add_markers(p = map,
                               data = xyz_plot,
                               x = ~lon,
                               y = ~lat,
                               text = ~label,
                               hoverinfo = "text",
                               alpha = 0.95,
                               size = ~size)
    map <- plotly::add_segments(p = map,
                                data = xyz_plot,
                                x = 9.9481,
                                y = 51.5564,
                                xend = ~lon,
                                yend = ~lat,
                                alpha = 0.5,
                                size = I(1),
                                hoverinfo = "none")

    ## add done project data
    map <- plotly::add_markers(p = map,
                               data = xyz_done_plot,
                               x = ~lon,
                               y = ~lat,
                               color = I("brown"),
                               text = ~label,
                               hoverinfo = "text",
                               alpha = 0.95,
                               size = ~size)
    map <- plotly::add_segments(p = map,
                                data = xyz_done_plot,
                                x = 9.9481,
                                y = 51.5564,
                                xend = ~lon,
                                yend = ~lat,
                                color = I("brown"),
                                alpha = 0.5,
                                size = I(1),
                                hoverinfo = "none")

    map <- plotly::layout(p = map,
                          geo = geo,
                          showlegend = FALSE)

    suppressWarnings(htmlwidgets::saveWidget(
      widget = map,
      file = paste0(filename, "/map.html"),
      selfcontained = TRUE))

  } else {

    jpeg(filename = paste0(filename, "/map.jpeg"),
         width = 1000,
         height = 600,
         res = 100)

    par(pty="s")

    ## classify point size by number of stations
    pt_size <- as.numeric(cut(x = projects_active_summary$n_stations,
                              breaks = c(0, 1, 4, 8, 16, 30))) / 5 * 3

    ## load world map
    map <- try(terra::rast(
      x = paste0(system.file("extdata", package = "eseisManager"),
                 "/map/blue_marble.tif")), silent = TRUE)

    ## plot world map and add station points and labels
    terra::plot(map)
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
  }

  ## create sensor availability plot
  jpeg(filename = paste0(filename, "/sensors.jpeg"),
       width = 800,
       height = 400,
       res = 100)
  barplot(n_sensors_barplot,
          col = c("grey", "darkgreen", "orange"),
          ylim = c(0, max(colSums(n_sensors_barplot)) * 1.1),
          ylab = "Number of sensors")
  box()
  axis(side = 3,
       at = seq(from = 0.5,
                to = length(n_sensors_storage) + 0.7,
                length.out = length(n_sensors_storage)),
       labels = n_sensors_storage,
       col = "darkgreen", col.ticks = "darkgreen")
  dev.off()

  ## create logger availability plot
  jpeg(filename = paste0(filename, "/loggers.jpeg"),
       width = 800,
       height = 400,
       res = 100)
  barplot(n_loggers_barplot,
          col = c("grey", "darkgreen", "orange"),
          ylim = c(0, max(colSums(n_loggers_barplot)) * 1.1),
          ylab = "Number of loggers")
  box()
  axis(side = 3,
       at = seq(from = 0.5,
                to = length(n_loggers_storage) + 0.7,
                length.out = length(n_loggers_storage)),
       labels = n_loggers_storage,
       col = "darkgreen", col.ticks = "darkgreen")
  dev.off()

  ## generate report document -------------------------------------------------

  ## set default css options
  css_content <- list(font_family = "arial",
                      headings_size = "166%",
                      content_color = "#a72925")

  ## define rmd and html file names
  file.rmd <- paste(filename,
                    "/report.Rmd",
                    sep = "")

  file.html <- html_out

  ## Create and open the file
  file.create(file.rmd)

  tmp <- file(file.rmd,
              open = "w")

  ## print info on HTML file location
  print(paste0("Writing HTML file to: ", file.html))

  ## write Rmd basic header information
  writeLines("---", con = tmp)
  writeLines("title: Pool report by eseisManager", con = tmp)
  writeLines("output:", con = tmp)
  writeLines("  html_document:", con = tmp)
  writeLines("    mathjax: null", con = tmp)
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
  writeLines(pander::pander_return(n_sensors_show),
             con = tmp)

  ## add table of loggers
  writeLines(pander::pander_return(n_loggers_show),
             con = tmp)

  writeLines(paste("<p>", "Sensors in storage (", nrow(sensors_storage_show),
                   "):</p>\n", sep = ""),
             con = tmp)

  ## add table of sensors
  writeLines(pander::pander_return(sensors_storage_show[,c(11, 9, 6)]),
             con = tmp)

  writeLines(paste("<p>", "Loggers in storage (", nrow(loggers_storage_show),
                   "):</p>\n", sep = ""),
             con = tmp)

  ## add table of sensors
  writeLines(pander::pander_return(loggers_storage_show[,c(12, 10, 17, 6)]),
             con = tmp)

  writeLines(paste("<p>", "Sensors in repair (", nrow(sensors_repair),
                   "):</p>\n", sep = ""),
             con = tmp)

  ## add table of sensors
  writeLines(pander::pander_return(sensors_repair[,c(11, 9, 6)]),
             con = tmp)

  writeLines(paste("<p>", "Loggers in repair (", nrow(loggers_repair),
                   "):</p>\n", sep = ""),
             con = tmp)

  ## add table of sensors
  writeLines(pander::pander_return(loggers_repair[,c(12, 10, 6)]),
             con = tmp)

  ## add map
  writeLines(paste("<div align='left'><h3>",
                   "Location map (Circle size depicts number of stations)",
                   "</h3></div>\n"),
             con = tmp)
  if(interactive == TRUE) {

    writeLines(paste0('"<iframe src="', paste0(filename, "/map.html"),
                      '" title="Locations"',
                      ' height="700" width="100%" style="border:none;" ',
                      '></iframe>'),
               con = tmp)
  } else {

    writeLines(paste0("<center>\n",
                      "![](", filename, "/map.jpeg)\n",
                      "</center>\n"),
               con = tmp)
  }


  ## add sensor availability
  writeLines(paste("<br><br><div align='center'><h3>",
                   "Sensor availability (grey - occupied, green - free, ",
                   "orange - repair)",
                   "</h3></div>\n"),
             con = tmp)
  writeLines(paste0("<left>\n",
                    "![](", filename, "/sensors.jpeg)\n",
                    "</left>\n"),
             con = tmp)

  ## add logger availability
  writeLines(paste("<br><br><div align='center'><h3>",
                   "Logger availability (grey - occupied, green - free, ",
                   "orange - repair)",
                   "</h3></div>\n"),
             con = tmp)
  writeLines(paste0("<left>\n",
                    "![](", filename, "/loggers.jpeg)\n",
                    "</left>\n"),
             con = tmp)

  ## add running project summaries
  writeLines(paste("<br><br><div align='center'><h2>",
                   "*** Running projects ***",
                   "</h3></div>\n"),
             con = tmp)

  for(i in 1:length(projects_running)) {

    project_table <- projects_running[[i]][,c(8, 9, 10, 11, 12)]
    names(project_table) <- c("ID", "UID Sen.", "UID Log.", "Type Sen.",
                              "Type Log.")

    writeLines(paste0("<br><br><div align='center'><h3>",
                      "Project summary: ", names(projects_running)[i],
                      " (Stop date: ", max(projects_running[[i]]$date_stop), ")",
                      "</h3></div>\n"),
               con = tmp)

    writeLines(pander::pander_return(project_table),
               con = tmp)
  }

  ## add finished project summaries
  writeLines(paste("<br><br><div align='center'><h2>",
                   "*** Finished projects ***",
                   "</h3></div>\n"),
             con = tmp)

  if(length(projects_done) > 0) {

    for(i in 1:length(projects_done)) {

      project_table <- projects_done[[i]][,c(8, 9, 10, 11, 12)]
      names(project_table) <- c("ID", "UID Sen.", "UID Log.", "Type Sen.",
                                "Type Log.")

      writeLines(paste0("<br><br><div align='center'><h3>",
                        "Project summary: ", names(projects_done)[i],
                        " (Stop date: ", max(projects_done[[i]]$date_stop), ")",
                        "</h3></div>\n"),
                 con = tmp)

      writeLines(pander::pander_return(project_table),
                 con = tmp)
    }
  } else {

    for(i in 1:length(projects_done)) {

      writeLines(paste("<br><br><div align='center'><h3>",
                       "No finished projects, yet",
                       "</h3></div>\n"),
                 con = tmp)
    }
  }

  ## close rmd file
  close(tmp)

  ## render html file
  try(rmarkdown::render(input = file.rmd,
                        output_file = file.html,
                        clean = TRUE,
                        quiet = TRUE), silent = TRUE)

  ## remove rmd file
  try(invisible(unlink(file.rmd,
                       recursive = TRUE)))
  try(invisible(unlink(paste0(filename, "/map.jpeg"),
                       recursive = TRUE)))
  try(invisible(unlink(paste0(filename, "/sensors.jpeg"),
                       recursive = TRUE)))
  try(invisible(unlink(paste0(filename, "/loggers.jpeg"),
                       recursive = TRUE)))

  ## optionally open file in browser
  if (browser == TRUE) {
    try(utils::browseURL(file.html))
  }
}
