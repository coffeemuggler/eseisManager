library("shiny")

sourcefiles <- readLines(paste0(con = system.file(
  "extdata",
  package = "eseisManager"),
  "/data/path.txt"))

print(paste("Using default sourcefile directory at:", sourcefiles))

## check/set source files directory
if(file.exists(paste0(sourcefiles, "batteries.txt")) == FALSE) {

  stop("No batteries file found! Check path and file name.")
}

if(file.exists(paste0(sourcefiles, "cables.txt")) == FALSE) {

  stop("No cables file found! Check path and file name.")
}

if(file.exists(paste0(sourcefiles, "loggers.txt")) == FALSE) {

  stop("No loggers file found! Check path and file name.")
}

if(file.exists(paste0(sourcefiles, "names.txt")) == FALSE) {

  stop("No names file found! Check path and file name.")
}

if(file.exists(paste0(sourcefiles, "projects.txt")) == FALSE) {

  stop("No projects file found! Check path and file name.")
}

if(file.exists(paste0(sourcefiles, "sensors.txt")) == FALSE) {

  stop("No sensors file found! Check path and file name.")
}

names <- readLines(con = paste0(sourcefiles,
                                "names.txt"),
                   warn = FALSE)
projects <- read.table(file = paste0(sourcefiles,
                                     "projects.txt"),
                       header = TRUE,
                       sep = ",",
                       stringsAsFactors = FALSE)
sensors <- read.table(file = paste0(sourcefiles,
                                    "sensors.txt"),
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE)
loggers <- read.table(file = paste0(sourcefiles,
                                    "loggers.txt"),
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE)
batteries <- read.table(file = paste0(sourcefiles,
                                      "batteries.txt"),
                        header = TRUE,
                        sep = ",",
                        stringsAsFactors = FALSE)
cables <- read.table(file = paste0(sourcefiles,
                                      "cables.txt"),
                        header = TRUE,
                        sep = ",",
                        stringsAsFactors = FALSE)

shinyUI(
    fluidPage(
        sidebarPanel(width = 2,
            textInput("date",
                      label = "Date",
                      value = Sys.time()),
            selectInput(inputId = "name",
                        label = "Name",
                        choices = names,
                        selected = NULL,
                        multiple = FALSE),
            selectInput(inputId = "project",
                        label = "Project",
                        choices = projects$name,
                        selected = NULL,
                        multiple = FALSE),
            textInput("id_station",
                      label="Station ID"),
            selectInput(inputId = "type_station",
                        label = "Station type",
                        choices = c("NA",
                                    "compact",
                                    "complex"),
                        multiple = FALSE),
            dateInput(inputId = "date_start",
                      label = "Start date",
                      format = "yyyy-mm-dd",
                      language = "en",
                      width = NULL),
            dateInput(inputId = "date_stop",
                      label = "Stop date",
                      format = "yyyy-mm-dd",
                      language = "en",
                      width = NULL),
            selectInput(inputId = "type_sensor",
                        label = "Sensor type",
                        choices = sensors$type,
                        multiple = FALSE),
            selectInput(inputId = "type_logger",
                        label = "Logger type",
                        choices = loggers$type,
                        multiple = FALSE),
            selectInput(inputId = "uid_sensor",
                        label = "Sensor UID",
                        choices = sensors$UID,
                        multiple = FALSE),
            selectInput(inputId = "uid_logger",
                        label = "Logger UID",
                        choices = loggers$UID,
                        multiple = FALSE),
            selectInput(inputId = "bob",
                        label = "Break-out box",
                        choices = c("Yes", "No"),
                        selected = "No",
                        multiple = FALSE),
            selectInput(inputId = "uid_battery",
                        label = "Battery UID",
                        choices = batteries$UID,
                        multiple = FALSE),
            selectInput(inputId = "uid_cable",
                        label = "Cable UID",
                        choices = cables$UID,
                        multiple = FALSE),
            actionButton("submit",
                         label = "Submit"),
            actionButton("check",
                         label = "Check"),
            helpText(paste("Source files are stored at:",
                           sourcefiles))
        ),
        mainPanel(
            tableOutput("table_1"))
    ))
