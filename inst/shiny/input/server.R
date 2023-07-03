library("shiny")
shinyServer(

  function(input, output) {

    ## find path to source files
    sourcefiles <- readLines(paste0(con = system.file(
      "extdata",
      package = "eseisManager"),
      "/data/path.txt"))

     ## define path to data base
    path_items <- paste0(sourcefiles,
                         "commits.txt")

    ## load data set
    items <- utils::read.table(file = path_items,
                               header = TRUE,
                               sep = "\t",
                               stringsAsFactors = FALSE)

    ## reassign data set
    values <- reactiveValues()
    values$df <- items

    ## listen to updates
    addData <- observe({

      if(input$submit > 0) {

        # create the new line to be added from your inputs
        newLine <- isolate(c(max(as.numeric(items[,1])) + 1,
                             input$date,
                             input$name,
                             input$project,
                             as.character(input$date_start),
                             as.character(input$date_stop),
                             input$type_station,
                             input$id_station,
                             input$uid_sensor,
                             input$uid_logger,
                             input$type_sensor,
                             input$type_logger,
                             input$bob,
                             input$uid_battery,
                             input$uid_cable,
                             TRUE))

        ## append new entry to data set
        isolate(values$df <- rbind(as.matrix(values$df),
                                   unlist(newLine)))

        ## re-assign data set
        items <<- values$df

        ## remove duplicates
        items <- items[!duplicated(items[,-1]),]

        ## save updated data set
        write.table(x = rbind(unlist(newLine)),
                    file = path_items,
                    append = TRUE,
                    row.names = FALSE,
                    col.names = FALSE,
                    sep = "\t")
      }
    })

    observeEvent(input$check, {

      chk <- eseisManager::check_validity(path_items)

      if(length(chk) == 0) {

        showNotification(ui = "No issues found",
                         duration = 5,
                         type = "message")
      } else {

        showNotification(ui = paste(chk, collapse = "\n"),
                         duration = NULL,
                         type = "error")
      }


    })

    ## render tabular output
    output$table_1 <- renderTable({

      colnames(values$df) <- c(
        "ID", "Date", "Name", "Project", "Start", "End",
        "Station_type", "Station_ID", "Sensor_UID", "Logger_UID",
        "Sensor_type", "Logger_type", "BOB", "Battery_UID", "Cable_UID", "check_include")

      if(nrow(values$df) > 1) {

        values$df[nrow(values$df):1,]
      }

    },
    include.rownames = FALSE)
  }
)
