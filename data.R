# Main logic block for data-related interactions.
data_logic <- function(input, output, react_values) {
  # Initialize a data frame to hold the datasets.
  react_values$data_table <- data.frame(
    Name = character(0),
    `Time units` = character(0),
    `Case counts` = character(0),
    check.names = FALSE
  )
  
  render_plot(input, output)
  single_entry(input, output, react_values)
  bulk_entry(input, output, react_values)
  upload_entry(input, output, react_values)
  sample_entry(input, output, react_values)
  render_data(output, react_values)
  delete_data(input, react_values)
  export_data(output, react_values)
}

warnings <- function(df, df_elems) {
  
  warning_text <- ''
  
  # Ensure the dataset names are neither blank nor duplicates.
  if (anyNA(df_elems[[1]]) || any(df_elems[[1]] == "")) {
    warning_text <- paste0(warning_text, sep = "<br>",
                           "Each row must begin with a non-blank dataset name."
    )
  } 
  
  if (length(unique(df_elems[[1]])) != length(df_elems[[1]])) {
    warning_text <- paste0(warning_text, sep = "<br>",
                           "The rows contain duplicate dataset names."
    )
  }
  
  if (any(df_elems[[1]] %in% react_values$data_table[, 1])) {
    warning_text <- paste0(warning_text, sep = "<br>",
                           "The rows contain dataset names which already exist."
    )
  }
  # Ensure the second entry in each row is a time unit equal to
  # "Days" or "Weeks".
  if (!all(df_elems[[2]] %in% c("Days", "Weeks"))) {
    warning_text <- paste0(warning_text, sep = "<br>",
                           "The second entry in each row must be either 'Days' or 'Weeks'."
    )
  }
  # Ensure the counts in each row have at least one non-negative integer.
  if (any(df_elems[[3]] == "")) {
    warning_text <- paste0(warning_text, sep = "<br>",
                           "Each row must contain at least one non-negative integer."
    )
  }
  return(warning_text)
}


# Convert the input case counts string to an integer vector.
tokenize_counts <- function(counts_str) {
  suppressWarnings(as.integer(unlist(strsplit(trimws(counts_str), ","))))
}

# Render the preview plot for single entry data.
render_plot <- function(input, output) {
  observe({
    counts <- tokenize_counts(input$data_counts)
    if (length(counts) > 0 && !anyNA(counts) && all(counts >= 0)) {
      output$data_plot <- renderPlot(
        plot(seq_along(counts) - 1, counts, type = "o", pch = 16, col = "black",
             xlab = input$data_units, ylab = "Cases", cex.lab = 1.5,
             xlim = c(0, max(length(counts) - 1, 1)), ylim = c(0, max(counts, 1))
        )
      )
    } else {
      output$data_plot <- renderPlot(
        plot(NULL, xlim = c(0, 10), ylim = c(0, 10),
             xlab = input$data_units, ylab = "Cases", cex.lab = 1.5
        )
      )
    }
  })
}

# Add a single dataset to the existing table.
single_entry <- function(input, output, react_values) {
  observeEvent(input$data_single, {
    valid <- TRUE
    
    # Ensure the dataset name is neither blank nor a duplicate.
    name <- trimws(input$data_name)
    if (name == "") {
      output$data_name_warn <- renderText("The dataset name cannot be blank.")
      valid <- FALSE
    } else if (name %in% react_values$data_table[, 1]) {
      output$data_name_warn <- renderText(
        "There is already a dataset with the specified name."
      )
      valid <- FALSE
    } else {
      output$data_name_warn <- renderText("")
    }
    
    # Ensure the case counts are specified as a comma-separated of one or more
    # non-negative integers.
    counts <- tokenize_counts(input$data_counts)
    if (length(counts) == 0) {
      output$data_counts_warn <- renderText("Case counts cannot be blank.")
      valid <- FALSE
    } else if (anyNA(counts) || any(counts < 0)) {
      output$data_counts_warn <- renderText(
        "Case counts can only contain non-negative integers."
      )
      valid <- FALSE
    } else {
      output$data_counts_warn <- renderText("")
    }
    
    if (valid) {
      # Add the new dataset to the data table.
      new_row <- data.frame(name, input$data_units, toString(counts))
      colnames(new_row) <- c("Name", "Time units", "Case counts")
      react_values$data_table <- rbind(react_values$data_table, new_row)
      
      # Evaluate all existing estimators on the new dataset and update the
      # corresponding row in the estimates table.
      update_estimates_rows(new_row, react_values)
      
      showNotification("Dataset added successfully.",
                       duration = 3, id = "notify-success"
      )
    }
  })
}

# Add multiple datasets to the existing table.
bulk_entry <- function(input, output, react_values) {
  observeEvent(input$data_bulk, {
    tryCatch(
      {
        datasets <- read.csv(text = input$data_area, header = FALSE, sep = ",")
        
        names <- trimws(datasets[, 1])
        units <- trimws(datasets[, 2])
        counts <- apply(datasets[, 3:ncol(datasets)], 1,
                        function(row) {
                          row <- suppressWarnings(as.integer(row))
                          toString(row[!is.na(row) & row >= 0])
                        }
        )
        output$data_area_warn <- renderText("")
        warning_text <- warnings(datasets, list(names, units, counts))
        
        if (warning_text == "") {
          # Add the new datasets to the data table.
          new_rows <- data.frame(names, units, counts)
          colnames(new_rows) <- c("Name", "Time units", "Case counts")
          react_values$data_table <- rbind(react_values$data_table, new_rows)
          
          # Evaluate all existing estimators on the new dataset and update the
          # corresponding row in the estimates table.
          update_estimates_rows(new_rows, react_values)
          
          showNotification("Datasets added successfully.",
                           duration = 3, id = "notify-success"
          )
        } else {
          output$data_area_warn <- renderUI(HTML(warning_text))
        }
      },
      error = function(e) {
        output$data_area_warn <- renderText(
          "The input does not match the required format."
        )
      }
    )
  })
}

# Upload datasets to the existing table.
upload_entry <- function(input, output, react_values) {
  observeEvent(input$data_load, {
    tryCatch(
      {
        df <- read.csv(file = input$upload_csv$datapath)
        names <- trimws(df[, 1])
        units <- trimws(df[, 2])
        counts <- sapply(tokenize_counts(df[, 3:ncol(df)]),
                         function(row) {
                           row <- suppressWarnings(as.integer(row))
                           toString(row[!is.na(row) & row >= 0])
                         }
        )
        output$data_load_warn <- renderText("")
        warning_text <- ''
        warning_text <- warnings(df, list(names, units, counts))
        
        if (warning_text == "") {
          
          # Add the new datasets to the data table.
          new_rows <- read.csv(file = input$upload_csv$datapath)
          colnames(new_rows) <- c("Name", "Time units", "Case counts")
          react_values$data_table <- rbind(react_values$data_table, new_rows)
          
          # Evaluate all existing estimators on the new dataset and update the
          # corresponding row in the estimates table.
          update_estimates_rows(new_rows, react_values)
          
          showNotification("Datasets added successfully.",
                           duration = 3, id = "notify-success")
          
          
        } else {
          output$data_load_warn <- renderUI(HTML(warning_text))        
        }
      },
      error = function(e) {
        output$data_load_warn <- renderText(
          "The input does not match the required format."
        )
      }
    )
  })
}

# Add sample datasets to the existing table.
sample_entry <- function(input, output, react_values) {
  observeEvent(input$sample_entry, {
    tryCatch(
      {
        # datasets <- read.csv(text = input$sample, header = FALSE, sep = ",")
        
        names <- c()
        units <- c()
        counts <-c()
        
        if (input$march){
          names <- append(names, c("Covid-19 March 2020"))
          units<-append(units, c("Daily"))
          counts<-append(counts,c(covid_cases[1]))}
        if (input$april){ names <- append(names, c("Covid-19 April 2020"))
        units<-append(units, c("Daily"))
        counts<-append(counts,c(covid_cases[2]))}
        if (input$may){ names <- append(names, c("Covid-19 May 2020"))
        units<-append(units, c("Daily"))
        counts<-append(counts,c(covid_cases[3]))}
        
        
        if (input$june){ names <- append(names, c("Covid-19 June 2020"))
        units<-append(units, c("Daily"))
        counts<-append(counts,c(covid_cases[4]))}
        
        if (input$july){ names <- append(names, c("Covid-19 July 2020"))
        units<-append(units, c("Daily"))
        counts<-append(counts,c(covid_cases[5]))}
        
        warning_text <- ""
        
        # Ensure the dataset names are not duplicates.
        
        
        if (any(names %in% react_values$data_table[, 1])) {
          warning_text <- paste0(warning_text, sep = "<br>",
                                 "The rows contain dataset names which already exist."
          )
          
        }
        
        
        output$sample_area_warn <- renderUI(HTML(warning_text))
        
        if (warning_text == "") {
          # Add the new datasets to the data table.
          
          new_rows <- data.frame(names, units, counts)
          colnames(new_rows) <- c("Name", "Time units", "Case counts")
          react_values$data_table <- rbind(react_values$data_table, new_rows)
          
          # Evaluate all existing estimators on the new dataset and update the
          # corresponding row in the estimates table.
          update_estimates_rows(new_rows, react_values)
          
          showNotification("Datasets added successfully.",
                           duration = 3, id = "notify-success"
          )
        }
      }
    )
  })
}

# Render the data table when new datasets are added.
render_data <- function(output, react_values) {
  observe({
    output$data_table <- DT::renderDataTable(react_values$data_table)
  })
}

# Delete rows in the data table,
# and the corresponding rows in the estimates table.
delete_data <- function(input, react_values) {
  observeEvent(input$data_delete, {
    new_table <- react_values$data_table[-input$data_table_rows_selected, ]
    if (nrow(new_table) > 0) {
      rownames(new_table) <- seq_len(nrow(new_table))
    }
    react_values$data_table <- new_table
    
    if (ncol(react_values$estimates_table) == 1) {
      react_values$estimates_table <- data.frame(
        Datasets = react_values$data_table[, 1]
      )
    } else {
      react_values$estimates_table <-
        react_values$estimates_table[-input$data_table_rows_selected, ]
    }
  })
}

# Export data table as a CSV file.
export_data <- function(output, react_values) {
  output$data_export <- downloadHandler(
    filename = function() {
      paste0("Rnaught_data_", format(Sys.time(), "%y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      write.csv(react_values$data_table, file, row.names = FALSE)
    }
  )
}

# When new datasets are added, evaluate all existing estimators on them and
# add new rows to the estimates table.
update_estimates_rows <- function(datasets, react_values) {
  new_rows <- data.frame(
    matrix(nrow = nrow(datasets), ncol = ncol(react_values$estimates_table))
  )
  colnames(new_rows) <- colnames(react_values$estimates_table)
  
  for (row in seq_len(nrow(datasets))) {
    new_rows[row, 1] <- datasets[row, 1]
    
    if (length(react_values$estimators) > 0) {
      for (col in 2:ncol(react_values$estimates_table)) {
        new_rows[row, col] <- eval_estimator(
          react_values$estimators[[col - 1]], datasets[row, ]
        )
      }
    }
  }
  
  react_values$estimates_table <- rbind(
    react_values$estimates_table, new_rows
  )
}
#Sample datasets case counts
covid_cases = c("7,1,13,10,6,10,13,28,47,53,62,90,88,130,143,150,186,276,279,350,458,604,570,667,878,883,785,1085,1252",
                "1469,1278,1346,1119,1109,1120,1202,1429,1178,1337,1165,1312,1551,1633,1870,1688,1888,1702,1535,1549,1563,1583,1777,1511,1482,1298,1350,1422,1502,1546",
                "1499,1330,1232,1205,1101,1306,1317,1187,1115,997,953,903,1086,1101,1198,1133,1219,1057,954,1061,1056,1094,922,884,963,660,762,781,1038,763,827",
                "678,656,602,545,557,497,464,411,391,481,402,427,380,322,309,345,358,375,373,300,315,340,288,297,280,330,344,358,242,267",
                "315,291,267,284,244,220,269,313,359,343,348,351,277,362,451,443,517,490,457,472,507,509,573,497,425,408,344,493,405,466,455")

