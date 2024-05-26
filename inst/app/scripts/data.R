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
  manual_bulk_entry(input, output, react_values)
  upload_data(input, output, react_values)
  load_samples(input, output, react_values)
  render_data(output, react_values)
  delete_data(input, react_values)
  export_data(output, react_values)
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

manual_bulk_entry <- function(input, output, react_values) {
  observeEvent(input$data_bulk, {
    validate_bulk_data(input, output, react_values, "data_area")
  })
}

upload_data <- function(input, output, react_values) {
  observeEvent(input$data_upload, {
    validate_bulk_data(input, output, react_values, "data_upload")
  })
}

validate_bulk_data <- function(input, output, react_values, data_source) {
  tryCatch(
    {
      if (data_source == "data_area") {
        datasets <- read.csv(text = input$data_area, header = FALSE, sep = ",")
      } else if (data_source == "data_upload") {
        datasets <- read.csv(
          file = input$data_upload$datapath, header = FALSE, sep = ","
        )
      }

      names <- trimws(datasets[, 1])
      units <- trimws(datasets[, 2])
      counts <- apply(data.frame(datasets[, 3:ncol(datasets)]), 1,
        function(row) {
          row <- suppressWarnings(as.integer(row))
          toString(row[!is.na(row) & row >= 0])
        }
      )

      warning_text <- ""

      # Ensure the dataset names are neither blank nor duplicates.
      if (anyNA(names) || any(names == "")) {
        warning_text <- paste0(warning_text,
          "Each row must begin with a non-blank dataset name.<br>"
        )
      } else {
        if (length(unique(names)) != length(names)) {
          warning_text <- paste0(warning_text,
            "The rows contain duplicate dataset names.<br>"
          )
        }
        if (any(names %in% react_values$data_table[, 1])) {
          warning_text <- paste0(warning_text,
            "The rows contain dataset names which already exist.<br>"
          )
        }
      }

      # Ensure the second entry in each row is a time unit equal to
      # "Days" or "Weeks".
      if (!all(units %in% c("Days", "Weeks"))) {
        warning_text <- paste0(warning_text,
          "The second entry in each row must be either 'Days' or 'Weeks'.<br>"
        )
      }

      # Ensure the counts in each row have at least one non-negative integer.
      if (any(counts == "")) {
        warning_text <- paste0(warning_text,
          "Each row must contain at least one non-negative integer.<br>"
        )
      }

      output[[paste0(data_source, "_warn")]] <- renderUI(HTML(warning_text))

      if (warning_text == "") {
        # Add the new datasets to the data table.
        new_rows <- data.frame(names, units, counts)
        colnames(new_rows) <- c("Name", "Time units", "Case counts")
        react_values$data_table <- rbind(react_values$data_table, new_rows)

        # Evaluate all existing estimators on the new datasets and update the
        # corresponding rows in the estimates table.
        update_estimates_rows(new_rows, react_values)

        showNotification("Datasets added successfully.",
          duration = 3, id = "notify-success"
        )
      }
    },
    error = function(e) {
      output[[paste0(data_source, "_warn")]] <- renderText(
        "The input does not match the required format."
      )
    }
  )
}

# Load sample datasets.
load_samples <- function(input, output, react_values) {
  observeEvent(input$data_samples, {
    names <- c()
    units <- c()
    counts <- c()

    # COVID-19 Canada, March 2020 (weekly).
    if (input$covid_canada) {
      names <- c(names, "COVID-19 Canada 2020/03/03 - 2020/03/31")
      units <- c(units, "Weeks")
      counts <- c(counts, toString(Rnaught::COVIDCanada[seq(41, 69, 7), 2]))
    }
    # COVID-19 Ontario, March 2020 (weekly).
    if (input$covid_ontario) {
      names <- c(names, "COVID-19 Ontario 2020/03/03 - 2020/03/31")
      units <- c(units, "Weeks")
      counts <- c(counts,
        toString(Rnaught::COVIDCanadaPT[seq(10176, 10204, 7), 3])
      )
    }

    if (length(names) == 0) {
      output$data_samples_warn <- renderText(
        "At least one sample dataset must be selected."
      )
    } else if (any(names %in% react_values$data_table[, 1])) {
      output$data_samples_warn <- renderText(
        "At least one of the selected dataset names already exist."
      )
    } else {
      output$data_samples_warn <- renderText("")

      new_rows <- data.frame(names, units, counts)
      colnames(new_rows) <- c("Name", "Time units", "Case counts")
      react_values$data_table <- rbind(react_values$data_table, new_rows)

      # Evaluate all existing estimators on the sample datasets and update the
      # corresponding rows in the estimates table.
      update_estimates_rows(new_rows, react_values)

      showNotification("Datasets added successfully.",
        duration = 3, id = "notify-success"
      )
    }
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
