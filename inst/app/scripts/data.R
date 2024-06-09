# Main logic block for data-related interactions.
data_logic <- function(input, output, react_values) {
  # Initialize a data frame to hold the datasets.
  react_values$data_table <- data.frame(
    Name = character(0),
    `Time units` = character(0),
    `Case counts` = character(0),
    check.names = FALSE
  )

  manual_entry(input, output, react_values)
  upload_data(input, output, react_values)
  load_samples(input, output, react_values)
  render_data_table(output, react_values)
  render_plot(input, output, react_values, "Days")
  render_plot(input, output, react_values, "Weeks")
  delete_data(input, react_values)
  export_data(output, react_values)
}

# Convert the input case counts string to an integer vector.
tokenize_counts <- function(counts_str) {
  suppressWarnings(as.integer(unlist(strsplit(trimws(counts_str), ","))))
}

# Render the plots for daily and weekly data when the data table is updated.
render_plot <- function(input, output, react_values, time_units) {
  observe({
    datasets <- react_values$data_table[
      which(react_values$data_table[["Time units"]] == time_units),
    ]

    data_plot <- plotly::plot_ly(type = "scatter", mode = "lines")
    if (nrow(datasets) > 0) {
      for (i in seq_len(nrow(datasets))) {
        counts <- tokenize_counts(datasets[i, 3])
        data_plot <- plotly::add_trace(data_plot,
          x = seq_along(counts) - 1, y = counts, name = datasets[i, 1]
        )
      }
    }

    plot_title <- paste(
      if (time_units == "Days") "Daily" else "Weekly", "case counts"
    )

    data_plot <- plotly::layout(data_plot, title = plot_title,
      xaxis = list(title = time_units), yaxis = list(title = "Cases")
    )

    data_plot <- plotly::config(data_plot, displaylogo = FALSE,
      toImageButtonOptions = list(
        filename = paste0("Rnaught_data_", tolower(time_units), "_plot")
      )
    )

    output[[paste0("data_plot_", tolower(time_units))]] <-
      plotly::renderPlotly(data_plot)
  })
}

# Validate and add manually-entered datasets.
manual_entry <- function(input, output, react_values) {
  observeEvent(input$data_bulk, {
    validate_data(input, output, react_values, "data_area")
  })
}

# Validate and add datasets from a CSV file.
upload_data <- function(input, output, react_values) {
  observeEvent(input$data_upload, {
    validate_data(input, output, react_values, "data_upload")
  })
}

# Validate datasets and update the data table.
validate_data <- function(input, output, react_values, data_source) {
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
        # corresponding columns in the estimates table.
        update_estimates_cols(new_rows, react_values)

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
      # corresponding columns in the estimates table.
      update_estimates_cols(new_rows, react_values)

      showNotification("Datasets added successfully.",
        duration = 3, id = "notify-success"
      )
    }
  })
}

# Render the data table when new datasets are added.
render_data_table <- function(output, react_values) {
  observe({
    output$data_table <- DT::renderDataTable(
      react_values$data_table, rownames = FALSE
    )
  })
}

# Delete rows in the data table and the corresponding columns in the estimates
# table.
delete_data <- function(input, react_values) {
  observeEvent(input$data_delete, {
    rows_selected <- input$data_table_rows_selected
    react_values$data_table <- react_values$data_table[-rows_selected, ]
    react_values$estimates_table <-
      react_values$estimates_table[, -(rows_selected + 2)]
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
# add new columns to the estimates table.
update_estimates_cols <- function(datasets, react_values) {
  new_cols <- data.frame(
    matrix(nrow = nrow(react_values$estimates_table), ncol = nrow(datasets))
  )
  colnames(new_cols) <- datasets[, 1]

  if (nrow(new_cols) > 0) {
    for (row in seq_len(nrow(new_cols))) {
      estimator <- react_values$estimators[[row]]
      for (col in seq_len(ncol(new_cols))) {
        new_cols[row, col] <- eval_estimator(estimator, datasets[col, ])
      }
    }
  }

  react_values$estimates_table <- cbind(
    react_values$estimates_table, new_cols
  )
}
