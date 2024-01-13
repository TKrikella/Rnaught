#' @importFrom methods is
#' @importFrom utils read.csv write.csv
server <- function(input, output) {
  reactive <- shiny::reactiveValues(
    data_table = data.frame(Name = character(0),
                            `Reporting Frequency` = character(0),
                            `Case Counts` = numeric(0), check.names = FALSE),
    est_table = data.frame(Dataset = character(0)),
    estimators = list()
  )

  # Validate and add datasets when button is clicked.
  # Also evaluate the new datasets on existing estimators.
  shiny::observeEvent(input$addData, {
    # Option 1: Manual entry.
    if (input$dataInputMethod == 1) {
      checks_passed <- TRUE

      # Ensure the dataset name is not blank.
      if (grepl("^\\s*$", input$dataName)) {
        output$dataNameWarn <- shiny::renderText(
          "Error: The dataset name cannot be blank.")
        checks_passed <- FALSE
      }
      # Ensure the dataset name is not a duplicate.
      else if (input$dataName %in% reactive$data_table[,1]) {
        output$dataNameWarn <- shiny::renderText(
          "Error: There is already a dataset with the specified name.")
        checks_passed <- FALSE
      }
      else
        output$dataNameWarn <- shiny::renderText("")

      # Ensure the case counts consist only of positive integers, separated by
      # commas.
      counts <- as.numeric(unlist(strsplit(input$dataCounts, split = ",")))
      if (any(is.na(counts)) || any(counts <= 0) || any(counts %% 1 != 0)) {
        output$dataCountsWarn <- shiny::renderText("Error: The list of case
          counts should only contain positive integers, separated by commas.")
        checks_passed <- FALSE
      }
      # Ensure the case counts contain at least two entries.
      else if (length(counts) < 2) {
        output$dataCountsWarn <- shiny::renderText(
          "Error: The list of case counts should contain at least two entries.")
        checks_passed <- FALSE
      }
      else
        output$dataCountsWarn <- shiny::renderText("")
      
      if (checks_passed)
        d <- data.frame(input$dataName, input$dataUnits, t(counts))
    }

    else {
      checks_passed <- FALSE

      # Option 2: Upload .csv
      if (input$dataInputMethod == 2)
        d <- try(read.csv(input$dataUpload$datapath, header = FALSE))
      # Option 3: Paste .csv
      else
        d <- try(read.csv(text = input$dataPaste, header = FALSE))

      if (is(d, "try-error"))
        output$dataCSVWarn <- shiny::renderText("Error reading file.")
      else if (ncol(d) < 4 || anyNA(d[,1]) || anyNA(sapply(d[,3:4], as.numeric))
               || !all(trimws(d[,2]) %in% c("Daily", "Weekly")))
        output$dataCSVWarn <- shiny::renderText(
          "Error: The provided .csv file does not match the required format.")
      else if (length(intersect(reactive$data_table[,1], d[,1])) > 0)
        output$dataCSVWarn <- shiny::renderText("Error: The provided .csv file
          contains dataset names which already exist.")
      else if (length(unique(d[,1])) != length(d[,1]))
        output$dataCSVWarn <- shiny::renderText(
          "Error: The provided .csv file contains duplicate dataset names.")
      else {
        output$dataCSVWarn <- shiny::renderText("")
        checks_passed <- TRUE
      }
    }

    if (checks_passed) {
      d[,3:ncol(d)] <- apply(d[,3:ncol(d)], 2, as.numeric)
      d[,3] <- data.frame(I(lapply(split(d[,3:ncol(d)], 1:nrow(d)),
                                   function(x) x[!is.na(x)])))
      d <- d[,1:3]
      d[,2] <- trimws(d[,2])
      colnames(d) <- c("Name", "Reporting Frequency", "Case Counts")
      reactive$data_table <- rbind(reactive$data_table, d)
      reactive$est_table <- update_est_row(input, output, d,
        reactive$estimators, reactive$est_table)
    }
  })
  
  output$dataTable <- shiny::renderDataTable(reactive$data_table,
                                             escape = FALSE)
  output$estTable <- shiny::renderDataTable(reactive$est_table,
                                            escape = FALSE)

  # Download table of estimates as a .csv file.
  output$downloadEst <- shiny::downloadHandler(
    filename = function() { paste0("Rnaught-", Sys.Date(), ".csv") },
    content = function(file) { write.csv(reactive$est_table, file) }
  )

  shiny::observeEvent(input$addWP, {
    if (input$serialWPKnown == 1) {
      serial <- validate_serial(input, output, "serialWPInput", "serialWPWarn")
      if (!is.na(serial)) {
        reactive$estimators[[length(reactive$estimators) + 1]] <- list(
          method = "WP", mu = serial, mu_units = input$serialWPUnits,
          search = list(B = 100, shape.max = 10, scale.max = 10))
        reactive$est_table <- update_est_col(input, output, reactive$data_table,
          reactive$estimators[[length(reactive$estimators)]],
          reactive$est_table)
      }
    }
    else {
      checks_passed <- TRUE

      grid_length <- as.numeric(input$gridLengthInput)
      max_shape <- as.numeric(input$gridShapeInput)
      max_scale <- as.numeric(input$gridScaleInput)

      if (is.na(grid_length) || grid_length <= 0 || grid_length %% 1 != 0) {
        output$gridLengthWarn <- shiny::renderText(
          "Error: The grid size must be a positive integer.")
        output$gridShapeWarn <- shiny::renderText("")
        output$gridScaleWarn <- shiny::renderText("")
        checks_passed <- FALSE
      }
      else {
        output$gridLengthWarn <- shiny::renderText("")

        if (is.na(max_shape) || max_shape < 1 / grid_length) {
          output$gridShapeWarn <- shiny::renderText("Error: The maximum shape
            must be at least the reciprocal of the grid length.")
          checks_passed <- FALSE
        }
        else
          output$gridShapeWarn <- shiny::renderText("")

        if (is.na(max_scale) || max_scale < 1 / grid_length) {
          output$gridScaleWarn <- shiny::renderText("Error: The maximum scale
            must be at least the reciprocal of the grid length.")
          checks_passed <- FALSE
        }
        else
          output$gridScaleWarn <- shiny::renderText("")
      }

      if (checks_passed) {
        reactive$estimators[[length(reactive$estimators) + 1]] <- list(
          method = "WP", mu = NA, mu_units = input$serialWPUnits,
          search = list(B = grid_length, shape.max = max_shape,
                        scale.max = max_scale))
        reactive$est_table <- update_est_col(input, output, reactive$data_table,
          reactive$estimators[[length(reactive$estimators)]],
          reactive$est_table)
      }  
    }
  })

  shiny::observeEvent(input$addseqB, {
    serial <- validate_serial(input, output, "serialseqBInput",
                              "serialseqBWarn")
    checks_passed <- !is.na(serial)

    kappa <- as.numeric(input$kappaInput)
    if (is.na(kappa) || kappa <= 0) {
      output$kappaWarn <- shiny::renderText(
        "Error: The maximum value must be a positive number.")
      checks_passed <- FALSE
    }
    else
      output$kappaWarn <- shiny::renderText("")

    if (checks_passed) {
      reactive$estimators[[length(reactive$estimators) + 1]] <- list(
        method="seqB", mu = serial, kappa = kappa,
        mu_units = input$serialseqBUnits)
      reactive$est_table <- update_est_col(input, output, reactive$data_table,
        reactive$estimators[[length(reactive$estimators)]], reactive$est_table)
    }
  })

  shiny::observeEvent(input$addID, {
    serial <- validate_serial(input, output, "serialIDInput", "serialIDWarn")
    if (!is.na(serial)) {
      reactive$estimators[[length(reactive$estimators) + 1]] <- list(
        method = "ID", mu = serial, mu_units = input$serialIDUnits)
      reactive$est_table <- update_est_col(input, output, reactive$data_table,
        reactive$estimators[[length(reactive$estimators)]], reactive$est_table)
    }
  })

  shiny::observeEvent(input$addIDEA, {
    serial <- validate_serial(input, output, "serialIDEAInput",
                              "serialIDEAWarn")
    if (!is.na(serial)) {
      reactive$estimators[[length(reactive$estimators) + 1]] <- list(
        method = "IDEA", mu = serial, mu_units = input$serialIDEAUnits)
      reactive$est_table <- update_est_col(input, output, reactive$data_table,
        reactive$estimators[[length(reactive$estimators)]], reactive$est_table)
    }
  })
}

validate_serial <- function(input, output, serialInputId, serialWarnId) {
  serial <- as.numeric(input[[serialInputId]])
  if (is.na(serial) || serial <= 0) {
    output[[serialWarnId]] <- shiny::renderText(
      "Error: The mean serial interval should be a positive number.")
    serial <- NA
  }
  else
    output[[serialWarnId]] <- shiny::renderText("") # Clear warning text.

  return(serial)
}

# Create a new column in the estimator table when a new estimator is added.
update_est_col <- function(input, output, datasets, estimator, est_table) {
  if (nrow(datasets) == 0)
    new_est_table <- data.frame(matrix(nrow = 0, ncol = ncol(est_table) + 1))
  else {
    estimates <- rep(NA, nrow(datasets))

    for (row in 1:nrow(datasets))
      estimates[row] <- eval_estimator(input, output, estimator, datasets[row,])

      if (nrow(est_table) == 0)
        new_est_table <- cbind(datasets[,1], estimates)
      else
        new_est_table <- cbind(est_table, estimates)
  }

  colnames(new_est_table) <- c(colnames(est_table), shiny::HTML(
    paste0(estimator$method, "<br/>(&mu; = ", estimator$mu, " ",
           tolower(estimator$mu_units), ")")))

  return(new_est_table)
}

# Create a new row in the estimator table when new datasets are added.
update_est_row <- function(input, output, datasets, estimators, est_table) {
  if (length(estimators) == 0) {
    if (nrow(est_table) == 0)
      new_est_table <- data.frame(datasets[,1])
    else
      new_est_table <- data.frame(c(est_table[,1], datasets[,1]))

    colnames(new_est_table) <- colnames(est_table)
  }
  else {
    new_est_table <- data.frame(matrix(nrow = nrow(datasets),
                                       ncol = length(estimators)))

    for (row in 1:nrow(datasets))
      for (col in 1:length(estimators))
        new_est_table[row, col] <- eval_estimator(input, output,
          estimators[[col]], datasets[row,])

    new_est_table <- cbind(datasets[,1], new_est_table)
    colnames(new_est_table) <- colnames(est_table)
    new_est_table <- rbind(est_table, new_est_table)
  }

  return(new_est_table)
}

# Evaluate an estimator on a given dataset.
eval_estimator <- function(input, output, estimator, dataset) {
  # Adjust serial interval to match time unit of case counts.
  serial <- estimator$mu
  if (estimator$mu_units == "Days" && dataset[2] == "Weekly")
    serial <- serial / 7
  else if (estimator$mu_units == "Weeks" && dataset[2] == "Daily")
    serial <- serial * 7

  # White and Panago
  if (estimator$method == "WP") {
    estimate <- WP(unlist(dataset[3]), mu = serial, search = estimator$search)

    if (!is.na(estimator$mu))
      estimate <- round(estimate$Rhat, 2)
    # Display the estimated mean of the serial distribution if mu was not
    # specified.
    else {
      if (dataset[2] == "Daily")
        mu_units <- "days"
      else
        mu_units <- "weeks"
      MSI <- sum(estimate$SD$supp * estimate$SD$pmf)
      estimate <- shiny::HTML(paste0(round(estimate$Rhat, 2), "<br/>(&mu; = ",
                                     round(MSI, 2), " ", mu_units, ")"))
    }
  }
  # Sequential Bayes
  else if (estimator$method == "seqB")
    estimate <- round(seq_bayes(unlist(dataset[3]), mu = serial,
                           kappa = estimator$kappa), 2)
  # Incidence Decay
  else if (estimator$method == "ID")
    estimate <- round(id(unlist(dataset[3]), mu = serial), 2)
  # Incidence Decay with Exponential Adjustement
  else if (estimator$method == "IDEA")
    estimate <- round(idea(unlist(dataset[3]), mu = serial), 2)

  return(estimate)
}
