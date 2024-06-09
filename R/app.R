#' Launch the Rnaught Web Application
#'
#' @importFrom utils install.packages
#'
#' @export
app <- function() {
  missing_pkgs <- c()
  # Check for any missing, required packages.
  if (!requireNamespace("bslib", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "bslib")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "shiny")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "DT")
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "plotly")
  }

  # If any of the required packages are missing,
  # prompt the user to install them.
  if (length(missing_pkgs) > 0) {
    cat("The following packages must be installed to run the",
      "Rnaught web application:\n"
    )
    writeLines(missing_pkgs)
    answer <- readline("Begin installation? [Y/n] ")

    if (answer == "Y" || answer == "y") {
      install.packages(missing_pkgs)
    } else {
      stop("Aborting due to missing, required packages.", call. = FALSE)
    }
  }

  shiny::runApp(appDir = system.file("app", package = "Rnaught"))
}
