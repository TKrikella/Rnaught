#' Launch the Rnaught Web Application
#'
#' @importFrom utils install.packages
#'
#' @export
app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    answer <- readline(paste0("The package 'shiny' must be installed to ",
      "launch the Rnaught web application.\nWould you like to install it? ",
      "[Y/n] "))

    if (answer == "Y" || answer == "y")
      install.packages("shiny")
    else
      stop("Aborting.", call. = FALSE)
  }

  shiny::shinyApp(ui(), server)
}
