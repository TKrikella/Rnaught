#' Launch the Rnaught Web Application
#'
#' @export
app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("The package 'shiny' must be installed to launch the Rnaught web application.")

  shiny::shinyApp(ui, server)
}
