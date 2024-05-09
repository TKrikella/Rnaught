ui <- htmlTemplate("index.html")

server <- function(input, output) {
  source("scripts/data.R", local = TRUE)
  source("scripts/estimators.R", local = TRUE)

  react_values <- reactiveValues()

  data_logic(input, output, react_values)
  estimators_logic(input, output, react_values)
}

shinyApp(ui, server)
