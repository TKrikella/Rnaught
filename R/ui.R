ui <- function() {
  shiny::fluidPage(
    # Title.
    shiny::titlePanel(shiny::HTML(
      paste0("Rnaught: An Estimation Suite for R", shiny::tags$sub("0")))),
    # Sidebar layout.
    shiny::sidebarLayout(
      # Sidebar. Hidden if the 'About' tab is selected.
      shiny::conditionalPanel(condition = "input.tabset != 'About'",
        shiny::sidebarPanel(id = "sidebar", data_sidebar(), est_sidebar())),
      # Main panel.
      shiny::mainPanel(id = "main",
        shiny::tabsetPanel(id = "tabset", type = "tabs",
          shiny::tabPanel("About", shiny::br(), "Hello"),
          shiny::tabPanel("Data", shiny::br(),
            shiny::dataTableOutput(outputId = "dataTable"),
            shiny::tags$style(type = "text/css",
                              "#dataTable tfoot { display:none; }")),
          shiny::tabPanel("Estimators", shiny::br(),
            shiny::dataTableOutput(outputId = "estTable"),
            shiny::tags$style(type = "text/css",
                              "#estTable tfoot { display:none; }"),
            shiny::downloadButton(outputId = "downloadEst",
                                  label = "Download table as .csv"))
        )
      )
    )
  )
}

# Data tab sidebar.
data_sidebar <- function() {
  shiny::conditionalPanel(condition = "input.tabset == 'Data'",
    shiny::h3("Enter data"),
    # Data input method selection.
    shiny::radioButtons(inputId = "dataInputMethod", label = "",
      choices=list("Manually" = 1, "Upload a .csv file" = 2,
                   "Paste a .csv file" = 3)),
    # Option 1: Manual entry.
    shiny::conditionalPanel(condition = "input.dataInputMethod == '1'",
      shiny::textInput(inputId = "dataName", label = "Dataset name"),
      shiny::span(shiny::textOutput(outputId = "dataNameWarn"),
                  style = "color: red;"),
      shiny::fluidRow(
        shiny::column(8,
          shiny::textInput(inputId = "dataCounts",
            label = help_tool("Case counts", paste0("Enter as a ",
              "comma-separated list of positive integers, with at least two ",
              "entries. (Example: 1,1,2,3,5,8)")))),
        shiny::column(4, shiny::selectInput(inputId = "dataUnits",
          label = "Reporting frequency", choices = list("Daily", "Weekly")))
      ),
      shiny::span(shiny::textOutput(outputId = "dataCountsWarn"),
                  style = "color: red;")
    ),
    # Option 2: Upload .csv file.
    shiny::conditionalPanel(condition = "input.dataInputMethod == '2'",
      shiny::fileInput(inputId = "dataUpload", label = "",
        accept = c("text/csv", "text/comma-separated-values,text/plain",
                   ".csv")),
    ),
    # Option 3: Paste .csv file.
    shiny::conditionalPanel(condition = "input.dataInputMethod == '3'",
      shiny::textAreaInput(inputId = "dataPaste", label = "",
                           rows = 8, resize = "none"),
    ),
    # Warning text for .csv upload / paste.
    shiny::conditionalPanel(
      condition = "['2', '3'].includes(input.dataInputMethod)",
      shiny::span(shiny::textOutput(outputId = "dataCSVWarn"),
                  style = "color: red;"),
    ),
    # Button to add data.
    shiny::actionButton(inputId = "addData", label = "Add")
  )
}

# Estimators tab sidebar.
est_sidebar <- function() {
  shiny::conditionalPanel(condition = "input.tabset == 'Estimators'",
    shiny::h3("Estimators"),
    WP_collapse(),
    seqB_collapse(),
    ID_collapse(),
    IDEA_collapse(),

    shiny::tags$style(type = "text/css",
                      "summary { display: list-item; cursor: pointer; }"),
    shiny::tags$style(type = "text/css", "summary h4 { display: inline; }")
  )
}

# Collapsable entry for White & Pagano (WP) method.
WP_collapse <- function() {
  shiny::tags$details(
    shiny::tags$summary(shiny::h4("White & Pagano (WP)")),
    shiny::p("Method due to White and Pagano (2008), assumes a branching process
             based model. Serial distribution can be assumed known or can be
             estimated using maximum likelihood;  When serial interval is
             unknown the method takes longer to compute, though is still
             real-time."),
    shiny::br(),
    shiny::radioButtons(inputId = "serialWPKnown",
                        label = "Is the mean serial interval known?",
                        inline = TRUE, choices = list("Yes" = 1, "No" = 2)),
    # Known serial interval.
    shiny::conditionalPanel(condition = "input.serialWPKnown == '1'",
      serial_fields("WP")),
    # Unknown serial interval.
    shiny::conditionalPanel(condition = "input.serialWPKnown == '2'",
      shiny::h5("Grid Search Parameters"),
      shiny::fluidRow(
        shiny::column(4, shiny::textInput(inputId = "gridLengthInput",
          label = "Grid length", value = "100")),
        shiny::column(4, shiny::textInput(inputId = "gridShapeInput",
          label = "Max. shape", value = "10")),
        shiny::column(4, shiny::textInput(inputId = "gridScaleInput",
          label = "Max. scale", value = "10"))
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::span(shiny::textOutput(
          outputId = "gridLengthWarn"), style = "color: red;")),
        shiny::column(4, shiny::span(shiny::textOutput(
          outputId = "gridShapeWarn"), style = "color: red;")),
        shiny::column(4, shiny::span(shiny::textOutput(
          outputId = "gridScaleWarn"), style = "color: red;"))
      )
    ),
    shiny::actionButton(inputId = "addWP", label = "Add")
  )
}

# Collapsable entry for Sequential Bayes (seqB) method.
seqB_collapse <- function() {
  shiny::tags$details(
    shiny::tags$summary(shiny::h4("Sequential Bayes (seqB)")),
    shiny::p("This is a description of the method."),
    shiny::br(),
    serial_fields("seqB"),
    shiny::textInput(inputId = "kappaInput",
      label = help_tool("Maximum value", paste0("This describes the prior ",
        "belief of R0, and should be set to a higher value if R0 is believed ",
        "be larger. (Default: 20)")), value = "20"),
    shiny::span(shiny::textOutput(outputId = "kappaWarn"),
                style = "color: red;"),
    shiny::actionButton(inputId = "addseqB", label = "Add")
  )
}

# Collapsable entry for Incidence Decay (ID) method.
ID_collapse <- function() {
  shiny::tags$details(
    shiny::tags$summary(shiny::h4("Incidence Decay (ID)")),
    shiny::p("This is a description of the method."),
    shiny::br(),
    serial_fields("ID"),
    shiny::actionButton(inputId = "addID", label = "Add")
  )
}

# Collapsable entry for Incidence Decay & Exponential Adjustment (IDEA) method.
IDEA_collapse <- function() {
  shiny::tags$details(
    shiny::tags$summary(
      shiny::h4("Incidence Decay and Exponential Adjustment (IDEA)")),
    shiny::p("This is a description of the method."),
    shiny::br(),
    serial_fields("IDEA"),
    shiny::actionButton(inputId = "addIDEA", label = "Add")
  )
}

# Input fields and warning text for the mean serial interval.
serial_fields <- function(method) {
  shiny::HTML(paste0(
    shiny::fluidRow(
      shiny::column(8, shiny::textInput(
        inputId = paste0("serial", method, "Input"),
        label = "Mean Serial Interval")),
      shiny::column(4, shiny::selectInput(
        inputId = paste0("serial", method, "Units"),
        label = "Time units", choices = list("Days", "Weeks")))
    ),
    shiny::span(shiny::textOutput(outputId = paste0("serial", method, "Warn")),
                style = "color: red;")
  ))
}

# Display help information on hover.
help_tool <- function(label, help_text) {
  shiny::HTML(paste0(label, shiny::tags$sup("[?]", title = help_text)))
}
