ui <- function() { shiny::fluidPage(
  # Title.
  shiny::titlePanel(shiny::HTML(
    paste0("Rnaught: An Estimation Suite for R", shiny::tags$sub("0")))),
  # Sidebar layout.
  shiny::sidebarLayout(
    # Sidebar. Hidden if the 'About' tab is selected.
    shiny::conditionalPanel(condition = "input.tabset != 'About'",
      shiny::sidebarPanel(id = "sidebar",
        # Data tab sidebar.
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
                shiny::textInput(inputId = "dataCounts", label = shiny::HTML(
                  paste0("Case counts", shiny::tags$sup("[?]",
                    title = paste0("Enter as a comma-separated list of ",
                                   "positive integers, with at least two ",
                                   "entries. Example: 1,1,2,3,5,8")))))),
              shiny::column(4,
                shiny::selectInput(inputId = "dataUnits",
                                   label = "Reporting frequency",
                                   choices = list("Daily", "Weekly")))
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
          shiny::actionButton(inputId = "addData", label = "Add"),
        ),
        # Estimators tab sidebar (collapsable entries).
        shiny::conditionalPanel(condition = "input.tabset == 'Estimators'",
          shiny::h3("Estimators"),
          # WHITE & PANAGO (WP).
          shiny::tags$details(
            shiny::tags$summary(shiny::h4("White & Panago (WP)")),
            shiny::p("Method due to [White and Pagano(2008)], assumes a branching process based model. Serial
distribution can be assumed known or can be estimated using maximum likelihood;  When serial interval is unknown
the method takes longer to compute, though is still real-time."),
            shiny::br(),
            shiny::radioButtons(inputId = "serialWPKnown",
                                label = "Is the mean serial interval known?",
                                inline = TRUE,
                                choices = list("Yes" = 1, "No" = 2)),
            # Known serial interval.
            shiny::conditionalPanel(condition = "input.serialWPKnown == '1'",
              shiny::fluidRow(
                shiny::column(8,
                  shiny::textInput(inputId = "serialWPInput",
                                   label = "Mean Serial Interval")),
                shiny::column(4,
                  shiny::selectInput(inputId = "serialWPUnits",
                                     label = "Time units",
                                     choices = list("Days", "Weeks")))
              ),
              shiny::span(shiny::textOutput(outputId = "serialWPWarn"),
                          style = "color: red;")
            ),
            # Unknown serial interval.
            shiny::conditionalPanel(condition = "input.serialWPKnown == '2'",
              shiny::h5("Grid Search Parameters"),
              shiny::fluidRow(
                shiny::column(4,
                  shiny::textInput(inputId = "gridLengthInput",
                                   label = "Grid length", value = "100")),
                shiny::column(4,
                  shiny::textInput(inputId = "gridShapeInput",
                                   label = "Max. shape", value = "10")),
                shiny::column(4,
                  shiny::textInput(inputId = "gridScaleInput",
                                   label = "Max. scale", value = "10"))
              ),
              shiny::fluidRow(
                shiny::column(4,
                  shiny::span(shiny::textOutput(outputId = "gridLengthWarn"),
                              style = "color: red;")),
                shiny::column(4,
                  shiny::span(shiny::textOutput(outputId = "gridShapeWarn"),
                              style = "color: red;")),
                shiny::column(4,
                  shiny::span(shiny::textOutput(outputId = "gridScaleWarn"),
                              style = "color: red;"))
              )
            ),
            shiny::actionButton(inputId = "addWP", label = "Add")
          ),
          # SEQUENTIAL BAYES (seqB).
          shiny::tags$details(
            shiny::tags$summary(shiny::h4("Sequential Bayes (seqB)")),
            shiny::p("This is a description of the method."),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(8,
                shiny::textInput(inputId = "serialseqBInput",
                                 label = "Mean Serial Interval")),
              shiny::column(4,
                shiny::selectInput(inputId = "serialseqBUnits",
                                   label = "Time units",
                                   choices = list("Days", "Weeks")))
            ),
            shiny::span(shiny::textOutput(outputId = "serialseqBWarn"),
                        style = "color: red;"),
            shiny::textInput(inputId = "kappaInput",
              label = shiny::HTML(paste0("Maximum value", shiny::tags$sup("[?]",
                title = paste0("This describes the prior belief of R0, and ",
                               "should be set to a higher value if R0 is ",
                               "believed to be larger. (Default: 20)")))),
              value = "20"),
            shiny::span(shiny::textOutput(outputId = "kappaWarn"),
                        style = "color: red;"),
            shiny::actionButton(inputId = "addseqB", label = "Add")
          ),
          # INCIDENCE DECAY (ID).
          shiny::tags$details(
            shiny::tags$summary(shiny::h4("Incidence Decay (ID)")),
            shiny::p("This is a description of the method."),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(8,
                shiny::textInput(inputId = "serialIDInput",
                                 label = "Mean Serial Interval")),
              shiny::column(4,
                shiny::selectInput(inputId = "serialIDUnits",
                                   label = "Time units",
                                   choices = list("Days", "Weeks")))
            ),
            shiny::span(shiny::textOutput(outputId = "serialIDWarn"),
                        style = "color: red;"),
            shiny::actionButton(inputId = "addID", label = "Add")
          ),
          # INCIDENCE DECAY & EXPONENTIAL ADJUSTEMENT (IDEA).
          shiny::tags$details(
            shiny::tags$summary(
              shiny::h4("Incidence Decay and Exponential Adjustement (IDEA)")),
            shiny::p("This is a description of the method."),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(8,
                shiny::textInput(inputId = "serialIDEAInput",
                                 label = "Mean Serial Interval")),
              shiny::column(4,
                shiny::selectInput(inputId = "serialIDEAUnits",
                                   label = "Time units",
                                   choices = list("Days", "Weeks")))
            ),
            shiny::span(shiny::textOutput(outputId = "serialIDEAWarn"),
                        style = "color: red;"),
            shiny::actionButton(inputId = "addIDEA", label = "Add")
          ),
          shiny::tags$style(type = "text/css",
                            "summary { display: list-item; cursor: pointer; }"),
          shiny::tags$style(type = "text/css",
                            "summary h4 { display: inline; }")
        )
      )
    ),
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
)}
