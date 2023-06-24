ui <- function() {
    shiny::fluidPage(
        shinyjs::useShinyjs(),

        # Title.
        shiny::titlePanel(shiny::HTML(paste0("Rnaught: an estimation suite for R", shiny::tags$sub("0")))),
  
        # Sidebar layout.
        shiny::sidebarLayout(
            # Sidebar.
            shiny::sidebarPanel(id="sidebar",
                # Data tab sidebar.
                shiny::conditionalPanel(condition="input.tabset == 'Data'",
                    shiny::h4("Enter data"),
                    # Data input method selection.
                    shiny::radioButtons(inputId="dataInputMethod", label="", choices=list("Manually"=1, "Upload a .csv file"=2, "Paste a .csv file"=3)),
                    # Option 1: Manual entry.
                    shiny::conditionalPanel(condition="input.dataInputMethod == '1'",
                        shiny::textInput(inputId="dataName", label="Dataset name"),
                        shiny::span(shiny::textOutput(outputId="dataNameWarn"), style="color: red"),
                        shiny::fluidRow(
                            shiny::column(8, shiny::textInput(inputId="dataCounts", label=shiny::HTML(paste0("Case counts", shiny::tags$sup(id="dataCountsHelp", "[?]"))))),
                            shiny::column(4, shiny::selectInput(inputId="dataUnits", label="Reporting frequency", choices=list("Daily", "Weekly")))
                        ),
                        shinyBS::bsTooltip(id="dataCountsHelp", "Enter as a comma-separated list of positive integers, with at least two entries. Ex: 1,1,2,3,5,8", placement="right", trigger="hover"),
                        shiny::span(shiny::textOutput(outputId="dataCountsWarn"), style="color: red")
                    ),
                    # Option 2: Upload .csv file.
                    shiny::conditionalPanel(condition="input.dataInputMethod == '2'",
                        shiny::fileInput(inputId="dataUpload", label="", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                    ),
                    # Option 3: Paste .csv file.
                    shiny::conditionalPanel(condition="input.dataInputMethod == '3'",
                        shiny::textAreaInput(inputId="dataPaste", label="", rows=8, resize="none"),
                    ),
                    # Warning text for .csv upload / paste.
                    shiny::conditionalPanel(condition="['2', '3'].includes(input.dataInputMethod)",
                        shiny::span(shiny::textOutput(outputId="dataCSVWarn"), style="color: red"),
                    ),
                    # Button to add data.
                    shiny::actionButton(inputId="addData", label="Add"),
                ),
                # Estimators tab sidebar
                shiny::conditionalPanel(condition="input.tabset == 'Estimators'",
                    shiny::h4("Estimators"),
                    # Collapsible menu for estimation methods.
                    shinyBS::bsCollapse(id="estCollapse",
                        # WHITE & PANAGO (WP).
                        shinyBS::bsCollapsePanel(title="White & Panago (WP)",
                            "This is a description of the method.",
                            shiny::br(), shiny::br(),
                            shiny::radioButtons(inputId="serialWPKnown", label="Is the mean serial interval known?", inline=TRUE, choices=list("Yes"=1, "No"=2)),
                            # Known serial interval.
                            shiny::conditionalPanel(condition="input.serialWPKnown == '1'",
                                shiny::fluidRow(
                                    shiny::column(8, shiny::textInput(inputId="serialWPInput", label="Mean Serial Interval")),
                                    shiny::column(4, shiny::selectInput(inputId="serialWPUnits", label="Time units", choices=list("Days", "Weeks")))
                                ),
                                shiny::span(shiny::textOutput(outputId="serialWPWarn"), style="color: red")
                            ),
                            # Unknown serial interval.
                            shiny::conditionalPanel(condition="input.serialWPKnown == '2'",
                                shiny::h5("Grid Search Parameters"),
                                shiny::fluidRow(
                                    shiny::column(4, shiny::textInput(inputId="gridLengthInput", label="Grid length", value="100")),
                                    shiny::column(4, shiny::textInput(inputId="gridShapeInput", label="Max. shape", value="10")),
                                    shiny::column(4, shiny::textInput(inputId="gridScaleInput", label="Max. scale", value="10"))
                                ),
                                shiny::fluidRow(
                                    shiny::column(4, shiny::span(shiny::textOutput(outputId="gridLengthWarn"), style="color: red")),
                                    shiny::column(4, shiny::span(shiny::textOutput(outputId="gridShapeWarn"), style="color: red")),
                                    shiny::column(4, shiny::span(shiny::textOutput(outputId="gridScaleWarn"), style="color: red"))
                                )
                            ),
                            shiny::actionButton(inputId="addWP", label="Add")
                        ),
                        # SEQUENTIAL BAYES (seqB).
                        shinyBS::bsCollapsePanel(title="Sequential Bayes (seqB)",
                            "This is a description of the method.",
                            shiny::br(), shiny::br(),
                            shiny::fluidRow(
                                shiny::column(8, shiny::textInput(inputId="serialseqBInput", label="Mean Serial Interval")),
                                shiny::column(4, shiny::selectInput(inputId="serialseqBUnits", label="Time units", choices=list("Days", "Weeks")))
                            ),
                            shiny::span(shiny::textOutput(outputId="serialseqBWarn"), style="color: red"),
                            shiny::textInput(inputId="kappaInput", label=shiny::HTML(paste0("Maximum value of the uniform prior", shiny::tags$sup(id="kappaTool", "[?]"))), value="20"),
                            shinyBS::bsTooltip(id="kappaTool", "Some information. By default, this is set to 20.", placement="right", trigger="hover"),
                            shiny::span(shiny::textOutput(outputId="kappaWarn"), style="color: red"),
                            shiny::actionButton(inputId="addseqB", label="Add")
                        ),
                        # INCIDENCE DECAY (ID).
                        shinyBS::bsCollapsePanel(title="Incidence Decay (ID)",
                            "This is a description of the method.",
                            shiny::br(), shiny::br(),
                            shiny::fluidRow(
                                shiny::column(8, shiny::textInput(inputId="serialIDInput", label="Mean Serial Interval")),
                                shiny::column(4, shiny::selectInput(inputId="serialIDUnits", label="Time units", choices=list("Days", "Weeks")))
                            ),
                            shiny::span(shiny::textOutput(outputId="serialIDWarn"), style="color: red"),
                            shiny::actionButton(inputId="addID", label="Add")
                        ),
                        # INCIDENCE DECAY & EXPONENTIAL ADJUSTEMENT (IDEA).
                        shinyBS::bsCollapsePanel(title="Incidence Decay and Exponential Adjustement (IDEA)",
                            "This is a description of the method.",
                            shiny::br(), shiny::br(),
                            shiny::fluidRow(
                                shiny::column(8, shiny::textInput(inputId="serialIDEAInput", label="Mean Serial Interval")),
                                shiny::column(4, shiny::selectInput(inputId="serialIDEAUnits", label="Time units", choices=list("Days", "Weeks")))
                            ),
                            shiny::span(shiny::textOutput(outputId="serialIDEAWarn"), style="color: red"),
                            shiny::actionButton(inputId="addIDEA", label="Add")
                        )
                    )
                )
            ),
            # Main panel.
            shiny::mainPanel(id="main",
                shiny::tabsetPanel(id="tabset", type="tabs",
                    shiny::tabPanel("About", shiny::br(), "Hello"),
                    shiny::tabPanel("Data", shiny::br(), shiny::dataTableOutput(outputId="dataTable"), shiny::tags$style(type="text/css", '#dataTable tfoot {display:none;}')),
                    shiny::tabPanel("Estimators", shiny::br(),
                        shiny::dataTableOutput(outputId="estTable"),
                        shiny::tags$style(type="text/css", "#estTable tfoot {display:none;}"),
                        shiny::downloadButton(outputId="downloadEst", label="Download table as .csv")
                    ),
                )
            )
        )
    )
}
