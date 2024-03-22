setwd(".")

source("Libraries.R")
source("functions.R")
source("ThemeShiny_QBio.R")
source("Palettes.R")
source("Univar_functs.R")
source("Bivar_functs.R")

ui <- fluidPage(
  tags$head(
    tags$script(
      "$(document).on('shiny:inputchanged', function(event) {
          if (event.name != 'changed') {
            Shiny.setInputValue('changed', event.name);
          }
        });"
    )
  ),
  #theme="Cerulean",
  theme_QBio,
  #includeCSS("bootstrap.css"),
  headerPanel("Preprocessing"),
  conditionalPanel(
    "!output.AllAlright",
    
    conditionalPanel("!output.fileUploaded",
                     fluidRow(
                       fileInput("file", "Upload your file",
                                 multiple = FALSE,
                                 accept = c(
                                   ".txt",
                                   ".csv",
                                   ".tsv"
                                 )
                       ),
                       align="center",
                       style = "margin-top: 10%;"
                     ),
                     p("It is possible to upload table files with separators of your choice in .csv, .tsv or .txt format"),
    align="center",
    style="width: 80%; margin-left: 10%; margin-right: 10%;"
  ),
  conditionalPanel(
      "output.fileUploaded",
      fluidRow(
        fileInput("file1", "Upload your file",
                  multiple = FALSE,
                  accept = c(
                    ".txt",
                    ".csv",
                    ".tsv"
                  )
        ),
        align="center",
        style = "margin-top: 2%;"
      ),
      fluidRow(
        column(
          offset=1,
          width = 2,
          actionButton("manually",
                       label = "Manually adjust",
                       class = "btn btn-secondary",
                       style = "margin:1em;"
          ),
          conditionalPanel(
            condition = "input.manually%2 !=0",
            materialSwitch(
              inputId = "header",
              label = "Header",
              value = FALSE,
              status = "default"
            ),
            textInput("sep",
                      "Separator",
                      value = "",
                      width = "50px"
                      
            ),
            prettyRadioButtons("quote",
                               "Quote",
                               choices = c(
                                 None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"
                               ),
                               selected = '"',
                               status = "danger"
                               
            )
          ),
          uiOutput("colTypeInputs"),
          align="left",
        ),
        column(
          width=8,
          wellPanel(
            DTOutput("df_table"),
            style="overflow-x: scroll"
          ),
          style = "padding:1em;"
        )
        
      )
    ),
    fluidRow(shinyjs::useShinyjs(),
             actionButton("load",
                          label = "Load",
                          styleclass = "default",
                          style = "margin:1em;"
             ),
             align="center"
    )
  ),
  conditionalPanel(
    "input.load!=0 && output.AllAlright",
    fluidRow(
      column(
        width = 2,
        uiOutput("variables_input"),
        style = "margin:1em;"
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width=8,
            wellPanel(
              fluidRow(plotOutput(outputId = "distPlot")),
              style="width:100%;"
            )
          ),
          column(
            width=3,
            fluidRow(
              column(
                width=2,
                downloadButton("downloadPlot",""),
                offset = 8
              ),
              column(
                width=2,
                actionButton("refresh","Other")
              )
            ),
            fluidRow(
              
            )
          )
        ),
        fluidRow(
          wellPanel(
          tableOutput(outputId = "Table"),
          class = "fit-to-content",
          style="max-width:100%;overflow-x: scroll"
        ),
        wellPanel(
          # verbatimTextOutput()
        ),
        wellPanel(
          p("Altre cose altre")
        )
        )
        
        #style = "margin:1em;"
      )
    )
  )
)