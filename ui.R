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
  #theme_QBio,
  #includeCSS("prova-theme.css"),
  headerPanel("Preprocessing"),
  conditionalPanel(
    "!output.AllAlright",
    fluidRow(
      fileInput("file", "Upload your file",
                multiple = FALSE,
                accept = c(
                  ".txt",
                  ".csv",
                  ".tsv"
                )
      ),
      align="center"
    ),
    conditionalPanel("!output.fileUploaded",
                     p("It is possible to upload table files with separators of your choice in .csv, .tsv or .txt format"),
    align="center"
  ),
  conditionalPanel(
      "output.fileUploaded",
      fluidRow(
        column(
          actionButton("manually",
                       label = "Manually adjust",
                       styleclass = "secondary",
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
          width = 3,
          #offset = 1
          style = "padding:2em;"
        ),
        column(
          wellPanel(
            DTOutput("df_table"),
            style="overflow-x: scroll"
          ),
          width=9,
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
    #"output.AllAlright",
    "input.load!=0 && output.AllAlright",
    h1("Preprocessing"),
    fluidRow(
      column(
        uiOutput("variables_input"),
        #offset = 1,
        width = 3,
        style = "margin:1em;"
      ),
      column(
        fluidRow(
          wellPanel(
            fluidRow(downloadButton("downloadPlot","",style = "margin-top:1em;
                                                        margin-right:1em;
                                                        float: right;")),
            fluidRow(plotOutput(outputId = "distPlot")),
            fluidRow(actionButton("refresh","Other",class="red-button",style ="margin:1em;")),
            style="width:100%;"
          ),
          wellPanel(
            tableOutput(outputId = "Table"),
            class = "fit-to-content",
            style="max-width:100%;overflow-x: scroll"
          )
        ),
        width = 7,
        style = "margin:1em;"
      )
    )
  )
)