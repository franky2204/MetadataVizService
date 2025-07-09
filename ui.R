setwd(".")

source("Libraries.R")
source("functions.R")
source("ShinyMirriTemplate/ThemeShiny_QBio.R")
source("Palettes.R")
source("Univar_functs.R")
source("Bivar_functs.R")

setwd(".")











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
  theme_QBio,
  headerPanel("Preprocessing"),
  conditionalPanel(
    "!output.AllAlright",
    upload_page,
    table_page,
    load_button
  ),
  conditionalPanel(
    "input.load!=0 && output.AllAlright",
    style="margin:5%;",
    fluidRow(
      column(
        width = 2,
        uiOutput("variables_input"),
        style = "margin:1em;"
      ),
      column(
        width = 9,
        fluidRow(
          style="height:30%;margin-bottom:5%;",
          column(
            style="height:400px;",
            width=8,
            wellPanel(
              plotOutput(outputId = "distPlot",width="100%",height="100%"),
              style="width:100%; height: 100%;"
            )
          ),
          column(
            style="height:400px;",
            width=4,
            fluidRow(
              column(
                width=10,
                varbatimTextOutput_h3(outputId="Title")
              ),
              column(
                width=2,
                downloadButton("downloadPlot","")
              )
            ),
            fluidRow(
              style="padding:10%;",
              varbatimTextOutput_p(outputId = "Description")
            ),
            fluidRow(
              column(width=7,
                     h4("Next:")),
              column(width=5,
                     actionButton("refresh","Other",style="float: right;"))
              ),
            fluidRow(
              column(
                width=12,
                wellPanel(
                  plotOutput(outputId = "distPlot_next",width="200px",height="100px"),
                  #style="width: 100%; height: 100%;"
                )
              )
            )
          )
        ),
        fluidRow(
          wellPanel(
          tableOutput(outputId = "Table"),
          style="max-width:100%;overflow-x: scroll; width: fit-content;"
        ),
        wellPanel(
          p("Altre cose altre")
        )
        )
      )
    )
  ),
  footerPanel()
)
