library(tidyverse)
library(shiny)
library(readr)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(shinyalert)
library(sortable)
# library(numbers)
# install.packages("devtools")
library("MoMAColors")

#setwd("/Users/danielavolpatto/Documents/Università/dottorato/Interfaccia grafica")
source("server.R")
source("functions.R")
source("ShinyMirriTemplate/ThemeShiny_QBio.R")
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
  theme_QBio,
  # includeCSS("bootstrap.css"),
  # includeCSS("bootstrap-icons.min.css"),
  # includeCSS("prism-okaidia.css"),
  # includeCSS("custom.min.css"),
  # theme=shinytheme("cerulean"),
  headerPanel("Preprocessing"),
  conditionalPanel(
    "input.load == 0",
    fluidRow(
      fileInput("file", "Upload your tile",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
      ),
      conditionalPanel(
        "output.fileUploaded",
        sidebarLayout(
          sidebarPanel(
            actionButton("manually",
                         label = "Manually adjust",
                         styleclass = "secondary",
                         style = "margin:1em;"
            ),
            conditionalPanel(
              condition = "input.manually !=0",
              materialSwitch(
                inputId = "header",
                label = "Header",
                value = TRUE,
                status = "default"
              ),
              textInput("sep",
                        "Separator",
                        value = ",",
                        width = "100px"
              ),
              radioButtons("quote",
                           "Quote",
                           choices = c(
                             None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"
                           ),
                           selected = '"'
              )
            ),
            uiOutput("colTypeInputs"),
            width = 3,
            style = "margin:1em;"
          ),
          mainPanel(
            DTOutput("df_table")
          )
        )
      ),
      shinyjs::useShinyjs(),
      actionButton("load",
                   label = "Load",
                   styleclass = "default",
                   style = "margin:1em;"
      ),
      align = "center"
    )
  ),
  conditionalPanel(
    "input.load != 0",
    h1("Preprocessing"),
    fluidRow(
      column(
         uiOutput("variables_input"),
        offset = 1,
        width = 3
      ),
      column(
        fluidRow(
          wellPanel(
             plotOutput(outputId = "distPlot"),
             downloadButton("downloadData", "Download"),
             actionButton("refresh","Other",class="red-button")
          ),
          wellPanel(  tableOutput(outputId = "Table"),
            class = "fit-to-content"
          )
        ),
        width = 7
      )
    )
  ),
  footerPanel()
)



shinyApp(ui, server)
