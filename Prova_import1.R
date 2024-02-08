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

setwd("/Users/danielavolpatto/Documents/Università/dottorato/Interfaccia grafica")

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
        # uiOutput("variables_input"),
        offset = 1,
        width = 3
      ),
      column(
        fluidRow(
          wellPanel(
            # plotOutput(outputId = "distPlot"),
            # downloadButton("downloadData", "Download"),
            # actionButton("refresh","Other",class="red-button")
          ),
          wellPanel( # tableOutput(outputId = "Table"),
            class = "fit-to-content"
          )
        ),
        width = 7
      )
    )
  )
)

server <- function(input, output) {
  # definition of reactive variables
  rv <- reactiveValues(
    df = NULL,
    df_post = NULL,
    variables = NULL,
    ncol = NULL,
    colTypes = NULL,
    levels = NULL,
    ord_factor = NULL,
    type_ord = NULL,
    type_facets = NULL,
    type_boole = NULL,
    ntype_ord = NULL,
    ntype_facets = NULL,
    ndata = NULL,
    plot_n = NULL,
    plot = NULL
  )
  
  # condition to show the table adjustments buttons
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output,
                "fileUploaded",
                suspendWhenHidden = FALSE
  )
  
  # load del dataframe
  observe({
    if (input$load == 0) {
      req(input$file)
      df <- NULL
      ext <- tools::file_ext(input$file$name)
      if (ext %in% c("csv", "txt", "tsv") && input$manually == 0) {
        read_fun <- switch(ext,
                           csv = read.csv,
                           txt = read.table,
                           tsv = read.table,
                           default = read.table
        )
        
        df <- read_fun(input$file$datapath,
                       header = TRUE,
                       stringsAsFactors = FALSE
        )
      }
      else {
        df <- tryCatch(
          {
            read.delim(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       #quote = input$quote
            )
          },
          warning = function(cond) {
            message("Here's the original error message:")
            message(conditionMessage(cond))
            # Choose a return value in case of error
            read.table(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote
            )
          },
          error = function(cond) {
            message("Here's the original error message:")
            message(conditionMessage(cond))
            # Choose a return value in case of error
            read.table(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote
            )
          }
        )
      }
      rv$df <- df
      rv$variables <- colnames(df)
      rv$ncol <- ncol(df)
      rv$levels <- vector("list", ncol(df))
      rv$colTypes <- rep("", ncol(df))
    }
  })
  
  # show tabella
  output$df_table <- renderDataTable(DT::datatable(rv$df,
                                                   callback = JS(callback),
                                                   rownames = FALSE,
                                                   options = list(dom = "t", ordering = F)
  ))
  
  # trigger cambiamento nei tipi delle variabili
  change_colType <- reactive({
    req(input$file)
    if(rv$ncol>1){
      lapply(1:rv$ncol, function(i) {
        return(input[[paste0("colType_", i)]])
      })
    }
    else{
      return(input$colType_1)
    }
  })
  
  # aggiornamento nomi variabili
  observeEvent(append(input$colnames, change_colType()), {
    if (!is.null(rv$df) &
        input$load == 0 &
        all(!is.null(input$colnames))) {
      columnNames <- input$colnames
      rv$variables <- columnNames
      for (i in 1:rv$ncol) {
        if (!is.null(input[[paste0("colType_", i)]])) {
          names(rv$colTypes) <- columnNames
          colTypeInput <- input[[paste0("colType_", i)]]
          rv$colTypes[i] <- colTypeInput
        }
      }
    }
  })
  
  # crea picker
  output$colTypeInputs <- renderUI({
    if (!is.null(rv$df)) {
      columnNames <- rv$variables
      tagList(
        lapply(1:rv$ncol, function(i) {
          pickerInput(paste0("colType_", i),
                      label = columnNames[i],
                      choices = c("", "character", "numeric", "factor"),
                      selected = rv$colTypes[i]
          )
        })
      )
    }
  })
  
  # mostra tendina per scegliere ordine dei factor
  observeEvent(change_colType(), {
    if (input$load == 0) {
      req(input$file)
      req(input$changed)
      Input <- input$changed
      if (any(Input == sapply(1:rv$ncol, function(i) {
        return(paste0("colType_", i))
      }))) {
        i <- which(Input == sapply(1:rv$ncol, function(i) {
          return(paste0("colType_", i))
        }))
        colTypeInput <- input[[Input]]
        if (colTypeInput == "factor") {
          levels_ord <- rv$levels[[i]]
          if (is.null(levels_ord)) {
            levels_non_ord <- unique(rv$df[, i])
          } else {
            levels_non_ord <- NULL
          }
          showModal(
            modalDialog(
              bucket_list(
                header = "Drag the items in any desired bucket",
                group_name = paste0("bucket_ord_", Input),
                orientation = "horizontal",
                add_rank_list(
                  text = "Drag from here",
                  labels = levels_non_ord,
                  input_id = paste0("factor_nonord_", i)
                ),
                add_rank_list(
                  text = "to here",
                  labels = levels_ord,
                  input_id = paste0("factor_ord_", i)
                )
              ),
              footer = tagList(
                shinyjs::useShinyjs(),
                actionButton(paste0("submit_ord_", i), "Save with this order"),
                # actionButton("submit_ord","Save with this order"),
                modalButton("Order is not meaningful")
              )
            )
          )
        }
      }
    }
  })
  
  # attiva load se tutti campi fillati
  observe({
    if (input$load == 0) {
      toggleState(
        "load",
        !is.null(rv$df) & all(rv$colTypes != "") & all(!is.null(rv$colTypes))
      )
      req(input$file)
      lapply(1:rv$ncol, function(col) {
        toggleState(
          paste0("submit_ord_", col),
          length(input[[paste0("factor_nonord_", col)]]) == 0
        )
      })
    }
  })
  
  # trigger ordine salvato
  factor_ord_save <- reactive({
    req(input$file)
    lapply(1:rv$ncol, function(i) {
      return(input[[paste0("submit_ord_", i)]])
    })
  })
  
  # salva ordine e chiudi finestra
  observeEvent(factor_ord_save(), {
    if (input$load == 0) {
      req(input$file)
      req(input$changed)
      Input <- input$changed
      if (any(Input == sapply(1:rv$ncol, function(i) {
        return(paste0("submit_ord_", i))
      }))) {
        i <- which(Input == sapply(1:rv$ncol, function(i) {
          return(paste0("submit_ord_", i))
        }))
        req(input[[paste0("factor_ord_", i)]])
        rv$levels[[i]] <- input[[paste0("factor_ord_", i)]]
        removeModal()
      }
    }
  })
  
  # load del dataframe parsificato e delle variabili
  observeEvent(input$load, {
    names(rv$levels) <- rv$variables
    rv$ord_factor <- !sapply(rv$levels, is.null)
    names(rv$ord_factor) <- rv$variables
    rv$levels[rv$colTypes == "factor" & !rv$ord_factor] <- lapply(
      rv$df %>%
        dplyr::select(
          all_of(
            rv$variables[
              which(rv$colTypes == "factor" & rv$ord_factor == FALSE)
            ]
          )
        ),
      unique
    )
    rv$colTypes[rv$colTypes == "factor" & sapply(rv$df, function(v) {
      length(unique(v))
    }) == 2] <- "logic"
    rv$ntype_ord <- lapply(
      rv$levels[rv$colTypes == "factor" & rv$ord_factor],
      length
    )
    rv$ntype_facets <- lapply(
      rv$levels[rv$colTypes == "factor" & !rv$ord_factor],
      length
    )
    colnames(rv$df) <- rv$variables
    
    
    rv$ndata <- nrow(rv$df)
    rv$df_post <- as_tibble(
      lapply(rv$variables,
             change_type,
             df = rv$df,
             levels = rv$levels,
             colTypes = rv$colTypes,
             ord_factor = rv$ord_factor
      ),
      .name_repair = "universal"
    )
    
    colnames(rv$df_post) <- rv$variables
    rv$colTypes <- factor(rv$colTypes,
                          levels = c("character", "logic", "factor", "numeric")
    )
  })
}



shinyApp(ui, server)
