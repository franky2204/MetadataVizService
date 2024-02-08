library(tidyverse)
library(shiny)
library(readr)
library(DT)
library(shinyWidgets)
library(shinyjs)
#library(shinyBS)
library(ggridges)
library(shinyalert)
library(sortable)
library(waffle)
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
                  ".csv",
                  ".tsv"
                )
      ),
      align="center"
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
                        value = " ",
                        width = "100px"
              ),
              prettyRadioButtons("quote",
                           "Quote",
                           status = "default",
                           choices = c(
                             None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"
                           ),
                           selected = '"'
              )
            ),
            uiOutput("colTypeInputs"),
            align="left",
            width = 3,
            style = "margin:1em;"
          ),
          mainPanel(
            DTOutput("df_table")
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
            fluidRow(downloadButton("downloadPlot","",style = "margin-top:1em;
                                                        margin-right:1em;
                                                        float: right;")),
            fluidRow(plotOutput(outputId = "distPlot")),
            fluidRow(actionButton("refresh","Other",class="red-button",style ="margin:1em;"))
            
          ),
          wellPanel(
            tableOutput(outputId = "Table"),
            class = "fit-to-content"
          )
        ),
        width = 7
      )
    )
  )
)

server<-function(input,output, session){
  # definition of reactive variables
  rv <- reactiveValues(
    df = NULL,
    variables = NULL,
    ncol = NULL,
    colTypes = NULL,
    levels = NULL,
    ntype_ord = NULL,
    ntype_facets =NULL,
    ord_factor =NULL,
    palettes=NULL,
    plot_n=1,
    plot=NULL
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
  observeEvent(list(input$file,input$header,input$sep,input$quote),{
      req(input$file)
      df <- NULL
      ext <- tools::file_ext(input$file$name)
      if (ext %in% c("csv", "tsv") && input$manually == 0) {
        read_fun <- switch(ext,
                           csv = read.csv,
                           #txt = read.table,
                           tsv = read.table,
                           default = read.table
        )
        
        df <- read_fun(input$file$datapath,
                       header = TRUE,
                       stringsAsFactors = FALSE
        )
        rv$df <- df
        rv$variables <- colnames(df)
        rv$ncol <- ncol(df)
        rv$levels <- vector("list", ncol(df))
        rv$colTypes <- rep("", ncol(df))
      }
      else {
        tryCatch(
          {
            df<-read.delim(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote
            )
            rv$df <- df
            rv$variables <- colnames(df)
            rv$ncol <- ncol(df)
            rv$levels <- vector("list", ncol(df))
            rv$colTypes <- rep("", ncol(df))
          },
          warning = function(cond) {
            message("Here's the original error message:")
            message(conditionMessage(cond))
            df<-read.table(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote
            )
          },
          error = function(cond) {
            message("There is something wrong:")
            message(conditionMessage(cond))
          }
        )
      }
      
  })
  
  # show tabella
  output$df_table <- renderDataTable(DT::datatable(rv$df,
                                                   callback = JS(callback),
                                                   rownames = FALSE,
                                                   options = list(dom = "t", ordering = F)
  ))

  
  # aggiornamento nomi variabili
  observeEvent(input$colnames, {
    req(input$file)
    req(input$colnames)
    rv$variables <- input$colnames
    names(rv$colTypes) <- rv$variables
  })
  
  # crea Picker
  output$colTypeInputs <- renderUI({
    req(input$file)
    columnNames <- rv$variables
    tagList(
      lapply(1:rv$ncol, function(i) {
        pickerInput(paste0("colType_", i),
                    width = "fit",
                    label = paste(columnNames[i],": ",sep=""),
                    choices = c("", "character", "numeric", "factor"),
                    selected = rv$colTypes[i]
        )
      })
    )
  })
  
  # aggiornamento tipi colonne
  observeEvent(
    lapply(1:req(rv$ncol),
           function(i) {
             return(input[[paste0("colType_", i)]])
             }
           ),
    {
    req(input$file)
    req(input$changed)
    last_input <- input$changed
    i<-which(last_input==sapply(1:rv$ncol,
                                function(i) {
                                  return(paste0("colType_", i))
                                }
                                )
    )
    if(length(i)!=0){
      colTypeInput <-input[[last_input]]
      rv$colTypes[i] <- colTypeInput
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
                 header = "Do the factors have a preferential order? (i.e. they are time steps) \n
                 if so drag them in the right bucket in proper order.",
                 group_name = paste0("bucket_ord_", last_input),
                 orientation = "horizontal",
                 add_rank_list(
                   text = "Unordered factor detected",
                   labels = levels_non_ord,
                   input_id = paste0("factor_nonord_", i)
                 ),
                 add_rank_list(
                   text = "Correct order",
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
  })
  
  # salva ordine factor
  observeEvent(lapply(1:req(rv$ncol),
                      function(i) {
                        return(input[[paste0("submit_ord_", i)]])
                        }
                      ),
               {
                 req(input$file)
                 last_input <- input$changed
                 i<-which(last_input==sapply(1:rv$ncol,
                                             function(i) {
                                               return(paste0("submit_ord_", i))
                                             }
                 )
                 )
                 if(length(i)!=0){
                   rv$levels[[i]] <- req(input[[paste0("factor_ord_", i)]])
                   removeModal()
                 }
                 
  })
  
  # attiva load se tutti campi fillati
  observe({
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
  })
  
  # Sistemazione df
  observeEvent(input$load, {
    colnames(rv$df) <- rv$variables
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
    
    rv$ndata <- nrow(rv$df)
    rv$df <- as_tibble(
      lapply(rv$variables,
             change_type,
             df = rv$df,
             levels = rv$levels,
             colTypes = rv$colTypes,
             ord_factor = rv$ord_factor
      ),
      .name_repair = "universal"
    )
    
    colnames(rv$df) <- rv$variables
    rv$colTypes <- factor(rv$colTypes,
                          levels = c("character", "logic", "factor", "numeric")
    )
   
    palettes_factord<- mapply(moma.colors,
                              possible_palettes_gradient[1:sum(rv$ord_factor)],
                              n=rv$ntype_ord,
                              SIMPLIFY = FALSE,
                              USE.NAMES = FALSE)
    names(palettes_factord)<-names(rv$colTypes)[rv$ord_factor]

    palettes_factnonord<-mapply(moma.colors,
                                possible_palettes_facets[1:sum(!rv$ord_factor[rv$colTypes=="factor"])],
                                n=rv$ntype_facets,
                                SIMPLIFY = FALSE,
                                USE.NAMES = FALSE)
    names(palettes_factnonord)<-names(rv$colTypes)[!rv$ord_factor&rv$colTypes=="factor"]

    palettes_logic<-possible_palettes_neutral[1:sum(rv$colTypes=="logic")]
    names(palettes_logic)<-names(rv$colTypes)[rv$colTypes=="logic"]
    
    rv$palettes<-append(append(palettes_factnonord,palettes_factord),palettes_logic)
  })
  
  # crea le variabili da mostrare per la scelta
  output$variables_input<-renderUI({
    if(input$load!=0){
      req(rv$variables,rv$colTypes)
      tagList(
        checkboxGroupInput(
          inputId = "var",
          label = "Chose the variables you want to explore",
          choices = rv$variables[rv$colTypes!="character"],
          inline = FALSE
        ))
    }
  })
  
  # aggiorna il numero del plot da mostrare
  observeEvent(input$refresh, {
    rv$plot_n<-rv$plot_n+1
  })
  
  # aggiorna le variabili da mostrare
  observeEvent(input$var,{
    var<-names(sort(rv$colTypes[input$var]))
    var[rv$colTypes[var]=="factor"]<-names(sort(rv$ord_factor[names(rv$colTypes[var][rv$colTypes[var]=="factor"])]))
    if (length(var)==1){
      var<-as.character(input$var[1])
      
      if(rv$colTypes[var]=="logic"){
        rv$plot<-list(barplot_logic(rv$df,var,rv$palettes),
                      pie_logic(rv$df,var,rv$palettes)
        )
        rv$table<-rv$df%>%count(.data[[var]])%>%column_to_rownames(var)%>%t()
        rownames(rv$table)<-"Number \n of samples"
      }
      else if(rv$colTypes[var]=="factor"&!rv$ord_factor[var]){
        rv$plot<-list(waffle_factor_nonord(rv$df,var,rv$palettes,rv$ndata,rv$levels),
                      barplot_factor_nonord(rv$df,var,rv$palettes,rv$levels)
        )
        rv$table<-rv$df%>%count(.data[[var]])%>%column_to_rownames(var)%>%t()
        rownames(rv$table)<-"Number \n of samples"
      }
      else if(rv$colTypes[var]=="factor"&rv$ord_factor[var]){
        rv$plot<-list(barplot_factor_ord_x(rv$df,var,rv$palettes),
                      barplot_factor_ord_stack(rv$df,var,rv$palettes)
        )
        rv$table<-rv$df%>%count(.data[[var]])%>%column_to_rownames(var)%>%t()
        rownames(rv$table)<-"Number \n of samples"
      }
      else if(rv$colTypes[var]=="numeric"){
        rv$plot<-list(hist_numeric(rv$df,var),
                      density_numeric(rv$df,var,rv$palettes),
                      boxplot_numeric(rv$df,var,rv$palettes)
        )
        rv$table<-data.frame(
          as.array(
            summary(rv$df%>%select(.data[[var]])%>%pull())))%>%
          column_to_rownames("Var1")%>%t()
        rownames(rv$table)<-"values"
      }
    }
    else if (length(var)==2){
      var1<-as.character(var[1])
      var2<-as.character(var[2])
      
      if(rv$colTypes[var1]=="logic"&rv$colTypes[var2]=="logic"){
        rv$plot<-list(barplot_logic_logic(rv$df,var1,var2,rv$palettes),
                      barplot_logic_logic(rv$df,var2,var1,rv$palettes))
        rv$table<-rv$df%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)%>%
          column_to_rownames(var1)
        
      }
      else if(rv$colTypes[var1]=="logic" &
              rv$colTypes[var2]=="factor" & !rv$ord_factor[var2]){
        rv$plot<-list(barplot_logic_factnonord_stack(rv$df,var1,var2,rv$palettes),
                      barplot_logic_factnonord_dodge(rv$df,var1,var2,rv$palettes),
                      barplot_logic_factnonord_stack(rv$df,var2,var1,rv$palettes),
                      barplot_logic_factnonord_dodge(rv$df,var2,var1,rv$palettes)
        )
        rv$table<-rv$df%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)%>%
          column_to_rownames(var1)
      }
      else if(rv$colTypes[var1]=="logic" &
              rv$colTypes[var2]=="factor" & rv$ord_factor[var2]){
        rv$plot<-list(barplot_logic_factord_stack(rv$df,var1,var2,rv$palettes),
                      barplot_logic_factord_dodge(rv$df,var1,var2,rv$palettes),
                      barplot_logic_factord_stack(rv$df,var2,var1,rv$palettes),
                      barplot_logic_factord_dodge(rv$df,var2,var1,rv$palettes)
        )
        rv$table<-rv$df%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)%>%
          column_to_rownames(var1)
      }
      else if(rv$colTypes[var1]=="logic" &
              rv$colTypes[var2]=="numeric"){
        rv$plot<-list(density_logic_num(rv$df,var1,var2,rv$palettes),
                      boxplot_logic_num(rv$df,var1,var2,rv$palettes),
                      histogram_logic_num_stack(rv$df,var1,var2,rv$palettes),
                      histogram_logic_num_identity(rv$df,var1,var2,rv$palettes)
        )
        rv$table<-data.frame(
          do.call(
            rbind,
            tapply(
              rv$df%>%select(.data[[var2]])%>%pull(),
              rv$df%>%select(.data[[var1]])%>%pull(),
              summary)
          )
        )
      }
      else if(rv$colTypes[var1]=="factor" & !rv$ord_factor[var1] &
              rv$colTypes[var2]=="factor" & !rv$ord_factor[var2]){
        rv$plot<-list(barplot_factornonord_factornonord(rv$df,var1,var2,rv$palettes),
                      barplot_factornonord_factornonord(rv$df,var2,var1,rv$palettes)
        )
        rv$table<-rv$df%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)%>%
          column_to_rownames(var1)
      }
      else if(rv$colTypes[var1]=="factor" & !rv$ord_factor[var1] &
              rv$colTypes[var2]=="factor"& rv$ord_factor[var2]){
        rv$plot<-list(barplot_factornonord_factorord(rv$df,var1,var2,rv$palettes),
                      barplot_factorord_factornonord(rv$df,var1,var2,rv$palettes)
        )
        rv$table<-rv$df%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)%>%
          column_to_rownames(var1)
      }
      else if(rv$colTypes[var1]=="factor" & !rv$ord_factor[var1] &
              rv$colTypes[var2]=="numeric"){
        rv$plot<-list(density_factornonord_num(rv$df,var1,var2,rv$palettes),
                      histogram_factornonord_num_identity(rv$df,var1,var2,rv$palettes),
                      histogram_factornonord_num_stack(rv$df,var1,var2,rv$palettes),
                      boxplot_factornonord_num(rv$df,var1,var2,rv$palettes)
        )
        rv$table<-data.frame(
          do.call(
            rbind,
            tapply(
              rv$df%>%select(.data[[var2]])%>%pull(),
              rv$df%>%select(.data[[var1]])%>%pull(),
              summary)
          )
        )
      }
      else if(rv$colTypes[var1]=="factor" & rv$ord_factor[var1] &
              rv$colTypes[var2]=="factor" & rv$ord_factor[var2]){
        rv$plot<-list(barplot_factord_factord_dodge(rv$df,var1,var2,rv$palettes),
                      barplot_factord_factord_dodge(rv$df,var2,var1,rv$palettes),
                      barplot_factord_factord_stack(rv$df,var1,var2,rv$palettes),
                      barplot_factord_factord_stack(rv$df,var2,var1,rv$palettes)
        )
        rv$table<-rv$df%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)%>%
          column_to_rownames(var1)
      }
      else if(rv$colTypes[var1]=="factor" & rv$ord_factor[var1] &
              rv$colTypes[var2]=="numeric"){
        rv$plot<-list(density_factorord_num(rv$df,var1,var2,rv$palettes),
                      boxplot_factorord_num(rv$df,var1,var2,rv$palettes)
        )
        rv$table<-data.frame(
          do.call(
            rbind,
            tapply(
              rv$df%>%select(.data[[var2]])%>%pull(),
              rv$df%>%select(.data[[var1]])%>%pull(),
              summary)
          )
        )
      }
      else if(rv$colTypes[var1]=="numeric"&rv$colTypes[var2]=="numeric"){
        rv$plot<-list(scatter_num_num(rv$df,var1,var2,rv$palettes),
                      scatter_num_num(rv$df,var2,var1,rv$palettes)
        )
        rv$table<-data.frame(
          do.call(
            rbind,
            tapply(
              rv$df%>%
                select(all_of(c(var1,var2)))%>%
                pivot_longer(c(var1,var2))%>%
                select(value)%>%
                pull(),
              rv$df%>%
                select(all_of(c(var1,var2)))%>%
                pivot_longer(c(var1,var2))%>%
                select(name)%>%
                pull(),
              summary)
          )
        )
      }
    }
    else{rv$plot<-NULL}
    print(rv$plot[[numbers::mod(rv$plot_n, length(rv$plot))+1]])
  })
  
  # plotta grafici
  output$distPlot <- renderPlot({
    return(rv$plot[[numbers::mod(rv$plot_n, length(rv$plot))+1]])
  },bg="transparent")
  
  # printa tabella
  output$Table <- renderTable({
    return(rv$table)
  }, rownames = TRUE)
  
  output$downloadPlot <- downloadHandler(
       filename = function() {
         paste('plot-',paste(input$var,collapse="_"), '.png', sep='')
       },
       content = function(con) {
         ggsave(
           con,
           plot = rv$plot[[numbers::mod(rv$plot_n, length(rv$plot))+1]],
         )
       }
     )
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)


