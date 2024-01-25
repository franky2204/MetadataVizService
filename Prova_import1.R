library(shiny)
library(readr)
library(DT)
library(shinyWidgets)
setwd("/Users/danielavolpatto/Documents/Università/dottorato/Interfaccia grafica")

{
callback <- c(
  "var colnames = table.columns().header().to$().map(function() {",
  "  return this.innerHTML;",
  "}).get();",
  "Shiny.onInputChange('colnames', colnames);",
  "table.on('dblclick.dt', 'thead th', function(e) {",
  "  var $th = $(this);",
  "  var index = $th.index();",
  "  var colname = $th.text(), newcolname = colname;",
  "  var $input = $('<input type=\"text\">')",
  "  $input.val(colname);",
  "  $th.empty().append($input);",
  "  $input.on('change', function() {",
  "    newcolname = $input.val();",
  "    if(newcolname != colname){",
  "      $(table.column(index).header()).text(newcolname);",
  "      colnames[index] = newcolname;",
  "      Shiny.onInputChange('colnames', colnames);",
  "    }",
  "    $input.remove();",
  "  }).on('blur', function() {",
  "    $(table.column(index).header()).text(newcolname);",
  "    $input.remove();",
  "  });",
  "});"
)
golden<-(1+sqrt(5))/2
}

source("ThemeShiny_QBio.R")


ui <- fluidPage(
  theme_QBio,
  #includeCSS("bootstrap.css"),
  #includeCSS("bootstrap-icons.min.css"),
  #includeCSS("prism-okaidia.css"),
  #includeCSS("custom.min.css"),
  #theme=shinytheme("cerulean"),
  headerPanel("Preprocessing"),
  conditionalPanel(
    "input.load == 0",
    fluidRow(
      fileInput("file", "Upload your tile",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      conditionalPanel("output.fileUploaded",
                     sidebarLayout(
                       sidebarPanel(
                         actionButton("manually",
                                               label = "Manually adjust",
                                               styleclass = "secondary",
                                               style='margin:1em;'),
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
                                     width="100px"),
                           radioButtons("quote",
                                        "Quote",
                                        choices = c(None = "",
                                                    "Double Quote" = '"',
                                                    "Single Quote" = "'"),
                                        selected = '"')
                         ),
                         uiOutput("colTypeInputs"),
                         width=3,
                         style='margin:1em;'
                       ),
                       mainPanel(
                         verbatimTextOutput("colnames"),
                         verbatimTextOutput("colTypes"),
                         DTOutput("df_table"))
                     )
      ),
      actionButton("load",
               label = "Load",
               styleclass = "default",
               style='margin:1em;'),
      align="center")
  ),
  conditionalPanel(
    "input.load != 0",
    h1("Preprocessing")
  )
)

server <- function(input, output) {
  
  rv<-reactiveValues(df=NULL,
                     variables=NULL,
                     colTypes = NULL)
  
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  observe({
    req(input$file)
    df<-NULL
    
    ext <- tools::file_ext(input$file$name)
    if (ext %in% c("csv", "txt", "tsv") && input$manually == 0) {
      read_fun <- switch(ext,
                         csv = read.csv,
                         txt = read.table,
                         tsv = read.table,
                         default = read.table)
      
      df<-read_fun(input$file$datapath, header = TRUE)
    }
    else if(input$sep!=""){
    tryCatch(
      {
        df <- read.delim(input$file$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote
                         )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    }
    rv$df<-df
    rv$variables<-colnames(df)
    rv$colTypes<-rep("",ncol(df))
    })
  
  output$df_table <- renderDataTable(DT::datatable(rv$df,
                                     callback = JS(callback),
                                     rownames = FALSE,
                                     options = list(dom='t',ordering=F)
                                     ))
  
  observe({
    if(!is.null(rv$df)&
       all(input$colnames!=" ")&
       all(input$colnames!="NA"))
      {
      columnNames<-input$colnames
      print(input$colnames)
      colnames(rv$df)<-columnNames
      rv$variables<-columnNames
      names(rv$colTypes)<-columnNames
      for (col in columnNames) {
        if(!is.null(input[[paste0("colType_", col)]])){
          colTypeInput <- input[[paste0("colType_", col)]]
          rv$colTypes[col] <- colTypeInput
        }
      }
    }
  })
  
  output$colTypeInputs <- renderUI({
    if (!is.null(rv$df)) {
      columnNames <- colnames(rv$df)
      tagList(
        lapply(columnNames, function(col) {
          pickerInput(paste0("colType_", col),
                      label = col,
                      choices = c("character", "numeric", "integer", "factor",""),
                      selected = rv$colTypes[col])
        })
      )
    }
  })
  
  output$colnames<-renderPrint({print(rv$variables)})
  output$colTypes<-renderPrint({rv$colTypes})
  
}

shinyApp(ui, server)



{
fluidRow(
  column(
    checkboxGroupInput(
      inputId = "var",
      label = "Chose the variables you want to explore",
      choices = variables,
      inline = FALSE
    ),
    offset = 1,
    width=3
  ),
  column(
    fluidRow(
      wellPanel(
        plotOutput(outputId = "distPlot"),
        # downloadButton("downloadData", "Download"),
        actionButton("refresh","Other",class="red-button")),
      wellPanel(tableOutput(outputId = "Table"),
                class="fit-to-content")
    ),
    width = 7
  )
)
}

{
rv$classes<-factor(rv$classes,levels = c("character","logic","factor","numeric"))
names(rv$classes)<-rv$variables
rv$ord_factor<-c(FALSE,FALSE,TRUE,FALSE,TRUE)
names(rv$ord_factor)<-variables[rv$which(rv$classes=="factor")]

rv$lev_factor<-list(c("I","II","III"),c("DIAG","FU3","M1","M2"))
names(rv$lev_factor)<-variables[rv$classes=="factor"][rv$ord_factor]

rv$countable_variables<-rv$variables[rv$classes=="factor"|rv$classes=="logic"]
rv$type_ord<-lapply(rv$df%>%
                      select(all_of(rv$variables[which(rv$classes=="factor")][ord_factor])),
                    unique)
rv$type_facets<-lapply(rv$df%>%
                         select(all_of(rv$variables[which(rv$classes=="factor")][!ord_factor])),
                       unique)
rv$type_boole<-lapply(rv$df%>%
                        select(all_of(rv$variables[which(rv$classes=="logic")])),
                      unique)

rv$ntype_ord<-lapply(rv$type_ord,length)
rv$ntype_facets<-lapply(rv$type_facets,length)

rv$ndata<-nrow(df)

rv$nfact<-0
rv$nordfact<-0

rv$change_type<-function(variable) {
  column_class <- classes[variable]
  if (column_class == "character") {
    as.character(df[,variable])
  }
  else if (column_class == "numeric") {
    as.numeric(df[,variable])
  }
  else if (column_class == "factor") {
    if(ord_factor[variable]){
      factor(df[,variable],
             ordered=TRUE,
             levels = lev_factor[variable][[1]])
    }
    else{
      as.factor(df[,variable])
    }
  }
  else if (column_class == "logic") {
    if(any(is.na(as.logical(type_boole[variable][[1]])))){
      as.factor(df[,variable])}
    else{as.logical(df[,variable])}
  }
}

df$df <- data.frame(lapply(rv$variables,rv$change_type))
colnames(rv$df)<-rv$variables

rv$plot_n<-1

observeEvent(input$refresh, {
  rv$plot_n<-rv$plot_n+1
})

output$distPlot <- renderPlot({
  var<-names(sort(classes[input$var]))
  var[classes[var]=="factor"]<-names(sort(ord_factor[names(classes[var][classes[var]=="factor"])]))
  
  if (length(var)==1){
    var<-as.character(input$var[1])
    
    if(classes[var]=="logic"){
      rv$plot<-list(barplot_logic(df,var),
                    pie_logic(df,var)
      )
    }
    else if(classes[var]=="factor"&!ord_factor[var]){
      rv$plot<-list(waffle_factor_nonord(df,var),
                    barplot_factor_nonord(df,var)
      )
    }
    else if(classes[var]=="factor"&ord_factor[var]){
      rv$plot<-list(barplot_factor_ord_x(df,var),
                    barplot_factor_ord_stack(df,var)
      )
    }
    else if(classes[var]=="numeric"){
      rv$plot<-list(hist_numeric(df,var),
                    density_numeric(df,var),
                    boxplot_numeric(df,var)
      )
    }
  }
  else if (length(var)==2){
    var1<-as.character(var[1])
    var2<-as.character(var[2])
    
    if(classes[var1]=="logic"&classes[var2]=="logic"){
      rv$plot<-list(barplot_logic_logic(df,var1,var2),
                    barplot_logic_logic(df,var2,var1))
    }
    else if(classes[var1]=="logic"&classes[var2]=="factor"&!ord_factor[var2]){
      rv$plot<-list(barplot_logic_factnonord_stack(df,var1,var2),
                    barplot_logic_factnonord_dodge(df,var1,var2),
                    barplot_logic_factnonord_stack(df,var2,var1),
                    barplot_logic_factnonord_dodge(df,var2,var1)
      )
    }
    else if(classes[var1]=="logic"&classes[var2]=="factor"&ord_factor[var2]){
      rv$plot<-list(barplot_logic_factord_stack(df,var1,var2),
                    barplot_logic_factord_dodge(df,var1,var2),
                    barplot_logic_factord_stack(df,var2,var1),
                    barplot_logic_factord_dodge(df,var2,var1)
      )
    }
    else if(classes[var1]=="logic"&classes[var2]=="numeric"){
      rv$plot<-list(density_logic_num(df,var1,var2),
                    boxplot_logic_num(df,var1,var2),
                    histogram_logic_num_stack(df,var1,var2),
                    histogram_logic_num_identity(df,var1,var2)
      )
    }
    else if(classes[var1]=="factor"&!ord_factor[var1]&classes[var2]=="factor"&!ord_factor[var2]){
      rv$plot<-list(barplot_factornonord_factornonord(df,var1,var2),
                    barplot_factornonord_factornonord(df,var2,var1)
      )
    }
    else if(classes[var1]=="factor"&!ord_factor[var1]&classes[var2]=="factor"&ord_factor[var2]){
      rv$plot<-list(barplot_factornonord_factorord(df,var1,var2),
                    barplot_factorord_factornonord(df,var1,var2)
      )
    }
    else if(classes[var1]=="factor"&!ord_factor[var1]&classes[var2]=="numeric"){
      rv$plot<-list(density_factornonord_num(df,var1,var2),
                    histogram_factornonord_num_identity(df,var1,var2),
                    histogram_factornonord_num_stack(df,var1,var2),
                    boxplot_factornonord_num(df,var1,var2)
      )
    }
    else if(classes[var1]=="factor"&ord_factor[var1]&classes[var2]=="factor"&ord_factor[var2]){
      rv$plot<-list(barplot_factord_factord_dodge(df,var1,var2),
                    barplot_factord_factord_dodge(df,var2,var1),
                    barplot_factord_factord_stack(df,var1,var2),
                    barplot_factord_factord_stack(df,var2,var1)
      )
    }
    else if(classes[var1]=="factor"&ord_factor[var1]&classes[var2]=="numeric"){
      rv$plot<-list(density_factorord_num(df,var1,var2),
                    boxplot_factorord_num(df,var1,var2)
      )
    }
    else if(classes[var1]=="numeric"&classes[var2]=="numeric"){
      rv$plot<-list(scatter_num_num(df,var1,var2),
                    scatter_num_num(df,var2,var1)
      )
    }
  }
  else{plot<-NULL}
  
  return(rv$plot[[mod(rv$plot_n, length(rv$plot))+1]])
},bg="transparent")

output$Table <- renderTable({
  var<-names(sort(classes[input$var]))
  var[classes[var]=="factor"]<-names(sort(ord_factor[names(classes[var][classes[var]=="factor"])]))
  
  if (length(var)==1){
    var<-as.character(input$var[1])
    
    if(classes[var]=="logic"|classes[var]=="factor"){
      table<-df%>%count(.data[[var]])%>%column_to_rownames(var)%>%t()
      rownames(table)<-"Number \n of samples"
    }
    else if(classes[var]=="numeric"){
      table<-data.frame(
        as.array(
          summary(df%>%select(.data[[var]])%>%pull())))%>%
        column_to_rownames("Var1")%>%t()
      rownames(table)<-"values"
    }
  }
  else if (length(var)==2){
    var1<-as.character(var[1])
    var2<-as.character(var[2])
    
    if((classes[var1]=="logic"|classes[var1]=="factor")&(classes[var2]=="logic"|classes[var2]=="factor")){
      table<-df%>%
        count(.data[[var1]],.data[[var2]])%>%
        pivot_wider(names_from = var2,values_from = n)%>%
        column_to_rownames(var1)
    }
    else if((classes[var1]=="logic"|classes[var1]=="factor")&classes[var2]=="numeric"){
      table<-data.frame(
        do.call(
          rbind,
          tapply(
            df%>%select(.data[[var2]])%>%pull(),
            df%>%select(.data[[var1]])%>%pull(),
            summary)
        )
      )
    }
    else if(classes[var1]=="numeric"&classes[var2]=="numeric"){
      table<-data.frame(
        do.call(
          rbind,
          tapply(
            df%>%
              select(all_of(c(var1,var2)))%>%
              pivot_longer(c(var1,var2))%>%
              select(value)%>%
              pull(),
            df%>%
              select(all_of(c(var1,var2)))%>%
              pivot_longer(c(var1,var2))%>%
              select(name)%>%
              pull(),
            summary)
        )
      )
      
    }
  }
  else{table<-NULL}
  
  return(table)
}, rownames = TRUE)

}

