library(shiny)
library(tidyverse)
library(MoMAColors)
library(shinyWidgets)
library(waffle)
library(ggmosaic)
library(shinythemes)
library(numbers)
library(ggridges)
library(bslib)
library(corrr)

setwd("/Users/danielavolpatto/Documents/Università/dottorato/Interfaccia grafica")
source("df_Prova.R")

classes<-factor(classes,levels = c("character","logic","factor","numeric"))
names(classes)<-variables
ord_factor<-c(FALSE,FALSE,TRUE,FALSE,TRUE)
names(ord_factor)<-variables[which(classes=="factor")]

lev_factor<-list(c("I","II","III"),c("DIAG","FU3","M1","M2"))
names(lev_factor)<-variables[classes=="factor"][ord_factor]

countable_variables<-variables[classes=="factor"|classes=="logic"]
type_ord<-lapply(df%>%
                   select(all_of(variables[which(classes=="factor")][ord_factor])),
                 unique)
type_facets<-lapply(df%>%
                      select(all_of(variables[which(classes=="factor")][!ord_factor])),
                    unique)
type_boole<-lapply(df%>%
                     select(all_of(variables[which(classes=="logic")])),
                   unique)

ntype_ord<-lapply(type_ord,length)
ntype_facets<-lapply(type_facets,length)
golden<-(1+sqrt(5))/2

ndata<-nrow(df)

change_type<-function(variable) {
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

df <- data.frame(lapply(variables,change_type))
colnames(df)<-variables

source("Palettes.R")
source("Univar_functs.R")
source("Bivar_functs.R")
source("ThemeShiny_QBio.R")

# x <- df %>%
#   correlate() %>%    # Create correlation data frame (cor_df)
#   focus(variables[classes=="numeric"],mirror = TRUE) %>%  # Focus on cor_df without 'cyl' and 'vs'
#   rearrange() %>%  # rearrange by correlations
#   shave()
# df%>%correlate()%>% 
#   network_plot(min_cor = .2)
# rplot(x)

ui <- fluidPage(
  theme_QBio,
  headerPanel("Preprocessing"),
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
)
server <- function(input, output) {
  
  v <- reactiveValues(data = NULL)
  v$plot_n<-1
  
  observeEvent(input$refresh, {
    v$plot_n<-v$plot_n+1
  })
  
  output$distPlot <- renderPlot({
    var<-names(sort(classes[input$var]))
    var[classes[var]=="factor"]<-names(sort(ord_factor[names(classes[var][classes[var]=="factor"])]))
    
    if (length(var)==1){
      var<-as.character(input$var[1])
      
      if(classes[var]=="logic"){
        v$plot<-list(barplot_logic(df,var),
                     pie_logic(df,var)
                     )
      }
      else if(classes[var]=="factor"&!ord_factor[var]){
        v$plot<-list(waffle_factor_nonord(df,var),
                     barplot_factor_nonord(df,var)
                     )
      }
      else if(classes[var]=="factor"&ord_factor[var]){
        v$plot<-list(barplot_factor_ord_x(df,var),
                     barplot_factor_ord_stack(df,var)
                     )
      }
      else if(classes[var]=="numeric"){
        v$plot<-list(hist_numeric(df,var),
                     density_numeric(df,var),
                     boxplot_numeric(df,var)
                     )
      }
    }
    else if (length(var)==2){
      var1<-as.character(var[1])
      var2<-as.character(var[2])
      
      if(classes[var1]=="logic"&classes[var2]=="logic"){
        v$plot<-list(barplot_logic_logic(df,var1,var2),
                     barplot_logic_logic(df,var2,var1))
        }
      else if(classes[var1]=="logic"&classes[var2]=="factor"&!ord_factor[var2]){
        v$plot<-list(barplot_logic_factnonord_stack(df,var1,var2),
                     barplot_logic_factnonord_dodge(df,var1,var2),
                     barplot_logic_factnonord_stack(df,var2,var1),
                     barplot_logic_factnonord_dodge(df,var2,var1)
        )
        }
      else if(classes[var1]=="logic"&classes[var2]=="factor"&ord_factor[var2]){
        v$plot<-list(barplot_logic_factord_stack(df,var1,var2),
                     barplot_logic_factord_dodge(df,var1,var2),
                     barplot_logic_factord_stack(df,var2,var1),
                     barplot_logic_factord_dodge(df,var2,var1)
        )
      }
      else if(classes[var1]=="logic"&classes[var2]=="numeric"){
        v$plot<-list(density_logic_num(df,var1,var2),
                     boxplot_logic_num(df,var1,var2),
                     histogram_logic_num_stack(df,var1,var2),
                     histogram_logic_num_identity(df,var1,var2)
        )
      }
      else if(classes[var1]=="factor"&!ord_factor[var1]&classes[var2]=="factor"&!ord_factor[var2]){
        v$plot<-list(barplot_factornonord_factornonord(df,var1,var2),
                     barplot_factornonord_factornonord(df,var2,var1)
        )
      }
      else if(classes[var1]=="factor"&!ord_factor[var1]&classes[var2]=="factor"&ord_factor[var2]){
        v$plot<-list(barplot_factornonord_factorord(df,var1,var2),
                     barplot_factorord_factornonord(df,var1,var2)
        )
      }
      else if(classes[var1]=="factor"&!ord_factor[var1]&classes[var2]=="numeric"){
        v$plot<-list(density_factornonord_num(df,var1,var2),
                     histogram_factornonord_num_identity(df,var1,var2),
                     histogram_factornonord_num_stack(df,var1,var2),
                     boxplot_factornonord_num(df,var1,var2)
        )
      }
      else if(classes[var1]=="factor"&ord_factor[var1]&classes[var2]=="factor"&ord_factor[var2]){
        v$plot<-list(barplot_factord_factord_dodge(df,var1,var2),
                     barplot_factord_factord_dodge(df,var2,var1),
                     barplot_factord_factord_stack(df,var1,var2),
                     barplot_factord_factord_stack(df,var2,var1)
        )
      }
      else if(classes[var1]=="factor"&ord_factor[var1]&classes[var2]=="numeric"){
        v$plot<-list(density_factorord_num(df,var1,var2),
                     boxplot_factorord_num(df,var1,var2)
        )
      }
      else if(classes[var1]=="numeric"&classes[var2]=="numeric"){
        v$plot<-list(scatter_num_num(df,var1,var2),
                     scatter_num_num(df,var2,var1)
        )
      }
    }
    else{v$plot<-NULL}
    
    return(v$plot[[mod(v$plot_n, length(v$plot))+1]])
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

 shinyApp(ui = ui, server = server)

