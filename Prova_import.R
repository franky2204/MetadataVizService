library(shiny)
library(readr)
library(DT)

ui <- fluidPage(
  titlePanel("Dataset Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file",
                "Choose a file",
                accept = c(".csv", ".tsv", ".txt")),
      checkboxInput("header", "Header", TRUE)
    ),
    
    mainPanel(
      DTOutput("table")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    
    # Determine the file format and read the dataset
    ext <- tools::file_ext(input$file$name)
    if (ext %in% c("csv", "txt", "tsv")) {
      read_fun <- switch(ext,
                         csv = read.csv,
                         txt = read.table,
                         tsv = read.table,
                         default = read.table)
      
      read_fun(input$file$datapath, header = input$header)
    } else {
      stop("Unsupported file format")
    }
  })
  
  output$table <- renderDT({
    datatable(data())
  })
}
shinyApp(ui, server)

