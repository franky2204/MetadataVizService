theme_QBio<-list(includeCSS("ShinyMirriTemplate/Style/Settings.css"),
                 includeCSS("ShinyMirriTemplate/Style/Components.css"),
                 includeCSS("ShinyMirriTemplate/Style/font.css"),
                 includeHTML("ShinyMirriTemplate/head.html")
)
                                               
headerPanel<-function (title="", windowTitle = title) {
  tagList(tags$head(tags$title(windowTitle)), 
          htmltools::div(class = "headerMirri", 
                         htmltools::img(src = knitr::image_uri("ShinyMirriTemplate/Loghi/loghi_Mirri.png"), 
                                        alt = 'logo', 
                                        style = "float: left;
                                        padding-bottom:5px;
                                        padding-top:5px;
                                        padding-left:15px;
                                        height:40px"),
                         h1(title,style="float: right;
                                        padding-bottom:5px;
                                        padding-top:5px;
                                        margin: 10px;
                                        padding-right:15px;")
                         )
  )
}



footerPanel <- function(title = "", windowTitle = title) {
  tagList(
    tags$footer(),
    htmltools::div(
      class = "footerMirri",
      htmltools::div(
        class = "row",
        htmltools::div(
          class = "row",
          htmltools::div(
            class = "col-md-2"),
          htmltools::div(
            class = "col-md-3", 
            htmltools::div(
              h4("SUS-MIRRI.IT Headquarter"),
              h5(HTML("University of Turin<br>
              Via Verdi 8, 10124, Turin, Italy<br>
              Scientific Coordinator: Prof. Cristina Varese"),
              style="font-size: 13px;"),
              style="margin: 7.6%;"
            ),
            style="font-family: Martel Sans;"
          ),
          htmltools::div(
            class = "col-md-5 ", 
            style="font-size: 13px; font-family: Martel Sans;",
            htmltools::div(
              h5(class="text-right",
              HTML("This web platform was produced under the workpackage 3 of the project “<strong>Strengthening the MIRRI Italian Research Infrastructure for Sustainable Bioscience and Bioeconomy</strong>” (SUS-MIRRI.IT) and has received funding from the “PNRR M4C2 Iniziativa 3.1 - Infrastrutture di Ricerca (IR), cod. MUR IR0000005”.")),
              style="margin-top: 5%; margin-bottom: 5%; margin-left: 2.5%; margin-right: 2%;"
            )
          ),
          htmltools::div(
            class = "col-md-2")
        ),
        htmltools::div(
          class = "col-md-12 text-center",
          style="font-size: 13px;",
          h5("© 2024 - SUS-MIRRI.IT mBRC.")
        )
      )
    )
  )
}


mainPanel<-function (..., width = 8) {
  div(class = paste0("col-sm-", width), tags$form(role = "main",class = "well", ...))
}

sidebarPanel<-function (..., width = 4) {
  div(class = paste0("col-sm-", width), role = "complementary", ...)
}

varbatimTextOutput_h3<-function (outputId, placeholder = FALSE) 
{
  h3(id = outputId, class = "shiny-text-output", class = if (!placeholder) 
    "noplaceholder")
}

varbatimTextOutput_p<-function (outputId, placeholder = FALSE) 
{
  p(id = outputId, class = "shiny-text-output", class = if (!placeholder) 
    "noplaceholder")
}