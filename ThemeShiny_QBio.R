theme_QBio<-list(includeCSS("Settings.css"),
                 includeCSS("Components.css"))
                                               
headerPanel<-function (title="", windowTitle = title) 
  {
  tagList(tags$head(tags$title(windowTitle)), 
          htmltools::div(class = "headerQBio", 
                         htmltools::img(src = knitr::image_uri("logo_unito_neg.png"), 
                                        alt = 'logo', 
                                        style = "float: left;
                                        padding-bottom:5px;
                                        padding-top:5px;
                                        padding-left:15px;
                                        height:50px"),
                         h1(title),
                         htmltools::img(src = knitr::image_uri("Logo_QBio_sfondoscuro.png"), 
                                        alt = 'logo', 
                                        style = "float: right;
                                        padding-top:10px;
                                        padding-bottom:10px;
                                        padding-right:15px;
                                        height:50px")
                         )
  )
}

