setwd(".")

source("Libraries.R")
source("functions.R")
source("ThemeShiny_QBio.R")
source("Palettes.R")
source("Univar_functs.R")
source("Bivar_functs.R")

source("server.R")
source("ui.R")

options(shiny.port = 8180)
options(shiny.host = "0.0.0.0")

shinyApp(ui, server)


