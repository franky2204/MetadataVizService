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

source("server.R")
source("ui.R")


shinyApp(ui, server)


