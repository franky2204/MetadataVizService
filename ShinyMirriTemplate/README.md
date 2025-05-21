# Template for Mirri Shiny app

## Import

For downloading the template into your local storage use the following code in your terminal:

```{bash}
mkdir projectdirectory
cd projectdirectory
git clone https://github.com/qBioTurin/ShinyMirriTemplate.git
```

(you can substitute *projectdirectory* with the name/path of the folder you want to use as project directory).

## Getting started

Into your *projectdirectory* folder create two R files:

-   ui.R

-   server.R

! be careful the names of the files are sensitive !

Then open them on RStudio and initialize them respectively with

```{r ui}
setwd(".")

if(!require("shiny")) install.packages("shiny", dependencies = TRUE)
source("Layout/ThemeShiny_QBio.R")

ui <- fluidPage(
  theme_QBio,
  headerPanel("Your Title"),
  
  footerPanel()
)
```

```{r}
server<-function(input,output, session){
  
}
```

Enjoy :)
