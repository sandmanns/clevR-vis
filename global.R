loadOrInstall <- function (packageName, type = "CRAN") {
  
  isPackageInstalled <- packageName %in% rownames(installed.packages())
  
  if (!isPackageInstalled) {
    
    if (type == "CRAN") {
      
      install.packages(packageName)
      
    } 
    
  }
  
  library(packageName, character.only = TRUE)
}

cranPackages <- c(
  "shiny",
  "RColorBrewer",
  "ggraph",
  "igraph",
  "ggiraph", 
  "cowplot",
  "htmlwidgets",
  "plotwidgets",
  "readxl",
  "dplyr",
  "readr",
  "purrr",
  "tibble",
  "patchwork",
  "R.utils",
  "shinyWidgets",
  "colorspace",
  "shinyhelper",
  "shinycssloaders",
  "ggnewscale",
  "shinydashboard",
  "DT",
  "colourpicker",
  "readxl",
  "plotwidgets",
  "ggplot2",
  "R.utils",
  "shinycssloaders"
)


for (package in cranPackages) {
  
  loadOrInstall(package)
  
}



source("R/clevRvisShiny.R")
source("R/createSeaObject.R")
source("R/dolphinPlot.R")
source("R/extSharkPlot.R")
source("R/helper.R")
source("R/plaicePlot.R")
source("R/plotWidget.R")
source("R/sharkPlot.R")


