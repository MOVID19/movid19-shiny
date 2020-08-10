if (!require("remotes")) install.packages("remotes")

if (!require("highcharter")) remotes::install_github("jbkunst/highcharter")

if (!require("bs4Dash")) remotes::install_github("RinteRface/bs4Dash", force = TRUE)

# CRAN
paquetes <- c("shiny", "tidyverse", "shinyWidgets", "ggsci")

for(p in paquetes) {
  
  if (!require(p)) install.packages(p)
  
}