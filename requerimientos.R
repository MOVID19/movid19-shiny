if (!require("remotes")) install.packages("remotes")

if (!require("highcharter")) remotes::install_github("jbkunst/highcharter")

paquetes <- c("shiny", "bs4Dash", "dplyr", "shinyWidgets", "purrr", "ggsci")

for(p in paquetes) {
  
  if (!require(p)) install.packages(p)
  
}