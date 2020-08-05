message("global.R")

suppressPackageStartupMessages({

  library(shiny)
  library(bs4Dash)
  library(dplyr)
  library(stringr)
  library(forcats)
  library(highcharter)
  library(shinyWidgets)
  library(ggsci)
  
})

# data --------------------------------------------------------------------
movid <- readRDS("data/movid.rds")

movid <- movid %>% 
  mutate(semana_fecha = as.Date(paste(2020, semana, 1, sep="-"), "%Y-%U-%u"))


# highcharter -------------------------------------------------------------
newlang_opts <- getOption("highcharter.lang")
newlang_opts$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                         "agosto", "septiembre", "octubre", "noviembre", "diciembre")
newlang_opts$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", 
                              "oct", "nov", "dic")
newlang_opts$thousandsSep <- "."
newlang_opts$decimalPoint <- ","

options(
  highcharter.lang = newlang_opts,
  highcharter.google_fonts = TRUE,
  highcharter.theme = 
    hc_theme_smpl(
      chart = list(
        style = list(fontFamily = "Roboto")
      ),
      title = list(
        style = list(fontFamily = "Montserrat")
      ),
      colors = ggsci::pal_jama()(7)[-1],
      xAxis = list(gridLineWidth = 1),
      yAxis = list(gridLineWidth = 1),
      plotOptions = list(
        series = list(
          marker = list(symbol = "circle")
        )
      ),
      tooltip = list(
        useHTML = TRUE
      ),
      
      legend = list(
        verticalAlign = "top",
        align = "left",
        itemStyle =  list(
          fontWeight = "normal"
        )
      )
    )
)
