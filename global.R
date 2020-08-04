message("global.R")

suppressPackageStartupMessages({

  library(shiny)
  library(bs4Dash)
  library(dplyr)
  library(highcharter)
  library(shinyWidgets)
  library(ggsci)
  
})



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
    hc_theme(
      chart = list(
        style = list(fontFamily = "Roboto")
      ),
      colors = ggsci::pal_jama(alpha = 0.7)(7),
      xAxis = list(gridLineWidth = 1),
      yAxis = list(gridLineWidth = 1),
      plotOptions = list(
        series = list(
          marker = list(symbol = "circle")
        )
      ),
      exporting = list(
        buttons = list(
          contextButton = list(
            symbol = 'url(https://www.iconsdb.com/icons/preview/gray/download-2-xxl.png)',
            symbolSize = 18,
            symbolX = 21,
            symbolY = 20,
            titleKey = "Descargar",
            y = -05
          )
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
