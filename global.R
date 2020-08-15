message("global.R")

suppressPackageStartupMessages({

  library(shiny)
  library(bs4Dash)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(highcharter)
  library(shinyWidgets)
  library(ggsci)
  
})

# data --------------------------------------------------------------------
movid <- readRDS("data/movid.rds")

# # demograficas
# movid$sexo      %>% table()
# movid$prev      %>% table()
# movid$edad_3cat %>% table()
# movid$educ_3cat %>% table()

# sintomas ----------------------------------------------------------------
OPTS_SINTOMAS <- c(
    `Fiebre (temperatura axilar sobre los 37,8° C)` = "snt_fiebre",
    Tos = "snt_tos",
    `Dificultad para respirar` = "snt_disnea",
    `Dolor muscular` = "snt_mialgias",
    `Dolor de garganta` = "snt_odinofagia",
    `Disminución o pérdida del olfato` = "snt_anosmia",
    `Disminución o pérdida del gusto` = "snt_disgeusia",
    `Dolor en el pecho` = "snt_dol_torax",
    `Dolor de cabeza` = "snt_cefalea",
    Diarrea = "snt_diarrea",
    `No he tenido ninguno de estos síntomas` = "snt_null"
  )

OPTS_SINTOMAS_DF <- OPTS_SINTOMAS %>%
  as.list() %>% 
  tibble::enframe() %>% 
  unnest(cols = c(value)) %>% 
  mutate(value = paste0("s1_", value))


# sistema salud -----------------------------------------------------------
OPTS_DESAGREGAR <- c(
  "Sin desagregar" = "todo",
  "Sexo" = "sexo", 
  "Edad" = "edad_3cat",
  # región, 
  "Previsión" = "prev",
  "Ocupación" = "pr3_ocupacion", 
  "Educación" = "educ_3cat" 
  # residencia en comuna en cuarentena
)

OPTS_BARRERAS_CONSULTA <- movid %>%
  head(1) %>% 
  select(starts_with("s3_cons")) %>% 
  names()

OPTS_BARRERAS_CONSULTA <- setNames(OPTS_BARRERAS_CONSULTA, str_remove(OPTS_BARRERAS_CONSULTA, "s3_cons_"))

OPTS_BARRERAS_EXAMEN <- movid %>%
  head(1) %>% 
  select(starts_with("s8_exmn")) %>% 
  names()

OPTS_BARRERAS_EXAMEN <- setNames(OPTS_BARRERAS_EXAMEN, str_remove(OPTS_BARRERAS_EXAMEN, "s8_exmn_"))


# PRACTICAS ---------------------------------------------------------------



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
      subtitle = list(
        style = list(fontFamily = "Roboto", fontSize = "12px")
      ),
      # colors = ggsci::pal_jama()(7)[-1],
      colors = c("#093C66",
                 "#DCA11D",
                 "#00C6BA",
                 "#887456",
                 "#ABCBFF"),
      xAxis = list(gridLineWidth = 1),
      yAxis = list(gridLineWidth = 1),
      plotOptions = list(
        line = list(marker = list(enabled = FALSE), lineWidth = 4, opacity = 0.85),
        series = list(
          marker = list(symbol = "circle")
        )
      ),
      tooltip = list(
        useHTML = TRUE,
        valueDecimals = 2
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
