# setup -------------------------------------------------------------------
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
  library(shinyWidgets)
  library(cicerone)
  
})

# data --------------------------------------------------------------------
dsoc  <- readRDS("data/dsoc.rds")
movid <- readRDS("data/movid.rds")

movid <- movid %>% 
  mutate(
    sexo = factor(sexo, levels = c("Femenino", "Masculino", "Otro")),
    edad_3cat = factor(edad_3cat, levels = c("18 a 39", "40 a 64", "65 y más")),
    prev = factor(prev, levels = c("ISAPRE", "FONASA", "Ninguna", "Fuerzas Armadas y de Orden", 
                                   "Otra")),
    pr3_ocupacion = factor(pr3_ocupacion, levels = c("Trabaja de manera remunerada", "Otra actividad (jubilado, pensionado, recibe pensión de invalidez u otro)", 
                                                     "Desempleado o desempleada", "Quehaceres del hogar, cuidando niños y otras personas", 
                                                     "Estudia")),
    educ_3cat = factor(educ_3cat, levels = c("Profesional", "Técnica", "Media o menos")),
    region = factor(
      territorial::estandariza_regiones(region),
      levels = c(
          "Arica y Parinacota",
          "Tarapacá",
          "Antofagasta",
          "Atacama",
          "Coquimbo",
          "Valparaíso",
          "Metropolitana de Santiago",
          "Libertador General Bernardo O'Higgins",
          "Maule",
          "Ñuble",
          "Biobío",
          "La Araucanía",
          "Los Ríos",
          "Los Lagos",
          "Aysén del General Carlos Ibañez del Campo",
          "Magallanes y la Antártica Chilena"
          )
      )
    ) %>% 
  mutate(crn3_pq_tiempo.1_reg = crn3_pq_tiempo.1)
  


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

OPTS_SOSPECHOSO_DEFINICION <- c(
  `Definición MINSAL actual`  = "sosp_minsal0530",
  `Definición MINSAL inicial` = "sosp_minsal0326",
  `Definición MOVID19`        = "sosp_movid19"
)


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

OPTS_RAZONES <- c(
  "Porque tenía que esperar mucho tiempo" = "tiempo",
  "Porque no le pareció importante" = "nimporta",
  "Por el costo económico" = "costo",
  "Porque no sabía dónde realizarlo" = "nosabia",
  "Porque está evaluando si los síntomas empeoran antes de consultar" = "empeorar",
  "Por miedo a contagiarse" = "temor",
  "Porque los síntomas son leves o habituales" = "leves",
  "Por que el sistema de salud está muy lleno" = "sistlleno",
  "Otra" = "otra"
)

OPTS_RAZONES2 <- c(
  "Porque no le pareció importante" = "nimporta",
  "Por el costo económico" = "costo",
  "Porque no sabía dónde realizarlo" = "nosabia",
  "Porque tenía que esperar mucho tiempo" = "tiempo",
  "Porque el examen no estaba disponible" = "nodisp",
  "Porque está esperando poder realizárselo" = "espera",
  "Porque no estaba suficientemente grave" = "nograve",
  "Otra" = "otra"
)

OPTS_RAZONES2_DF <- OPTS_RAZONES2 %>%
  as.list() %>% 
  tibble::enframe() %>% 
  unnest(cols = c(value)) %>% 
  mutate(value = paste0("s8_exmn_", value))


# no covid razones --------------------------------------------------------

OPTS_RAZONES4 <- c(
  `Consulta atención médica` = "consulta",
  `Vacunación obligatoria` = "vacuna",
  `Examenes` = "examen",
  `Retiro o compra de medicamentos y/o ayudas técnicas` = "insumos",
  `Cirugías electivas o no urgentes` = "cirugia",
  `Otra` = "otra",
  `Ninguna de las anteriores` = "null"
)

OPTS_RAZONES4_DF <- OPTS_RAZONES4 %>%
  as.list() %>% 
  tibble::enframe() %>% 
  unnest(cols = c(value)) %>% 
  mutate(value = paste0("nc3_posp_", value, "_reg"))


# cronicos ----------------------------------------------------------------

OPTS_RAZONES3 <- c(
  `Porque no le pareció importante` = "nimporta",
  `Por el costo económico` = "costo",
  `Porque no sabía dónde realizarlo` = "nosabia",
  `Porque tenía que esperar mucho tiempo` = "tiempo.1",
  `Por miedo a contagiarse de COVID-19` = "temor",
  `Por que el sistema de salud está muy lleno` = "sistlleno",
  `No contó con transporte para trasladarse al lugar de atención` = "transporte",
  `No tuvo con quien dejar a personas a su cuidado` = "cuidados",
  `El servicio de salud le suspendió la hora y no le ha reagendado` = "cancela",
  Otra = "otra"
)

OPTS_RAZONES3_DF <- OPTS_RAZONES3 %>%
  as.list() %>% 
  tibble::enframe() %>% 
  unnest(cols = c(value)) %>% 
  mutate(value = paste0("crn3_pq_", value, "_reg"))

# practicas ---------------------------------------------------------------
# Acá son las etiquetas para cada práctica social
PRACTICAS_DF <- tibble(
  tipo = c("p1_pra_trabajo", "p1_pra_tramite", "p1_pra_recrea", "p1_pra_visita", 
    "p1_pra_invitado", "p1_pra_transporte", "p1_pra_protesta"),
  tipo_lbl = c("Trabajar", "Trámite", "Recreación", "Visitar amigos o familiares",
               "Recibido vistias de amigos o familiares", "Utilizado transporte público",
               "Protestas")
) %>% 
  mutate(tipo_lbl = fct_inorder(tipo_lbl))

TRANSPORTE_DF <- tibble(
  tipo = c("p3_transp_publico",  "p3_transp_taxi", "p3_transp_vehiculo", 
           "p3_transp_bici", "p3_transp_otra"),
  tipo_lbl = c("Transporte público", "Taxi o colectivo", "Vehículo particular",
               "Bicicleta", "Otra")) %>% 
  mutate(tipo_lbl = fct_inorder(tipo_lbl))


# highcharter -------------------------------------------------------------
hc_lang <- getOption("highcharter.lang")
hc_lang$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
hc_lang$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                         "agosto", "septiembre", "octubre", "noviembre", "diciembre")
hc_lang$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", 
                              "oct", "nov", "dic")
hc_lang$thousandsSep <- "."
hc_lang$decimalPoint <- ","
hc_lang$downloadJPEG <- "Descargar imagen"
hc_lang$downloadXLS <- "Descargar excel"
hc_lang$viewFullscreen <- "Ver en pantalla completa"
 

hc_chart <- getOption("highcharter.chart")
hc_chart$exporting <- list(enabled = TRUE)
hc_chart$credits <- list(
  enbled = TRUE,
  href = "https://www.movid19.cl/",
  text = "MOVID19"
)

options(
  highcharter.chart = hc_chart,
  highcharter.lang  = hc_lang,
  highcharter.google_fonts = TRUE,
  highcharter.theme = 
    hc_theme_smpl(
      chart = list(
        style = list(fontFamily = "Roboto")
      ),
      title = list(
        style = list(fontFamily = "Source Sans Pro", fontWeight  = "normal")
      ),
      subtitle = list(
        style = list(fontFamily = "Roboto", fontSize = "12px", fontWeight  = "normal")
      ),
      # colors = ggsci::pal_jama()(7)[-1],
      colors = c("#093C66",
                 "#DCA11D",
                 "#00C6BA",
                 "#887456",
                 "#ABCBFF",
                 # falta opcion, agrego rojo
                 "#d35400"
                 ),
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
      ),
      exporting = list(
        # scale = 1.75,
        sourceWidth = 1024,
        sourceHeight = 768,
        chartOptions = list(
          credits = list(
            text = "Monitoreo Nacional de Prácticas y Síntomas COVID-19 (MOVID19). www.movid19.cl",
            style = list(fontSize = "11px")
          )
        ),
        buttons = list(
          contextButton = list(
            symbol = 'url(img/downloadicon.png)',
            # symbol = "url(https://icon-library.com/images/3-dots-icon/3-dots-icon-28.jpg)",
            symbolSize = 18,
            symbolX = 21,
            symbolY = 20,
            titleKey = "Descargar",
            y = -05,
            menuItems = c("downloadJPEG", "downloadXLS")
            )
          )
        )
      )
  )


# tour --------------------------------------------------------------------
guide <- Cicerone$
  new(
    next_btn_text = "Siguiente",
    stage_background = "#F5F5F5",
    prev_btn_text = "Anterior",
    done_btn_text = "¡Listo!",
    close_btn_text = "Cerrar"
  )$ 
  step(
    el = "sidebarItemExpanded",
    title = "Secciones",
    description = "Cada sección esta asociada a ciertos aspectos relevantes
    de la encuesta MOVID19.",
    position = "right-center"
  )$
  step(
    "shiny-tab-inicio",
    "Sección",
    "Cada sección posee visualizaciones las cuales puedes controlar con 
    selectores que aparecen en la misma sección.",
    position = "mid-center"
  )$
  step(
    "inicial",
    "Gráficos",
    "Cada uno de los gráficos es interactivo, y en cada uno de ellos puedes
    descargar tanto la visualización como imagen o los datos en un archivo
    excel haciendo click en el ícono <img width='16px' src='img/downloadicon.png'/>
    ubicado en la esquina superior derecha.",
    position = "mid-center"
  )

