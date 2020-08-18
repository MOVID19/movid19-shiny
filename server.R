input <- list(
  snt_opt = c("snt_tos", "snt_odinofagia"), 
  ssd_opt = "sexo",
  razones_opt = "tiempo"
  )

shinyServer(function(input, output, session) {
  
# inicio ------------------------------------------------------------------
  
  cifras <- function() {
    
    l <- list()
    
    d <- read_csv(
      "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo.csv",
      col_types = cols(
        .default = col_double(),
        Region = col_character()
      )
    )
    
    l[["casos_totales"]] <- d %>% 
      filter(Region == "Total") %>%
      select(last_col()) %>% 
      pull()
    
    suppressMessages({
      d <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto14/FallecidosCumulativo_T.csv")
    })
    
    l[["fallecidos_totales"]] <- d %>% 
      select(Total) %>% 
      filter(row_number() == n()) %>% 
      pull()
    
    
    l[["respuestas"]] <- nrow(movid)
    
    l[["numero_participantes"]] <- movid %>% 
      distinct(pob_id) %>% 
      nrow()
    
    l
      
  }
  
  cifras_actuales <- cifras()
  
  output$vb_casos <- renderValueBox({
    valueBox(
      cifras_actuales$casos_totales, "Casos", icon = "lungs-virus", 
      footer = tags$small("Fuente ", tags$a("MINSAL (2020)", target="_blank", href = "https://www.gob.cl/coronavirus/cifrasoficiales/"))
      )
  })
  output$vb_fallc <- renderValueBox({
    valueBox(
      cifras_actuales$fallecidos_totales, "Fallecidos", icon = "bookmark",
      footer = tags$small("Fuente ", tags$a("MINSAL (2020)", target="_blank", href = "https://www.gob.cl/coronavirus/cifrasoficiales/"))
      )
  })
  output$vb_resps <- renderValueBox({
    valueBox(
      cifras_actuales$respuestas, "Respuestas", icon = "list",
      footer = tags$small("MOVID19")
      )
  })
  output$vb_partc <- renderValueBox({
    valueBox(
      cifras_actuales$numero_participantes, "Participantes", icon = "users",
      footer = tags$small("MOVID19")
      )
  })
  
  output$inc_respuestas <- renderHighchart({
    
    hc <- movid %>%
      count(semana_fecha) %>% 
      hchart(
        "line",
        hcaes(semana_fecha, n),
        name = "Cantidad de respuestas"
      ) %>%
      hc_tooltip(shared = TRUE, table = TRUE, valueDecimals = 0) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""), min = 0)
    
    # htmlwidgets::saveWidget(hc, "chartiframe.html")
    # fs::file_move("chartiframe.html", "www/chartiframe.html")
    
    hc
    # %>% 
    #   hc_add_dependency("custom/appear.js")
    # 
  })
  
  output$inc_genero <- renderHighchart({
    
    movid %>%
      count(sexo) %>% 
      mutate(sexo = coalesce(sexo, "No responde")) %>% 
      mutate(p = scales::percent(n/sum(n))) %>% 
      hchart(
        "pie",
        hcaes(name = sexo, y = n),
        innerSize = "75%",
        name = "Género",
        tooltip = list(valueDecimals = 0, pointFormat = '<b>{point.y}</b><br/>'),
        dataLabels = list(
          format =  '{point.name} <span style="opacity: 0.4">{point.p}</span>'
        )
      )
        
  })
  

# sintomas ----------------------------------------------------------------
  output$snt_hc_tlsnt <- renderHighchart({
   
    selected <- input$snt_opt
    
    if(is.null(selected)) {
      updateSelectInput(session, "snt_opt", selected =  OPTS_SINTOMAS[1])
      selected <- OPTS_SINTOMAS[1]
    } 
    
    d <- movid %>%
      select(semana_fecha, all_of(paste0("s1_", selected))) %>%
      gather(tipo, valor, -semana_fecha) %>%
      left_join(OPTS_SINTOMAS_DF, by = c("tipo" = "value")) %>%
      group_by(semana_fecha, name) %>%
      summarise(proporcion = mean(100 * valor, na.rm = TRUE), .groups = "drop") %>% 
      rename(sintoma = name)

    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = sintoma)
      ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)
    
  })

  output$snt_hc_sospc <- renderHighchart({

    d <- movid %>%
      select(semana_fecha, sosp_minsal0326, sosp_minsal0530, sosp_movid19) %>%
      gather(tipo, valor, -semana_fecha) %>%
      group_by(semana_fecha, tipo) %>%
      summarise(proporcion = mean(100 * valor, na.rm = TRUE), .groups = "drop") %>% 
      mutate(
        definicion = case_when(
          tipo == "sosp_minsal0326" ~ "MINSAL Inicial",
          tipo == "sosp_minsal0530" ~ "MINSAL Actual",
          tipo == "sosp_movid19"    ~ "MOVID19"
          ),
        definicion = factor(definicion, levels = c("MINSAL Actual", "MINSAL Inicial", "MOVID19"))
        ) %>% 
    arrange(semana_fecha, definicion)

    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = definicion),
      visible = c(TRUE, FALSE, FALSE)
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = "Es caso sospechoso según definición contenida en decreto sanitario y MOVID19")
    
  })
  
  output$snt_hc_contc <- renderHighchart({
    
    d <- movid %>%
      select(semana_fecha, contacto) %>%
      # gather(tipo, valor, -semana_fecha) %>%
      group_by(semana_fecha) %>%
      summarise(proporcion = mean(100 * contacto, na.rm = TRUE), .groups = "drop") %>% 
      arrange(semana_fecha)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion),
      name = "Proporción de personas que declaran un contacto estrecho con caso confirmado COVID19",
      showInLegend = TRUE
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = "Ha tenido alguna forma de contacto con un paciente confirmado de enfermedad COVID19 en la última semana")
    
  })
  
  output$snt_hc_confm <- renderHighchart({
    
    d <- movid %>%
      group_by(semana_fecha) %>%
      summarise(
        `Proporción caso confirmado COVID19` = mean(100 * coalesce(s10_exmn_confirmado, 0), na.rm = TRUE),
        `Proporción caso probable COVID19` = mean(100 * caso_probable2, na.rm = TRUE),
        `Totales` =  mean(100 * (caso_probable2 | coalesce(s10_exmn_confirmado, 0)), na.rm = TRUE),
        .groups = "drop"
        ) %>% 
      gather(tipo, proporcion, -semana_fecha) %>% 
      arrange(semana_fecha)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo)
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = "Ha tenido alguna forma de contacto con un paciente confirmado de 
                  enfermedad COVID19 o presenta al menos un síntoma en la última semana.")
    
  })

# sistema salud -----------------------------------------------------------

  dssd <- reactive({
    
    # OPTS_DESAGREGAR
    # input$ssd_opt <- "educ_3cat"
    
    dssd <- movid %>%
      select(
        semana_fecha, 
        all_of(input$ssd_opt), 
        s2_consulta, 
        sosp_minsal0326, 
        s7_exmn_realizado,
        s10_exmn_confirmado,
        s4_consulta_dias,
        s11_exmn_espera,
        starts_with("s3_cons"),
        starts_with("s8_exmn")
        ) %>% 
      rename_at(vars(2), ~"tipo")
    
    # dssd %>% count(tipo, sort = TRUE)
    # dssd %>%
    #   count(tipo, sort = TRUE) %>% 
    #   pull(tipo) %>% 
    #   # setNames(rep(TRUE, length(.)), .) %>% 
    #   dput()

    tipo_detalles <- switch(
      input$ssd_opt,
      todo = c("Total" = TRUE),
      sexo = c("Femenino" = TRUE, "Masculino" = TRUE, "Otro" = FALSE),
      edad_3cat = c("18 a 39" = TRUE, "40 a 64" = TRUE, "65 y más" = TRUE),
      prev = c("ISAPRE" = TRUE, "FONASA" = TRUE, "Ninguna" = FALSE, "Fuerzas Armadas y de Orden" = FALSE, "Otra" = FALSE),
      pr3_ocupacion = c("Trabaja de manera remunerada" = TRUE, "Otra actividad (jubilado, pensionado, recibe pensión de invalidez u otro)" = TRUE, 
                        "Desempleado o desempleada" = TRUE, "Quehaceres del hogar, cuidando niños y otras personas" = FALSE, 
                        "Estudia" = FALSE),
      educ_3cat = c("Profesional" = TRUE, "Técnica" = TRUE, "Media o menos" = TRUE)
    )
    
    dssd <- dssd %>% 
      filter(!is.na(tipo)) %>% 
      mutate(tipo = factor(tipo, levels = names(tipo_detalles)))
    
    # dssd %>% count(tipo)
    
    attr(dssd, "visible") <- unname(tipo_detalles)
    
    dssd
    
  })

  values <- reactiveValues(lastUpdated = NULL)

  # observe ({
  #   input[["ssd_opt"]]
  #   values$lastUpdated <- "ssd_opt"
  # })
  # observe ({
  #   input[["ssd_opt2"]]
  #   values$lastUpdated <- "ssd_opt2"
  # })
# 
#   observe({
#     lapply(names(input), function(x) {
#       observe({
#         message("observe: ", x)
#         input[[x]]
#         values$lastUpdated <- x
#       })
#     })
#   })
# 
#   observeEvent(input, {
#     
#     x <- values$lastUpdated
#     message("observeEvent: ", x)
#     v <- input[[x]]
#     
#     lapply(
#       c("ssd_opt", "ssd_opt2"),
#       function(x) {
#        
#         updateSelectInput(
#           session = session,
#           inputId = x,
#           selected = v
#           )
#          
#       })
#   })
#     
  output$ssd_hc_cslta <- renderHighchart({
    
    dssd <- dssd()
  
    d <- dssd %>%
      filter(s2_consulta == 1) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * sosp_minsal0326, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = "Consultó a un profesional de la salud por síntomas.")
    
  })
  
  output$ssd_hc_examn <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      filter(s7_exmn_realizado == 1) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * sosp_minsal0326, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = "Se ha realizado uno o más exámenes de enfermedad COVID19 
                  (alguna vez para primeras observaciones o en la última semana 
                  para observaciones de seguimiento).")
    
  })
  
  output$ssd_hc_posit <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      filter(s7_exmn_realizado == 1) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * s10_exmn_confirmado, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = "Recibió resultado positivo en algún examen para 
                  enfermedad COVID19 realizado durante la última semana.")
    
  })
  
  output$ssd_hc_ctads <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(s4_consulta_dias, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0) %>% 
      hc_subtitle(text = "Promedio de días de espera entre inicio de síntomas y 
                  consulta al médico (casos que reportan presencia de síntomas).")
    
  })
  
  output$ssd_hc_exesp <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(s11_exmn_espera, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0) %>% 
      hc_subtitle(text = "Promedio de días de espera entre toma y 
                  resultado del test diagnóstico ")
    
  })
  
  output$ssd_hc_ctaex <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(s4_consulta_dias + s11_exmn_espera, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0) %>% 
      hc_subtitle(text = "Promedio de días de espera entre inicio de síntomas y 
                  resultado del test diagnóstico.")
    
  })
  
  output$ssd_hc_s3con <- renderHighchart({
    
    dssd <- dssd()
    
    dssd <- dssd %>%
      select(all_of(str_c("s3_cons_", input$razones_opt)), everything()) %>% 
      rename_at(vars(1), ~ "variable")
    
    d <- dssd %>%
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * variable, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_title(text = "Razones para no consultar profesional")
    
  })
  
  output$ssd_hc_s8exm <- renderHighchart({
    
    dssd <- dssd()
    
    dssd <- dssd %>%
      select(all_of(str_c("s8_exmn_", input$razones_opt)), everything()) %>% 
      rename_at(vars(1), ~ "variable")
    
    d <- dssd %>%
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * variable, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)  %>% 
      hc_title(text = "Razones para no realizarse exámen indicado por profesional")
    
  })  
  

# prácticas sociales ------------------------------------------------------

  output$pcsoc_frec_salida <- renderHighchart({
    
    d <- movid %>% 
      select(semana_fecha, contains("p1_pra"), -p1_pra_otro_TEXT, -p1_pra_otro) %>% 
      gather(tipo, valor, -semana_fecha) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(promedio = mean(valor, na.rm = TRUE), .groups = "drop") %>% 
      left_join(PRACTICAS_DF, by = "tipo") %>% 
      arrange(semana_fecha, tipo_lbl)
  
    hchart(
      d,
      "line",
      hcaes(semana_fecha, promedio, group = tipo_lbl)
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0)  
    
  })  
  
  output$pcsoc_prop2 <- renderHighchart({
    
    d <- movid %>% 
      select(semana_fecha, contains("p1_pra"), -p1_pra_otro_TEXT, -p1_pra_otro) %>% 
      gather(tipo, valor, -semana_fecha) %>%
      mutate(valor = valor >= 2) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(proporcion = mean(100 * valor, na.rm = TRUE), .groups = "drop") %>% 
      left_join(PRACTICAS_DF, by = "tipo") %>% 
      arrange(semana_fecha, tipo_lbl)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)  
    
  }) 
  
  output$pcsoc_nosalen <- renderHighchart({
    
    d <- movid %>% 
      select(semana_fecha, contains("p1_pra"), -p1_pra_otro_TEXT, -p1_pra_otro) %>% 
      gather(tipo, valor, -semana_fecha) %>%
      mutate(valor = valor == 0) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(proporcion = mean(100 * valor, na.rm = TRUE), .groups = "drop") %>% 
      left_join(PRACTICAS_DF, by = "tipo") %>% 
      arrange(semana_fecha, tipo_lbl)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_tooltip(table = TRUE, sort = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100)  
    
  })  
  
# percepcion de riesgo ----------------------------------------------------

  output$persgo_alto <- renderHighchart({
    
    
  })
  
        
})