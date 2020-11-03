input <- list(
  snt_opt = c("snt_tos", "snt_odinofagia"), 
  snt_sos = "sosp_minsal0530",
  ssd_opt = "prev",
  # ssd_opt = "todo",
  razones_opt = "temor",
  razones_opt2 = c("espera", "nodisp", "nograve", "nosabia", "nimporta"),
  razones_opt3 = c("temor", "cancela", "costo", "sistlleno"),
  razones_opt4 = c("consulta", "examen", "insumos", "vacuna")
  )

shinyServer(function(input, output, session) {
  
# ayuda -------------------------------------------------------------------


  observeEvent(input$current_tab, {
    
    if (input$current_tab == "ayuda") {
      
      bs4Dash::updateTabItems(session, "current_tab", selected = "inicio")
      
      guide$init()$start()
    
    }
  })
  

# data por variable a desagregar y linkeo de selectores -------------------
  
  dssd <- reactive({
    
    # OPTS_DESAGREGAR
    # input$ssd_opt <- "educ_3cat"
    
    # input$
    # input$ssd_opt2
    
    dssd <- movid %>%
      select(
        # importante que esta vaya al principio
        all_of(input$ssd_opt), 
        pob_id,
        semana_fecha, 
        s2_consulta, 
        sosp_minsal0326, 
        s7_exmn_realizado,
        s10_exmn_confirmado,
        s4_consulta_dias,
        s11_exmn_espera,
        starts_with("s3_cons"),
        starts_with("s8_exmn"),
        sosp_minsal0530,
        sosp_minsal0326,
        sosp_movid19,
        contacto,
        sintoma,
        s6_exmn_indicado,
        crn1_consulta_reg,
        crn2_posponer_reg,
        starts_with("crn3_pq"),
        starts_with("nc"),
        starts_with("nc3_posp")
      ) %>% 
      rename_at(vars(1), ~"tipo")
    
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
    
    attr(dssd, "visible") <- tipo_detalles
    
    dssd
    
  })
  
  values <- reactiveValues(lastUpdated = NULL)
  
  observeEvent(input$ssd_opt,  { values$lastUpdated <- "ssd_opt"  })
  observeEvent(input$ssd_opt2, { values$lastUpdated <- "ssd_opt2" })
  observeEvent(input$ssd_opt3, { values$lastUpdated <- "ssd_opt3" })
  observeEvent(input$ssd_opt4, { values$lastUpdated <- "ssd_opt4" })
  
  observeEvent(c(input$ssd_opt, input$ssd_opt2, input$ssd_opt3, input$ssd_opt4), {
    
    x <- values$lastUpdated
    # message("observeEvent: ", x)
    v <- input[[x]]
    
    lapply(
      c("ssd_opt", "ssd_opt2", "ssd_opt3"),
      function(x) {
        updateSelectInput(
          session = session,
          inputId = x,
          selected = v
          )
        })
    })
  
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
  
  output$inicial <- renderHighchart({
    
    d <- movid %>% 
      select(semana_fecha, contains("p1_pra"), -contains("p1_pra_otro_TEXT"),
             -contains("p1_pra_otro"), -p1_pra_invitado, -p1_pra_transporte) %>% 
      gather(tipo, valor, -semana_fecha) %>%
      mutate(valor = valor >= 2) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * valor, na.rm = TRUE), 
        cantidad = n(),
        .groups = "drop"
      ) %>% 
      left_join(PRACTICAS_DF, by = "tipo") %>% 
      arrange(semana_fecha, tipo_lbl)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_tooltip_n(separado = FALSE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)  
    
  })
  
  output$inicial_2 <- renderHighchart({
    
    # dssd <- dssd()
    
    d <- movid %>%
      filter(sosp_minsal0530 == 1) %>% 
      group_by(semana_fecha, tipo = prev) %>% 
      summarise(
        proporcion = mean(100 * s2_consulta, na.rm = TRUE),
        cantidad = sum(s2_consulta, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      filter(!is.na(tipo))
    
    vsbl <- c("ISAPRE" = TRUE, "FONASA" = TRUE, "Ninguna" = FALSE, "Fuerzas Armadas y de Orden" = FALSE, "Otra" = FALSE)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = vsbl
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = " Porcentaje de personas que cumplen con definición 
                  vigente de casos sospechosos que consulta a un profesional 
                  de salud por sus síntomas")  
    
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
      summarise(
        proporcion = mean(100 * valor, na.rm = TRUE),
        cantidad = sum(valor, na.rm = TRUE),
        .groups = "drop"
        ) %>% 
      rename(sintoma = name)

    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = sintoma)
      ) %>%
      hc_tooltip_n(separado = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)
    
  })

  output$snt_hc_sospc <- renderHighchart({

    dssd <- dssd()
    
    d <- dssd %>% 
      select(semana_fecha, tipo,  all_of(input$snt_sos)) %>% 
      rename_at(vars(3),  ~ "definicion") %>% 
      group_by(semana_fecha, tipo) %>%
      summarise(
        proporcion = mean(100 * definicion, na.rm = TRUE),
        cantidad = sum(definicion, na.rm = TRUE),
        .groups = "drop"
        )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) 
    

  })
  
  output$snt_hc_contc <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>% 
      select(semana_fecha, tipo,  contacto) %>%
      group_by(semana_fecha, tipo) %>%
      summarise(
        proporcion = mean(100 * contacto, na.rm = TRUE), 
        cantidad = sum(contacto, na.rm = TRUE),
        .groups = "drop"
        ) 
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(
        title = list(text =  "Proporción de personas que declaran<br>un contacto estrecho con caso confirmado COVID19"),
        labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(
        text = "¿Ha tenido alguna forma de contacto con un paciente confirmado de enfermedad COVID19 en la última semana?"
      )
    
  })
  
  output$snt_hc_confm <- renderHighchart({
    
    d <- movid %>%
      group_by(semana_fecha) %>%
      summarise(
        # `Proporción caso confirmado COVID19` = mean(100 * coalesce(s10_exmn_confirmado, 0), na.rm = TRUE),
        `Proporción caso probable COVID19` = mean(100 * caso_probable2, na.rm = TRUE),
        # `Totales` =  mean(100 * (caso_probable2 | coalesce(s10_exmn_confirmado, 0)), na.rm = TRUE),
        .groups = "drop"
        ) %>% 
      gather(tipo, proporcion, -semana_fecha) %>% 
      arrange(semana_fecha)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo)
    ) %>%
      hc_tooltip(table = TRUE) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(
      text = "Proporción de personas con síntomas COVID-19 y antecedente de contacto estrecho con 
      caso confirmado"
      )
    
  })

# sistema salud -----------------------------------------------------------
  output$ssd_hc_cslta <- renderHighchart({
    
    dssd <- dssd()
  
    d <- dssd %>%
      filter(sintoma == 1) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * s2_consulta, na.rm = TRUE),
        cantidad = sum(s2_consulta, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = paste(
        "Porcentaje de personas sintomáticas que consulta",
        "a un profesional de salud por sus síntomas")
      )
    
  })
  output$ssd_hc_cslta_b <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      filter(sosp_minsal0530 == 1) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * s2_consulta, na.rm = TRUE),
        cantidad = sum(s2_consulta, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = " Porcentaje de personas que cumplen con definición 
                  vigente de casos sospechosos que consulta a un profesional 
                  de salud por sus síntomas")
    
  })
  
  output$ssd_hc_examn <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      filter(sosp_minsal0530 == 1) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * s7_exmn_realizado, na.rm = TRUE),
        cantidad = sum(s7_exmn_realizado, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(
        text = "Porcentaje de personas que cumplen definición de caso 
        sospechoso que se realizan una prueba diagnóstica de COVID-19"
        )
    
  })
  output$ssd_hc_examn_b <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      filter(sosp_minsal0530 == 1) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * s6_exmn_indicado, na.rm = TRUE),
        cantidad = sum(s6_exmn_indicado, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(
        text = "Porcentaje de personas con indicación médica de realizarse un
         test que se realizan una prueba diagnóstica de COVID-19"
      )
    
  })
  
  output$ssd_hc_posit <- renderHighchart({
    
    dssd <- dssd()
    
    # dssd %>% 
    #   count(semana_fecha, s7_exmn_realizado) %>% 
    #   spread(s7_exmn_realizado, n)
    
    d <- dssd %>%
      select(semana_fecha, tipo, s7_exmn_realizado) %>% 
      filter(!is.na(s7_exmn_realizado)) %>%
      group_by(semana_fecha, tipo) %>%
      summarise(
        proporcion = mean(s7_exmn_realizado, na.rm = TRUE) * 1000,
        cantidad = sum(s7_exmn_realizado, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), min = 0) %>%
      hc_subtitle(text = "Tasa de test diagnóstico SARS-CoV-2 por cada mil personas")
    
  })
  
  output$ssd_hc_posit2 <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      select(semana_fecha, tipo, s10_exmn_confirmado) %>%
      filter(!is.na(s10_exmn_confirmado)) %>%
      group_by(semana_fecha, tipo) %>%
      summarise(
        proporcion = mean(100 * s10_exmn_confirmado, na.rm = TRUE),
        cantidad = sum(s10_exmn_confirmado, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>%
      hc_subtitle(text = "Recibió resultado positivo en algún examen para
                  enfermedad COVID19 realizado durante la última semana.")
  })
  
  output$ssd_hc_ctads <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      filter(sintoma == 1, s2_consulta == 1) %>%
      select(semana_fecha, tipo, sintoma, s2_consulta, s4_consulta_dias) %>%
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(s4_consulta_dias, na.rm = TRUE),
        cantidad = n(),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0) %>% 
      hc_subtitle(text = "Promedio de días de espera entre inicio de síntomas y 
                  consulta al médico (en casos sintomáticos de deciden consultar)")
    
  })
  
  output$ssd_hc_exesp <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      select(semana_fecha, tipo, s7_exmn_realizado, s11_exmn_espera) %>% 
      # cambio 1
      filter(s7_exmn_realizado == 1) %>%
      filter(s11_exmn_espera > 0) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(s11_exmn_espera, na.rm = TRUE),
        cantidad = n(),
        .groups = "drop"
      )
    
    presentes <- d %>% count(tipo) %>% pull(tipo) %>% as.character()
    vsble <- attr(dssd, "visible")
    vsble2 <- vsble[which(presentes %in% names(vsble))]
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = vsble2
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0) %>% 
      hc_subtitle(text = "Promedio de días de espera entre toma y 
                  resultado del test diagnóstico ")
    
  })
  
  output$ssd_hc_ctaex <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>%
      select(semana_fecha, tipo, s4_consulta_dias, s11_exmn_espera, s7_exmn_realizado, sintoma) %>% 
      filter(sintoma == 1, s7_exmn_realizado == 1) %>% 
      filter(!is.na(s4_consulta_dias) | !is.na(s11_exmn_espera)) %>% 
      # cambio 1
      mutate(
        s4_consulta_dias = coalesce(s4_consulta_dias, 0),
        s4_consulta_dias = ifelse(s4_consulta_dias < 0, 0, s4_consulta_dias),
        s11_exmn_espera = coalesce(s11_exmn_espera, 0),
        s11_exmn_espera = ifelse(s11_exmn_espera < 0, 0, s11_exmn_espera)
        ) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(s4_consulta_dias + s11_exmn_espera, na.rm = TRUE),
        cantidad = n(),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>% 
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0) %>% 
      hc_subtitle(text = "Promedio de días de espera entre inicio de síntomas y 
                  resultado del test diagnóstico.")
    
    
  })
  
  output$ssd_hc_s3con <- renderHighchart({
    
    dssd <- dssd()
    
    dssd <- dssd %>%
      filter(sintoma == 1) %>% 
      select(all_of(str_c("s3_cons_", input$razones_opt)), everything()) %>% 
      rename_at(vars(1), ~ "variable")
    
    d <- dssd %>%
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * variable, na.rm = TRUE),
        cantidad = sum(variable, na.rm = TRUE),
        .groups = "drop"
      )
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) 
      # hc_title(text = "Razones para no consultar profesional")
    
  })
  
  output$ssd_hc_s8exm <- renderHighchart({
    
    dssd <- dssd()
  
    d <- dssd %>% 
      select(tipo, all_of(str_c("s8_exmn_", input$razones_opt2))) %>% 
      gather(cat, valor, -tipo) %>% 
      count(tipo, cat, valor) %>% 
      filter(!is.na(valor)) %>% 
      spread(valor, n) %>% 
      mutate(proporcion = `1` / (`1` + `0`), cantidad = (`1` + `0`))
    
    d <- d %>% 
      left_join(OPTS_RAZONES2_DF, by = c("cat" = "value")) %>% 
      rename(categoria = name)
    
    d <- d %>% 
      mutate(categoria = fct_reorder(categoria, cantidad, .fun = sum, .desc = TRUE)) %>% 
      arrange(tipo, categoria)
    
    hchart(
      d,
      "column",
      hcaes(categoria, proporcion, group = tipo),
      visible = attr(dssd, "visible")
      # stacking = "normal"
      ) %>% 
      hc_tooltip_n() %>% 
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) 
    
  })  



# sistema salud no covid y cronico ----------------------------------------

  output$ssd_hc_cslta_nocovid <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>% 
      group_by(pob_id) %>% 
      filter(semana_fecha == max(semana_fecha)) %>% 
      ungroup() %>% 
      filter(!is.na(nc1_problema_reg)) %>% 
      group_by(nc1_problema_reg, tipo) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(prop = round(n/sum(n), 4)* 100) %>% 
      rename(cantidad = n)
    
    d <- d %>%
      arrange(nc1_problema_reg) %>% 
      mutate(
        etiqueta = ifelse(
          nc1_problema_reg == 0,
          "Sin problema",
          "Nuevo problema"
        ),
        etiqueta = fct_inorder(etiqueta)
      )
    
    hchart(
      d,
      "column",
      hcaes(etiqueta, prop, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = paste(
        "Porcentaje de personas que presentan un nuevo problema de salud",
        "no relacionado con COVID-19, para la última semana")
      )
    
  })
  
  output$ssd_hc_cslta_b_nocovid <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>% 
      group_by(pob_id) %>% 
      filter(semana_fecha == max(semana_fecha), nc1_problema_reg == 1) %>% 
      ungroup() %>% 
      filter(!is.na(nc2_consulta_reg)) %>% 
      group_by(nc2_consulta_reg, tipo) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(prop = round(n/sum(n), 4)* 100) %>% 
      rename(cantidad = n)
    
    d <- d %>%
      arrange(nc2_consulta_reg) %>% 
      mutate(
        etiqueta = ifelse(
          nc2_consulta_reg == 1,
          "Consulta",
          "No Consulta"
        ),
        etiqueta = fct_inorder(etiqueta)
      )
    
    hchart(
      d,
      "column",
      hcaes(etiqueta, prop, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = paste(
        "Porcentaje de personas que consultan",
        "por ese nuevo problema de salud, para la última semana")
      )
    
  })
  
  output$ssd_hc_nc3_pq_nocovid <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>% 
      select(tipo, all_of(str_c("nc3_posp_", input$razones_opt4, "_reg"))) %>% 
      gather(cat, valor, -tipo) %>% 
      count(tipo, cat, valor) %>% 
      filter(!is.na(valor)) %>% 
      spread(valor, n) %>% 
      mutate(proporcion = `1` / (`1` + `0`), cantidad = (`1` + `0`))
    
    d <- d %>% 
      left_join(OPTS_RAZONES4_DF, by = c("cat" = "value")) %>% 
      rename(categoria = name)
    
    d <- d %>% 
      mutate(categoria = fct_reorder(categoria, cantidad, .fun = sum, .desc = TRUE)) %>% 
      arrange(tipo, categoria)
    
    hchart(
      d,
      "column",
      hcaes(categoria, proporcion, group = tipo),
      visible = attr(dssd, "visible")
      # stacking = "normal"
    ) %>% 
      hc_tooltip_n() %>% 
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) 
    
  })  
  

# cronicos ----------------------------------------------------------------

  output$ssd_hc_cslta_cronico <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>% 
      group_by(pob_id) %>% 
      filter(semana_fecha == max(semana_fecha)) %>% 
      ungroup() %>% 
      filter(!is.na(crn1_consulta_reg)) %>% 
      group_by(crn1_consulta_reg, tipo) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(prop = round(n/sum(n), 4)* 100) %>% 
      rename(cantidad = n)
    
    d <- d %>%
      arrange(crn1_consulta_reg) %>% 
      mutate(
        etiqueta = ifelse(
          crn1_consulta_reg == 0,
          "No consulta",
          "Consulta"
        ),
        etiqueta = fct_inorder(etiqueta)
      )
    
    hchart(
      d,
      "column",
      hcaes(etiqueta, prop, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = paste(
        "Porcentaje de personas que no han asistido a una consulta",
        "para controlar su enfermedad crónica")
      )
    
    
  })
  
  output$ssd_hc_cslta_cronico_posponer <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>% 
      group_by(pob_id) %>% 
      filter(semana_fecha == max(semana_fecha)) %>% 
      ungroup() %>% 
      filter(!is.na(crn2_posponer_reg)) %>% 
      group_by(crn2_posponer_reg, tipo) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(prop = round(n/sum(n), 4)* 100) %>% 
      rename(cantidad = n)
    
    d <- d %>%
      arrange(crn2_posponer_reg) %>% 
      mutate(
        etiqueta = ifelse(
          crn2_posponer_reg == 0,
          "No pospone",
          "Pospone"
        ),
        etiqueta = fct_inorder(etiqueta)
      )
    
    hchart(
      d,
      "column",
      hcaes(etiqueta, prop, group = tipo),
      visible = attr(dssd, "visible")
    ) %>%
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = paste(
        "Porcentaje de personas que posponen una consulta",
        "para controlar su enfermedad crónica")
      )
    
  })
  
  output$ssd_hc_crn3_pq_cronico <- renderHighchart({
    
    dssd <- dssd()
    
    d <- dssd %>% 
      select(tipo, all_of(str_c("crn3_pq_", input$razones_opt3, "_reg"))) %>% 
      gather(cat, valor, -tipo) %>% 
      count(tipo, cat, valor) %>% 
      filter(!is.na(valor)) %>% 
      spread(valor, n) %>% 
      mutate(proporcion = `1` / (`1` + `0`), cantidad = (`1` + `0`))
    
    d <- d %>% 
      left_join(OPTS_RAZONES3_DF, by = c("cat" = "value")) %>% 
      rename(categoria = name)
    
    d <- d %>% 
      mutate(categoria = fct_reorder(categoria, cantidad, .fun = sum, .desc = TRUE)) %>% 
      arrange(tipo, categoria)
    
    hchart(
      d,
      "column",
      hcaes(categoria, proporcion, group = tipo),
      visible = attr(dssd, "visible")
      # stacking = "normal"
    ) %>% 
      hc_tooltip_n() %>% 
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) 
    
  })  
  
# prácticas sociales ------------------------------------------------------

  output$pcsoc_frec_salida <- renderHighchart({
    
    d <- movid %>% 
      select(semana_fecha, contains("p1_pra"), -contains("p1_pra_otro_TEXT"),
             -contains("p1_pra_otro"), -p1_pra_invitado, -p1_pra_transporte) %>% 
      gather(tipo, valor, -semana_fecha) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        promedio = mean(valor, na.rm = TRUE),
        cantidad = n(),
        .groups = "drop"
        ) %>% 
      left_join(PRACTICAS_DF, by = "tipo") %>% 
      arrange(semana_fecha, tipo_lbl)
  
    hchart(
      d,
      "line",
      hcaes(semana_fecha, promedio, group = tipo_lbl)
    ) %>%
      hc_tooltip_n(separado = FALSE) %>%
      hc_tooltip(sort = TRUE) %>% 
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0)  
    
  })  
  
  output$pcsoc_prop2 <- renderHighchart({
    
    d <- movid %>% 
      select(semana_fecha, contains("p1_pra"), -contains("p1_pra_otro_TEXT"),
             -contains("p1_pra_otro"), -p1_pra_invitado, -p1_pra_transporte) %>% 
      gather(tipo, valor, -semana_fecha) %>%
      mutate(valor = valor >= 2) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * valor, na.rm = TRUE), 
        cantidad = n(),
        .groups = "drop"
        ) %>% 
      left_join(PRACTICAS_DF, by = "tipo") %>% 
      arrange(semana_fecha, tipo_lbl)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_tooltip_n(separado = FALSE) %>%
      hc_tooltip(sort = TRUE) %>% 
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)  
    
  }) 
  
  output$pcsoc_nosalen <- renderHighchart({
    
    d <- movid %>% 
      select(semana_fecha, contains("p1_pra"), -contains("p1_pra_otro_TEXT"),
             -contains("p1_pra_otro"), -p1_pra_invitado, -p1_pra_transporte) %>% 
      gather(tipo, valor, -semana_fecha) %>%
      mutate(valor = valor == 0) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = mean(100 * valor, na.rm = TRUE),
        cantidad = n(),
        .groups = "drop"
        ) %>% 
      left_join(PRACTICAS_DF, by = "tipo") %>% 
      arrange(semana_fecha, tipo_lbl)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_tooltip_n(separado = FALSE) %>%
      hc_tooltip(sort = TRUE) %>% 
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100)  
    
  })  
  
  output$pcsoc_transporte <- renderHighchart({
    
    d <- movid %>% 
      select(semana_fecha, contains("p3_tra"), -p3_transp_otra_TEXT) %>% 
      filter(complete.cases(.)) %>% 
      mutate_if(is.numeric, ~ ifelse(.x > 0, 1, 0)) %>% gather(tipo, valor, -semana_fecha) %>% 
      group_by(semana_fecha, tipo) %>% 
      summarise(
        proporcion = 100 * mean(valor, na.rm = TRUE),
        cantidad = n(),
        .groups = "drop"
      ) %>% 
      left_join(TRANSPORTE_DF, by = "tipo") %>% 
      arrange(semana_fecha, tipo_lbl)
    
    hchart(
      d,
      "line",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_tooltip_n(separado = FALSE) %>%
      hc_tooltip(sort = TRUE) %>% 
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)  
    
  })
  
# percepcion de riesgo ----------------------------------------------------

  output$persgo_alto <- renderHighchart({
    
    d <- movid %>% 
      count(semana_fecha, tipo_lbl = cr1_per_riesgo) %>% 
      filter(!is.na(tipo_lbl)) %>%
      filter(tipo_lbl != "") %>% 
      group_by(semana_fecha) %>% 
      mutate(
        proporcion = round(100 * n/sum(n), 2),
        tipo_lbl = factor(
          tipo_lbl,
          levels = c("Muy de acuerdo",
                     "De acuerdo",
                     "Ni de acuerdo ni en desacuerdo",
                     "En desacuerdo", 
                     "Muy en desacuerdo"
                     )
          )
        ) %>% 
      rename(cantidad = n)
    
    
    hchart(
      d,
      "column",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_colors(c("#093C66", "#00668D", "#00B994", "#8ADD7E", "#F9F871")) %>% 
      hc_plotOptions(
        series = list(
          stacking = "percent", 
          borderWidth = 0,
          dataLabels = list(enabled = TRUE)
          )
        ) %>% 
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100)  
    
    
  })
  
  output$pergdo_cump <- renderHighchart({
    
    d <- movid %>% 
      count(semana_fecha, tipo_lbl = cr2_normas) %>% 
      filter(!is.na(tipo_lbl)) %>%
      filter(tipo_lbl != "") %>% 
      group_by(semana_fecha) %>% 
      mutate(
        proporcion = round(100 * n/sum(n), 2),
        tipo_lbl = factor(
          tipo_lbl,
          levels = c("Completamente",
                     "En gran medida",
                     "Bastante",
                     "Algo", 
                     "Poco",
                     "Nada"
          )
        )
      ) %>% 
      rename(cantidad = n)
    
    
    hchart(
      d,
      "column",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_colors(c("#093C66", "#00668D", "#00919D", "#00B994", "#8ADD7E", "#F9F871")) %>% 
      hc_plotOptions(
        series = list(
          stacking = "percent", 
          borderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )
      ) %>% 
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100)
    
    
  })

# percepcion de legitimidad ----------------------------------------------------
  
  output$peleg_bienestar <- renderHighchart({
    
    d <- dsoc %>% 
      count(semana_fecha, tipo_lbl = soc1_bienestar) %>% 
      filter(!is.na(tipo_lbl)) %>%
      filter(tipo_lbl != "") %>% 
      group_by(semana_fecha) %>% 
      mutate(
        proporcion = round(100 * n/sum(n), 2),
        tipo_lbl = factor(
          tipo_lbl,
          levels = c("Muy de acuerdo",
                     "De acuerdo",
                     "Ni de acuerdo ni en desacuerdo",
                     "En desacuerdo", 
                     "Muy en desacuerdo"
          )
        )
      ) %>% 
      rename(cantidad = n)
    
    
    hchart(
      d,
      "column",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_colors(c("#093C66", "#00668D", "#00B994", "#8ADD7E", "#F9F871")) %>% 
      hc_plotOptions(
        series = list(
          stacking = "percent", 
          borderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )
      ) %>% 
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100)  
    
    
  })
  output$peleg_obedecer <- renderHighchart({
    
    d <- dsoc %>% 
      count(semana_fecha, tipo_lbl = soc2_obedecer) %>% 
      filter(!is.na(tipo_lbl)) %>%
      filter(tipo_lbl != "") %>% 
      group_by(semana_fecha) %>% 
      mutate(
        proporcion = round(100 * n/sum(n), 2),
        tipo_lbl = factor(
          tipo_lbl,
          levels = c("Muy de acuerdo",
                     "De acuerdo",
                     "Ni de acuerdo ni en desacuerdo",
                     "En desacuerdo", 
                     "Muy en desacuerdo"
          )
        )
      ) %>% 
      rename(cantidad = n)
    
    
    hchart(
      d,
      "column",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_colors(c("#093C66", "#00668D", "#00B994", "#8ADD7E", "#F9F871")) %>% 
      hc_plotOptions(
        series = list(
          stacking = "percent", 
          borderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )
      ) %>% 
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100)  
    
    
  })
  
  output$peleg_desigualdad <- renderHighchart({
    
    d <- dsoc %>% 
      count(semana_fecha, tipo_lbl = soc3_desigualdad) %>% 
      filter(!is.na(tipo_lbl)) %>%
      filter(tipo_lbl != "") %>% 
      group_by(semana_fecha) %>% 
      mutate(
        proporcion = round(100 * n/sum(n), 2),
        tipo_lbl = factor(
          tipo_lbl,
          levels = c("Muy de acuerdo",
                     "De acuerdo",
                     "Ni de acuerdo ni en desacuerdo",
                     "En desacuerdo", 
                     "Muy en desacuerdo"
          )
        )
      ) %>% 
      rename(cantidad = n)
    
    
    hchart(
      d,
      "column",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_colors(c("#093C66", "#00668D", "#00B994", "#8ADD7E", "#F9F871")) %>% 
      hc_plotOptions(
        series = list(
          stacking = "percent", 
          borderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )
      ) %>% 
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100)  
    
    
  })
  
  output$peleg_represion <- renderHighchart({
    
    d <- dsoc %>% 
      count(semana_fecha, tipo_lbl = soc4_represion) %>% 
      filter(!is.na(tipo_lbl)) %>%
      filter(tipo_lbl != "") %>% 
      group_by(semana_fecha) %>% 
      mutate(
        proporcion = round(100 * n/sum(n), 2),
        tipo_lbl = factor(
          tipo_lbl,
          levels = c("Muy de acuerdo",
                     "De acuerdo",
                     "Ni de acuerdo ni en desacuerdo",
                     "En desacuerdo", 
                     "Muy en desacuerdo"
          )
        )
      ) %>% 
      rename(cantidad = n)
    
    
    hchart(
      d,
      "column",
      hcaes(semana_fecha, proporcion, group = tipo_lbl)
    ) %>%
      hc_colors(c("#093C66", "#00668D", "#00B994", "#8ADD7E", "#F9F871")) %>% 
      hc_plotOptions(
        series = list(
          stacking = "percent", 
          borderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )
      ) %>% 
      hc_tooltip_n() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100)  
    
    
  })
  
# participantes -----------------------------------------------------------
  
  output$part_sexo <- renderHighchart({
    
    hc_demografica("sexo")
    
  })
  
  output$part_edad <- renderHighchart({
    
    hc_demografica("edad_3cat")
    
  })
  
  output$part_prev <- renderHighchart({
    
    hc_demografica("prev")
    
  })
  
  output$part_ocup <- renderHighchart({
    
    hc_demografica("pr3_ocupacion")
    
  })
  
  output$part_educ <- renderHighchart({
    
    hc_demografica("educ_3cat")
    
  })
  
  output$part_region <- renderHighchart({
    
    hc_demografica("region") 
    
  })
  
  output$part_respuestas <- renderHighchart({
    
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
    
    hc
    
  })
  
        
})