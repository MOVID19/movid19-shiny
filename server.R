input <- list(snt_opt = c("snt_tos", "snt_odinofagia"), ssd_opt = "todo")
input <- list(snt_opt = c("snt_tos", "snt_odinofagia"), ssd_opt = "sexo")

shinyServer(function(input, output, session) {
  
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
    
    l[["numero_participantes"]] <- 12345
    
    l
      
  }
  
  cifras_actuales <- cifras()
  
# inicio ------------------------------------------------------------------
  
  output$vb_casos <- renderValueBox({
    valueBox(cifras_actuales$casos_totales, "Casos", icon = "lungs-virus")
  })
  output$vb_fallc <- renderValueBox({
    valueBox(cifras_actuales$fallecidos_totales, "Fallecidos", icon = "bookmark")
  })
  output$vb_resps <- renderValueBox({
    valueBox(cifras_actuales$respuestas, "Respuestas", icon = "list")
  })
  output$vb_partc <- renderValueBox({
    valueBox(cifras_actuales$numero_participantes, "Participantes", icon = "users")
  })
  
  output$inc_respuestas <- renderHighchart({
    
    movid %>%
      count(semana_fecha) %>% 
      hchart(
        "line",
        hcaes(semana_fecha, n),
        name = "Cantidad de respuestas"
      ) %>%
      hc_tooltip(shared = TRUE, table = TRUE) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""), min = 0)
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
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)
    
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
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)
    
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
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)
    
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
        s11_exmn_espera
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
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)
    
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
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)
    
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
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0)
    
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
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0)
    
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
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0)
    
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
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0)
    
  })
  
        
})