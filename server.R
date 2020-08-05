shinyServer(function(input, output, session) {
    
    output$chart <- renderHighchart({
      
      glimpse(movid)
      
      movid %>%
        count(sexo, prev) %>% 
        mutate(
          sexo = coalesce(sexo, "Otro"),
          prev = coalesce(prev, "Otro"),
          prev = ifelse(str_detect(prev, "Otra"), "Otro", prev),
          prev = factor(prev),
          prev = fct_reorder(prev, -n)
        ) %>% 
        count(sexo, prev, wt = n) %>%
        hchart("column", hcaes(sexo, n, group = prev)) %>% 
        hc_tooltip(shared = TRUE, table = TRUE) %>% 
        hc_title(text = "Previsión según sexo") %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_credits(enabled = TRUE, text = "MOVID19")
    
    })
    
    output$chart2 <- renderHighchart({
      
      movid %>%
        count(semana_fecha, prev) %>% 
        mutate(
          prev = coalesce(prev, "Otro"),
          prev = ifelse(str_detect(prev, "ISAPRE|FONASA"), prev, "Otro"),
          prev = factor(prev),
          prev = fct_reorder(prev, -n)
          ) %>% 
        count(semana_fecha, prev, wt = n) %>%
        hchart("line", hcaes(semana_fecha, n, group = prev)) %>% 
        hc_tooltip(shared = TRUE, table = TRUE) %>% 
        hc_title(text = "Previsión según encuestra")
    })
    
})
                                                