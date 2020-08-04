shinyServer(function(input, output, session) {
    
    output$chart <- renderHighchart({
      
      highcharts_demo() %>% 
        hc_tooltip(shared = TRUE, table = TRUE) 
    
    })
    
})
