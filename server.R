shinyServer(function(input, output, session) {
    
    output$chart <- renderHighchart({
        
        highcharts_demo() %>%
            hc_tooltip(shared = TRUE, table = TRUE) %>%
            hc_colors(ggsci::pal_jama()(3)) 

      
    })
    
})
