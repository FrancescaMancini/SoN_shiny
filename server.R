function(input, output, session){
  thematic::thematic_shiny()
  
observeEvent(input$selected_metric,
             {  updateSelectInput(session,
                    "selected_group",
                    choices = {
                      all_ind %>%
                        filter(metric == input$selected_metric) %>%
                        pull(group)
                    })
  })
observeEvent(input$selected_group,
             {  updateSelectInput(session,
                    "selected_country",
                    choices = {
                      all_ind %>%
                        filter(metric == input$selected_metric & 
                                 group == input$selected_group) %>%
                        pull(country)
                    })
  })

ind_data <- eventReactive(c(input$selected_metric,
                            input$selected_group,
                            input$selected_country),
                          {
                            all_ind %>%
                              filter(metric == isolate(input$selected_metric)) %>%
                              filter(group == isolate(input$selected_group)) %>%
                              filter(country == isolate(input$selected_country))
                          })

ind_plot <- reactive({
 
   input$plot  
  # we add this so that the plot is only generated when the action button is pressed
  
  if(input$plot == 0){
    return()
  }
  
  ind_data <- isolate(ind_data())
  

  ind_data%>%
  plot_ly(x = ~year, y = ~upper, type = 'scatter', mode = 'lines',
          line = list(color = 'transparent'),
          showlegend = FALSE, name = 'Upper CI') %>%
    add_trace(y = ~lower, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', 
              line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Lower CI') %>%
    add_trace(y = ~indicator, type = 'scatter', mode = 'lines',
              line = list(color='rgba(0,100,80)'),
              name = 'Indicator') %>%
    layout(xaxis = list(title = 'Year'), 
           yaxis = list(range = c(0, max(ind_data$upper)+5),
                        title = 'Occupancy index'))
  

})
  
output$indicator_plot <- renderPlotly({
  
  print(ind_plot())
  
  })


output$download_data <- downloadHandler(
  filename = function() {
    paste0(paste(input$selected_metric,
                 input$selected_group,
                 input$selected_country,
                 sep = "_"), ".csv")
  },
  content = function(file) {
    write.csv(ind_data(), file)
  }
)



output$interpretation_plot <- renderPlot({
  
  ggplot(data = all_ind %>%
           filter(metric == "Occupancy") %>%
           filter(group == "All invertebrates") %>%
           filter(country == "UK"), 
         aes(x = year, y = indicator)) + 
    geom_line(colour = "black") +
    geom_ribbon(
      aes(x = year, ymax = upper, ymin = lower),
      fill = "black", alpha = 0.3) +
    xlab("Year") +
    ylab("Occupancy index") +
    theme_minimal() +
    theme(text = element_text(size = 20))
  
  
})


}