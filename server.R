function(input, output, session){
  thematic::thematic_shiny()
  
observeEvent(input$selected_occ_group,
             {  updateSelectInput(session,
                    "selected_occ_country",
                    choices = {
                      all_ind %>%
                        filter(group == input$selected_occ_group) %>%
                        pull(country)
                    })
  })

ind_occ_data <- eventReactive(c(#input$selected_metric,
                            input$selected_occ_group,
                            input$selected_occ_country),
                          {
                            all_ind %>%
                              # filter(metric == isolate(input$selected_metric)) %>%
                              filter(group == isolate(input$selected_occ_group)) %>%
                              filter(country == isolate(input$selected_occ_country))
                          })

spp_occ_data <- eventReactive(c(# input$selected_metric,
                            input$selected_occ_group,
                            input$selected_occ_country),
                          {
                            cat_data %>%
                              filter(group == isolate(input$selected_occ_group)) %>%
                              filter(country == isolate(input$selected_occ_country)) %>%
                              group_by(time_period, category) %>%
                              summarise(n_species = n()) %>%
                              mutate(proportion_species = n_species / sum(n_species)) %>%
                              ungroup()
                          })

ind_occ_plot <- reactive({
 
   input$plot_occ  
  # we add this so that the plot is only generated when the action button is pressed
  
  if(input$plot_occ == 0){
    return()
  }
  
  ind_occ_data <- isolate(ind_occ_data())
  spp_occ_data <- isolate(spp_occ_data())

 
  
  line_plot <- ind_occ_data %>%
    plot_ly(x = ~year) %>%
    add_ribbons(ymin = ~lower, ymax = ~upper, 
                line = list(color = 'transparent'),
                fillcolor = 'rgba(0,100,80,0.2)', 
                name = '95% CI', hoverinfo = 'none',
                showlegend = TRUE) %>%
    add_lines(y = ~lower, line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Lower CI') %>%
    add_lines(y = ~upper, line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Upper CI') %>%
    add_trace(y = ~indicator, type = 'scatter', mode = 'lines',
              line = list(color='rgb(0,100,80)'),
              showlegend = TRUE, name = 'Indicator') %>%
    layout(xaxis = list(title = 'Year'), 
           yaxis = list(range = c(0, max(ind_occ_data$upper)+5),
                        title = 'Occupancy index',
                        hoverformat = ".2f"))
  
  
  barplot <- spp_occ_data %>%
    plot_ly(x = ~time_period, y = ~proportion_species,
                    type = "bar", showlegend=TRUE,
                    color = ~category) %>% 
    layout(barmode = "stack",
           xaxis = list(title = '', tickvals = c('st', 'lt'),
                        ticktext = c('Short term', 'Long term')),
           yaxis = list(title = 'Proportion of species',
                        hoverformat = '.0%',
                        tickvals = list(0, 0.25, 0.5, 0.75, 1),
                        tickformat = '.0%'))  
  
 subplot(line_plot,barplot, widths = c(0.7, 0.3),
         margin = 0.05, shareX = FALSE, shareY = FALSE,
         titleX = TRUE, titleY = TRUE) |> 
    layout(title = "")

})
  
output$indicator_occ_plot <- renderPlotly({
  
  print(ind_occ_plot())
  
  })


output$download_occ_data <- downloadHandler(
  filename = function() {
    paste0(paste("Occupancy",
                 input$selected_occ_group,
                 input$selected_occ_country,
                 sep = "_"), ".xlsx")
  },
  content = function(file) {
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Indicator")
    writeData(wb, ind_occ_data(), sheet = "Indicator")
    addWorksheet(wb, sheetName = "Species")
    writeData(wb, spp_occ_data(), sheet = "Species")
    
    saveWorkbook(wb, file = file)
    # write.csv(ind_data(), file)
  },
  contentType = "file/xlsx"
)

# abundance tab

ind_abnd_data <- eventReactive(input$selected_abnd_country,
  {
    all_ind_abnd %>%
      filter(country == isolate(input$selected_abnd_country))
  })

spp_abnd_data <- eventReactive(input$selected_occ_country,
  {
    abnd_cat_data %>%
      filter(country == isolate(input$selected_occ_country)) 
  })

ind_abnd_plot <- reactive({
  
  input$plot_abnd  
  # we add this so that the plot is only generated when the action button is pressed
  
  if(input$plot_abnd == 0){
    return()
  }
  
  ind_abnd_data <- isolate(ind_abnd_data())
  spp_abnd_data <- isolate(spp_abnd_data())
  
  
  # line_plot <- ind_abnd_data %>%
  #   plot_ly(x = ~year, y = ~upper*100, type = 'scatter', mode = 'lines',
  #           line = list(color = 'transparent'),
  #           showlegend = FALSE, name = 'Upper CI') %>%
  #   add_trace(y = ~lower*100, type = 'scatter', mode = 'lines',
  #             fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
  #             line = list(color = 'transparent'),
  #             showlegend = FALSE, name = 'Lower CI') %>%
  #   add_trace(y = ~indicator*100, type = 'scatter', mode = 'lines',
  #             line = list(color='rgba(0,100,80)'),
  #             name = 'Indicator') %>%
  #   add_trace(y = ~indicator_unsm*100, type = 'scatter', mode = 'markers',
  #             marker = list(color='rgb(0,100,80)'),
  #             name = 'Indicator unsmoothed') %>%
  #   layout(xaxis = list(title = 'Year'),
  #          yaxis = list(range = c(0, max(ind_abnd_data$indicator_unsm*100)+5),
  #                       title = 'Abundance index',
  #                       hoverformat = '.2f'))
  
  
  line_plot <- ind_abnd_data %>%
    plot_ly(x = ~year) %>%
    add_ribbons(ymin = ~lower*100, ymax = ~upper*100, 
                line = list(color = 'transparent'),
                fillcolor = 'rgba(0,100,80,0.2)', 
                name = '95% CI', hoverinfo = 'none',
                showlegend = TRUE) %>%
    add_lines(y = ~lower*100, line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Lower CI') %>%
    add_lines(y = ~upper*100, line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Upper CI') %>%
    add_trace(y = ~indicator*100, type = 'scatter', mode = 'lines',
              line = list(color='rgb(0,100,80)'),
              showlegend = TRUE, name = 'Indicator') %>%
      add_trace(y = ~indicator_unsm*100, type = 'scatter', mode = 'markers',
                marker = list(color='rgb(0,100,80)'),
                name = 'Indicator unsmoothed', showlegend = TRUE) %>%
      layout(xaxis = list(title = 'Year'),
             yaxis = list(range = c(0, max(ind_abnd_data$indicator_unsm*100)+5),
                          title = 'Abundance index',
                          hoverformat = '.2f'))
    
  barplot <- spp_abnd_data %>%
    plot_ly(x = ~period, y = ~cat_prop/100,
            type = "bar", showlegend=TRUE,
            color = ~cat_chg) %>% 
    layout(barmode = "stack",
           xaxis = list(title = '', tickvals = c('short-term', 'long-term'),
                        ticktext = c('Short term', 'Long term')),
           yaxis = list(title = 'Proportion of species',
                        position = "right",
                        hoverformat = '.0%',
                        tickvals = list(0, 0.25, 0.5, 0.75, 1),
                        tickformat = '.0%'))  
  
  subplot(line_plot,barplot, widths = c(0.7, 0.3),
          margin = 0.05, shareX = FALSE, shareY = FALSE,
          titleX = TRUE, titleY = TRUE) |> 
    layout(title = "")
  
})

output$indicator_abnd_plot <- renderPlotly({
  
  print(ind_abnd_plot())
  
})


output$download_abnd_data <- downloadHandler(
  filename = function() {
    paste0(paste("Abundance_all_species",
                 input$selected_occ_country,
                 sep = "_"), ".xlsx")
  },
  content = function(file) {
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Indicator")
    writeData(wb, ind_abnd_data(), sheet = "Indicator")
    addWorksheet(wb, sheetName = "Species")
    writeData(wb, spp_abnd_data(), sheet = "Species")
    
    saveWorkbook(wb, file = file)
    # write.csv(ind_data(), file)
  },
  contentType = "file/xlsx"
)



output$interpretation_plot <- renderImage({
  list(
    src = "Data/interpretation_plot.png",
    contentType = "image/png",
    width = 500,
    height = 350
  )
}, deleteFile = FALSE)


}