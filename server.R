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
           yaxis = list(range = c(0, max(ind_occ_data$upper)+5),
                        title = 'Occupancy index'))
  
  barplot <- spp_occ_data %>%
    plot_ly(x = ~time_period, y = ~proportion_species,
                    type = "bar", showlegend=FALSE,
                    color = ~category) %>% 
    layout(barmode = "stack",
                   xaxis = list(title = '', tickvals = c('st', 'lt'),
                                ticktext = c('Short term', 'Long term')),
                   yaxis = list(title = 'Proportion of species',
                                hoverformat = '.2%',
                                zeroline = FALSE,
                                showline = FALSE,
                                showticklabels = FALSE,
                                showgrid = FALSE))  
  
 subplot(line_plot,barplot, widths = c(0.7, 0.3)) |> 
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
  
  
  line_plot <- ind_abnd_data %>%
    plot_ly(x = ~year, y = ~upper*100, type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Upper CI') %>%
    add_trace(y = ~lower*100, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
              line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Lower CI') %>%
    add_trace(y = ~indicator*100, type = 'scatter', mode = 'lines',
              line = list(color='rgba(0,100,80)'),
              name = 'Indicator') %>%
    add_trace(y = ~indicator_unsm*100, type = 'scatter', mode = 'markers',
              marker = list(color='rgb(0,100,80)'),
              name = 'Indicator unsmoothed') %>%
    layout(xaxis = list(title = 'Year'),
           yaxis = list(range = c(0, max(ind_abnd_data$indicator_unsm*100)+5),
                        title = 'Abundance index'))
  
  barplot <- spp_abnd_data %>%
    plot_ly(x = ~period, y = ~cat_prop/100,
            type = "bar", showlegend=FALSE,
            color = ~cat_chg) %>% 
    layout(barmode = "stack",
           xaxis = list(title = '', tickvals = c('short-term', 'long-term'),
                        ticktext = c('Short term', 'Long term')),
           yaxis = list(title = 'Proportion of species',
                        hoverformat = '.2%',
                        zeroline = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        showgrid = FALSE))   
  
  subplot(line_plot,barplot, widths = c(0.7, 0.3)) |> 
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