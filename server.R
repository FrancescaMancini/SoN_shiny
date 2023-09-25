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
  
  
  ind_occ_data <- isolate(ind_occ_data())
  spp_occ_data <- isolate(spp_occ_data())
  
  
  
  line_plot <- ind_occ_data %>%
    plot_ly(x = ~year) %>%
    add_ribbons(ymin = ~lower, ymax = ~upper, 
                line = list(color = 'transparent'),
                fillcolor = 'rgba(0,100,80,0.2)', 
                name = 'Uncertainty interval (UI)', hoverinfo = 'none',
                showlegend = TRUE) %>%
    add_lines(y = ~lower, line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Lower UI') %>%
    add_lines(y = ~upper, line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Upper UI') %>%
    add_trace(y = ~indicator, type = 'scatter', mode = 'lines',
              line = list(color='rgb(0,100,80)'),
              showlegend = TRUE, name = 'Indicator') %>%
    layout(xaxis = list(title = 'Year'), 
           yaxis = list(range = c(0, max(ind_occ_data$upper)+5),
                        title = 'Distribution index',
                        hoverformat = ".2f"))
  
  if(input$selected_occ_group == "Benthic") {
    return(line_plot)
  }
  
  barplot <- spp_occ_data %>%
    plot_ly(x = ~time_period, y = ~proportion_species,
            type = "bar", showlegend=TRUE,
            color = ~category) %>% 
    layout(barmode = "stack",
           xaxis = list(title = '', tickvals = c('st', 'lt'),
                        ticktext = c('Short term', 'Long term')),
           yaxis = list(title = 'Percentage of species',
                        hoverformat = '.0%',
                        tickvals = list(0, 0.25, 0.5, 0.75, 1),
                        tickformat = '.0%'))  
  
  subplot(line_plot,barplot, widths = c(0.7, 0.3),
          margin = 0.05, shareX = FALSE, shareY = FALSE,
          titleX = TRUE, titleY = TRUE) |> 
    layout(title = "")
  
})

output$indicator_occ_plot <- renderPlotly({
  
  input$plot_occ  
  # we add this so that the plot is only generated when the action button is pressed
  
  if(input$plot_occ == 0){
    return()
  }
  
  isolate(print(ind_occ_plot()))
  
  })


output$download_occ_data <- downloadHandler(
  filename = function() {
    paste0(paste("Distribution",
                 input$selected_occ_group,
                 input$selected_occ_country,
                 sep = "_"), ".xlsx")
  },
  content = function(file) {
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Metadata")
    writeData(wb, occ_meta, sheet = "Metadata")
    writeData(wb, 
              "The data shared here is provided on the following conditions,",
              sheet = "Metadata",
              startCol = 1,startRow = 16)
    writeData(wb, 
              "1. The graphs and data downloaded will be used solely to represent UK and UK country multi taxa species abundance indicators, and UK and UK country occupancy indicators.",
              sheet = "Metadata",
              startCol = 1, startRow = 17)
    writeData(wb, 
              "2. The graphs and data remain the intellectual property of the providers and should be cited as: Burns, F, Mordue, S, al Fulaij, N, Boersch-Supan, PH, Boswell, J, Boyd, RJ, Bradfer-Lawrence, T, de Ornellas, P, de Palma, A, de Zylva, P, Dennis, EB, Foster, S, Gilbert, G, Halliwell, L, Hawkins, K, Haysom, KA, Holland, MM, Hughes, J, Jackson, AC, Mancini, F, Mathews, F, McQuatters-Gollop, A, Noble, DG, O’Brien, D, Pescott, OL, Purvis, A, Simkin, J, Smith, A, Stanbury, AJ, Villemot, J, Walker, KJ, Walton, P, Webb, TJ, Williams, J, Wilson, R, Gregory, RD, 2023. State of Nature 2023, the State of Nature partnership, Available at: www.stateofnature.org.uk",
              sheet = "Metadata",
              startCol = 1, startRow = 18)
    writeData(wb, 
              "Copywrite of data and or/information presented here does not reside solely with the distributor (NBN). Please contact the State of Nature Partnership stateofnature@rspb.org.uk for copywrite requests.",
              sheet = "Metadata",
              startCol = 1, startRow = 19)
    writeData(wb, 
              "Data sources and references are available in the associated metadata file available for download from the Interpretation tab.",
              sheet = "Metadata",
              startCol = 1, startRow = 20)
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

spp_abnd_data <- eventReactive(input$selected_abnd_country,
  {
    abnd_cat_data %>%
      filter(country == isolate(input$selected_abnd_country)) 
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
    plot_ly(x = ~year) %>%
    add_ribbons(ymin = ~lower, ymax = ~upper, 
                line = list(color = 'transparent'),
                fillcolor = 'rgba(0,100,80,0.2)', 
                name = 'Uncertainty interval (UI)', hoverinfo = 'none',
                showlegend = TRUE) %>%
    add_lines(y = ~lower, line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Lower UI') %>%
    add_lines(y = ~upper, line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Upper UI') %>%
    add_trace(y = ~indicator, type = 'scatter', mode = 'lines',
              line = list(color='rgb(0,100,80)'),
              showlegend = TRUE, name = 'Indicator') %>%
      add_trace(y = ~indicator_unsm, type = 'scatter', mode = 'markers',
                marker = list(color='rgb(0,100,80)'),
                name = 'Indicator unsmoothed', showlegend = TRUE) %>%
      layout(xaxis = list(title = 'Year'),
             yaxis = list(range = c(0, max(ind_abnd_data$indicator_unsm)+5),
                          title = 'Abundance index',
                          hoverformat = '.2f'))
    
  barplot <- spp_abnd_data %>%
    plot_ly(x = ~period, y = ~cat_prop/100,
            type = "bar", showlegend=TRUE,
            color = ~cat_chg) %>% 
    layout(barmode = "stack",
           xaxis = list(title = '', tickvals = c('short-term', 'long-term'),
                        ticktext = c('Short term', 'Long term')),
           yaxis = list(title = 'Percentage of species',
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
                 input$selected_abnd_country,
                 sep = "_"), ".xlsx")
  },
  content = function(file) {
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Metadata")
    writeData(wb, abnd_meta, sheet = "Metadata")
    writeData(wb, 
              "The data shared here is provided on the following conditions,",
              sheet = "Metadata",
              startCol = 1,startRow = 16)
    writeData(wb, 
              "1. The graphs and data downloaded will be used solely to represent UK and UK country multi taxa species abundance indicators, and UK and UK country occupancy indicators.",
              sheet = "Metadata",
              startCol = 1, startRow = 17)
    writeData(wb, 
              "2. The graphs and data remain the intellectual property of the providers and should be cited as: Burns, F, Mordue, S, al Fulaij, N, Boersch-Supan, PH, Boswell, J, Boyd, RJ, Bradfer-Lawrence, T, de Ornellas, P, de Palma, A, de Zylva, P, Dennis, EB, Foster, S, Gilbert, G, Halliwell, L, Hawkins, K, Haysom, KA, Holland, MM, Hughes, J, Jackson, AC, Mancini, F, Mathews, F, McQuatters-Gollop, A, Noble, DG, O’Brien, D, Pescott, OL, Purvis, A, Simkin, J, Smith, A, Stanbury, AJ, Villemot, J, Walker, KJ, Walton, P, Webb, TJ, Williams, J, Wilson, R, Gregory, RD, 2023. State of Nature 2023, the State of Nature partnership, Available at: www.stateofnature.org.uk",
              sheet = "Metadata",
              startCol = 1, startRow = 18)
    writeData(wb, 
              "Copywrite of data and or/information presented here does not reside solely with the distributor (NBN). Please contact the State of Nature Partnership stateofnature@rspb.org.uk for copywrite requests.",
              sheet = "Metadata",
              startCol = 1, startRow = 19)
    writeData(wb, 
              "Data sources and references are available in the associated metadata file available for download from the Interpretation tab.",
              sheet = "Metadata",
              startCol = 1, startRow = 20)
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


output$download_metadata <- downloadHandler(
  filename="SoN_metadata.xlsx",  # desired file name on client 
  content=function(con) {
    file.copy("Data/stateofnature2023_datasources_metadata - CopyToUpdate.xlsx", con)
  },
  contentType = "file/xlsx"
)


}