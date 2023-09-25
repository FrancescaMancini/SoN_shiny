  navbarPage(
    "Explore the State of Nature Indicators",
    tabPanel(
    "Distribution indicators",
    fluidPage(
      theme = bslib::bs_theme(bootswatch = "sandstone"),
    sidebarLayout(
      sidebarPanel(
        selectInput("selected_occ_group",
                    "choose a taxonomic group",
                    choices = ind_groups),
        selectInput("selected_occ_country",
                    "choose a country",
                    choices = NULL),      
        actionButton("plot_occ", "plot"),
        downloadButton("download_occ_data", "Download .xlsx")), 
      mainPanel(plotlyOutput("indicator_occ_plot")
        )
    )
  )
  ),
  tabPanel(
    "Abundance indicators",
    fluidPage(
      theme = bslib::bs_theme(bootswatch = "sandstone"),
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_abnd_country",
                      "choose a country",
                      choices = abnd_countries),      
          actionButton("plot_abnd", "plot"),
          downloadButton("download_abnd_data", "Download .xlsx")), 
        mainPanel(plotlyOutput("indicator_abnd_plot")
        )
      )
    )
  ),
  tabPanel("Interpretation",
           fluidPage(
             theme = bslib::bs_theme(bootswatch = "sandstone"),
             fluidRow(
               column(width = 7,
                      h1("What are the graphs telling me?"),
                      p("The measures we present, at a UK and individual country level, show the following: "),
                      p("- Change over time – Species’ indicator – The average change in the status of species, based on abundance or distribution data. "),
                      p("- Categories of change – The percentage of species in each trend category eg strong increase or little change. "),
                      p("Please note that our measures are not directly comparable with those presented in the previous State of Nature reports because the current report is based on an increased number of species, updated methods and, in some cases, different data sources."),
                      h2("Change over time – Species’ indicator"),
                      p("These graphs show indicators based on the abundance data and distribution data separately. Species’ indicator graphs show the average change in the status of species based on either abundance or distribution data. The shaded areas show a measure of uncertainty around the indicator, the uncertainty intervals (UI).")),
               column(5, 
                      imageOutput("interpretation_plot")),
               column(12,
                      h2("Categories of change"),
                      p("Each species was placed into one of three or five trend categories based on annual percentage changes. Results reported for each figure include the percentage of species that showed strong or moderate changes, and those showing little change, in each time period."),
                      p("Thresholds for assigning species’ trends to the categories are given in the main report. A small number of species did not have a short-term assessment as data were unavailable for recent years."),
                      h2("What time period does the report cover?"),
                      p("We in general show abundance trends in species from 1970 to 2021 and distribution trends from 1970 to 2020. We refer to this as our long-term period. Our short-term period covers the final 10 years of an indicator, often 2010 to 2020."),
                      h1("State of Nature Data"),
                      p("The data shared here is provided on the following conditions,"),
                      p("1. The graphs and data downloaded will be used solely to represent UK and UK country multi taxa species abundance indicators, and UK and UK country occupancy indicators."),
                      p("2. The graphs and data remain the intellectual property of the providers and should be cited as: Burns, F, Mordue, S, al Fulaij, N, Boersch-Supan, PH, Boswell, J, Boyd, RJ, Bradfer-Lawrence, T, de Ornellas, P, de Palma, A, de Zylva, P, Dennis, EB, Foster, S, Gilbert, G, Halliwell, L, Hawkins, K, Haysom, KA, Holland, MM, Hughes, J, Jackson, AC, Mancini, F, Mathews, F, McQuatters-Gollop, A, Noble, DG, O’Brien, D, Pescott, OL, Purvis, A, Simkin, J, Smith, A, Stanbury, AJ, Villemot, J, Walker, KJ, Walton, P, Webb, TJ, Williams, J, Wilson, R, Gregory, RD, 2023. State of Nature 2023, the State of Nature partnership, Available at: www.stateofnature.org.uk"),
                      p("Copywrite of data and or/information presented here does not reside solely with the distributor (NBN). Please contact the State of Nature Partnership stateofnature@rspb.org.uk for copywrite requests."),
                      p("Data sources and references are available in the associated metadata file below"),
                      downloadLink("download_metadata", "Download metadata"))
             )

             )
           ),
  collapsible = TRUE
)
  
  
