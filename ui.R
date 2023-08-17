  navbarPage(
    "Explore the State of Nature Indicators",
    tabPanel(
    "Indicators",
    fluidPage(
      theme = bslib::bs_theme(bootswatch = "sandstone"),
    sidebarLayout(
      sidebarPanel(
        selectInput("selected_metric",
                    "choose a metric",
                    choices = ind_metric),
        selectInput("selected_group",
                    "choose a taxonomic group",
                    choices = NULL),
        selectInput("selected_country",
                    "choose a country",
                    choices = NULL),      
        actionButton("plot", "plot"),
        downloadButton("download_data", "Download .csv")), 
      mainPanel(plotlyOutput("indicator_plot")
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
                      p("These graphs show indicators based on the abundance data and distribution data separately. Species’ indicator graphs show the average change in the status of species based on either abundance or distribution data. The shaded areas show a measure of uncertainty around the indicator."),
                      h2("Categories of change"),
                      p("Each species was placed into one of three or five trend categories based on annual percentage changes. Results reported for each figure include the percentage of species that showed strong or moderate changes, and those showing little change, in eachtime period."),
                      p("Thresholds for assigning species’ trends to the categories are given in the main report. A small number of species did not have a short-term assessment as data were unavailable for recent years."),
                      h2("What time period does the report cover?"),
                      p("We in general show abundance trends in species from 1970 to 2021 and distribution trends from 1970 to 2020. We refer to this as our long-term period. Our short-term period covers the final 10 years of an indicator, often 2010 to 2020.")),
               column(5, 
                      plotOutput("interpretation_plot"))
             )

             )
           ),
  collapsible = TRUE
)
  
  
  # to do: 
  # 1. add download figure and download data buttons
  # 2. add percentage of species to the right of the plot  
  # 4. hover on the graph to show number of species

