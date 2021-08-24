
#________________________________________________________________________________#
# NOTE:

## By default, Shiny is styled off Bootstrap 3.3
## https://getbootstrap.com/docs/3.3/
## A few modifications were made on top of this, saved in 'www/bootstrap_mod.css'


#________________________________________________________________________________#
# Clear memory

## remove variables
rm(list=ls())

## remove variables and packages
rm(list = ls(all.names = TRUE))

## clear memory
gc()

#________________________________________________________________________________#
# Load libraries

## shiny
if(!require(shiny)) {install.packages("shiny"); library(shiny)}

## data frame manipulation
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)) {install.packages("lubridate"); library(lubridate)}

## interactive data tables 
if(!require(DT)) {install.packages("DT"); library(DT)}

## plots
if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
if(!require(gghighlight)) {install.packages("gghighlight"); library(gghighlight)}
if(!require(plotly)) {install.packages("plotly"); library(plotly)}

## plots - mapdata
if(!require(rnaturalearth)) {install.packages("rnaturalearth"); library(rnaturalearth)}
if(!require(rnaturalearthdata)) {install.packages("rnaturalearthdata"); library(rnaturalearthdata)}
if(!require(sf)) {install.packages("sf"); library(sf)}

### plots - animation
if(!require(gganimate)) {install.packages("gganimate"); library(gganimate)}
if(!require(gifski)) {install.packages("gifski"); library(gifski)}
if(!require(transformr)) {install.packages("transformr"); library(transformr)}

### plots - custom labels
if(!require(directlabels)) {install.packages("directlabels"); library(directlabels)}
if(!require(ggrepel)) {install.packages("ggrepel"); library(ggrepel)}
if(!require(scales)) {install.packages("scales"); library(scales)}

#________________________________________________________________________________#
# Load data

## offline data (contained in 'data' file.)
## refer to '/0 data/ncov_data_v2_02.Rmd' for code used to create this

if (TRUE) {
  load(file.path("data", "ncov_clean.RData"))
}

#________________________________________________________________________________#
# Default load:
  # The intention here is to have a selection of countries loaded by default.
  # The user can change this, selecting different countries as he/she interacts...
  #   with the data, but the idea is to have one pre-loaded, with some comments...
  #   on what the data shows.

## list of country names & unique country code identifier
countries <- ncov %>%
  select(country, iso3c) %>%
  distinct()

countries_named_list <- countries$iso3c %>%
  unlist() %>%
  as.character()

names(countries_named_list) <- countries$country %>%
  unlist() %>%
  as.character()

rm(countries)

## list of case types
types_named_list <- c("confirmed", "active", "recovered", "deaths")
names(types_named_list) <- c("confirmed", "active", "recovered", "deaths")

## character vector of top 25 countries by confirmed cases (default load)
countries_confirmed_top25 <- ncov %>%
  arrange(country, desc(date)) %>%
  group_by(country) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(confirmed)) %>%
  filter(level == "country") %>% # exclude continent summaries
  head(25) %>%
  select(iso3c) %>%
  unlist() %>%
  as.vector()

## smaller ncov df for quicker search
ncov.min <- ncov %>% filter(date == max(date))


#______________________________________________________________________________________________________________#
# ui ----------------------------------------------------------------------------------------------------------0

#________________________________________________________________________________#
# (NOTE: This line tries to separate sections to make it easier to read.)
# 0: ui/ server
# 1: Page Header
# 2: Quick Search (main interaction for user, filter country)
# 3: 

ui <- navbarPage(
  title = "COVID-19 Shiny App",
  id = "navbar",
  theme = "css/bootstrap_mod.css", # small modifications to some elements

#________________________________________________________________________________#      
  # 'Home' tab ------------------------------------------------------------------------------------------------1
  tabPanel("Home", icon = icon("home"),
    # Page Header ---------------------------------------------------------------1
    fluidRow(
      column(10, offset=1,
        tags$div(class="jumbotron",
          tags$h1("Hello, (brave new) World!"),
          tags$p(
            "An ",
            tags$span(style="background-color:#FFFF00;", "interactive")
            ," visualisation of the coronavirus pandemic using R & Shiny."
          ),
          tags$code("<student> Christopher Conrady, CNRCHR003 </student>")
        ) # div.jumbotron
      ) # column
    ), # fluidRow: Page header

#________________________________________________________________________________#
    # Quick Search --------------------------------------------------------------------------------------------2
    fluidRow(
      column(10, offset=1,
             
        # Tip: Quick Selection --------------------------------------------------2
        tags$div(class = "alert alert-info",
          tags$p(tags$strong("Tip:"), "Everytime you add (or remove) a country from the list below, all visualisations in this app will update!")
        ), # div: Tips     
        
        # Select countries for graphs (filter) ----------------------------------2
        selectInput(
          inputId = "countries_selected",
          label = "Select a country (or countries)",
          choices = countries_named_list,
          selected = countries_confirmed_top25,
          multiple = TRUE
        ), # selectInput

        # Quick selection of countries (filter) ---------------------------------2
        tags$h5(tags$strong("Quick Selection Tool")),
        wellPanel(
          actionButton(inputId = "selectNone",         label = "Clear",                 icon = icon("remove")),
          actionButton(inputId = "selectAfrica",       label = "Africa",                icon = icon("plus")),
          actionButton(inputId = "selectAfricaList",   label = "Africa (countries)",    icon = icon("plus")),
          actionButton(inputId = "selectAsia",         label = "Asia",                  icon = icon("plus")),
          actionButton(inputId = "selectAsiaList",     label = "Asia (countries)",      icon = icon("plus")),
          actionButton(inputId = "selectEurope",       label = "Europe",                icon = icon("plus")),
          actionButton(inputId = "selectEuropeList",   label = "Europe (countries)",    icon = icon("plus")),
          actionButton(inputId = "selectNAmerica",     label = "N.America",             icon = icon("plus")),
          actionButton(inputId = "selectNAmericaList", label = "N.America (countries)", icon = icon("plus")),
          actionButton(inputId = "selectSAmerica",     label = "S.America",             icon = icon("plus")),
          actionButton(inputId = "selectSAmericaList", label = "S.America (countries)", icon = icon("plus")),
          actionButton(inputId = "selectOceania",      label = "Oceania",               icon = icon("plus")),
          actionButton(inputId = "selectOceaniaList",  label = "Oceania (countries)",   icon = icon("plus")),
          actionButton(inputId = "selectWorld",        label = "World",                 icon = icon("plus")),
          actionButton(inputId = "selectWorldList",    label = "World (countries)",     icon = icon("plus")),
          actionButton(inputId = "selectDefault",      label = "Default",               icon = icon("redo"))
        ) # wellPanel

      ) # column
    ), # fluidRow: Quick Search

#________________________________________________________________________________#
    # Data section (Tabs: A & B) ------------------------------------------------------------------------------3
    fluidRow(
      column(10, offset=1, 
        tabsetPanel(

          # Tab A: Multiple Countries (Compare) --------------------------------------------------------------3A 
          tabPanel("1: Compare Countries",
            
            # Header -----------------------------------------------------------3A
            fluidRow(
              column(12, 
                tags$h4(tags$strong("Overview")),
                tags$p("The purpose of this section is to compare countries in as many ways as possible."),
                tags$div(class = "alert alert-warning", role="alert", 
                  tags$p("For a select few visualisations there are comments on some of the more interesting points. These comments are specific to the default selection of countries, and therefore should be ignored if the selected countries are changed.")
                ),
                tags$hr()
              ) # column
            ), # fluidRow: Header
            
            # Row 1 ------------------------------------------------------------3A
            fluidRow(
              column(12,
                tags$h4(tags$strong("1. Country Trajectories")),
                tags$p("Comparing the confirmed cases (or recoveries/ deaths) of countries (or continents), and how these numbers change over time.")
              ), # column
              
              # Graph A1 ----------------------------------------------------3A-A1 
              column(6,
                wellPanel(style = "background-color: #fafafa; height: 725px",
                  tags$h4(tags$strong("A Comparison of Total Cases")),
                  tags$p("Daily totals (cumulative) of confirmed cases, active cases, recoveries and deaths for all countries. Here, all countries have been anchored to an origin, in this case, the date when the country had at least 100 confirmed cases, to make comparisons more meaningful. For each country, the x-axis represents the number of days since this date."),
                  column(3,
                    ## select a case type (confirmed, recovered, deaths)
                    selectInput(
                      inputId = "A1YAxisChoice",
                      label = "Scale Y-axis?",
                      choices = c("log", "linear"),
                      selected = "log",
                      multiple = FALSE
                    )
                  ), # column
                  column(3,
                    ## select a case type (confirmed, recovered, deaths)
                    selectInput(
                      inputId = "A1TypeSelected",
                      label = "Case Type:",
                      choices = types_named_list,
                      selected = "confirmed",
                      multiple = FALSE
                    )
                  ), # column
                  column(6,
                    ## Select countries to highlight
                    selectInput(
                      inputId = "A1CountriesHighlighted",
                      label = "Highlight a country (or countries)?",
                      choices = countries_named_list,
                      selected = character(0),
                      multiple = TRUE
                    )
                  ),
                  plotOutput(outputId = "A1Plot", width = "100%", height="500px")
                ) # wellPanel
              ), # column
              
              # Graph A2 ----------------------------------------------------3A-A2 
              column(6,
                wellPanel(style = "background-color: #fafafa; height: 725px",
                  tags$h4(tags$strong("A Comparison of Total Cases")),
                  tags$p("Daily totals (cumulative) of confirmed cases, active cases, recoveries and deaths for all countries. Each date on the x-axis represents the total cumulative count of cases by type (confirmed/ recovered/ active or deaths) on that particular day."),
                  column(3,
                  ## select a case type (confirmed, recovered, deaths)
                  selectInput(
                    inputId = "A2YAxisChoice",
                    label = "Scale Y-axis?",
                    choices = c("log", "linear"),
                    selected = "log",
                    multiple = FALSE
                  )
                  ), # column
                  column(3,
                    ## select a case type (confirmed, recovered, deaths)
                    selectInput(
                      inputId = "A2TypeSelected",
                      label = "By:",
                      choices = types_named_list,
                      selected = "confirmed",
                      multiple = FALSE
                    )
                  ), # column
                  column(6,
                    ## Select countries to highlight
                    selectInput(
                      inputId = "A2CountriesHighlighted",
                      label = "Highlight a country (or countries)?",
                      choices = countries_named_list,
                      selected = character(0),
                      multiple = TRUE
                    )
                  ),
                  plotOutput(outputId = "A2Plot", width = "100%", height="500px")
                ) # wellPanel
              ) # column
            ), # fluidRow: Row 1
            
            tags$hr(),
            
            # Row 2 ------------------------------------------------------------3A
            fluidRow(
              column(12,
                tags$h4(tags$strong("2. Countries Hardest Hit")),
                tags$p("These heatmaps are extremelly useful for visualising time-series data without the need for any animation. Here we look at 4 statistics and how they've changed over the duration of the pandemic.")
              ) # column
            ), #fluidRow
            
            tabsetPanel(
              tabPanel("Daily Cases",
                fluidRow(
                  # Graph A3 ------------------------------------------------3A-A3
                  column(6,
                    wellPanel(style = "background-color: #fafafa;",
                      tags$h4(tags$strong("Daily Deaths: How Countries Compare")),
                      tags$p("Reported deaths per day by country (ranked) over the first four months of the year. The number of deaths reported daily in the US has steadily increased, while China's daily death count is now at an all time low since its peak in mid February."),
                      plotOutput(outputId = "A3Plot")
                    ) # wellPanel
                    ), # column
                    
                  # Graph A4 ------------------------------------------------3A-A4 
                  column(6,
                    wellPanel(style = "background-color: #fafafa;",
                      tags$h4(tags$strong("New Confirmed Cases: How Countries Compare")),
                      tags$p("Daily increase in confirmed cases per day by country (ranked) over the first four months of the year. For countries like Italy, Spain, Germany, the number of new confirmed cases reported each day has declined since their April peak."),
                      plotOutput(outputId = "A4Plot")
                    ) # wellPanel
                  ) # column
                ), # fluidRow: Tab 1 of graphs/plots
              ), # tabPanel
              
              tabPanel("Rates",
                fluidRow(
                  # Graph A5 ------------------------------------------------3A-A5 
                  column(6,
                    wellPanel(style = "background-color: #fafafa;",
                      tags$h4(tags$strong("Tracking A Country's Recovery")),
                      tags$p("Of all confirmed cases, how many of these have since recovered? For countries like China, Iran, Switzerland and Germany, the majority of those infected have recovered by the end of April."),
                      plotOutput(outputId = "A5Plot")
                    ) # wellPanel
                  ), # column
                  
                  # Graph A6 ------------------------------------------------3A-A6 
                  column(6,
                    wellPanel(style = "background-color: #fafafa;",
                      tags$h4(tags$strong("Mortality Rates: How Countries Compare")),
                      tags$p("A comparison of the mortality rates of different countries, and how these have changed over the first four months of the year."),
                      plotOutput(outputId = "A6Plot")
                    ) # wellPanel
                  ) # column
                ), # fluidRow: Tab 2 of graphs/plots
              ) # tabPanel
    
            ), # tabsetPanel

            tags$hr(),
        
            # Row 3 ------------------------------------------------------------3A
            # Graph A6 ------------------------------------------------------3A-A7
            fluidRow(
              column(12,
                tags$h4(tags$strong("3. A Global Perspective")),
                tags$p("As of April 29th, this is the number of confirmed cases around the world. In general, Africa has seen the fewest cases of COVID-19."),
                plotOutput(outputId = "A7Plot")
              ) # column
            ), #fluidRow
            
            tags$hr(),
            
            # Row 4 ------------------------------------------------------------3A
            fluidRow(
              # Data Table A8 -----------------------------------------------3B-A8
              column(12,
                     tags$h4(tags$strong("Interactive Data Table")),
                     tags$p("If you prefer to look at the data yourself, this is a raw tabular output of the data for all countries selected."),     
                     DT::dataTableOutput(outputId="A8DataTable")
              ) # column
            ), # fluidRow: Row 4
            
            # Row 5 (whitespace) -----------------------------------------------3A
            tags$div(style="height: 100px;")
                   
          ), # tabPanel: 1: Compare Countries
          
#________________________________________________________________________________#
          # B: Single Country (Deep Dive) Overview -----------------------------------------------------------3B 
          tabPanel("2: Country Deep Dive",

            # Header & Country Filter-------------------------------------------3B
            fluidRow(
              column(12, 
                tags$h4(tags$strong("Overview")),
                tags$p("The purpose of this section is not to compare countries, but to interact with the data for a single country."),
                tags$p("There are a number of ways you can do this:"),
                tags$ol(
                  tags$li("by ", tags$strong("Total cases")," (graph on the left)"),
                  tags$li("by ", tags$strong("New cases")," (graph on the right)"),
                  tags$li(tags$strong("All data"),  "(table below)")
                ),
                tags$div(class = "alert alert-info", tags$strong("Tips"),
                  tags$p("Try viewing new cases by 'weekly' date (total new cases per week). This can smooth out erratic daily data making it easier to spot trends. For example, are the number of new deaths in Belgium increasing or decreasing? This trend is hard to spot on daily data, but is very clear when looking at weekly totals."),
                  tags$p("Try setting the y-axis to 'percentage' for a different perspective. For example, Italy was hard hit by the virus, reporting a large number of new cases daily, but by the end of March one third of its sick population had recovered. (Try to slide the date slider to only include data from about 100 confirmed cases or more. Under this, percentages can be erratic since the numbers are so low that even small changes day to day can have big effects.)"),
                ), # div: Tips
                tags$hr(),
                selectInput(
                  inputId = "BSelectCountry",
                  label = "Select a country",
                  choices = countries_named_list[(countries_named_list %in% countries_confirmed_top25)],
                  selected = countries_confirmed_top25[1],
                  multiple = FALSE
                ) # selectInput
              ) # column
            ), # fluidRow
            
            # Header & Country Filter-------------------------------------------3B
            
            # Row 1 ------------------------------------------------------------3B
            fluidRow(
              # Graph B1 ----------------------------------------------------3B-B1 
              column(6,
                wellPanel(style = "background-color: #fafafa;",
                  tags$h4(tags$strong("Confirmed Cases (Total)")),
                  tags$p("Daily totals (cumulative) of confirmed cases, active cases, recoveries and deaths, for the country selected."),
                  actionButton(inputId = "B1ButtonSwitchY", label = "percent", icon = icon("hand-point-down")),
                  plotlyOutput(outputId = "B1Plot", width = "100%", height="500px")
                ) # wellPanel
              ), # column
             
              # Graph B2 ----------------------------------------------------3B-B2 
              column(6,
                wellPanel(style = "background-color: #fafafa;",
                  tags$h4(tags$strong("Confirmed Cases (New)")),
                  tags$p("Daily (or weekly) new confirmed cases, active cases, recoveries or deaths, for the country selected."),
                  actionButton(inputId = "B2ButtonSwitchX", label = "weekly", icon = icon("signal")),
                  plotlyOutput(outputId = "B2Plot", width = "100%", height="500px")
                ) # wellPanel
              ) # column
            ), # fluidRow: Row 1
            
            tags$hr(),
            
            # Row 2 ------------------------------------------------------------3B
            fluidRow(
              # Data Table B3 -----------------------------------------------3B-B3
              column(12,
                tags$h4(tags$strong("Interactive Data Table")),
                tags$p("If you prefer to look at the data yourself, this is a raw tabular output of the data for the selected country."),     
                DT::dataTableOutput(outputId="B3DataTable")
              ) # column
            ), # fluidRow: Row 2
            
            # Row 3 (whitespace) -----------------------------------------------3B
            tags$div(style="height: 100px;")
            
          ) # tabPanel: 2: Country Deep Dive
        
        ) # tabsetPanel: Data section (Tabs: A & B)  
      ) # column: Data section (Tabs: A & B)
    ) # fluidRow: Data section (Tabs: A & B)
    
  ), # tabPanel: Home
  
#________________________________________________________________________________#
  # 'How to use' Tab --------------------------------------------------------------------------------------4
  tabPanel("How to use", icon = icon("info"),
    fluidRow(
      column(10, offset = 1,
        
        # link to video guide
        tags$p(
          "Please see the ",
          tags$a(href = "https://youtu.be/DAGeI-0pWPI", target="_blank", "How to, video guide"),
          "if you would like to learn how to use this Shiny App, or view it directly below."
        ),
        
        tags$hr(),
        
        # embed video guide
        tags$iframe(width="80%", height="600px", src="https://www.youtube.com/embed/DAGeI-0pWPI", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"),
        
        tags$hr()
        
      ) # column
    ) # fluidRow: How to use
  ) # tabPanel: How to use
                 
) # navbarPage







#________________________________________________________________________________#
# server ------------------------------------------------------------------------------------------------------0

server <- function(input, output, session) {
  
  # Quick selection of countries (filter) --------------------------------------Y
  observeEvent(input$selectNone, {
    updateSelectInput(session, inputId="countries_selected", selected = character(0))
  }) # observeEvent: selectNone
  
  # Default loadout
  observeEvent(input$selectDefault, {
    updateSelectInput(session, inputId="countries_selected", selected = countries_confirmed_top25)
  }) # observeEvent: selectDefault
  
  observeEvent(input$selectAfrica, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(iso3c == "AF") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectAfrica
  
  observeEvent(input$selectAfricaList, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(continent == "Africa" & iso3c != "AF") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectAfricaList
  
  observeEvent(input$selectAsia, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(iso3c == "AS") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectAsia
  
  observeEvent(input$selectAsiaList, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(continent == "Asia" & iso3c != "AS") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectAsiaList
  
  observeEvent(input$selectEurope, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(iso3c == "EU") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectEurope
  
  observeEvent(input$selectEuropeList, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(continent == "Europe" & iso3c != "EU") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectEuropeList
  
  observeEvent(input$selectNAmerica, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(iso3c == "NM") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectNAmerica
  
  observeEvent(input$selectNAmericaList, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(continent == "North America" & iso3c != "NM") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectNAmericaList
  
  observeEvent(input$selectSAmerica, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(iso3c == "SA") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectSAmerica
  
  observeEvent(input$selectSAmericaList, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(continent == "South America" & iso3c != "SA") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectSAmericaList
  
  observeEvent(input$selectOceania, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(iso3c == "OC") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectOceania
  
  observeEvent(input$selectOceaniaList, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(continent == "Oceania" & iso3c != "OC") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectOceaniaList
  
  observeEvent(input$selectWorld, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(iso3c == "WD") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectWorld
  
  observeEvent(input$selectWorldList, {
    updateSelectInput(session, inputId="countries_selected", selected = unique(c(
      input$countries_selected,
      ncov.min %>% filter(level == "country") %>% select(iso3c) %>% unlist() %>% as.character()))
    ) # updateSelectInput
  }) # observeEvent: selectWorldList
  

  # Update (reactively) data selection  ----------------------------------------Z
  data <- reactive({
    ncov %>% filter(iso3c %in% input$countries_selected)
  })
  
  # A & B: Select Country Filter  ----------------------------------------------Z
  observeEvent(input$countries_selected, {
    updateSelectInput(session, inputId="BSelectCountry",
      choices = countries_named_list[(countries_named_list %in% input$countries_selected)],
      selected = input$countries_selected[1]
    ) # updateSelectInput
    
    updateSelectInput(session, inputId="A1CountriesHighlighted",
      choices = countries_named_list[(countries_named_list %in% input$countries_selected)],
      selected = character(0)
    ) # updateSelectInput
    
    updateSelectInput(session, inputId="A2CountriesHighlighted",
      choices = countries_named_list[(countries_named_list %in% input$countries_selected)],
      selected = character(0)
    ) # updateSelectInput
    
  }) # observeEvent: countries_selected
  

  
  
  # Plots/ Graphs Section  -----------------------------------------------------Z
  
  ## A1Plot -----------------------------------------------------------------Z-A1
  
  output$A1Plot <- renderPlot({
    
    p <- as.data.frame(data()) %>%
      
      # reduce size of dataset
      select(date, iso3c, country, continent, confirmed, active, recovered, deaths) %>%
      
      # convert to long (so we can select by type)
      pivot_longer(
        cols=c("confirmed", "active", "recovered", "deaths"),
        names_to = "type",
        values_to = "value") %>%
      
      filter(type == input$A1TypeSelected) %>%
      
      filter(value > 0) %>% # to accomodate log scale (no zeroes or negatives allowed)
      
      # center x option ----- delete ------------------------------------ [1]
      group_by(iso3c) %>%
      mutate(date_x = cumsum(ifelse(value >= 100, 1, 0))) %>%
      ungroup() %>%
      filter(date_x > 0) %>%
      
      # --------- ggplot ---------
    
      mutate(label = ifelse(date == max(date), as.character(country), NA)) %>%
      
      ggplot(aes(x = date_x, y = value, group=iso3c, color=continent)) +
      geom_point(size = 2, alpha=0.7) +
      geom_line(size = 1.5, alpha=0.7)
      
      # give some space for labels
      #scale_x_continuous(limits = c(NA, length(unique(ncov$date))*1.05))
      
      if (input$A1YAxisChoice == "log") {
        p <- p +
          # scale axis
          scale_y_log10(
            breaks = scales::trans_breaks("log10", function(x) 10^x),
            labels = scales::trans_format("log10", scales::math_format(10^.x))
          ) +
          annotation_logticks(sides="l") +
          # format axis label
          labs(
            x = "Days since 100 cases",
            y = "Number of cases (log scale)",
            color = "Continent"
          )
          
      } else {
        p <- p + 
          # scale axis
          scale_y_continuous(
            labels = scales::comma
          ) +
          
          # format axis label
          labs(
            x = "Days since 100 cases",
            y = "Number of cases",
            color = "Continent"
          )
      }
      
    p <- p +
      # formatting
      theme_bw() +
      theme(
        panel.border = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13),
        legend.title = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 13),
        
        plot.background = element_rect(fill = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa"),
        legend.key = element_rect(fill = "#fafafa"),
        legend.position = "bottom",
        legend.direction = "horizontal"
      ) +

      geom_text_repel(aes(label = label), color="black",
                      nudge_x = 5,
                      size = 4,
                      na.rm = TRUE)
    
      if (length(input$A1CountriesHighlighted) > 0) {
        p <- p + gghighlight(iso3c %in% input$A1CountriesHighlighted)
      }
     
    
    # return
    p

  })

  
  ## A2Plot -----------------------------------------------------------------Z-A1

  
  output$A2Plot <- renderPlot({
    
    p <- as.data.frame(data()) %>%
      
      # reduce size of dataset
      select(date, iso3c, country, continent, confirmed, active, recovered, deaths) %>%
      
      # convert to long (so we can select by type)
      pivot_longer(
        cols=c("confirmed", "active", "recovered", "deaths"),
        names_to = "type",
        values_to = "value") %>%
      
      filter(type == input$A2TypeSelected) %>%
      
      filter(value > 0) %>% # to accomodate log scale (no zeroes or negatives allowed)
      
      # --------- ggplot ---------
    
      mutate(label = ifelse(date == max(date), as.character(country), NA)) %>%
      
      ggplot(aes(x = date, y = value, group=iso3c, color=continent)) +
      geom_point(size = 2, alpha=0.7) +
      geom_line(size = 1.5, alpha=0.7) +
      
      # give some space for labels
      #scale_x_continuous(limits = c(NA, max(ncov$date)+1)) + 
      scale_x_date(
        limits = c(min(ncov$date), max(ncov$date+20)),
        date_breaks = "weeks",
        date_labels = "%d-%b-%y"
      )
    
    if (input$A2YAxisChoice == "log") {
      p <- p +
        # scale axis
        scale_y_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) +
        annotation_logticks(sides="l") +
        # format axis label
        labs(
          x = "Date",
          y = "Number of cases (log scale)"
        )
      
    } else {
      p <- p + 
        # scale axis
        scale_y_continuous(
          labels = scales::comma
        ) +
        
        # format axis label
        labs(
          x = "Date",
          y = "Number of cases"
        )
    }
    
    p <- p +
      # formatting
      theme_bw() +
      theme(
        panel.border = element_blank(),
        axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13),
        legend.title = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 13),
        
        plot.background = element_rect(fill = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa"),
        legend.key = element_rect(fill = "#fafafa"),
        legend.position = "bottom",
        legend.direction = "horizontal"
      ) +
      
      geom_text_repel(aes(label = label), color="black",
                      nudge_x = 5,
                      size = 4,
                      na.rm = TRUE)
    
    if (length(input$A2CountriesHighlighted) > 0) {
      p <- p + gghighlight(iso3c %in% input$A2CountriesHighlighted)
    }
    
    
    # return
    p
    
  })
  



  
  ## A3Plot -----------------------------------------------------------------Z-A1
  
  output$A3Plot <- renderPlot({
    
    # rank countries by total number of deaths (order appearing on plot)
    # note: plot can only accomodate top 25
    countries_ranked <- as.data.frame(data()) %>% 
      filter(date == max(date)) %>% 
      arrange(deaths) %>% 
      select(country) %>% tail(25) %>% unlist() %>% as.character()
    
    # plot
    p <- as.data.frame(data()) %>%
      
      # reduce size of dataset
      select(date, iso3c, country, deaths, deaths_new) %>%
      filter(country %in% countries_ranked) %>%
    
      # order by descending value for chosen type (order appearing on plot)
      mutate(country = factor(country, levels = unlist(countries_ranked))) %>%
      
      # put each data point into a bin
      mutate(bins = cut(deaths_new, breaks = c(-100000, 0, 10, 100, 1000, 5000))) %>%
      
      # ---- plot ----
      ggplot(aes(x=date, y=country, fill=bins)) +
      geom_tile(colour = "#ffffff", na.rm=T) +
      
      ## formatting
      theme_bw() +
      theme(
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 13),
        axis.text.y = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        plot.caption = element_text(size = 12),
        plot.background = element_rect(fill = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa"),
        legend.key = element_rect(fill = "#fafafa"),
        legend.position = "bottom",
        legend.direction = "horizontal"
      ) +
      guides(fill = guide_legend(nrow = 1)) +
      scale_fill_brewer(
        name = "Daily Deaths",
        palette = "YlOrRd",
        labels = c("0", "1-10", "11-100", "101-1000", "1001-5000", "")
      ) +
      labs(
        caption = "*Limited to top 25 countries (ranked by total deaths)"
      )
    
    # return
    p
  })
  
  ## A4Plot -----------------------------------------------------------------Z-A2
  
  output$A4Plot <- renderPlot({
    
    # rank countries by total number of confirmed cases (order appearing on plot)
    # note: plot can only accomodate top 25
    countries_ranked <- as.data.frame(data()) %>% 
      filter(date == max(date)) %>% 
      arrange(confirmed) %>% 
      select(country) %>% tail(25) %>% unlist() %>% as.character()
    
    # plot
    p <- as.data.frame(data()) %>%
      
      # reduce size of dataset
      select(date, iso3c, country, confirmed, confirmed_new) %>%
      filter(country %in% countries_ranked) %>%
      
      # order by descending value for chosen type (order appearing on plot)
      mutate(country = factor(country, levels = unlist(countries_ranked))) %>%
      
      # put each data point into a bin
      mutate(bins = cut(confirmed_new, breaks = c(-100000, 0, 1000, 5000, 10000, 50000))) %>%
      
      # ---- plot ----
    ggplot(aes(x=date, y=country, fill=bins)) +
      geom_tile(colour = "#ffffff", na.rm=T) +
      
      ## formatting
      theme_bw() +
      theme(
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 13),
        axis.text.y = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        plot.caption = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa"),
        legend.key = element_rect(fill = "#fafafa")
      ) +
      guides(fill = guide_legend(nrow = 1)) +
      scale_fill_brewer(
        name = "Confirmed Cases (New)",
        palette = "YlOrRd",
        labels = c("0", "1-1k", "1k-5k", "5k-10k", "10k-50k", "")
      ) +
      labs(
        caption = "*Limited to top 25 countries (ranked by total confirmed cases)"
      )
    
    # return
    p
  })
#________________________________________________________________________________#
  ## A5Plot -----------------------------------------------------------------Z-A3
  
  output$A5Plot <- renderPlot({
    
    # plot
    p <- as.data.frame(data()) %>%
      
      # reduce size of dataset
      select(date, iso3c, country, confirmed, active, recovered, deaths) %>%
      
      # convert to long
      pivot_longer(
        cols=c("active", "recovered", "deaths"),
        names_to = "type",
        values_to = "value"
      ) %>%
      
      # percentage are not reliabel indicators when numbers are so low
      filter(confirmed >= 150) %>%
      
      # get rel. by each case
      group_by(country, date, type) %>%
      summarise(n = sum(value)) %>%
      mutate(percentage = 100*(n / sum(n))) %>%
      ungroup()
    
    countries_ranked <- p %>% 
      filter(date == max(date)) %>% 
      filter(type == "recovered") %>%
      arrange(percentage) %>%
      select(country) %>% tail(25) %>% unlist() %>% as.character()
    
    p <- p %>%
      
      filter(country %in% countries_ranked) %>%
      
      filter(type == "recovered") %>%
      
      # order by descending value for chosen type (order appearing on plot)
      mutate(country = factor(country, levels = unlist(countries_ranked))) %>%
      
      # put each data point into a bin
      mutate(bins = cut(percentage, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))) %>%
      
      # ---- plot ----
    ggplot(aes(x=date, y=country, fill=bins)) +
      geom_tile(colour = "#ffffff", na.rm=T) +
      
      ## formatting
      theme_bw() +
      theme(
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 13),
        axis.text.y = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        plot.caption = element_text(size = 12),
        legend.position = "right",
        legend.direction = "vertical",
        plot.background = element_rect(fill = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa"),
        legend.key = element_rect(fill = "#fafafa")
      ) +
      scale_fill_brewer(
        name = "Recovered (%)",
        palette = "RdYlGn",
        labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%", "")
      ) +
      labs(
        caption = "*Limited to top 25 countries (ranked by highest recovered percentage)"
      )
    
    # return
    p
    
    
  })
  
#________________________________________________________________________________#
  ## A6Plot -----------------------------------------------------------------Z-A4
  
  output$A6Plot <- renderPlot({
    
    # plot
    p <- as.data.frame(data()) %>%

      # reduce size of dataset
      select(date, iso3c, country, confirmed, active, recovered, deaths) %>%
      
      # convert to long
      pivot_longer(
        cols=c("active", "recovered", "deaths"),
        names_to = "type",
        values_to = "value"
      ) %>%
      
      # percentage are not reliabel indicators when numbers are so low
      filter(confirmed >= 150) %>%
      
      # get rel. by each case
      group_by(country, date, type) %>%
      summarise(n = sum(value)) %>%
      mutate(percentage = 100*(n / sum(n))) %>%
      ungroup()
    
    countries_ranked <- p %>% 
      filter(date == max(date)) %>% 
      filter(type == "deaths") %>%
      arrange(percentage) %>%
      select(country) %>% tail(25) %>% unlist() %>% as.character()
    
    p <- p %>%
      
      filter(country %in% countries_ranked) %>%
      
      filter(type == "deaths") %>%
      
      # order by descending value for chosen type (order appearing on plot)
      mutate(country = factor(country, levels = unlist(countries_ranked))) %>%
      
      # put each data point into a bin
      mutate(bins = cut(percentage, breaks = c(0, 5, 10, 15, 20, 25))) %>%
      
      # ---- plot ----
    ggplot(aes(x=date, y=country, fill=bins)) +
      geom_tile(colour = "#ffffff", na.rm=T) +
      
      ## formatting
      theme_bw() +
      theme(
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 13),
        axis.text.y = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        plot.caption = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa"),
        legend.key = element_rect(fill = "#fafafa")
      ) +
      guides(fill = guide_legend(nrow = 1)) +
      scale_fill_brewer(
        name = "Death Rate",
        palette = "YlOrRd",
        labels = c("0-5%", "5-10%", "10-15%", "15-20%", "")
      ) +
      labs(
        caption = "*Limited to top 25 countries (ranked by highest death rates)"
      )
    
    # return
    p
  })
  
  ## A7Plot -----------------------------------------------------------------Z-A5
  
  output$A7Plot <- renderPlot({
   
    p <- ncov_sf %>% 
      filter(date == max(date)) %>%     # can I do a date slider? or gganimate
      
      
      ggplot(aes(fill=confirmed)) +
      geom_sf() + 
      scale_fill_viridis_c(option = "plasma", trans = "log10") +
      theme_bw() +
      
      theme(
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        plot.caption = element_text(size = 10),
        legend.position = "right",
        legend.direction = "vertical",
        plot.background = element_rect(fill = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa"),
        legend.key = element_rect(fill = "#fafafa")
      ) +
      #guides(fill = guide_legend(nrow = 1)) +
      
      labs(
        caption = "*Will not update if selected countries are changed.",
        fill = "Confirmed Cases (log scale)"
      )
    
    # return
    p
    
  })
  
  ## A8Plot -----------------------------------------------------------------Z-B3
  
  output$A8DataTable <- DT::renderDataTable({
    
    p <- as.data.frame(data()) %>%
      
      # reduce dataset size for quicker processing
      select(date, country, confirmed, confirmed_new, active, active_new, recovered, recovered_new, deaths, deaths_new)
    
    # return
    p
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## B1Plot -----------------------------------------------------------------Z-B1
  
  ### option to switch axis from absolute to percentage
  observeEvent(input$B1ButtonSwitchY, {
    if (input$B1ButtonSwitchY %% 2) {
      updateSelectInput(session, inputId="B1ButtonSwitchY", label = "total")
    } else {
      updateSelectInput(session, inputId="B1ButtonSwitchY", label = "percent")
    }
  }) # observeEvent: B1ButtonSwitchY
  
  ### create plot
  output$B1Plot <- renderPlotly({
    
    p <- as.data.frame(data()) %>%
      # can view only one country at a time in section B
      filter(iso3c == input$BSelectCountry) %>%
      
      # reduce dataset size for quicker processing
      select(date, iso3c, confirmed, active, recovered, deaths) %>%

      # convert to long format
      pivot_longer(
        cols=c("confirmed", "active", "recovered", "deaths"),
        names_to = "type",
        values_to = "value")
      
      # axis: absolute==0, or percent==1
      if (input$B1ButtonSwitchY %% 2 == 0) {
        p <- p %>%
          # order legend titles
          mutate(type = factor(type, levels=c("confirmed", "active", "recovered", "deaths"))) %>%
          # plot
          ggplot(aes(x=date, y=value, group=type, color=type))
      } else {
        p <- p %>%
          # get rel. proportion by type
          filter(type != "confirmed") %>%
          group_by(date, type) %>%
          summarise(n = sum(value)) %>%
          mutate(percentage = n / sum(n)) %>%
          # order legend titles
          mutate(type = factor(type, levels=c("active", "recovered", "deaths"))) %>%
          # plot
          ggplot(aes(x=date, y=percentage, group=type, color=type))
      }
    
      p <- p +
        # continue with plot options
        geom_point() +
        geom_line() +
        
        # formatting
        scale_x_date(date_breaks = "weeks", date_labels = "%d-%b-%y") +
        ## need to be consistent with legend colours (i.e. confirmed always "red")
        scale_color_manual(
          breaks = c("confirmed", "active", "recovered", "deaths"),
          values=c("#F8766D", "#C77CFF", "#00BA42", "#000000")
        ) +
        theme_bw() +
        theme(
          legend.title = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "#fafafa"),
          panel.background = element_rect(fill = "#fafafa"),
          legend.background = element_rect(fill = "#fafafa"),
          legend.key = element_rect(fill = "#fafafa")
        )
      
      # axis: absolute==0, or percent==1
      if (input$B1ButtonSwitchY %% 2 == 0) {
        p <- p +
          scale_y_continuous(labels = scales::comma) +
          labs(
            x = "Date",
            y = "Number of cases"
          )
      } else {
        p <- p +
          scale_y_continuous(labels = scales::percent) +
          labs(
            x = "Date",
            y = "Percent of cases (%)"
          )
      }
      
      # plotly
      pl <- ggplotly(p) %>% 
        layout(
          legend = list(
            orientation = "h", # show entries horizontally
            yanchor = "bottom",
            xanchor = "center",  # use center of legend as anchor
            x = 0.5 # put legend in center of x-axis
          ), # legend
          xaxis = list(
            rangeslider = list(type = "date")
          ) # xaxis
        ) # layout

    
      # return
      pl
    
  }) # p1 width = 600, height=400
  
  
  ## B2Plot ---------------------------------------------------------------Z-B2T1
  
  ### option to switch x-axis to weekly sum of new cases
  observeEvent(input$B2ButtonSwitchX, {
    if (input$B2ButtonSwitchX %% 2) {
      updateSelectInput(session, inputId="B2ButtonSwitchX", label = "daily")
    } else {
      updateSelectInput(session, inputId="B2ButtonSwitchX", label = "weekly")
    }
  }) # observeEvent: B2ButtonSwitchX
  
  ### create plot
  output$B2Plot <- renderPlotly({
    
    p <- as.data.frame(data()) %>%
      # can view only one country at a time in section B
      filter(iso3c == input$BSelectCountry) %>%
      
      # reduce dataset size for quicker processing
      select(date, iso3c, confirmed_new, active_new, recovered_new, deaths_new) %>%
      
      # convert to long format
      pivot_longer(
        cols=c("confirmed_new", "active_new", "recovered_new", "deaths_new"),
        names_to = "type",
        values_to = "value") %>%
      
      # order legend items
      mutate(type = factor(type, levels=c("confirmed_new", "active_new", "recovered_new", "deaths_new")))
    
      # axis: daily==0, or weekly==1
      if (input$B2ButtonSwitchX %% 2 == 0) {
        p <- p %>%
          # plot
          ggplot(aes(x=date, y=value, group=type, color=type)) +
          labs(
            x = "Date",
            y = "Number of new cases"
          )
      } else {
        p <- p %>%
          # get weekly summaries
          mutate(week = lubridate::week(date+1)) %>% #+1 to align lubridate week w/ ggplot scale_x_date
          group_by(iso3c, week, type) %>%
          mutate(
            week_beginning = min(date),
            week_ending = max(date),
            days_in_week = (interval(min(date),max(date)) %/% days(1))+1,
            value_sum = sum(value)
          ) %>%
          ungroup() %>%
          # filter: only include data where a full week is available
          filter(days_in_week == 7) %>%
          # plot
          ggplot(aes(x=week_ending, y=value_sum, group=type, color=type)) +
          labs(
            x = "Week ending",
            y = "Total number of new cases"
          )
      }

      # continue with plot options
      p <- p +
      geom_point() +
      geom_line() +
      
      # formatting
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_breaks = "weeks", date_labels = "%d-%b-%y") +
      ## need to be consistent with legend colours (i.e. confirmed always "red")
      scale_color_manual(
        breaks = c("confirmed_new", "active_new", "recovered_new", "deaths_new"),
        values=c("#F8766D", "#C77CFF", "#00BA42", "#000000")
      ) +
      theme_bw() +
      theme(
        legend.title = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa"),
        legend.key = element_rect(fill = "#fafafa")
      )
    
      # plotly
      pl <- ggplotly(p) %>% 
        layout(
          legend = list(
            orientation = "h", # show entries horizontally
            yanchor = "bottom",
            xanchor = "center",  # use center of legend as anchor
            x = 0.5 # put legend in center of x-axis
          ), # legend
          xaxis = list(
            rangeslider = list(type = "date")
          ) # xaxis
        ) # layout
    
      # return
      pl
    
  }) # p1 
  
  ## B3Plot -----------------------------------------------------------------Z-B3

  output$B3DataTable <- DT::renderDataTable({
    
    p <- as.data.frame(data()) %>%
      
      # can view only one country at a time in section B
      filter(iso3c == input$BSelectCountry) %>%
      
      # reduce dataset size for quicker processing
      select(date, country, confirmed, confirmed_new, active, active_new, recovered, recovered_new, deaths, deaths_new)
    
    # return
    p
    
  })
  

  
} # server -------------------------------------------------------------------END


shinyApp(ui = ui, server = server)