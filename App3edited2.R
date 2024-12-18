library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(stringr)
library(scales)
library(viridis)
library(forcats)
library(plotly)
library(zoo)

# Read and process the data with better NA handling
nj_diabetes_data <- read.csv("combined_data.csv", check.names = FALSE, na.strings = c("", "NA", "N/A"))
nj_diabetes_data <- nj_diabetes_data %>%
  mutate(across(ends_with("Number"), ~as.numeric(gsub("[^0-9.-]", "", as.character(.)))))

# Fill missing 2019 data using the average of 2018 and 2020
nj_diabetes_data <- nj_diabetes_data %>%
  group_by(County) %>%
  mutate(
    `20-44 - Number` = ifelse(Year == 2019, round((lag(`20-44 - Number`) + lead(`20-44 - Number`)) / 2, 0), `20-44 - Number`),
    `45-64 - Number` = ifelse(Year == 2019, round((lag(`45-64 - Number`) + lead(`45-64 - Number`)) / 2, 0), `45-64 - Number`),
    `65+ - Number` = ifelse(Year == 2019, round((lag(`65+ - Number`) + lead(`65+ - Number`)) / 2, 0), `65+ - Number`)
  ) %>%
  ungroup()

# Population data
population_data <- data.frame(
  County = c("Atlantic County", "Bergen County", "Burlington County", "Camden County", 
             "Cape May County", "Cumberland County", "Essex County", "Gloucester County", 
             "Hudson County", "Hunterdon County", "Mercer County", "Middlesex County", 
             "Monmouth County", "Morris County", "Ocean County", "Passaic County", 
             "Salem County", "Somerset County", "Sussex County", "Union County", "Warren County"),
  Population = c(274536, 955743, 461869, 523498, 95262, 154160, 862768, 302273, 
                 724858, 128950, 387328, 863202, 643612, 509288, 637235, 525054, 
                 64830, 345353, 144220, 575363, 109637)
)

# Process and enhance the dataset
nj_diabetes_data <- nj_diabetes_data %>%
  left_join(population_data, by = "County") %>%
  mutate(
    `20-44 Rate` = (`20-44 - Number` / Population) * 100000,
    `45-64 Rate` = (`45-64 - Number` / Population) * 100000,
    `65+ Rate` = (`65+ - Number` / Population) * 100000,
    Total_Cases = `20-44 - Number` + `45-64 - Number` + `65+ - Number`,
    Total_Rate = (Total_Cases / Population) * 100000,
    Year = as.integer(Year),
    Population_Category = cut(Population, 
                              breaks = c(0, 200000, 500000, 800000, Inf),
                              labels = c("Small (<200k)", "Medium (200k-500k)", 
                                         "Large (500k-800k)", "Very Large (>800k)"))
  )

# Define UI
ui <- fluidPage(
  titlePanel("New Jersey Diabetes Analytics Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = sort(unique(nj_diabetes_data$Year)), selected = max(nj_diabetes_data$Year)),
      selectInput("county", "Select County:", choices = unique(nj_diabetes_data$County), selected = unique(nj_diabetes_data$County)[1]),
      radioButtons("metric", "Metric to Display:", choices = c("Raw Numbers" = "numbers", "Per Capita" = "per_capita"), selected = "per_capita")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summary_table")),
        tabPanel("Trends Chart", plotOutput("trends_chart")),
        tabPanel("County Rankings", plotOutput("rankings_chart"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$year)
    nj_diabetes_data %>% filter(Year == input$year & County == input$county)
  })
  
  # Render summary table
  output$summary_table <- renderDT({
    filtered_data() %>%
      select(
        County, Year, Population, 
        `20-44 Cases` = `20-44 - Number`,
        `45-64 Cases` = `45-64 - Number`,
        `65+ Cases` = `65+ - Number`,
        `20-44 Rate`, `45-64 Rate`, `65+ Rate`,
        Total_Cases, Total_Rate
      ) %>%
      arrange(desc(Total_Cases)) %>%
      mutate(across(starts_with("Rate"), round, 2))
  })
  
  # Render trends chart
  output$trends_chart <- renderPlot({
    req(input$metric)
    metric_col <- if (input$metric == "numbers") "Total_Cases" else "Total_Rate"
    
    nj_diabetes_data %>%
      group_by(Year) %>%
      summarise(Value = sum(get(metric_col), na.rm = TRUE), .groups = 'drop') %>%
      ggplot(aes(x = Year, y = Value)) +
      geom_line(size = 1.2, color = "steelblue") +
      geom_point(size = 3, color = "orange") +
      theme_minimal() +
      labs(
        title = if (input$metric == "numbers") "Total Cases Over Time" else "Diabetes Rate Over Time",
        x = "Year", 
        y = if (input$metric == "numbers") "Total Cases" else "Cases per 100,000 Population"
      )
  })
  
  # Render rankings chart
  output$rankings_chart <- renderPlot({
    req(input$year, input$metric)
    metric_col <- if (input$metric == "numbers") "Total_Cases" else "Total_Rate"
    
    nj_diabetes_data %>%
      filter(Year == input$year) %>%
      arrange(desc(get(metric_col))) %>%
      mutate(County = fct_reorder(County, get(metric_col))) %>%
      ggplot(aes(x = County, y = get(metric_col), fill = Population_Category)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(
        title = "County Rankings by Diabetes Metric",
        x = "County", 
        y = if (input$metric == "numbers") "Total Cases" else "Rate per 100,000 Population",
        fill = "Population Category"
      )
  })
}

shinyApp(ui, server)
