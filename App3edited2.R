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

# Calculate state-level statistics
state_stats <- nj_diabetes_data %>%
  group_by(Year) %>%
  summarise(
    Total_Population = sum(Population),
    Total_State_Cases = sum(Total_Cases, na.rm = TRUE),
    State_Rate = (Total_State_Cases / Total_Population) * 100000,
    .groups = 'drop'
  )

# Define UI
ui <- page_sidebar(
  title = "New Jersey Diabetes Analytics Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Controls",
    selectInput("view", "Select View:", 
                choices = c("Executive Summary" = "Summary",
                            "Age Distribution Analysis" = "Age Distribution",
                            "County-Level Data" = "County Table",
                            "Temporal Trends" = "Trends",
                            "Statistical Overview" = "Statistics",
                            "County Rankings" = "Rankings",
                            "Age-County Heatmap" = "Heatmap",
                            "Top Counties Analysis" = "Top Counties")),
    
    conditionalPanel(
      condition = "input.view != 'Summary'",
      selectInput("year", "Select Year(s):",
                  choices = sort(unique(nj_diabetes_data$Year)),
                  multiple = TRUE,
                  selected = max(nj_diabetes_data$Year))
    ),
    
    conditionalPanel(
      condition = "input.view != 'Summary' && input.view != 'Rankings' && input.view != 'Top Counties'",
      selectInput("county", "Select County:", 
                  choices = unique(nj_diabetes_data$County),
                  multiple = TRUE,
                  selected = unique(nj_diabetes_data$County)[1])
    ),
    
    radioButtons("metric", "Display Metric:",
                 choices = c("Raw Numbers" = "numbers", 
                             "Per 100,000 Population" = "per_capita"),
                 selected = "per_capita"),
    
    checkboxInput("compare_state", "Compare with State Average", FALSE),
    
    hr(),
    
    helpText("Data represents diabetes cases across New Jersey counties.",
             "Population figures are based on 2020 census data.")
  ),
  
  mainPanel(
    uiOutput("dynamic_ui")
  )
)

# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$year)
    data <- nj_diabetes_data
    
    if (!is.null(input$county)) {
      data <- data %>% filter(County %in% input$county)
    }
    
    data <- data %>% filter(Year %in% input$year)
    
    if(nrow(data) == 0) {
      return(NULL) # or display a message if no data
    }
    
    return(data)
  })
  
  output$dynamic_ui <- renderUI({
    switch(input$view,
           "Summary" = tableOutput("summary_table"),
           "Trends" = plotOutput("trends_chart"),
           "Rankings" = plotOutput("rankings_chart")
    )
  })
  
  # Enhanced summary table
  output$summary_table <- renderTable({
    if (input$metric == "numbers") {
      filtered_data() %>%
        summarise(
          Total_Cases = sum(Total_Cases, na.rm = TRUE),
          .groups = 'drop'
        )
    } else {
      filtered_data() %>%
        summarise(
          Total_Rate = sum(Total_Rate, na.rm = TRUE),
          .groups = 'drop'
        )
    }
  })
  
  # Trends chart
  output$trends_chart <- renderPlot({
    metric_col <- if (input$metric == "numbers") "Total_Cases" else "Total_Rate"
    filtered_data() %>%
      ggplot(aes(x = Year, y = !!sym(metric_col), color = County)) +
      geom_line() +
      geom_point() +
      theme_minimal()
  })
}

shinyApp(ui, server)
