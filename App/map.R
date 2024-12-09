library(sf)
library(tigris)
library(viridis)
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)

# Read the actual CSV file
nj_diabetes_data <- read.csv("~/Desktop/SOC360/HW/finalproj/Data/combined_data.csv", check.names = FALSE)
nj_diabetes_data <- nj_diabetes_data %>%
  mutate(
    `20-44 - Number` = as.numeric(`20-44 - Number`),
    `45-64 - Number` = as.numeric(`45-64 - Number`),
    `65+ - Number` = as.numeric(`65+ - Number`)
  )

# Get NJ county boundaries
nj_counties <- counties(state = "NJ", cb = TRUE) %>%
  st_transform(crs = 4326)

# Create a function to prepare map data
prepare_map_data <- function(data, year) {
  # Calculate total cases per county for the selected year
  county_totals <- data %>%
    filter(Year == year) %>%
    group_by(County) %>%
    summarise(Total_Cases = sum(`20-44 - Number`, `45-64 - Number`, `65+ - Number`, na.rm = TRUE))
  
  # Convert county names to uppercase to match with spatial data
  county_totals$County <- toupper(county_totals$County)
  
  # Join with spatial data
  nj_counties %>%
    left_join(county_totals, by = c("NAME" = "County"))
}

# Define the UI
ui <- page_sidebar(
  title = "Interactive New Jersey Diabetes Statistics App",
  sidebar = sidebar(
    selectInput("view", "Select View:", 
                choices = c("Summary", "Age Distribution", "County Table", "Trends", "Statistics", "Rankings", "Heatmap", "Top Counties", "Map")),
    conditionalPanel(
      condition = "input.view != 'Summary'",
      selectInput("year", "Select Year(s):",
                  choices = sort(unique(nj_diabetes_data$Year)),
                  multiple = TRUE,
                  selected = unique(nj_diabetes_data$Year)),
      selectInput("county", "Select County:", 
                  choices = unique(nj_diabetes_data$County),
                  multiple = TRUE,
                  selected = unique(nj_diabetes_data$County)[1])
    ),
    conditionalPanel(
      condition = "input.view == 'Map'",
      selectInput("map_year", "Select Year for Map:",
                  choices = sort(unique(nj_diabetes_data$Year)),
                  selected = max(nj_diabetes_data$Year))
    )
  ),
  mainPanel(
    uiOutput("dynamic_ui")
  )
)

# Define the Server
server <- function(input, output) {
  # Filtered dataset based on user selections
  filtered_data <- reactive({
    req(input$year, input$county)
    nj_diabetes_data %>%
      filter(Year %in% input$year, County %in% input$county)
  })
  
  # Render dynamic UI based on dropdown selection
  output$dynamic_ui <- renderUI({
    switch(input$view,
           "Summary" = tagList(
             h3("Dataset Overview"),
             p("This section highlights key statistics and trends across all counties and years."),
             tableOutput("summary_table"),
             plotOutput("summary_trends")
           ),
           "Age Distribution" = plotOutput("age_distribution"),
           "County Table" = DTOutput("county_table"),
           "Trends" = plotOutput("trends_chart"),
           "Statistics" = tableOutput("stats_table"),
           "Rankings" = plotOutput("rankings_chart"),
           "Heatmap" = plotOutput("heatmap"),
           "Top Counties" = plotOutput("top_counties_chart"),
           "Map" = plotOutput("county_map", height = "600px"))
  })
  
  # Summary
  output$summary_table <- renderTable({
    nj_diabetes_data %>%
      summarise(
        `Total Cases (20-44)` = sum(`20-44 - Number`, na.rm = TRUE),
        `Total Cases (45-64)` = sum(`45-64 - Number`, na.rm = TRUE),
        `Total Cases (65+)` = sum(`65+ - Number`, na.rm = TRUE),
        `Total Cases (All Ages)` = sum(`20-44 - Number`, `45-64 - Number`, `65+ - Number`, na.rm = TRUE)
      )
  })
  
  output$summary_trends <- renderPlot({
    nj_diabetes_data %>%
      group_by(Year) %>%
      summarise(
        Total_Cases = sum(`20-44 - Number`, na.rm = TRUE) +
          sum(`45-64 - Number`, na.rm = TRUE) +
          sum(`65+ - Number`, na.rm = TRUE)
      ) %>%
      ggplot(aes(x = Year, y = Total_Cases)) +
      geom_line(size = 1.5, color = "steelblue") +
      geom_point(size = 3, color = "orange") +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Total Cases",
        title = "Total Diabetes Cases Over Time"
      )
  })
  
  # Map
  output$county_map <- renderPlot({
    req(input$map_year)
    map_data <- prepare_map_data(nj_diabetes_data, input$map_year)
    
    ggplot(map_data) +
      geom_sf(aes(fill = Total_Cases), color = "white") +
      scale_fill_viridis(option = "magma", 
                         name = "Total Cases",
                         labels = scales::comma) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      ) +
      labs(
        title = paste("New Jersey Diabetes Cases by County -", input$map_year),
        subtitle = "Darker colors indicate higher number of cases"
      )
  })
}

# Run the app
shinyApp(ui, server)

