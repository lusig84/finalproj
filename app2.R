library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)

# Read the actual CSV file
nj_diabetes_data <- read.csv("~/Desktop/SOC360/HW/finalproj/App/combined_data.csv", check.names = FALSE)
nj_diabetes_data <- nj_diabetes_data %>%
  mutate(
    `20-44 - Number` = as.numeric(`20-44 - Number`),
    `45-64 - Number` = as.numeric(`45-64 - Number`),
    `65+ - Number` = as.numeric(`65+ - Number`)
  )

# Define the UI
ui <- page_sidebar(
  title = "NJ Health Lense",
  sidebar = sidebar(
    selectInput("view", "Select View:", 
                choices = c("Summary", "Age Distribution", "County Table", "Trends", "Statistics", "Rankings", "Heatmap", "Top Counties")),
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
           "Top Counties" = plotOutput("top_counties_chart"))
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
  
  # Age Distribution
  output$age_distribution <- renderPlot({
    req(filtered_data())
    data_long <- filtered_data() %>%
      select(Year, County, `20-44 - Number`, `45-64 - Number`, `65+ - Number`) %>%
      pivot_longer(
        cols = c(`20-44 - Number`, `45-64 - Number`, `65+ - Number`),
        names_to = "Age_Group",
        values_to = "Cases"
      ) %>%
      mutate(Age_Group = gsub(" - Number", "", Age_Group))
    
    ggplot(data_long, aes(x = Age_Group, y = Cases, fill = County)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(
        x = "Age Group",
        y = "Number of Diabetes Cases",
        title = paste("Diabetes Cases by Age Group -", paste(input$year, collapse = ", "))
      )
  })
  
  # County Table
  output$county_table <- renderDT({
    filtered_data() %>%
      select(
        Year,
        County,
        `20-44 Cases` = `20-44 - Number`,
        `45-64 Cases` = `45-64 - Number`,
        `65+ Cases` = `65+ - Number`
      )
  })
  
  # Trends
  output$trends_chart <- renderPlot({
    req(filtered_data())
    filtered_data() %>%
      ggplot(aes(x = Year, y = `20-44 - Number`, group = County, color = County)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Number of Cases (20-44)",
        title = "Trends Over Time: Cases by Year"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Statistics
  output$stats_table <- renderTable({
    nj_diabetes_data %>%
      group_by(County) %>%
      summarise(
        `Mean Cases (20-44)` = round(mean(`20-44 - Number`, na.rm = TRUE), 1),
        `Median Cases (20-44)` = median(`20-44 - Number`, na.rm = TRUE),
        `Mean Cases (45-64)` = round(mean(`45-64 - Number`, na.rm = TRUE), 1),
        `Median Cases (45-64)` = median(`45-64 - Number`, na.rm = TRUE),
        `Mean Cases (65+)` = round(mean(`65+ - Number`, na.rm = TRUE), 1),
        `Median Cases (65+)` = median(`65+ - Number`, na.rm = TRUE)
      )
  })
  
  # Rankings
  output$rankings_chart <- renderPlot({
    nj_diabetes_data %>%
      group_by(County) %>%
      summarise(Total_Cases = sum(`20-44 - Number`, `45-64 - Number`, `65+ - Number`, na.rm = TRUE)) %>%
      arrange(desc(Total_Cases)) %>%
      ggplot(aes(x = reorder(County, -Total_Cases), y = Total_Cases, fill = Total_Cases)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(
        x = "County",
        y = "Total Cases",
        title = "County Rankings by Total Cases"
      ) +
      scale_fill_gradient(low = "lightblue", high = "blue")
  })
  
  # Heatmap
  output$heatmap <- renderPlot({
    nj_diabetes_data %>%
      pivot_longer(
        cols = c(`20-44 - Number`, `45-64 - Number`, `65+ - Number`),
        names_to = "Age_Group",
        values_to = "Cases"
      ) %>%
      ggplot(aes(x = reorder(County, -Cases), y = Age_Group, fill = Cases)) +
      geom_tile(color = "white") +
      theme_minimal() +
      labs(
        x = "County",
        y = "Age Group",
        title = "Heatmap of Diabetes Cases by Age and County"
      ) +
      scale_fill_gradient(low = "white", high = "red") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
      )
  })
  
  # Top Counties
  output$top_counties_chart <- renderPlot({
    nj_diabetes_data %>%
      group_by(County) %>%
      summarise(Total_Cases = sum(`20-44 - Number`, `45-64 - Number`, `65+ - Number`, na.rm = TRUE)) %>%
      arrange(desc(Total_Cases)) %>%
      slice(1:10) %>%
      ggplot(aes(x = reorder(County, -Total_Cases), y = Total_Cases, fill = County)) +
      geom_col() +
      theme_minimal() +
      labs(
        x = "County",
        y = "Total Cases",
        title = "Top 10 Counties with Most Diabetes Cases"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the app
shinyApp(ui, server)
