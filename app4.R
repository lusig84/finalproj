library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(stringr)
library(readxl)

# Read the actual CSV file
nj_diabetes_data <- read.csv("combined_data.csv", check.names = FALSE)

# Read the population data
population_data <- read_excel("co-est2023-pop-34.xlsx", skip = 3)

# Clean and prepare population data
population_data <- population_data %>%
  setNames(make.names(names(.), unique = TRUE)) %>%
  select(1, 3:6) %>%  # Select only needed columns
  rename(
    County = 1,
    `2020` = 2,
    `2021` = 3,
    `2022` = 4,
    `2023` = 5
  ) %>%
  filter(str_detect(County, "County")) %>%
  mutate(County = str_remove(County, "^.*?\\.\\s*")) %>%  # Remove leading numbers and dots
  pivot_longer(cols = `2020`:`2023`, 
               names_to = "Year", 
               values_to = "Population") %>%
  mutate(Year = as.integer(Year))

# Clean diabetes data
nj_diabetes_data <- nj_diabetes_data %>%
  mutate(
    across(matches("Number$"), as.numeric),
    Year = as.integer(Year)
  )

# Merge datasets
combined_data <- nj_diabetes_data %>%
  inner_join(population_data, by = c("County", "Year")) %>%
  mutate(
    `20-44 Rate` = (`20-44 - Number` / Population) * 100000,
    `45-64 Rate` = (`45-64 - Number` / Population) * 100000,
    `65+ Rate` = (`65+ - Number` / Population) * 100000,
    Total_Cases = `20-44 - Number` + `45-64 - Number` + `65+ - Number`,
    Total_Rate = (Total_Cases / Population) * 100000
  )

ui <- page_sidebar(
  title = "NJ Health Dashboard",
  sidebar = sidebar(
    selectInput("view", "Select View:", 
                choices = c("Summary", "Age Distribution", "County Table", "Rates Analysis")),
    conditionalPanel(
      condition = "input.view != 'Summary'",
      selectInput("year", "Select Year:",
                  choices = sort(unique(combined_data$Year)),
                  selected = max(combined_data$Year)),
      selectInput("county", "Select Counties:", 
                  choices = unique(combined_data$County),
                  multiple = TRUE,
                  selected = unique(combined_data$County)[1:3])
    )
  ),
  mainPanel(
    uiOutput("dynamic_ui")
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    req(input$year, input$county)
    combined_data %>%
      filter(Year == input$year, County %in% input$county)
  })
  
  output$dynamic_ui <- renderUI({
    switch(input$view,
           "Summary" = tagList(
             card(
               card_header("Overview"),
               tableOutput("summary_table")
             ),
             card(
               card_header("Trends"),
               plotOutput("summary_trends")
             )
           ),
           "Age Distribution" = card(
             card_header("Age Distribution Analysis"),
             plotOutput("age_distribution")
           ),
           "County Table" = card(
             card_header("Detailed County Data"),
             DTOutput("county_table")
           ),
           "Rates Analysis" = card(
             card_header("Population-Adjusted Rates"),
             plotOutput("rates_analysis")
           )
    )
  })
  
  # Summary Table
  output$summary_table <- renderTable({
    combined_data %>%
      group_by(Year) %>%
      summarise(
        Total_Population = sum(Population),
        Total_Cases = sum(Total_Cases, na.rm = TRUE),
        Average_Rate = mean(Total_Rate, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(Year))
  })
  
  # Summary Trends
  output$summary_trends <- renderPlot({
    combined_data %>%
      group_by(Year) %>%
      summarise(
        Total_Rate = mean(Total_Rate, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      ggplot(aes(x = Year, y = Total_Rate)) +
      geom_line(size = 1.2, color = "steelblue") +
      geom_point(size = 3, color = "orange") +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Average Rate per 100,000 Population",
        title = "Diabetes Rates Over Time"
      )
  })
  
  # Age Distribution
  output$age_distribution <- renderPlot({
    filtered_data() %>%
      select(County, `20-44 Rate`, `45-64 Rate`, `65+ Rate`) %>%
      pivot_longer(
        cols = c(`20-44 Rate`, `45-64 Rate`, `65+ Rate`),
        names_to = "Age_Group",
        values_to = "Rate"
      ) %>%
      mutate(Age_Group = str_remove(Age_Group, " Rate")) %>%
      ggplot(aes(x = Age_Group, y = Rate, fill = County)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      labs(
        x = "Age Group",
        y = "Rate per 100,000 Population",
        title = paste("Age-Specific Diabetes Rates -", input$year)
      ) +
      theme(legend.position = "bottom")
  })
  
  # County Table
  output$county_table <- renderDT({
    filtered_data() %>%
      select(
        County,
        Population,
        `20-44 Cases` = `20-44 - Number`,
        `20-44 Rate`,
        `45-64 Cases` = `45-64 - Number`,
        `45-64 Rate`,
        `65+ Cases` = `65+ - Number`,
        `65+ Rate`,
        Total_Cases,
        Total_Rate
      ) %>%
      arrange(desc(Total_Rate))
  })
  
  # Rates Analysis
  output$rates_analysis <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = reorder(County, -Total_Rate), y = Total_Rate)) +
      geom_col(aes(fill = Population)) +
      scale_fill_viridis_c() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        x = "County",
        y = "Rate per 100,000 Population",
        title = paste("Population-Adjusted Diabetes Rates -", input$year),
        fill = "Population"
      )
  })
}

shinyApp(ui, server)