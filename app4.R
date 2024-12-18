library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(stringr)

# Read the actual CSV file
nj_diabetes_data <- read.csv("combined_data.csv", check.names = FALSE)
nj_diabetes_data <- nj_diabetes_data %>%
  mutate(
    `20-44 - Number` = as.numeric(`20-44 - Number`),
    `45-64 - Number` = as.numeric(`45-64 - Number`),
    `65+ - Number` = as.numeric(`65+ - Number`)
  )

# Create population data
population_data <- data.frame(
  County = c("Atlantic County", "Bergen County", "Burlington County", "Camden County", 
             "Cape May County", "Cumberland County", "Essex County", "Gloucester County", 
             "Hudson County", "Hunterdon County", "Mercer County", "Middlesex County", 
             "Monmouth County", "Morris County", "Ocean County", "Passaic County", 
             "Salem County", "Somerset County", "Sussex County", "Union County", "Warren County"),
  pop_2020 = c(274536, 955743, 461869, 523498, 95262, 154160, 862768, 302273, 
               724858, 128950, 387328, 863202, 643612, 509288, 637235, 525054, 
               64830, 345353, 144220, 575363, 109637)
) %>%
  mutate(
    pop_2021 = pop_2020 * 1.005,
    pop_2022 = pop_2021 * 1.005,
    pop_2023 = pop_2022 * 1.005
  ) %>%
  pivot_longer(
    cols = starts_with("pop_"),
    names_to = "Year",
    values_to = "Population"
  ) %>%
  mutate(
    Year = as.integer(str_extract(Year, "\\d+"))
  )

# Merge diabetes data with population data
nj_diabetes_data <- nj_diabetes_data %>%
  inner_join(population_data, by = c("County", "Year")) %>%
  mutate(
    `20-44 Rate` = (`20-44 - Number` / Population) * 100000,
    `45-64 Rate` = (`45-64 - Number` / Population) * 100000,
    `65+ Rate` = (`65+ - Number` / Population) * 100000,
    Total_Cases = `20-44 - Number` + `45-64 - Number` + `65+ - Number`,
    Total_Rate = (Total_Cases / Population) * 100000
  )

# Define the UI
ui <- page_sidebar(
  title = "NJ Health Lens",
  sidebar = sidebar(
    selectInput("view", "Select View:", 
                choices = c("Summary", "Age Distribution", "County Table", "Trends", 
                            "Statistics", "Rankings", "Heatmap", "Top Counties")),
    radioButtons("metric", "Display Metric:",
                 choices = c("Raw Numbers" = "numbers", 
                             "Per Capita (per 100,000)" = "per_capita"),
                 selected = "numbers"),
    conditionalPanel(
      condition = "input.view != 'Summary'",
      selectInput("year", "Select Year(s):",
                  choices = sort(unique(nj_diabetes_data$Year)),
                  multiple = TRUE,
                  selected = max(nj_diabetes_data$Year)),
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

server <- function(input, output) {
  filtered_data <- reactive({
    req(input$year, input$county)
    nj_diabetes_data %>%
      filter(Year %in% input$year, County %in% input$county)
  })
  
  output$dynamic_ui <- renderUI({
    switch(input$view,
           "Summary" = tagList(
             card(
               card_header("Dataset Overview"),
               p("This section highlights key statistics and trends across all counties and years."),
               tableOutput("summary_table"),
               plotOutput("summary_trends")
             )
           ),
           "Age Distribution" = card(
             card_header("Age Distribution"),
             plotOutput("age_distribution")
           ),
           "County Table" = card(
             card_header("County Data"),
             DTOutput("county_table")
           ),
           "Trends" = card(
             card_header("Trends Over Time"),
             plotOutput("trends_chart")
           ),
           "Statistics" = card(
             card_header("Statistical Summary"),
             tableOutput("stats_table")
           ),
           "Rankings" = card(
             card_header("County Rankings"),
             plotOutput("rankings_chart")
           ),
           "Heatmap" = card(
             card_header("Heatmap Analysis"),
             plotOutput("heatmap")
           ),
           "Top Counties" = card(
             card_header("Top Counties"),
             plotOutput("top_counties_chart")
           ))
  })
  
  output$summary_table <- renderTable({
    if (input$metric == "numbers") {
      nj_diabetes_data %>%
        group_by(Year) %>%
        summarise(
          `Total Cases (20-44)` = sum(`20-44 - Number`, na.rm = TRUE),
          `Total Cases (45-64)` = sum(`45-64 - Number`, na.rm = TRUE),
          `Total Cases (65+)` = sum(`65+ - Number`, na.rm = TRUE),
          `Total Cases (All Ages)` = sum(Total_Cases, na.rm = TRUE)
        )
    } else {
      nj_diabetes_data %>%
        group_by(Year) %>%
        summarise(
          Population = sum(Population),
          Total_Cases = sum(Total_Cases, na.rm = TRUE),
          `Rate per 100,000` = (Total_Cases / Population) * 100000
        )
    }
  })
  
  output$summary_trends <- renderPlot({
    if (input$metric == "numbers") {
      nj_diabetes_data %>%
        group_by(Year) %>%
        summarise(Value = sum(Total_Cases, na.rm = TRUE)) %>%
        ggplot(aes(x = Year, y = Value)) +
        geom_line(size = 1.5, color = "steelblue") +
        geom_point(size = 3, color = "orange") +
        theme_minimal() +
        labs(x = "Year", y = "Total Cases",
             title = "Total Diabetes Cases Over Time")
    } else {
      nj_diabetes_data %>%
        group_by(Year) %>%
        summarise(
          Value = (sum(Total_Cases, na.rm = TRUE) / sum(Population)) * 100000
        ) %>%
        ggplot(aes(x = Year, y = Value)) +
        geom_line(size = 1.5, color = "steelblue") +
        geom_point(size = 3, color = "orange") +
        theme_minimal() +
        labs(x = "Year", y = "Cases per 100,000 Population",
             title = "Diabetes Rate Over Time")
    }
  })
  
  output$age_distribution <- renderPlot({
    if (input$metric == "numbers") {
      filtered_data() %>%
        select(Year, County, `20-44 - Number`, `45-64 - Number`, `65+ - Number`) %>%
        pivot_longer(
          cols = c(`20-44 - Number`, `45-64 - Number`, `65+ - Number`),
          names_to = "Age_Group",
          values_to = "Value"
        ) %>%
        mutate(Age_Group = str_remove(Age_Group, " - Number")) %>%
        ggplot(aes(x = Age_Group, y = Value, fill = County)) +
        geom_col(position = "dodge") +
        theme_minimal() +
        labs(x = "Age Group", y = "Number of Cases",
             title = paste("Diabetes Cases by Age Group -", 
                           paste(input$year, collapse = ", ")))
    } else {
      filtered_data() %>%
        select(Year, County, `20-44 Rate`, `45-64 Rate`, `65+ Rate`) %>%
        pivot_longer(
          cols = c(`20-44 Rate`, `45-64 Rate`, `65+ Rate`),
          names_to = "Age_Group",
          values_to = "Value"
        ) %>%
        mutate(Age_Group = str_remove(Age_Group, " Rate")) %>%
        ggplot(aes(x = Age_Group, y = Value, fill = County)) +
        geom_col(position = "dodge") +
        theme_minimal() +
        labs(x = "Age Group", y = "Rate per 100,000 Population",
             title = paste("Age-Specific Diabetes Rates -", 
                           paste(input$year, collapse = ", ")))
    }
  })
  
  output$county_table <- renderDT({
    if (input$metric == "numbers") {
      filtered_data() %>%
        select(
          Year, County, Population,
          `20-44 Cases` = `20-44 - Number`,
          `45-64 Cases` = `45-64 - Number`,
          `65+ Cases` = `65+ - Number`,
          Total_Cases
        )
    } else {
      filtered_data() %>%
        select(
          Year, County, Population,
          `20-44 Rate`,
          `45-64 Rate`,
          `65+ Rate`,
          Total_Rate
        )
    }
  })
  
  output$trends_chart <- renderPlot({
    if (input$metric == "numbers") {
      filtered_data() %>%
        ggplot(aes(x = Year, y = Total_Cases, group = County, color = County)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(x = "Year", y = "Total Cases",
             title = "Trends Over Time: Total Cases")
    } else {
      filtered_data() %>%
        ggplot(aes(x = Year, y = Total_Rate, group = County, color = County)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(x = "Year", y = "Cases per 100,000 Population",
             title = "Trends Over Time: Diabetes Rates")
    }
  })
  
  output$stats_table <- renderTable({
    if (input$metric == "numbers") {
      nj_diabetes_data %>%
        group_by(County) %>%
        summarise(
          Mean_Population = mean(Population),
          Mean_Cases = mean(Total_Cases, na.rm = TRUE),
          Median_Cases = median(Total_Cases, na.rm = TRUE),
          Min_Cases = min(Total_Cases, na.rm = TRUE),
          Max_Cases = max(Total_Cases, na.rm = TRUE)
        ) %>%
        arrange(desc(Mean_Cases))
    } else {
      nj_diabetes_data %>%
        group_by(County) %>%
        summarise(
          Mean_Population = mean(Population),
          Mean_Rate = mean(Total_Rate, na.rm = TRUE),
          Median_Rate = median(Total_Rate, na.rm = TRUE),
          Min_Rate = min(Total_Rate, na.rm = TRUE),
          Max_Rate = max(Total_Rate, na.rm = TRUE)
        ) %>%
        arrange(desc(Mean_Rate))
    }
  })
  
  output$rankings_chart <- renderPlot({
    if (input$metric == "numbers") {
      nj_diabetes_data %>%
        group_by(County) %>%
        summarise(
          Avg_Cases = mean(Total_Cases, na.rm = TRUE),
          Avg_Population = mean(Population)
        ) %>%
        arrange(desc(Avg_Cases)) %>%
        ggplot(aes(x = reorder(County, -Avg_Cases), y = Avg_Cases, fill = Avg_Population)) +
        geom_col() +
        coord_flip() +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(
          x = "County",
          y = "Average Total Cases",
          title = "County Rankings by Total Cases",
          fill = "Population"
        )
    } else {
      nj_diabetes_data %>%
        group_by(County) %>%
        summarise(
          Avg_Rate = mean(Total_Rate, na.rm = TRUE),
          Avg_Population = mean(Population)
        ) %>%
        arrange(desc(Avg_Rate)) %>%
        ggplot(aes(x = reorder(County, -Avg_Rate), y = Avg_Rate, fill = Avg_Population)) +
        geom_col() +
        coord_flip() +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(
          x = "County",
          y = "Average Rate per 100,000 Population",
          title = "County Rankings by Diabetes Rate",
          fill = "Population"
        )
    }
  })
  
  output$heatmap <- renderPlot({
    if (input$metric == "numbers") {
      filtered_data() %>%
        select(County, `20-44 - Number`, `45-64 - Number`, `65+ - Number`) %>%
        pivot_longer(
          cols = c(`20-44 - Number`, `45-64 - Number`, `65+ - Number`),
          names_to = "Age_Group",
          values_to = "Cases"
        ) %>%
        mutate(Age_Group = str_remove(Age_Group, " - Number")) %>%
        ggplot(aes(x = reorder(County, -Cases), y = Age_Group, fill = Cases)) +
        geom_tile(color = "white") +
        scale_fill_viridis_c() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
          title = "Heatmap of Diabetes Cases by Age and County",
          x = "County",
          y = "Age Group",
          fill = "Cases"
        )
    } else {
      filtered_data() %>%
        select(County, `20-44 Rate`, `45-64 Rate`, `65+ Rate`) %>%
        pivot_longer(
          cols = c(`20-44 Rate`, `45-64 Rate`, `65+ Rate`),
          names_to = "Age_Group",
          values_to = "Rate"
        ) %>%
        ggplot(aes(x = reorder(County, -Rate), y = Age_Group, fill = Rate)) +
        geom_tile(color = "white") +
        scale_fill_viridis_c() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
          title = "Heatmap of Diabetes Rates by Age and County",
          x = "County",
          y = "Age Group",
          fill = "Rate per 100,000"
        )
    }
  })
  
  output$top_counties_chart <- renderPlot({
    if (input$metric == "numbers") {
      nj_diabetes_data %>%
        group_by(County) %>%
        summarise(
          Avg_Cases = mean(Total_Cases, na.rm = TRUE)
        ) %>%
        arrange(desc(Avg_Cases)) %>%
        slice(1:10) %>%
        ggplot(aes(x = reorder(County, -Avg_Cases), y = Avg_Cases, fill = County)) +
        geom_col() +
        theme_minimal() +
        labs(
          x = "County",
          y = "Average Total Cases",
          title = "Top 10 Counties by Total Cases"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      nj_diabetes_data %>%
        group_by(County) %>%
        summarise(
          Avg_Rate = mean(Total_Rate, na.rm = TRUE)
        ) %>%
        arrange(desc(Avg_Rate)) %>%
        slice(1:10) %>%
        ggplot(aes(x = reorder(County, -Avg_Rate), y = Avg_Rate, fill = County)) +
        geom_col() +
        theme_minimal() +
        labs(
          x = "County",
          y = "Average Rate per 100,000 Population",
          title = "Top 10 Counties by Diabetes Rate"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

shinyApp(ui, server)