library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)

# Load Population Data
population_data <- data.frame(
  County = c("Atlantic County", "Bergen County", "Burlington County", "Camden County", 
             "Cape May County", "Cumberland County", "Essex County", "Gloucester County", 
             "Hudson County", "Hunterdon County", "Mercer County", "Middlesex County", 
             "Monmouth County", "Morris County", "Ocean County", "Passaic County", 
             "Salem County", "Somerset County", "Sussex County", "Union County", 
             "Warren County"),
  `2020` = c(274190, 953690, 461682, 523122, 95044, 153719, 859974, 302563, 721879, 
             128786, 386466, 861408, 642836, 508439, 638465, 523439, 64841, 344754, 
             143915, 573660, 109520),
  `2021` = c(274956, 955383, 464479, 524093, 95706, 152083, 854233, 304592, 703448, 
             129671, 382172, 862364, 646517, 510613, 649825, 518376, 65045, 346498, 
             145721, 572833, 110567),
  `2022` = c(275382, 953540, 466101, 524649, 95405, 151347, 849724, 306767, 702381, 
             129805, 380779, 861094, 644228, 511219, 655663, 513634, 65157, 347047, 
             145575, 570417, 110903),
  `2023` = c(275213, 957736, 469167, 527196, 94610, 152326, 851117, 308423, 705472, 
             130183, 381671, 863623, 642799, 514423, 659197, 513395, 65338, 348842, 
             146132, 572726, 111252)
)

# Load Diabetes Data
nj_diabetes_data <- read.csv("combined_data.csv", check.names = FALSE) %>%
  mutate(
    `20-44 - Number` = as.numeric(`20-44 - Number`),
    `45-64 - Number` = as.numeric(`45-64 - Number`),
    `65+ - Number` = as.numeric(`65+ - Number`)
  )

# Function to fill in missing 2019 data using average of 2018 and 2020
fill_missing_2019 <- function(data) {
  data %>%
    group_by(County) %>%
    mutate(
      `20-44 - Number` = ifelse(Year == 2019 & is.na(`20-44 - Number`),
                                (lag(`20-44 - Number`, 1) + lead(`20-44 - Number`, 1)) / 2,
                                `20-44 - Number`),
      `45-64 - Number` = ifelse(Year == 2019 & is.na(`45-64 - Number`),
                                (lag(`45-64 - Number`, 1) + lead(`45-64 - Number`, 1)) / 2,
                                `45-64 - Number`),
      `65+ - Number` = ifelse(Year == 2019 & is.na(`65+ - Number`),
                              (lag(`65+ - Number`, 1) + lead(`65+ - Number`, 1)) / 2,
                              `65+ - Number`)
    ) %>%
    ungroup()
}

# Apply function to fill missing 2019 data
nj_diabetes_data <- fill_missing_2019(nj_diabetes_data)

# Merge Population Data with Diabetes Data
prepare_merged_data <- function(diabetes_data, population_data) {
  population_long <- population_data %>%
    pivot_longer(cols = starts_with("202"), names_to = "Year", values_to = "Population") %>%
    mutate(Year = as.numeric(Year))
  
  merged_data <- diabetes_data %>%
    left_join(population_long, by = c("County" = "County", "Year" = "Year")) %>%
    mutate(
      Cases_Per_Capita = (`20-44 - Number` + `45-64 - Number` + `65+ - Number`) / Population
    )
  return(merged_data)
}

# Prepare merged data
merged_data <- prepare_merged_data(nj_diabetes_data, population_data)

# Define the UI
ui <- page_sidebar(
  title = "NJ Health Lense (Population Adjusted)",
  sidebar = sidebar(
    selectInput("view", "Select View:", 
                choices = c("Summary", "Age Distribution", "County Table", "Trends", 
                            "Statistics", "Rankings", "Top Counties")),
    conditionalPanel(
      condition = "input.view != 'Summary'",
      selectInput("year", "Select Year(s):",
                  choices = sort(unique(merged_data$Year)),
                  multiple = TRUE,
                  selected = unique(merged_data$Year)),
      selectInput("county", "Select County:", 
                  choices = unique(merged_data$County),
                  multiple = TRUE,
                  selected = unique(merged_data$County)[1])
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
    merged_data %>%
      filter(Year %in% input$year, County %in% input$county)
  })
  
  # Render dynamic UI based on dropdown selection
  output$dynamic_ui <- renderUI({
    switch(input$view,
           "Summary" = tagList(
             h3("Dataset Overview"),
             tableOutput("summary_table"),
             plotOutput("summary_trends")
           ),
           "Statistics" = tableOutput("stats_table"),
           "Rankings" = plotOutput("rankings_chart"),
           "Top Counties" = plotOutput("top_counties_chart"))
  })
  
  # Summary Table
  output$summary_table <- renderTable({
    merged_data %>%
      group_by(Year) %>%
      summarise(
        Total_Cases = sum(`20-44 - Number`, `45-64 - Number`, `65+ - Number`, na.rm = TRUE),
        Avg_Cases_Per_Capita = mean(Cases_Per_Capita, na.rm = TRUE)
      )
  })
  
  # Rankings Chart
  output$rankings_chart <- renderPlot({
    filtered_data() %>%
      group_by(County) %>%
      summarise(
        Total_Cases = sum(`20-44 - Number`, `45-64 - Number`, `65+ - Number`, na.rm = TRUE),
        Cases_Per_Capita = mean(Cases_Per_Capita, na.rm = TRUE)
      ) %>%
      arrange(desc(Cases_Per_Capita)) %>%
      ggplot(aes(x = reorder(County, -Cases_Per_Capita), y = Cases_Per_Capita, fill = Cases_Per_Capita)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(
        x = "County",
        y = "Cases Per Capita",
        title = "County Rankings by Cases Per Capita"
      )
  })
}

# Run the app
shinyApp(ui, server)
