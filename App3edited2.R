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
    `20-44 - Number` = ifelse(Year == 2019, round((lag(`20-44 - Number`) + lead(`20-44 - Number`)) / 2), `20-44 - Number`),
    `45-64 - Number` = ifelse(Year == 2019, round((lag(`45-64 - Number`) + lead(`45-64 - Number`)) / 2), `45-64 - Number`),
    `65+ - Number` = ifelse(Year == 2019, round((lag(`65+ - Number`) + lead(`65+ - Number`)) / 2), `65+ - Number`)
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
    Year = as.integer(Year)
  )

# Define UI
ui <- fluidPage(
  titlePanel("NJ Diabetes Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = sort(unique(nj_diabetes_data$Year)), selected = max(nj_diabetes_data$Year)),
      selectInput("county", "Select County:", choices = unique(nj_diabetes_data$County), selected = unique(nj_diabetes_data$County)[1])
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Per Capita Trends", plotOutput("trends_chart")),
        tabPanel("County Table", DTOutput("county_table"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    nj_diabetes_data %>%
      filter(Year == input$year, County == input$county)
  })
  
  output$trends_chart <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = Year, y = Total_Rate, color = County)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = "Diabetes Rate per 100,000 Population", x = "Year", y = "Rate per 100,000 Population")
  })
  
  output$county_table <- renderDT({
    nj_diabetes_data %>%
      filter(Year == input$year) %>%
      select(County, Year, Population, Total_Cases, Total_Rate) %>%
      arrange(desc(Total_Rate))
  })
}

shinyApp(ui, server)
