library(sf)
library(tigris)
library(viridis)
library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(DT)
library(ggplot2)
library(tidyr)

# Read the data
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
  county_totals <- data %>%
    filter(Year == year) %>%
    group_by(County) %>%
    summarise(Total_Cases = sum(`20-44 - Number`, `45-64 - Number`, `65+ - Number`, na.rm = TRUE))
  
  county_totals$County <- toupper(county_totals$County)
  
  nj_counties %>%
    left_join(county_totals, by = c("NAME" = "County"))
}

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "minty"),
  title = "Interactive New Jersey Diabetes Statistics App",
  sidebar = sidebar(
    selectInput("view", "Select View:", 
                choices = c("Summary", "Age Distribution", "County Table", "Trends", "Map")),
    conditionalPanel(
      condition = "input.view != 'Summary'",
      selectInput("year", "Select Year:",
                  choices = sort(unique(nj_diabetes_data$Year)),
                  multiple = FALSE,
                  selected = max(nj_diabetes_data$Year))
    )
  ),
  card(
    full_screen = TRUE,
    uiOutput("dynamic_ui")
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    req(input$year)
    nj_diabetes_data %>%
      filter(Year == input$year)
  })
  
  output$dynamic_ui <- renderUI({
    switch(input$view,
           "Summary" = tagList(
             card(
               card_header("Dataset Overview"),
               tableOutput("summary_table")
             )
           ),
           "Age Distribution" = card(
             card_header("Age Distribution by County"),
             plotOutput("age_distribution", height = "600px")
           ),
           "County Table" = card(
             card_header("County-Level Data"),
             DTOutput("county_table")
           ),
           "Trends" = card(
             card_header("Diabetes Cases Trends"),
             plotOutput("trends_chart", height = "600px")
           ),
           "Map" = card(
             card_header("Geographic Distribution"),
             leafletOutput("county_map", height = "600px")
           ))
  })
  
  # Summary
  output$summary_table <- renderTable({
    nj_diabetes_data %>%
      group_by(Year) %>%
      summarise(
        `Total Cases (20-44)` = sum(`20-44 - Number`, na.rm = TRUE),
        `Total Cases (45-64)` = sum(`45-64 - Number`, na.rm = TRUE),
        `Total Cases (65+)` = sum(`65+ - Number`, na.rm = TRUE),
        `Total Cases (All Ages)` = sum(`20-44 - Number`, `45-64 - Number`, `65+ - Number`, na.rm = TRUE)
      )
  })
  
  # Age Distribution
  output$age_distribution <- renderPlot({
    req(filtered_data())
    data_long <- filtered_data() %>%
      pivot_longer(
        cols = c(`20-44 - Number`, `45-64 - Number`, `65+ - Number`),
        names_to = "Age_Group",
        values_to = "Cases"
      ) %>%
      mutate(Age_Group = factor(Age_Group, 
                                levels = c(`20-44 - Number`, `45-64 - Number`, `65+ - Number`)))
    
    ggplot(data_long, aes(x = reorder(County, -Cases), y = Cases, fill = Age_Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis(discrete = TRUE) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = paste("Age Distribution by County -", input$year),
        x = "County",
        y = "Number of Cases"
      )
  })
  
  # County Table
  output$county_table <- renderDT({
    req(filtered_data())
    filtered_data() %>%
      arrange(County) %>%
      datatable(
        options = list(
          pageLength = 15,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      )
  })
  
  # Trends Chart
  output$trends_chart <- renderPlot({
    yearly_totals <- nj_diabetes_data %>%
      group_by(Year) %>%
      summarise(
        `20-44` = sum(`20-44 - Number`, na.rm = TRUE),
        `45-64` = sum(`45-64 - Number`, na.rm = TRUE),
        `65+` = sum(`65+ - Number`, na.rm = TRUE)
      ) %>%
      pivot_longer(
        cols = c(`20-44`, `45-64`, `65+`),
        names_to = "Age_Group",
        values_to = "Cases"
      )
    
    ggplot(yearly_totals, aes(x = Year, y = Cases, color = Age_Group, group = Age_Group)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_viridis(discrete = TRUE) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      labs(
        title = "Trends in Diabetes Cases by Age Group",
        y = "Number of Cases",
        x = "Year"
      )
  })
  
  # Interactive map
  output$county_map <- renderLeaflet({
    req(input$year)
    map_data <- prepare_map_data(nj_diabetes_data, input$year)
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data$Total_Cases,
      na.color = "transparent"
    )
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(Total_Cases),
        color = "white",
        weight = 1,
        opacity = 1.0,
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        popup = ~paste(
          "<strong>County:</strong>", NAME, "<br>",
          "<strong>Total Cases:</strong>", formatC(Total_Cases, format="f", big.mark=",", digits=0)
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = map_data$Total_Cases, 
        position = "bottomright",
        title = "Total Cases",
        labFormat = labelFormat(big.mark = ",")
      )
  })
}

shinyApp(ui, server)
