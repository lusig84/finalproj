library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(tidyr)

# Load GeoJSON file for NJ counties
geojson_path <- "~/Desktop/SOC360/HW/finalproj/njcountiesmap.geojson"
nj_geojson <- st_read(geojson_path)

# Read the CSV file for diabetes data
nj_diabetes_data <- read.csv("combined_data.csv", check.names = FALSE)
nj_diabetes_data <- nj_diabetes_data %>%
  mutate(
    `20-44 - Number` = as.numeric(`20-44 - Number`),
    `45-64 - Number` = as.numeric(`45-64 - Number`),
    `65+ - Number` = as.numeric(`65+ - Number`)
  )

ui <- page_sidebar(
  title = "New Jersey Diabetes Statistics by County",
  sidebar = sidebar(
    selectInput("year", "Select Year:",
                choices = sort(unique(nj_diabetes_data$Year)),
                selected = max(nj_diabetes_data$Year)),
    selectInput("age_group", "Select Age Group:",
                choices = c("20-44" = "20-44 - Number",
                            "45-64" = "45-64 - Number",
                            "65+" = "65+ - Number")),
    selectInput("county", "Select County:",
                choices = unique(nj_diabetes_data$County),
                multiple = TRUE,
                selected = unique(nj_diabetes_data$County))
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Total Cases (20-44)",
      value = textOutput("total_20_44"),
      theme = "primary"
    ),
    value_box(
      title = "Total Cases (45-64)",
      value = textOutput("total_45_64"),
      theme = "primary"
    ),
    value_box(
      title = "Total Cases (65+)",
      value = textOutput("total_65_plus"),
      theme = "primary"
    )
  ),
  
  card(
    full_screen = TRUE,
    card_header("Age Distribution by County"),
    plotOutput("age_distribution")
  ),
  
  card(
    full_screen = TRUE,
    card_header("New Jersey Counties Map"),
    leafletOutput("nj_map", height = "500px")
  ),
  
  card(
    full_screen = TRUE,
    card_header("Detailed County Data"),
    tableOutput("county_table")
  )
)

server <- function(input, output, session) {
  # Filtered dataset based on user selections
  filtered_data <- reactive({
    req(input$year, input$county)
    nj_diabetes_data %>%
      filter(
        Year == input$year,
        County %in% input$county
      )
  })
  
  # Calculate totals for each age group
  output$total_20_44 <- renderText({
    sum_cases <- sum(filtered_data()$`20-44 - Number`, na.rm = TRUE)
    format(round(sum_cases), big.mark = ",")
  })
  
  output$total_45_64 <- renderText({
    sum_cases <- sum(filtered_data()$`45-64 - Number`, na.rm = TRUE)
    format(round(sum_cases), big.mark = ",")
  })
  
  output$total_65_plus <- renderText({
    sum_cases <- sum(filtered_data()$`65+ - Number`, na.rm = TRUE)
    format(round(sum_cases), big.mark = ",")
  })
  
  # Create age distribution plot
  output$age_distribution <- renderPlot({
    req(filtered_data())
    data_long <- filtered_data() %>%
      select(Year, County, 
             `20-44 - Number`, 
             `45-64 - Number`, 
             `65+ - Number`) %>%
      tidyr::pivot_longer(
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
        title = paste("Diabetes Cases by Age Group -", input$year)
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  # Render interactive map
  output$nj_map <- renderLeaflet({
    req(filtered_data())
    
    # Summarize cases by county
    summarized_data <- filtered_data() %>%
      group_by(County) %>%
      summarize(
        Total_Cases = sum(`20-44 - Number`, na.rm = TRUE) +
          sum(`45-64 - Number`, na.rm = TRUE) +
          sum(`65+ - Number`, na.rm = TRUE)
      )
    
    # Join GeoJSON data with summarized diabetes data
    map_data <- nj_geojson %>%
      left_join(summarized_data, by = c("NAME" = "County"))
    
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorBin("YlOrRd", Total_Cases)(Total_Cases),
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.7,
        popup = ~paste0("<b>County: </b>", NAME, "<br><b>Total Cases: </b>", Total_Cases)
      ) %>%
      addLegend(
        "bottomright",
        pal = colorBin("YlOrRd", domain = map_data$Total_Cases),
        values = map_data$Total_Cases,
        title = "Total Cases",
        opacity = 1
      )
  })
  
  # Create regular table
  output$county_table <- renderTable({
    req(filtered_data())
    filtered_data() %>%
      select(
        Year,
        County,
        `20-44 Cases` = `20-44 - Number`,
        `45-64 Cases` = `45-64 - Number`,
        `65+ Cases` = `65+ - Number`
      ) %>%
      arrange(County)
  })
}

shinyApp(ui, server)
