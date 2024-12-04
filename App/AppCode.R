library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
library(sf)
library(scales)

# UI
ui <- page_sidebar(
  title = "County Demographics Map",
  sidebar = sidebar(
    selectInput("year", "Select Year:",
                choices = NULL),  # Will be updated in server
    radioButtons("ageGroup", "Select Age Group:",
                 choices = c("20-44 years" = "20-44",
                             "45-64 years" = "45-64",
                             "65+ years" = "65+")),
    checkboxInput("showCI", "Show Confidence Intervals", FALSE),
    hr(),
    helpText("Click on counties to see detailed information")
  ),
  
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Interactive County Map"),
      leafletOutput("map", height = "70vh")
    ),
    card(
      card_header("Selected County Details"),
      textOutput("countyInfo"),
      plotOutput("ciPlot", height = "200px")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Assuming combined_data is available in the environment
  observe({
    years <- sort(unique(combined_data$Year))
    updateSelectInput(session, "year", choices = years)
  })
  
  # Create reactive filtered dataset
  filtered_data <- reactive({
    req(input$year, input$ageGroup)
    
    number_col <- paste0(input$ageGroup, " - Number")
    lower_col <- paste0(input$ageGroup, " - 95% Lower Limit")
    upper_col <- paste0(input$ageGroup, " - 95% Upper Limit")
    
    combined_data %>%
      filter(Year == input$year) %>%
      select(County, Year, 
             number = !!sym(number_col),
             lower = !!sym(lower_col),
             upper = !!sym(upper_col))
  })
  
  # Create the map
  output$map <- renderLeaflet({
    req(filtered_data())
    
    data <- filtered_data()
    
    # Create color palette based on population numbers
    pal <- colorNumeric(
      palette = "viridis",
      domain = data$number,
      na.color = "#808080"
    )
    
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(number),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(County, ": ", number),
        layerId = ~County
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~number,
        title = paste("Population (", input$ageGroup, ")"),
        labFormat = labelFormat(prefix = "")
      )
  })
  
  # Display county information when clicked
  output$countyInfo <- renderText({
    req(input$map_shape_click)
    
    clicked_county <- input$map_shape_click$id
    data <- filtered_data()
    county_data <- data[data$County == clicked_county, ]
    
    paste0(
      "County: ", clicked_county, "\n",
      "Year: ", input$year, "\n",
      "Age Group: ", input$ageGroup, "\n",
      "Population: ", format(county_data$number, big.mark = ","), "\n",
      "95% CI: [", 
      format(county_data$lower, big.mark = ","), " - ",
      format(county_data$upper, big.mark = ","), "]"
    )
  })
  
  # Create confidence interval plot
  output$ciPlot <- renderPlot({
    req(input$map_shape_click)
    
    clicked_county <- input$map_shape_click$id
    data <- filtered_data()
    county_data <- data[data$County == clicked_county, ]
    
    if(input$showCI) {
      ggplot(county_data, aes(x = number, y = 1)) +
        geom_point(size = 3) +
        geom_errorbarh(aes(xmin = lower, xmax = upper, y = 1), height = 0.1) +
        theme_minimal() +
        labs(title = paste("Population Estimate with 95% CI -", clicked_county),
             x = "Population",
             y = "") +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    }
  })
}

shinyApp(ui, server)

