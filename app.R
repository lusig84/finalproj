library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# Read the actual CSV file
nj_diabetes_data <- read.csv("combined_data.csv", check.names = FALSE)  # This prevents R from modifying column names
nj_diabetes_data <- nj_diabetes_data %>%
  mutate(
    `20-44 - Number` = as.numeric(`20-44 - Number`),
    `45-64 - Number` = as.numeric(`45-64 - Number`),
    `65+ - Number` = as.numeric(`65+ - Number`)
  )
print("Data loaded. First few rows:")
print(head(nj_diabetes_data))

ui <- page_sidebar(
  title = "New Jersey Diabetes Statistics by County",
  sidebar = sidebar(
    selectInput("year", "Select Year:",
                choices = sort(unique(nj_diabetes_data$Year)),
                selected = max(nj_diabetes_data$Year)),
    selectInput("county", "Select County:",
                choices = unique(nj_diabetes_data$County),
                multiple = TRUE,
                selected = unique(nj_diabetes_data$County)[1])
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
    card_header("Detailed County Data"),
    tableOutput("county_table")
  )
)

server <- function(input, output) {
  
  # Filtered dataset based on user selections
  filtered_data <- reactive({
    req(input$year, input$county)
    data <- nj_diabetes_data %>%
      filter(
        Year == input$year,
        County %in% input$county
      )
    print("Filtered data:")
    print(head(data))
    data
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
