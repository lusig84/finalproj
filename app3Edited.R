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
    Year = as.integer(Year),
    Population_Category = cut(Population, 
                              breaks = c(0, 200000, 500000, 800000, Inf),
                              labels = c("Small (<200k)", "Medium (200k-500k)", 
                                         "Large (500k-800k)", "Very Large (>800k)"))
  )

nj_diabetes_data %>%
  group_by(County) %>%
  summarise(
    Min_Rate = ifelse(all(is.na(Total_Rate)), NA, min(Total_Rate, na.rm = TRUE)),
    .groups = 'drop'
  )

# Calculate state-level statistics
state_stats <- nj_diabetes_data %>%
  group_by(Year) %>%
  summarise(
    Total_Population = sum(Population),
    Total_State_Cases = sum(Total_Cases, na.rm = TRUE),
    State_Rate = (Total_State_Cases / Total_Population) * 100000,
    .groups = 'drop'
  )

# Define UI
ui <- page_sidebar(
  title = "New Jersey Diabetes Analytics Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Controls",
    selectInput("view", "Select View:", 
                choices = c("Executive Summary" = "Summary",
                            "Age Distribution Analysis" = "Age Distribution",
                            "County-Level Data" = "County Table",
                            "Temporal Trends" = "Trends",
                            "Statistical Overview" = "Statistics",
                            "County Rankings" = "Rankings",
                            "Age-County Heatmap" = "Heatmap",
                            "Top Counties Analysis" = "Top Counties")),
    
    conditionalPanel(
      condition = "input.view != 'Summary'",
      selectInput("year", "Select Year(s):",
                  choices = sort(unique(nj_diabetes_data$Year)),
                  multiple = TRUE,
                  selected = max(nj_diabetes_data$Year))
    ),
    
    conditionalPanel(
      condition = "input.view != 'Summary' && input.view != 'Rankings' && input.view != 'Top Counties'",
      selectInput("county", "Select County:", 
                  choices = unique(nj_diabetes_data$County),
                  multiple = TRUE,
                  selected = unique(nj_diabetes_data$County)[1])
    ),
    
    radioButtons("metric", "Display Metric:",
                 choices = c("Raw Numbers" = "numbers", 
                             "Per 100,000 Population" = "per_capita"),
                 selected = "per_capita"),
    
    checkboxInput("compare_state", "Compare with State Average", FALSE),
    
    hr(),
    
    helpText("Data represents diabetes cases across New Jersey counties.",
             "Population figures are based on 2020 census data.")
  ),
  
  mainPanel(
    uiOutput("dynamic_ui")
  )
)

# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$year)
    data <- nj_diabetes_data
    
    if (!is.null(input$county)) {
      data <- data %>% filter(County %in% input$county)
    }
    
    data <- data %>% filter(Year %in% input$year)
    
    if(nrow(data) == 0) {
      return(NULL) # or display a message if no data
    }
    
    return(data)
  })
  
  output$dynamic_ui <- renderUI({
    switch(input$view,
           "Summary" = tagList(
             layout_column_wrap(
               width = 1/2,
               card(
                 card_header("Key Statistics"),
                 tableOutput("summary_table")
               ),
               card(
                 card_header("Trend Analysis"),
                 plotOutput("summary_trends")
               )
             ),
             layout_column_wrap(
               width = 1/2,
               card(
                 card_header("Population Distribution"),
                 plotOutput("population_dist")
               ),
               card(
                 card_header("Year-over-Year Change"),
                 plotOutput("yoy_change")
               )
             )
           ),
           "Age Distribution" = layout_column_wrap(
             width = 1,
             card(
               card_header("Age Distribution Analysis"),
               plotOutput("age_distribution", height = "600px")
             )
           ),
           "County Table" = layout_column_wrap(
             width = 1,
             card(
               card_header("Detailed County-Level Data"),
               DTOutput("county_table")
             )
           ),
           "Trends" = layout_column_wrap(
             width = 1,
             card(
               card_header("Temporal Trends Analysis"),
               plotOutput("trends_chart", height = "600px")
             )
           ),
           "Statistics" = layout_column_wrap(
             width = 1,
             card(
               card_header("Statistical Summary by County"),
               DTOutput("stats_table")
             )
           ),
           "Rankings" = layout_column_wrap(
             width = 1,
             card(
               card_header("County Rankings Visualization"),
               plotOutput("rankings_chart", height = "800px")
             )
           ),
           "Heatmap" = layout_column_wrap(
             width = 1,
             card(
               card_header("Age-County Distribution Heatmap"),
               plotOutput("heatmap", height = "600px")
             )
           ),
           "Top Counties" = layout_column_wrap(
             width = 1,
             card(
               card_header("Top 10 Counties Analysis"),
               plotOutput("top_counties_chart", height = "600px")
             )
           )
    )
  })
  
  # Enhanced summary table
  output$summary_table <- renderTable({
    if (input$metric == "numbers") {
      nj_diabetes_data %>%
        group_by(Year) %>%
        summarise(
          Total_Population = sum(Population),
          `Total Cases (20-44)` = sum(`20-44 - Number`, na.rm = TRUE),
          `Total Cases (45-64)` = sum(`45-64 - Number`, na.rm = TRUE),
          `Total Cases (65+)` = sum(`65+ - Number`, na.rm = TRUE),
          `Total Cases` = sum(Total_Cases, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(across(where(is.numeric), comma))
    } else {
      nj_diabetes_data %>%
        group_by(Year) %>%
        summarise(
          Total_Population = sum(Population),
          `Rate (20-44)` = sum(`20-44 - Number`, na.rm = TRUE) / sum(Population) * 100000,
          `Rate (45-64)` = sum(`45-64 - Number`, na.rm = TRUE) / sum(Population) * 100000,
          `Rate (65+)` = sum(`65+ - Number`, na.rm = TRUE) / sum(Population) * 100000,
          `Overall Rate` = sum(Total_Cases, na.rm = TRUE) / sum(Population) * 100000,
          .groups = 'drop'
        ) %>%
        mutate(
          Total_Population = comma(Total_Population),
          across(starts_with("Rate"), round, 1)
        )
    }
  })
  
  # Population distribution plot
  output$population_dist <- renderPlot({
    ggplot(nj_diabetes_data, aes(x = Population_Category, fill = Population_Category)) +
      geom_bar() +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(title = "Distribution of Counties by Population Size",
           x = "Population Category",
           y = "Number of Counties") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  
  # Year-over-year change plot
  output$yoy_change <- renderPlot({
    yearly_data <- nj_diabetes_data %>%
      group_by(Year) %>%
      summarise(
        Total_Rate = sum(Total_Cases, na.rm = TRUE) / sum(Population) * 100000,
        .groups = 'drop'
      ) %>%
      mutate(YoY_Change = (Total_Rate - lag(Total_Rate)) / lag(Total_Rate) * 100)
    
    ggplot(filtered_data(), aes(x = Year, y = Total_Cases, group = County, color = County)) +
      geom_line(size = 1.2, na.rm = TRUE) +  # This ignores NAs
      geom_point(size = 3, na.rm = TRUE) +    # This ignores NAs
    
     ggplot(yearly_data, aes(x = Year, y = YoY_Change)) +
      geom_col(fill = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Year-over-Year Change in Diabetes Rate",
           y = "Percent Change",
           x = "Year")
  })
  
  # Enhanced trends chart
  output$trends_chart <- renderPlot({
    req(input$year)
    
    p <- filtered_data() %>%
      ggplot(aes(x = Year, 
                 y = if(input$metric == "numbers") Total_Cases else Total_Rate,
                 group = County, 
                 color = County)) +
      geom_line(size = 1.2, na.rm = TRUE) +
      geom_point(size = 3, na.rm = TRUE) +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_color_viridis_d() +
      labs(
        x = "Year",
        y = if(input$metric == "numbers") "Total Cases" else "Cases per 100,000 Population",
        title = if(input$metric == "numbers") "Trends Over Time: Total Cases" else "Trends Over Time: Diabetes Rates by County"
      )
    
    if(input$compare_state) {
      state_data <- state_stats %>%
        mutate(Value = if(input$metric == "numbers") Total_State_Cases else State_Rate)
      
      p <- p + 
        geom_line(data = state_data, 
                  aes(x = Year, y = Value, group = 1),
                  color = "red", 
                  size = 1.5, 
                  linetype = "dashed",
                  na.rm = TRUE) +
        annotate("text", 
                 x = max(state_data$Year), 
                 y = max(state_data$Value),
                 label = "State Average", 
                 color = "red",
                 hjust = 1, 
                 vjust = -0.5)
    }
    p
  })
  
  # Enhanced rankings chart with population size indicator
  output$rankings_chart <- renderPlot({
    nj_diabetes_data %>%
      group_by(County) %>%
      summarise(
        Avg_Rate = mean(Total_Rate, na.rm = TRUE),
        Population = first(Population),
        Population_Category = first(cut(Population, 
                                        breaks = c(0, 200000, 500000, 800000, Inf),
                                        labels = c("Small", "Medium", "Large", "Very Large")))
      ) %>%
      arrange(desc(Avg_Rate)) %>%
      ggplot(aes(x = reorder(County, -Avg_Rate), 
                 y = Avg_Rate, 
                 fill = Population_Category)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(
        x = "County",
        y = "Average Rate per 100,000 Population",
        title = "County Rankings by Diabetes Rate",
        fill = "Population Size",
        caption = "Population categories: Small (<200k), Medium (200k-500k), Large (500k-800k), Very Large (>800k)"
      )
  })
  
  # Enhanced heatmap with better color scaling
  output$heatmap <- renderPlot({
    filtered_data() %>%
      select(County, `20-44 Rate`, `45-64 Rate`, `65+ Rate`) %>%
      pivot_longer(
        cols = c(`20-44 Rate`, `45-64 Rate`, `65+ Rate`),
        names_to = "Age_Group",
        values_to = "Rate"
      ) %>%
      mutate(
        Age_Group = factor(str_remove(Age_Group, " Rate"),
                           levels = c("20-44", "45-64", "65+")),
        County = reorder(County, -Rate)
      ) %>%
      ggplot(aes(x = County, y = Age_Group, fill = Rate)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "plasma", name = "Rate per 100k") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right"
      ) +
      labs(
        title = "Heatmap of Diabetes Rates by Age and County",
        subtitle = paste("Selected Year(s):", paste(input$year, collapse = ", ")),
        x = "County",
        y = "Age Group"
      )
  })
  
  # Enhanced top counties chart with rate comparison
  output$top_counties_chart <- renderPlot({
    top_data <- nj_diabetes_data %>%
      group_by(County) %>%
      summarise(
        Avg_Rate = mean(Total_Rate, na.rm = TRUE),
        Population = first(Population)
      ) %>%
      arrange(desc(Avg_Rate)) %>%
      slice(1:10) %>%
      mutate(
        County = factor(County, levels = County),
        State_Avg = mean(nj_diabetes_data$Total_Rate, na.rm = TRUE)
      )
    
    ggplot(top_data) +
      geom_col(aes(x = County, y = Avg_Rate, fill = Population)) +
      geom_hline(aes(yintercept = State_Avg), 
                 linetype = "dashed", color = "red", size = 1) +
      scale_fill_viridis_c(name = "Population") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      ) +
      labs(
        x = "County",
        y = "Average Rate per 100,000 Population",
        title = "Top 10 Counties by Diabetes Rate",
        subtitle = "Red line indicates state average"
      )
  })
  
  # Additional statistics table with enhanced metrics
  output$stats_table <- renderTable({
    nj_diabetes_data %>%
      group_by(County) %>%
      summarise(
        Population = first(Population),
        Mean_Rate = mean(Total_Rate, na.rm = TRUE),
        Median_Rate = median(Total_Rate, na.rm = TRUE),
        Min_Rate = min(Total_Rate, na.rm = TRUE),
        Max_Rate = max(Total_Rate, na.rm = TRUE),
        Std_Dev = sd(Total_Rate, na.rm = TRUE),
        Total_Cases = sum(Total_Cases, na.rm = TRUE),
        Years_Above_Avg = sum(Total_Rate > 
                                mean(nj_diabetes_data$Total_Rate, na.rm = TRUE), na.rm = TRUE)
      ) %>%
      arrange(desc(Mean_Rate)) %>%
      mutate(across(where(is.numeric), round, 2))
  })
  
  # Time series decomposition plot
  output$decomposition_plot <- renderPlot({
    req(length(input$year) > 2)
    
    county_data <- filtered_data() %>%
      arrange(Year) %>%
      group_by(County) %>%
      mutate(
        Trend = zoo::rollmean(Total_Rate, k = 3, fill = NA),
        Seasonal = Total_Rate - Trend
      )
    
    ggplot(county_data, aes(x = Year, color = County)) +
      geom_line(aes(y = Total_Rate), size = 1, na.rm = TRUE) +
      geom_line(aes(y = Trend), linetype = "dashed", na.rm = TRUE) +
      facet_wrap(~County, scales = "free_y") +
      theme_minimal() +
      scale_color_viridis_d() +
      labs(
        title = "Time Series Analysis by County",
        subtitle = "Solid line: Actual rate, Dashed line: 3-year moving average",
        y = "Rate per 100,000 Population"
      )
  })
  
  # Age distribution analysis
  output$age_distribution <- renderPlot({
    filtered_data() %>%
      select(County, Year, `20-44 Rate`, `45-64 Rate`, `65+ Rate`) %>%
      pivot_longer(
        cols = c(`20-44 Rate`, `45-64 Rate`, `65+ Rate`),
        names_to = "Age_Group",
        values_to = "Rate"
      ) %>%
      mutate(Age_Group = factor(str_remove(Age_Group, " Rate"),
                                levels = c("20-44", "45-64", "65+"))) %>%
      ggplot(aes(x = Age_Group, y = Rate, fill = Age_Group)) +
      geom_boxplot(na.rm = TRUE) +
      scale_fill_viridis_d() +
      facet_wrap(~Year) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Distribution of Diabetes Rates by Age Group",
        subtitle = "Boxplots show distribution across counties",
        x = "Age Group",
        y = "Rate per 100,000 Population"
      )
  })
  
  # Summary trends
  output$summary_trends <- renderPlot({
    yearly_summary <- nj_diabetes_data %>%
      group_by(Year) %>%
      summarise(
        Total_Rate = sum(Total_Cases, na.rm = TRUE) / sum(Population) * 100000,
        `20-44_Rate` = sum(`20-44 - Number`, na.rm = TRUE) / sum(Population) * 100000,
        `45-64_Rate` = sum(`45-64 - Number`, na.rm = TRUE) / sum(Population) * 100000,
        `65+_Rate` = sum(`65+ - Number`, na.rm = TRUE) / sum(Population) * 100000,
        .groups = 'drop'
      ) %>%
      pivot_longer(
        cols = ends_with("Rate"),
        names_to = "Category",
        values_to = "Rate"
      ) %>%
      mutate(
        Category = factor(str_remove(Category, "_Rate"),
                          levels = c("Total", "20-44", "45-64", "65+"))
      )
    
    ggplot(yearly_summary, aes(x = Year, y = Rate, color = Category)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_viridis_d() +
      theme_minimal() +
      labs(
        title = "Statewide Diabetes Rates Over Time",
        y = "Rate per 100,000 Population",
        color = "Age Group"
      )
  })
  
  # County data table
  output$county_table <- renderDT({
    filtered_data() %>%
      select(
        County, Year, Population, 
        `20-44 Cases` = `20-44 - Number`,
        `45-64 Cases` = `45-64 - Number`,
        `65+ Cases` = `65+ - Number`,
        `20-44 Rate`, `45-64 Rate`, `65+ Rate`,
        Total_Cases, Total_Rate
      ) %>%
      arrange(County, Year) %>%
      mutate(
        across(ends_with("Rate"), round, 2),
        Population = format(Population, big.mark = ","),
        across(ends_with("Cases"), format, big.mark = ",")
      )
  }, options = list(
    pageLength = 15,
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))
}

# Run the application
shinyApp(ui, server)
