library(shiny)
library(plotly)
library(shinydashboard)
library(DT)
library(circular)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(ggridges)
library(rnaturalearth)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load dataset
flu_data <- read_csv("csv/main_dataset.csv")

seasonality_shift <- read_csv("csv/flu_net_shift.csv")

# Convert year and month into date format
flu_data <- flu_data %>%
  mutate(date = make_date(year, month = match(month, month.name)))

# Ensure week is numeric and remove NAs
flu_data <- flu_data %>%
  mutate(week = as.numeric(week)) %>%
  filter(!is.na(week))

# Categorise pre and post 2021
flu_data <- flu_data %>%
  mutate(period = ifelse(year < 2021, "Pre-2021", "Post-2021"))


# # Global Map Data and Plot
# seasonality_shift <- flu_data %>%
#   group_by(country, period, week) %>%
#   summarise(total_cases = sum(metric, na.rm = TRUE), .groups = "drop") %>%
#   group_by(country, period) %>%
#   filter(total_cases == max(total_cases)) %>%
#   summarise(peak_week = first(week), .groups = "drop") %>%
#   pivot_wider(names_from = period, values_from = peak_week) %>%
#   mutate(
#     raw_shift = `Post-2021` - `Pre-2021`,
#     week_shift = ifelse(abs(raw_shift) > 26, ifelse(raw_shift > 0, raw_shift - 52, raw_shift + 52), raw_shift)
#   ) %>% 
#   view()


world_map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  left_join(seasonality_shift, by = c("iso_a3" = "COUNTRY_CODE"))

ui <- dashboardPage(
  dashboardHeader(title = "Influenza & RSV Trends"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Individual Seasonality", tabName = "individual", icon = icon("mountain-sun")),
      menuItem("Global Seasonality", tabName = "global", icon = icon("globe")),
      selectInput("country", "Select Country:", choices = unique(flu_data$country), selected = "United Kingdom"),
      selectInput("disease", "Select Disease:", choices = unique(flu_data$disease), selected = "Influenza"),
      #selectInput("metric", "Select Metric:", choices = c("Cases", "Hospitalisations"), selected = "Cases"),
      selectInput("age_group", "Select Age Group:", choices = NULL, selected = "All")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "individual",
              fluidRow(
                box(plotlyOutput("seasonal_plot"), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("radial_plot"), width = 6),
                box(plotlyOutput("polarPlot"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("ridgePlot"), width = 12)
              ),
              fluidRow(
                box(DTOutput("data_table"), width = 12)
              )
      ),
      tabItem(tabName = "global",
              fluidRow(
                box(leafletOutput("world_map", height = "800"), width = 12, height = "auto")
              )
      )
    )
  )
)
  

server <- function(input, output, session) {
  
  # Reactive dataset based on selected inputs
  filtered_data <- reactive({
    df <- flu_data %>%
      filter(country == input$country, disease == input$disease)
    if (input$age_group != "All") {
      df <- df %>% filter(age == input$age_group)
    }
    df
  })
  
  # Dynamically update the available age groups based on selected country
  observeEvent(input$country, {
    # Safely get unique age groups for the selected country, handling potential NA values
    available_ages <- unique(flu_data %>% 
                               filter(country == input$country) %>% 
                               pull(age)) %>%
      na.omit()  # Remove any NAs
    
    # Update the age group selection input
    updateSelectInput(session, "age_group", 
                      choices = c("All", available_ages), 
                      selected = "All")  # Default to "All"
  })
  
  output$seasonal_plot <- renderPlotly({
    # Check the structure of filtered data
    # str(filtered_data())  # This will print the structure in the R console for debugging
    
    # Summarise the data
    p <- filtered_data() %>%
      group_by(period, week, age) %>%
      summarise(metric = sum(metric, na.rm = TRUE)) %>%
      ggplot(aes(x = week, y = metric, color = period)) +  # Use 'week' as x-axis
      geom_line() +
      facet_wrap(~age) +
      theme_minimal() +
      labs(title = paste("Seasonality of", input$disease, "in", input$country),
           x = "Week", y = "Metric") +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(),
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      )
    
    ggplotly(p)
  })
  
  output$radial_plot <- renderPlotly({
    radial_data <- filtered_data() %>%
      group_by(period, week) %>%
      summarise(total_metric = sum(metric, na.rm = TRUE), .groups = 'drop')
    
    radial_data$week <- factor(radial_data$week, levels = 1:53)
    
    p <- ggplot(radial_data, aes(x = week, y = total_metric, fill = period)) +
      geom_line(size = 1) +
      + coord_polar(theta = "x")
      theme_minimal() +
      labs(title = "Seasonal Distribution of Cases by Week", x = "Week", y = "Total Cases")
    
    ggplotly(p)
  })
  
  output$polarPlot <- renderPlotly({
    df_filtered <- filtered_data()
    if (nrow(df_filtered) == 0) return(NULL)
    
    df_filtered <- df_filtered %>%
      mutate(is_covid = if_else(year < 2021, "Before Lockdown", "After Lockdown")) %>%
      group_by(week, is_covid) %>%
      summarise(metric = sum(metric, na.rm = TRUE), .groups = "drop")
    
    plotly::plot_ly(df_filtered, 
                    type = "scatterpolar", 
                    mode = "lines", 
                    r = ~metric, 
                    theta = ~week, 
                    color = ~is_covid,
                    fill = "toself") %>%
      layout(
        title = paste(input$country, input$disease, "Polar Plot"),
        polar = list(radialaxis = list(visible = TRUE)),
        showlegend = TRUE
      )
  })
  
  output$ridgePlot <- renderPlotly({
    df_filtered <- filtered_data()
    if (nrow(df_filtered) == 0) return(NULL)
    
    df_filtered <- df_filtered %>%
      filter(age == "All") %>%
      group_by(year, week) %>%
      summarise(metric = sum(metric, na.rm = TRUE), .groups = "drop")
    
    plotly::plot_ly(df_filtered, 
                    x = ~week, 
                    y = ~year, 
                    z = ~metric, 
                    type = "heatmap", 
                    colorscale = "Viridis") %>%
      layout(title = paste(input$country, input$disease, "Density Heatmap"),
             xaxis = list(title = "Week"), 
             yaxis = list(title = "Year"))
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
  
  output$world_map <- renderLeaflet({
    leaflet(world_map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric(palette = c("blue", "white", "red"), domain = week_shift)(week_shift),
        color = "black",
        weight = 0.5,
        opacity = 1,
        fillOpacity = 0.7,
        popup = ~paste("Country: ", name, " - Week Shift: ", week_shift),
        label = ~paste("Country: ", name, " - Week Shift: ", week_shift),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorNumeric(palette = c("blue", "white", "red"), domain = world_map_data$week_shift),
        values = world_map_data$week_shift[!is.na(world_map_data$week_shift)],  # Remove NAs from the legend
        title = "Week Shift",
        opacity = 1
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
}

shinyApp(ui, server)