##----------------------------------------
## Shiny app providing an interactive dashboard for visualizing the seasonality of RSV and influenza rates
##----------------------------------------
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages('rsconnect')
# install.packages('bs4Dash')
# install.packages('plotly')
# install.packages('leaflet')
# install.packages('DT')
# install.packages('fresh')
# install.packages('shinyjqui')
# install.packages('bslib')

# ##load the libraries
# 
# library(shiny)
# library(vroom)
# library(tidyverse)
# library(shinythemes)
# 
# more packages if needed
# library(bs4Dash)
# library(dplyr)
# library(readr)
# library(plotly)
# library(leaflet)
# library(DT)
# library(fresh)
# library(shinyjqui)
library(shiny)
library(bs4Dash)
library(dplyr)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(fresh)
library(ISOweek)

library(circular)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(ggridges)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)
## REMOVE THIS after you have finished

# setwd("C:/Users/icnarc246/OneDrive - ICNARC/Desktop/Trainings and personal docs/LSHTM/Data challenge/sanofi-rsv-flu")

##------------------------------------------------------------------------------
# Load datasets
df <- read_csv("csv/main_dataset.csv")

seasonality_shift_flu <- df %>%
  filter(disease ==  "Influenza") %>% 
  mutate(period = if_else(year < 2021, "Before", "After")) %>%
  group_by(country, period, week) %>%
  summarise(total_cases = sum(metric, na.rm = TRUE), .groups = "drop") %>%
  group_by(country, period) %>%
  filter(total_cases == max(total_cases)) %>%
  summarise(peak_week = first(week), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = peak_week) %>%
  mutate(
    raw_shift = After - Before,
    week_shift = ifelse(
      abs(raw_shift) > 26,  # If shift is more than half a year, wrap around
      ifelse(raw_shift > 0, raw_shift - 52, raw_shift + 52),
      raw_shift
    )
  )

world_map_data_flu <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  right_join(seasonality_shift_flu, by = c("name" = "country")) %>% 
  drop_na(name_long)

seasonality_shift_rsv <- df %>%
  filter(disease ==  "RSV") %>%
  filter(country %in% c("United Kingdom", "Argentina", "Denmark", "Finland", "Germany", "Ireland", "Japan", "United States of America")) %>%  
  mutate(period = if_else(year < 2021, "Before", "After")) %>%
  group_by(country, period, week) %>%
  summarise(total_cases = sum(metric, na.rm = TRUE), .groups = "drop") %>%
  group_by(country, period) %>%
  filter(total_cases == max(total_cases)) %>%
  summarise(peak_week = first(week), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = peak_week) %>%
  mutate(
    raw_shift = After - Before,
    week_shift = ifelse(
      abs(raw_shift) > 26,  # If shift is more than half a year, wrap around
      ifelse(raw_shift > 0, raw_shift - 52, raw_shift + 52),
      raw_shift
    )
  )

world_map_data_rsv <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  right_join(seasonality_shift_rsv, by = c("name" = "country"))


NH_COUNTRIES = c("Europe", "North America", "Asia")

##------------------------------------------------------------------------------
## Data wrangling

# Convert year and month into date format
df <- df %>%
  mutate(date = make_date(year, month = match(month, month.name)))

# Ensure week is numeric and remove NAs
df <- df %>%
  mutate(week = as.numeric(week)) %>%
  filter(!is.na(week))

# Categorise pre and post 2021
df <- df %>%
  filter(year >= 2017 & year < 2025) %>% 
  mutate(period = if_else((year) < 2021, 0, 1),
           period = factor(period, labels = c("Before 2021", "After 2021")))


##------------------------------------------------------------------------------

# User Interface ----------------------------------------------------------

ui <- dashboardPage(
  title = "SANOFI RSV and Influenza Seasonality Trends",
  ##------------------------------------------------------------------------------        
  # Header ----
  header = dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; justify-content: center; flex-direction: column;",
      tags$img(
        src = "https://upload.wikimedia.org/wikipedia/en/thumb/6/6f/LSHTMLogo2020.svg/1200px-LSHTMLogo2020.svg.png",
        style = "height: 60px; width: auto; margin-bottom: 20px; margin-top: 10px;" 
      )
    )
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      #menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Global Seasonality", tabName = "global", icon = icon("globe")),
      menuItem("Individual Seasonality", tabName = "individual", icon = icon("chart-line")),
      menuItem("Export Data", tabName = "export", icon = icon("download"))
    )
  ),
  
  ##------------------------------------------------------------------------------        
  # Body ----
  body = dashboardBody(
    tabItems(
      # Home Tab ----
      tabItem(
        tabName = "home",
        jumbotron(
          title = "Influenza and RSV Seasonality Analysis",
          status = "info",
          lead = "How have the Seasonality of Respiratory Syncytial Virus (RSV) and Influenza Changed since the emergence of SARS-COV-2?",
          href = NULL,
          btnName = NULL,
          markdown(
          "By analysing data from geographic regions across both hemispheres, we seek to address the following key questions: \n
            1.	How have seasonal patterns of influenza and RSV changed post-COVID-19 \n
            2.	Are there regional differences in these changes based on climate or latitude? \n
            3.	What implications do these findings have for public health planning and resource allocation? \n

          
          
          ")
        ),
        fluidRow(
          box(
            title = "Summary Plot of The Dataset",
            width = 12,
            collapsible = FALSE,
            plotlyOutput("box_plot_sum")
          )
        ), # fluidrow
        fluidRow(
          box(
            title = "Cumulative Reported Influenza Cases By Continent",
            width = 12,
            status = "lightblue",
            collapsible = FALSE,
            plotlyOutput("cumulative_plot_flu")
          )
        ), # fluidrow
        fluidRow(
          box(
            title = "Cumulative Reported RSV Cases By Continent",
            width = 12,
            status = "maroon",
            collapsible = FALSE,
            plotlyOutput("cumulative_plot_rsv")
          )
        ) # fluidrow
      ), # tabitem
      ##------------------------------------------------------------------------------      
      
      ##### Dashboard Tab ----
      tabItem(
        tabName = "export",
        fluidRow(
          box(downloadButton('downloadData', 'Download CSV'),
              DTOutput("export_table"), width = 12, title = "Dataset Download"
              )
          )
        
        ),
      
      
      tabItem(
        tabName = "individual",
        fluidRow(
            infoBoxOutput("data_source_box"),
            infoBoxOutput("data_date_box"),
            infoBoxOutput("data_week_box")

          ),
          
          ##------------------------------------------------------------------------------
          ## Data boxes
          # fluidRow(
          #     # width = 6,
          #     box(
          #       title = "Dashboard Overview",
          #       status = "primary",
          #       solidHeader = TRUE,
          #       width = 12,
          #       "This section will display interactive charts and tables."
          #       ),
          #   ),  # fluid row
        fluidRow(
              column(4,selectInput("disease", "Select Disease:",
                                 choices = unique(df$disease),
                                 selected = "Influenza")),
              column(4,selectInput("country", "Select Country:",
                                        choices = unique(df$country),
                                        selected = "United Kingdom")),
              column(4,selectInput("age_group", "Select Age Group:", choices = NULL, selected = "All"))

                     

        ),
        
        fluidRow(
          box(plotOutput("polarPlot"), width = 6, title = "Seasonality Split by COVID-19 Emergence"),
          box(plotOutput("ridgePlot"), width = 6, title = "Density Plot of Case Distributions")
        ),
        fluidRow(
          box(
            plotlyOutput("corr_plot"), width = 12, title = "Correlation Between Different Years At Different Weeks",
            column(10, uiOutput("baseline_year_ui"), uiOutput("comp_years_ui"))
            )

        ),
        fluidRow(
          box(plotlyOutput("seasonal_plot"), width = 12, title = "Seasonality Compared and Stratified By Age")
        ),

        fluidRow(
          box(DTOutput("data_table"), width = 12)
        ),
        
      ), # tabItem
      tabItem(
        tabName = "global",
        fluidRow(
          box(
            title = HTML("Number of Weeks Difference Between the Peak of the Influenza Season Before COVID 19 and After"),
            status = "lightblue",
            leafletOutput("world_map_flu", height = "600"), 
            width = 12, 
            collapsible = FALSE, 
            height = "auto")
        ), # fluidrow
        fluidRow(

          box(
            title = HTML("Number of Weeks Difference Between the Peak of the RSV Season Before COVID 19 and After"),
            status = "maroon",
            leafletOutput("world_map_rsv", height = "600"), 
            width = 12,
            collapsible = FALSE, 
            height = "auto"
            ) 
        ) # fluidrow
      ) # tabitem
    ) # tabItems
  ) # dashboardBody
) # dashboardPage


server <- function(input, output, session) {
  
  # Reactive dataset based on selected inputs
  filtered_data <- reactive({
    local_df <- df %>%
      filter(country == input$country, disease == input$disease) %>% 
      filter(week < 53)
    if (input$age_group != "All") {
      local_df <- df %>% filter(age == input$age_group)
    }
    local_df
  })
  
  # Dynamically update the available age groups based on selected country
  observeEvent(input$country, {
    # Safely get unique age groups for the selected country, handling potential NA values
    available_ages <- unique(df %>% 
                               filter(country == input$country) %>% 
                               pull(age)) %>%
      na.omit()  # Remove any NAs
    
    # Update the age group selection input
    updateSelectInput(session, "age_group", 
                      choices = c("All", available_ages), 
                      selected = "All")  # Default to "All"
  })
  
  # Dynamically update the available age groups based on selected country
  observeEvent(input$disease, {
    # Safely get unique age groups for the selected country, handling potential NA values
    
    if (input$disease == "RSV") {
      available_counts <- c("United Kingdom", "Argentina", "Denmark", "Finland", "Germany", "Ireland", "Japan", "United States of America")
    }
    else {
      available_counts <- unique(world_map_data_flu$name)
    }
    
    # Update the age group selection input
    updateSelectInput(session, "country", 
                      choices = available_counts, 
                      selected = "United Kingdom")  # Default to "All"
  })
  
  output$data_source_box <- renderInfoBox({
    infoBox(
      width = 12,
      title = "Data Source",
      value = unique(filtered_data()$source),  ## ADD THE VALUE 
      icon = icon("database"), 
      color = "primary"
      
      )

  })
  
  output$data_date_box <- renderInfoBox({
    infoBox(
      width = 12,
      title = "Data Range",
      value = paste0(filtered_data()$year[which.min(filtered_data()$year)], "-", filtered_data()$year[which.max(filtered_data()$year)]),  ## ADD THE VALUE 
      icon = icon("calendar"), 
      color = "primary"
      
    )
    
    
  })
  
  output$data_week_box <- renderInfoBox({
    infoBox(
      width = 12,
      title = "Estimated Shift",
      value = paste0(world_map_data_flu %>% filter(name == input$country) %>% pull(week_shift), " Week(s)"),  ## ADD THE VALUE 
      icon = icon("arrows-left-right"), 
      color = "primary"
      
    )
    
    
  })
  
  output$baseline_year_ui <- renderUI({
    available_years <- unique(filtered_data()$year)
    selectInput("baseline_year", "Select Baseline Year:", choices = available_years, selected = min(available_years))
  })
  
  # Dynamically update available years for comparison selection
  output$comp_years_ui <- renderUI({
    available_years <- unique(filtered_data()$year)
    checkboxGroupInput("comp_years", "Select Comparison Years:", inline = TRUE, choices = available_years, selected = available_years[available_years != min(available_years)])
  })
  
  output$seasonal_plot <- renderPlotly({

    plot_age <- filtered_data() %>%
      filter(week < 53) %>%
      
      # Step 1: Summarise total metric by age and period
      group_by(period, age) %>%
      summarise(total_metric = sum(metric), .groups = "drop") %>%
      
      # Step 2: Filter out age groups with low data
      filter(total_metric > 10) %>%
      
      # Step 3: Join back to original data to retain only selected age groups
      inner_join(filtered_data(), by = c("period", "age")) %>%
      
      # Step 4: Summarise at the week level
      group_by(week, period, age) %>%
      summarise(metric = sum(metric), .groups = "drop") %>%
      
      # Remove any missing values
      na.omit()
    
    if (nrow(plot_age) == 0) {
      plot_age <- NULL  # Return NULL if no valid data
    } else {
    
      p <- plot_age %>%
        group_by(period, week, age) %>%
        summarise(metric = sum(metric, na.rm = TRUE)) %>%
        ggplot(aes(x = week, y = metric, color = period)) +  # Use 'week' as x-axis
        geom_line() +
        facet_wrap(~age, scales = "free_y") +
        theme_minimal() +
        labs(x = "Week", y = "Metric", fill = "") +
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(),
          panel.spacing = unit(0.1, "lines"),
          strip.text.x = element_text(size = 8)
        )
    }
    
    ggplotly(p) %>% layout(legend = list(title = list(text = NULL)))

  })
  
  output$polarPlot <- renderPlot({
    # Filter and prepare the data
    df_subset <- filtered_data() %>%
      group_by(week, period) %>%
      summarise(metric = sum(metric), .groups = "drop")
    
    # Plot the data
    ggplot(df_subset, aes(x = week, y = metric, color = factor(period))) +
      geom_line(size = 1) +
      labs(
        x = 'Week', 
        y = "Country Specific Metric",
      ) +
      theme_minimal() + 
      theme(
        axis.title = element_text(),
        legend.position = "bottom",
        axis.ticks.y = element_line(),
        axis.line.y.left = element_line(),
        legend.title = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text()
      ) +
      coord_polar(theta = "x")
  })
  
  output$ridgePlot <- renderPlot({
    
    df_subset <- filtered_data()
    
    if(any(df_subset$continent %in% NH_COUNTRIES)) {
      
      df_subset %>%
        filter(week < 53) %>% 
        group_by(year, week) %>%
        summarise(metric = sum(metric, na.rm = TRUE)) %>% 
        mutate(
          Adjusted_Week = week + 26,  # Shift all weeks forward by 26
          Adjusted_Year = if_else(Adjusted_Week > 52, year + 1, year),  # Increment year if week > 52
          Adjusted_Week = if_else(Adjusted_Week > 52, Adjusted_Week - 52, Adjusted_Week)  # Wrap weeks > 52
        ) %>%
        ggplot(aes(x = Adjusted_Week, y = factor(Adjusted_Year), height = metric, fill = factor(Adjusted_Year))) +
        geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01) +
        labs(x = "Season Week (Centred around Week 1)") +
        scale_x_continuous(
          breaks = c(1, 13, 26, 39),  # Key flu season weeks
          labels = c("Week 26", "Week 39", "Week 1", "Week 13")
        ) +
        scale_y_discrete(
          labels = function(x) paste0(as.numeric(x) - 1, "/", x)  # Convert years to "2014/2015" format
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.spacing = unit(0.1, "lines"),
          axis.title.x = element_text(),
          strip.text.x = element_text(size = 8),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_blank()
        )
      
    }
    
    else {
      print(df_subset$continent)
      df_subset %>%
        filter(week < 53) %>%
        group_by(week, year) %>%
        summarise(metric = sum(metric)) %>%
        na.omit() %>%
        ggplot(aes(x = week, y = factor(year), height = metric, fill = factor(year))) +
        geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01, size = 1) +
        labs(x = 'Week') +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.spacing = unit(0.1, "lines"),
          axis.title.x = element_text(),
          strip.text.x = element_text(size = 8),
          axis.text.x = element_text(),
          axis.title.y = element_blank() # Rotates the x-axis labels for better readability
        )
      
    }
  })
  
  output$data_table <- renderDT(server = FALSE, {
    datatable(filtered_data(), extensions = 'Buttons', 
              options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                             paging = TRUE, searching = TRUE,
                             fixedColumns = TRUE, autoWidth = TRUE,
                             ordering = TRUE, dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel')))
  })
  
  output$export_table <- renderDT(server = TRUE, {
    datatable(df, 
              options = list(scrollX=TRUE,
                             paging = TRUE, searching = TRUE,
                             fixedColumns = TRUE, autoWidth = TRUE,
                             ordering = TRUE))
  })
  
  output$downloadData <- downloadHandler(
      filename = function() {
        paste('flu-rsv-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(df, con, row.names = FALSE)
      }
    )
  
  output$world_map_flu <- renderLeaflet({
    leaflet(world_map_data_flu) %>%
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
        ),
        # label = ~name,  # Show country name on hover
        layerId = ~name  # Needed to track hovered country
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorNumeric(palette = c("blue", "white", "red"), domain = world_map_data_flu$week_shift),
        values = world_map_data_flu$week_shift[!is.na(world_map_data_flu$week_shift)],  # Remove NAs from the legend
        title = "Week Shift",
        opacity = 1,
        labFormat = function(type, cuts, p) {
          labels <- paste0(ifelse(cuts < 0, "−", ""), abs(cuts))  # Ensures minus signs are distinct
          return(labels)
        }
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  output$world_map_rsv <- renderLeaflet({
    leaflet(world_map_data_rsv) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric(palette = c("blue", "white", "red"), domain = c(-20, 20))(week_shift),
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
        pal = colorNumeric(palette = c("blue", "white", "red"), domain = c(-20, 20)),
        values = world_map_data_rsv$week_shift[!is.na(world_map_data_rsv$week_shift)],  # Remove NAs from the legend
        title = "Week Shift",
        opacity = 1,
        labFormat = function(type, cuts, p) {
          labels <- paste0(ifelse(cuts < 0, "−", ""), abs(cuts))  # Ensures minus signs are distinct
          return(labels)
        }
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  hovered_country <- reactive({
    input$map_shape_mouseover$id
  })
  
  output$country_plot <- renderPlotly({
    h_country <- hovered_country()
    if (is.null(country)) return(NULL)
    
    country_cases <- df %>% filter(country == h_country)
    
    p <- ggplot(country_cases, aes(x = week, y = cases)) +
      geom_line(color = "blue") +
      geom_point(size = 2) +
      labs(title = paste("Weekly Cases in", country),
           x = "Week", y = "Cases") +
      theme_minimal()
    
    ggplotly(p)  # Converts ggplot to plotly
  })
  
  output$corr_plot <- renderPlotly({
    
    req(input$baseline_year, input$comp_years)
    
    # Convert the filtered data to a tibble and ensure proper types
    data_tidy <- filtered_data() %>%
      as_tibble() %>%
      mutate(
        year   = as.numeric(year),
        week   = as.numeric(week),
        metric = as.numeric(metric)
      )
    
    # Check that the data has a 'metric' column
    if (!"metric" %in% colnames(data_tidy)) {
      message("ERROR: 'metric' column missing from filtered_data()")
      return(NULL)
    }
    
    # Convert UI inputs to numeric
    baseline_year <- as.numeric(input$baseline_year)
    comp_years    <- as.numeric(input$comp_years)
    
    # Ensure baseline year exists in the data
    if (!(baseline_year %in% data_tidy$year)) {
      message(paste("Baseline year", baseline_year, "not found in data"))
      return(NULL)
    }
    
    # Extract the baseline year's time series:
    baseline_ts <- data_tidy %>%
      filter(year == baseline_year) %>%
      complete(week = 1:52, fill = list(metric = 0)) %>%  
      arrange(week) %>%
      pull(metric)
    
    # Initialize a list to store CCF results for each comparison year
    ccf_results_list <- list()
    
    # Loop through each comparison year (only those present in the data)
    for (comp_year in comp_years) {
      if (!(comp_year %in% data_tidy$year)) {
        message(paste("Skipping - Comparison year", comp_year, "not found in data"))
        next
      }
      
      # For the current comparison year, complete missing weeks and extract metric vector
      comp_ts <- data_tidy %>%
        filter(year == comp_year) %>%
        complete(week = 1:52, fill = list(metric = 0)) %>%  
        arrange(week) %>%
        pull(metric)
      
      # Both vectors should be length 52 if complete() worked properly.
      # As a safeguard, trim both vectors to the minimum length.
      n <- min(length(baseline_ts), length(comp_ts))
      baseline_trim <- baseline_ts[1:n]
      comp_trim     <- comp_ts[1:n]
      
      # Compute the cross-correlation function (CCF) with a maximum lag of 20 weeks
      ccf_out <- ccf(baseline_trim, comp_trim, lag.max = 20, plot = FALSE)
      
      # Save the CCF results in a tibble
      ccf_df <- tibble(
        lag         = ccf_out$lag,
        correlation = ccf_out$acf,
        year        = as.character(comp_year)
      )
      
      ccf_results_list[[as.character(comp_year)]] <- ccf_df
    }
    
    # Combine all CCF results into one data frame
    ccf_results <- bind_rows(ccf_results_list)
    if (nrow(ccf_results) == 0) return(NULL)
    
    # Define colors and line types: baseline (grey, dashed) and comparison years (Set1 palette, solid)
    comp_colors <- setNames(RColorBrewer::brewer.pal(min(8, length(comp_years)), "Set1"),
                            as.character(comp_years))
    color_values <- c(setNames("grey50", as.character(baseline_year)), comp_colors)
    
    line_types <- c(setNames("dashed", as.character(baseline_year)),
                    setNames(rep("solid", length(comp_years)), as.character(comp_years)))
    
    # Create the ggplot object with the CCF results
    p <- ggplot(ccf_results, aes(x = lag, y = correlation, color = year,
                                 group = year, linetype = year)) +
      geom_line(size = 1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      scale_color_manual(values = color_values) +
      scale_linetype_manual(values = line_types) +
      labs(
        x       = "Lag (Weeks)",
        y       = "Correlation",
      ) +
      theme_minimal() +
      theme(
        legend.title = element_blank()
      )
      ylim(-1, 1)
    
    ggplotly(p)
  })
  
  output$cumulative_plot_flu <- renderPlotly({
    
    # Process the data as before
    p <- df %>%
      filter(year >= 2017 & year < 2025) %>%
      filter(disease == "Influenza") %>%
      mutate(date = ISOweek2date(paste0(year, "-W", sprintf("%02d", week), "-1"))) %>% 
      group_by(continent, date) %>%
      summarise(weekly_cases = sum(metric, na.rm = TRUE), .groups = "drop") %>%
      arrange(continent, date) %>%
      group_by(continent) %>%
      mutate(cumulative_cases = cumsum(weekly_cases)) %>%
      ggplot(aes(x = date, y = cumulative_cases, fill = continent)) +
      geom_area(alpha = 0.6, size = 0.5, colour = "black") +
      labs(
        x = "Date",
        y = "Cumulative Cases",
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(),
        legend.position = "bottom",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 10)
      )
    
    # Convert the ggplot object into an interactive Plotly object
    ggplotly(p)
  })
  
  output$cumulative_plot_rsv <- renderPlotly({
    
    # Process the data as before
    p <- df %>%
      filter(year >= 2017 & year < 2024) %>%
      filter(disease == "RSV") %>%
      mutate(date = ISOweek2date(paste0(year, "-W", sprintf("%02d", week), "-1"))) %>% 
      group_by(continent, date) %>%
      summarise(weekly_cases = sum(metric, na.rm = TRUE), .groups = "drop") %>%
      arrange(continent, date) %>%
      group_by(continent) %>%
      mutate(cumulative_cases = cumsum(weekly_cases)) %>%
      ggplot(aes(x = date, y = cumulative_cases, fill = continent)) +
      geom_area(alpha = 0.6, size = 0.5, colour = "black") +
      labs(
        x = "Date",
        y = "Cumulative Cases",
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(),
        legend.position = "bottom",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 10)
      )
    
    # Convert the ggplot object into an interactive Plotly object
    ggplotly(p)
  })
  
  
  output$box_plot_sum <- renderPlotly({
    # Process the data as before
    p <- ggplot(df, aes(x = continent, y = metric, fill = period)) +
      geom_boxplot(outlier.shape = NA) +
      scale_y_log10() +
      facet_wrap(~ disease, scales = "free_y") +
      labs(x = "Continent", y = "Cases (log scale)") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    # Convert the ggplot object into an interactive Plotly object
    ggplotly(p)
  })
  
  
  
  
  
}

shinyApp(ui, server, options = list(host = "192.168.68.67", port = 7047))
