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

## From Evan's shiny

# library(shinydashboard)

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

setwd("C:/Users/icnarc246/OneDrive - ICNARC/Desktop/Trainings and personal docs/LSHTM/Data challenge/sanofi-rsv-flu")

##------------------------------------------------------------------------------
# Load datasets
flu_data <- read_csv("csv/main_dataset.csv")

seasonality_shift <- read_csv("csv/flu_net_shift.csv")

##------------------------------------------------------------------------------
## Data wrangling

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

##------------------------------------------------------------------------------

# User Interface ----------------------------------------------------------

ui <- dashboardPage(
  title = "SANOFI RSV and influenza seasonality trends",
  ##------------------------------------------------------------------------------        
  # Header ----
  header = dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; justify-content: center; flex-direction: column;",
      tags$img(
        src = "LSHTM_Logo.png",
        style = "height: 60px; width: auto; margin-bottom: 20px;" 
      ),
      tags$span("Influenza and RSV seasonality trends pre - and post- pandemic", style = "font-size: 14px; font-weight: bold;")
    ),
    rightUi = dropdownMenu(
      #badgeStatus = "info",
      #type = "notifications",
      #dark = TRUE
    )
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      #menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Global Seasonality", tabName = "global", icon = icon("globe")),
      menuItem("Individual Seasonality", tabName = "individual", icon = icon("chart-line"))
    )
  ),
  
  # Controlbar & Footer ----
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  
  ##------------------------------------------------------------------------------        
  # Body ----
  body = dashboardBody(
    tabItems(
      # Home Tab ----
      tabItem(
        tabName = "home",
        jumbotron(
          title = "Welcome!",
          status = "info",
          lead = "How have the seasonality of Respiratory Syncytial Virus (RSV) and influenza hospitalisations changed since the emergence of SARS-COV-2? ",
          href = "https://www.icnarc.org/wp-content/uploads/2024/05/CMP-Comparative-Unit-Report-2022-23.xlsx.xlsx", ## REPLACE THE LINK
          btnName = "Download", 
          "Data available from XYZZZ"
        ),
        fluidRow(
          box(
            title = "add some context",
            #subtitle = "XYZZ",
            status = "purple",
            solidHeader = TRUE,
            "XYZZ"
          ),
          box(
            title = "CMP",
            width = 6,
            collapsible = FALSE,
            blockQuote(
              "XYZZ",
              color = "purple"
            )
          )
        ) # fluidrow
      ), # tabitem
      ##------------------------------------------------------------------------------      
      
      ##### Dashboard Tab ----
      
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Total XYZ",
              #value = total_adms,  ## ADD THE VALUE 
              icon = icon("list"), 
              color = "primary")
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              #value = observed_infections, ## ADD THE VALUE 
              title = "Total XYZ", 
              icon = icon("table"),
              color = "primary"
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              # value = Total_high_risk_sepsis, ## ADD THE VALUE 
              title = "High-risk XYZ",
              icon = icon("table"),
              color = "primary"
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              ##value = CMP_units, ## ADD THE VALUE 
              title = "Participating countries",
              icon = icon("location-dot"),
              color = "primary"
            )
          ),
          
          ##------------------------------------------------------------------------------
          ## Data boxes
          fluidRow(
            sortable(
              # width = 6,
              box(
                title = "Dashboard Overview",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                "This section will display interactive charts and tables."
              ),
              column(10,
                     checkboxGroupInput("country", "Select Country:",
                                        choices = unique(flu_data$country),
                                        selected = "United Kingdom",
                                        inline = TRUE)
                     ),
              column(6,
                     checkboxGroupInput("disease", "Select Disease:",
                                        choices = unique(flu_data$disease),
                                        selected = "Influenza",
                                        inline = TRUE)
                     ),
              # column(6,
              #        checkboxGroupInput("age_group", "Select Age Group:",
              #                           choices = NULL,
              #                           choices = unique(flu_data$disease),
              #                           selected = "All"
              #                           )
              # ),
              box(
                title = HTML("<b>High-risk admissions from the ward by region and unit type [2022-2023]</b>"), 
                #title = "High-risk admissions from the ward by region and unit type [2022-2023]", 
                width = 12, 
                status = "olive",
                collapsible = FALSE, 
                # ribbon(
                #   text = "",
                #   color = "olive"
                # ),
                plotlyOutput("high_risk_plot")
              ),
              ##------------ box for high risk sepsis
              box(
                title = HTML("<b>High-risk sepsis admissions from the ward by region and unit type [2022-2023]</b>"), 
                #title = "High-risk sepsis admissions from the ward by region and unit type [2022-2023]", 
                width = 12, 
                status = "olive",
                collapsible = FALSE, 
                ribbon(
                  text = "NEW",
                  color = "olive"
                ),
                plotlyOutput("sepsis_plot")
              )

            ) # sortable
          )  # fluid row
        ) # fluid row
      ) # tabItem
    ) # tabItems
  ) # dashboardBody
) # dashboardPage


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
  
  
  
}

shinyApp(ui, server)