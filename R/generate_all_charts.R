library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(ggridges)
library(rnaturalearth)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# rm(list = ls())

# Change this for all data
setwd("C:/Users/Evan/Documents/Code/sanofi-rsv-flu")
getwd()

df <- read_csv("csv/main_dataset.csv")


# Loop through each country and disease type
unique_countries <- unique(df$country)
unique_diseases <- unique(df$disease)

world_map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(iso_a3, name, pop_est, continent)

df <- merge(df, world_map_data, by.x = "country", by.y = "name")

NH_COUNTRIES = c("Europe", "North America", "Asia")

for (country_i in unique_countries) {
  for (disease_i in unique_diseases) {
    
    # Filter data for the specific country and disease
    df_subset <- df %>%
      filter(country == country_i, disease == disease_i)
    
    # Skip if there is no data for this combination
    if (nrow(df_subset) == 0) next
    
    plot_polar <- df_subset %>%
      mutate(is_covid = if_else(year < 2021, 0, 1),
             is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
      group_by(week, is_covid) %>%
      filter(week < 53) %>% 
      na.omit() %>% 
      summarise(metric = sum(metric)) %>% 
      ggplot(aes(x = week, y = metric, color = factor(is_covid))) +
      geom_line(size = 1) +
      labs(title = paste0(country_i, " ", disease_i, " Polar Plot"), subtitle = "Before Lockdown is defined as any data prior to 2021", x = 'Week', y = "Country Specific Metric") +
      theme_fivethirtyeight() + 
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
    
    ggsave(
      filename = paste0("plots/generated/test/",disease_i,"/", country_i, "_polar_plot.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
      plot = plot_polar,  # This refers to the last plot generated
      width = 6,  # Width of the plot (in inches)
      height = 6,  # Height of the plot (in inches)
      dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
    )
    
    if(any(df_subset$continent %in% NH_COUNTRIES)) {
      
      plot_ridge <- df_subset %>%
        filter(age == "All") %>%
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
        labs(
          title = paste0(country_i, " ", disease_i, " Density by Year"),
          x = paste0(disease_i, ' Season Week (Centred around Week 1)')
        ) +
        scale_x_continuous(
          breaks = c(1, 13, 26, 39),  # Key flu season weeks
          labels = c("Week 26", "Week 39", "Week 1", "Week 13")
        ) +
        scale_y_discrete(
          labels = function(x) paste0(as.numeric(x) - 1, "/", x)  # Convert years to "2014/2015" format
        ) +
        theme_fivethirtyeight() +
        theme(
          legend.position = "none",
          panel.spacing = unit(0.1, "lines"),
          axis.title.x = element_text(),
          strip.text.x = element_text(size = 8),
          axis.text.x = element_text(size = 10)
        )
      
    }
    
    else {
      
      plot_ridge <- df_subset %>%
        filter(age == "All") %>%
        filter(week < 53) %>% 
        group_by(week, year) %>%
        summarise(metric = sum(metric)) %>%
        na.omit() %>% 
        ggplot(aes(x = week, y = factor(year), height = metric, fill = factor(year))) +
        geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01, size = 1) +
        labs(title = paste0(country_i, " ", disease_i, " Density by Year"), , x = 'Week') +
        theme_fivethirtyeight() + 
        theme(
          legend.position = "none",
          panel.spacing = unit(0.1, "lines"),
          axis.title.x = element_text(),
          strip.text.x = element_text(size = 8),
          axis.text.x = element_text()  # Rotates the x-axis labels for better readability
        )
      
    }
    
    ggsave(
      filename = paste0("plots/generated/test/",disease_i,"/", country_i, "_ridge_density.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
      plot = plot_ridge,  # This refers to the last plot generated
      width = 6,  # Width of the plot (in inches)
      height = 6,  # Height of the plot (in inches)
      dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
    )
    
    if (length(unique(df_subset$age)) < 2) next
    
    plot_age <- df_subset %>%
      filter(week < 53) %>% 
      mutate(is_covid = if_else(year < 2021, 0, 1),
             is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
      group_by(week, is_covid, age) %>%
      summarise(metric = sum(metric)) %>% 
      na.omit() %>% 
      ggplot(aes(x = week, y = metric, color = factor(is_covid))) +
      geom_line(size = 1) +
      facet_wrap(~age) +
      labs(title = paste0(country_i, " ", disease_i, " by Age"), subtitle = "Before Lockdown is defined as any data prior to 2021", x = 'Week', y = "Country Specific Metric") +
      theme_fivethirtyeight() + 
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(),
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      )
    
    ggsave(
      filename = paste0("plots/generated/test/",disease_i,"/", country_i, "_age_strat_plot.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
      plot = plot_age,  # This refers to the last plot generated
      width = 6,  # Width of the plot (in inches)
      height = 6,  # Height of the plot (in inches)
      dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
    )
  
    
  }
}

print("Plots generated and saved in the 'plots' folder.")
