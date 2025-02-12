library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(ggridges)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)
library(viridis)
library(lubridate)
library(ISOweek)
library(bbplot)

# rm(list = ls())

# Change this for all data
# setwd("C:/Users/Evan/Documents/Code/sanofi-rsv-flu")
getwd()

df <- read_csv("csv/main_dataset.csv")

# df %>% filter(country == "Armenia" & disease == "RSV") %>% view()
# Loop through each country and disease type
unique_countries <- unique(df$country)
unique_diseases <- unique(df$disease)

NH_COUNTRIES = c("Europe", "North America", "Asia")

plot_final <- function(data, target_country, plot_age) {
    
    df_subset <- data %>%  
      filter(country == target_country, week < 53) %>%  
      filter(year >= 2017 & year < 2025) %>% 
      mutate(period = factor(if_else(year < 2021, "Before 2021", "After 2021"), 
                             levels = c("Before 2021", "After 2021")))  # Reverse order if needed
    
    country_i <- unique(df_subset$country)
    disease_i <- unique(df_subset$disease)
    
    caption_text <- paste0("Source: ", 
                           if (any(df_subset$source == "GOV")) { 
                             paste0(unique(df_subset$country), " Official Government Health Source") 
                           } else { 
                             "WHO FluNet" 
                           }, 
                           " | Data range: ", min(df_subset$year), "-", max(df_subset$year))
    
    plot_polar_pres <- df_subset %>%
      group_by(week, period, disease) %>%
      summarise(metric = sum(metric), .groups = "drop") %>% 
      ggplot(aes(x = week, y = metric, color = period)) +
      geom_line(size = 2) +
      facet_wrap(~disease, scales = "free_y") +  
      labs(title = paste0(country_i, " ", paste(disease_i, collapse = ", "), " Polar Plot"), 
           subtitle = "Weeks form the circumference; cases determine the distance from the centre.", 
           x = 'Week', 
           y = "Country-Specific Metric") +
      bbc_style() + 
      theme(
        #   axis.title = element_text(),
        #   legend.position = "bottom",
        axis.ticks.y = element_line(),
        axis.line.y.left = element_line(),
        #   legend.title = element_blank(),
        #   panel.spacing = unit(0.1, "lines"),
        #   strip.text.x = element_text(size = 8),
        axis.text.y = element_text()
      ) +
      coord_polar(theta = "x")
    
    finalise_plot(plot_name = plot_polar_pres, 
                  source = caption_text, 
                  save_filepath = paste0("plots/final/", country_i, "_polar_plot_pres.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
                  width_pixels = 1100, 
                  height_pixels = 700)
    
    
    plot_polar_rep <- df_subset %>%
      group_by(week, period, disease) %>%
      summarise(metric = sum(metric), .groups = "drop") %>% 
      ggplot(aes(x = week, y = metric, color = period)) +
      geom_line(size = 1) +
      facet_wrap(~disease, scales = "free_y") +  
      labs(x = 'Week', 
           y = "Country-Specific Metric") +
      theme_minimal() + 
      theme(
        #   axis.title = element_text(),
        legend.position = "right",
        axis.ticks.y = element_line(),
        axis.line.y.left = element_line(),
        legend.title = element_blank(),
        #   panel.spacing = unit(0.1, "lines"),
        #   strip.text.x = element_text(size = 8),
        axis.text.y = element_text()
      ) +
      coord_polar(theta = "x")
    
    ggsave(
      filename = paste0("plots/final/", country_i, "_polar_plot_rep.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
      plot = plot_polar_rep,  # This refers to the last plot generated
      width = 6,  # Width of the plot (in inches)
      height = 2,  # Height of the plot (in inches)
      dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
    )
  
  
  
  
  if(any(df_subset$continent %in% NH_COUNTRIES)) {
    
      
      plot_ridge_pres <- df_subset %>%
        group_by(year, week, disease) %>%
        summarise(metric = sum(metric, na.rm = TRUE)) %>% 
        mutate(
          Adjusted_Week = week + 26,  # Shift all weeks forward by 26
          Adjusted_Year = if_else(Adjusted_Week > 52, year + 1, year),  # Increment year if week > 52
          Adjusted_Week = if_else(Adjusted_Week > 52, Adjusted_Week - 52, Adjusted_Week)  # Wrap weeks > 52
        ) %>%
        ggplot(aes(x = Adjusted_Week, y = factor(Adjusted_Year), height = metric, fill = factor(Adjusted_Year))) +
        geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01) +
        facet_wrap(~disease) + 
        labs(
          title = paste0(country_i, " Reported Case Density by Year"), 
          subtitle = "Epidemiological Week (Centred around Week 1)",
          caption = paste0("Source: ", if_else(df_subset$source == "GOV", paste0(country_i, " Official Government Health Source"), "WHO FluNet"), " | Data range: ", df_subset$year[which.min(df_subset$year)], "-", df_subset$year[which.max(df_subset$year)])) +
        scale_x_continuous(
          breaks = c(1, 13, 26, 39),  # Key flu season weeks
          labels = c("Week 26", "Week 39", "Week 1", "Week 13")
        ) +
        scale_y_discrete(
          labels = function(x) paste0(as.numeric(x) - 1, "/", x)  # Convert years to "2014/2015" format
        ) +
        bbc_style() +
        theme(
          legend.position = "none",
          panel.spacing = unit(0.1, "lines"),
          panel.grid = element_line(),
          axis.title.x = element_blank()
        )
      
      finalise_plot(plot_name = plot_ridge_pres, 
                    source = caption_text, 
                    save_filepath = paste0("plots/final/", country_i, "_ridge_plot_pres.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
                    width_pixels = 1100, 
                    height_pixels = 700)
      
      plot_ridge_rep <- df_subset %>%
        group_by(year, week, disease) %>%
        summarise(metric = sum(metric, na.rm = TRUE)) %>% 
        mutate(
          Adjusted_Week = week + 26,  # Shift all weeks forward by 26
          Adjusted_Year = if_else(Adjusted_Week > 52, year + 1, year),  # Increment year if week > 52
          Adjusted_Week = if_else(Adjusted_Week > 52, Adjusted_Week - 52, Adjusted_Week)  # Wrap weeks > 52
        ) %>%
        ggplot(aes(x = Adjusted_Week, y = factor(Adjusted_Year), height = metric, fill = factor(Adjusted_Year))) +
        geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01) +
        facet_wrap(~disease) + 
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
          panel.grid = element_line(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      
      ggsave(
        filename = paste0("plots/final/", country_i, "_ridge_plot_rep.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
        plot = plot_ridge_rep,  # This refers to the last plot generated
        width = 6,  # Width of the plot (in inches)
        height = 2,  # Height of the plot (in inches)
        dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
      )
      
      
    
    
    
  }
  
  else {
  
      plot_ridge_pres <- df_subset %>%
        group_by(week, year) %>%
        summarise(metric = sum(metric)) %>%
        na.omit() %>%
        ggplot(aes(x = week, y = factor(year), height = metric, fill = factor(year))) +
        geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01, size = 1) +
        labs(title = paste0(country_i, " Reported Case Density by Year"), x = 'Week', caption = paste0("Source: ", if_else(df_subset$source == "GOV", paste0(country_i, " Official Government Health Source"), "WHO FluNet"), " | Data range: ", df_subset$year[which.min(df_subset$year)], "-", df_subset$year[which.max(df_subset$year)])) +
        bbc_style() +
        theme(
          legend.position = "none",
          panel.spacing = unit(0.1, "lines"),
          axis.title.x = element_text(),
          strip.text.x = element_text(size = 8),
          axis.text.x = element_text()  # Rotates the x-axis labels for better readability
        )
      
      finalise_plot(plot_name = plot_ridge_pres, 
                    source = caption_text, 
                    save_filepath = paste0("plots/final/", country_i, "_ridge_plot_pres.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
                    width_pixels = 1100, 
                    height_pixels = 700)
      
      
      plot_ridge_rep <- df_subset %>%
        group_by(week, year, disease) %>%
        summarise(metric = sum(metric)) %>%
        na.omit() %>%
        ggplot(aes(x = week, y = factor(year), height = metric, fill = factor(year))) +
        geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01, size = 1) +
        labs(x = 'Week') +
        theme_minimal() +
        facet_wrap(~disease) +
        theme(
          legend.position = "none",
          panel.spacing = unit(0.1, "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 8),
          axis.text.x = element_blank()  # Rotates the x-axis labels for better readability
        )
      
      
      ggsave(
        filename = paste0("plots/final/", country_i, "_ridge_plot_rep.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
        plot = plot_ridge_rep,  # This refers to the last plot generated
        width = 6,  # Width of the plot (in inches)
        height = 2,  # Height of the plot (in inches)
        dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
      )
      

    
    
  }
    
    
    if (plot_age == TRUE) {
      
      plot_age_pres <- df_subset %>%
        
        # Step 1: Summarise total metric by age and period
        group_by(period, age, disease) %>%
        summarise(total_metric = sum(metric), .groups = "drop") %>%
        
        # Step 2: Filter out age groups with low data
        
        # Step 3: Join back to original data to retain only selected age groups
        inner_join(df_subset, by = c("period", "age", "disease")) %>%
        
        # Step 4: Summarise at the week level
        group_by(week, period, age, disease) %>%
        summarise(metric = sum(metric), .groups = "drop") %>%
        
        # Remove any missing values
        na.omit() %>% 
        
        ggplot(aes(x = week, y = metric, color = factor(period))) +
        geom_line(size = 1) +
        facet_wrap(~age+disease, scales = "free_y") + 
        labs(
          title = paste0(country_i, " ", disease_i, " by Age"), 
          subtitle = "Before Lockdown is defined as any data prior to 2021", 
          x = 'Week', 
          y = "Country-Specific Metric", 
          caption = paste0(
            "Source: ", 
            ifelse(unique(df_subset$source) == "GOV", 
                   paste0(country_i, " Official Government Health Source"), 
                   "WHO FluNet"), 
            " | Data range: ", min(df_subset$year), "-", max(df_subset$year)
          )
        ) +
        bbc_style() + 
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(),
          panel.spacing = unit(0.1, "lines"),
        )
      
      finalise_plot(plot_name = plot_age_pres, 
                    source = caption_text, 
                    save_filepath = paste0("plots/final/", country_i, "_age_plot_pres.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
                    width_pixels = 1200, 
                    height_pixels = 750)
      
      plot_age_rep <- df_subset %>%
        
        # Step 1: Summarise total metric by age and period
        group_by(period, age, disease) %>%
        summarise(total_metric = sum(metric), .groups = "drop") %>%
        
        # Step 2: Filter out age groups with low data
        
        # Step 3: Join back to original data to retain only selected age groups
        inner_join(df_subset, by = c("period", "age", "disease")) %>%
        
        # Step 4: Summarise at the week level
        group_by(week, period, age, disease) %>%
        summarise(metric = sum(metric), .groups = "drop") %>%
        
        # Remove any missing values
        na.omit() %>% 
        
        ggplot(aes(x = week, y = metric, color = factor(period))) +
        geom_line(size = 1) +
        facet_wrap(~age+disease, scales = "free_y") + 
        labs(
          x = 'Week', 
          y = "Country-Specific Metric", 
        ) +
        theme_minimal() + 
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(),
          panel.spacing = unit(0.1, "lines"),
        )
      
      ggsave(
        filename = paste0("plots/final/", country_i, "_age_plot_rep.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
        plot = plot_age_rep,  # This refers to the last plot generated
        width = 10,  # Width of the plot (in inches)
        height = 6,  # Height of the plot (in inches)
        dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
      )
      
      
    }
    
  
}

target_countries = c("United Kingdom", "United States of America", "Argentina", "Australia", "South Africa", "Japan")

for (country in target_countries) {
  
  age_groups <- df %>% filter(country == !!country) %>% select(age)
  
  print((nrow(unique(age_groups)) < 2) )
  
  if (nrow(unique(age_groups)) < 2) {
    plot_age = FALSE
  } else {plot_age = TRUE}
  
  plot_final(df, country, plot_age)
}


cum_continent_pres <- df %>%
  filter(year >= 2017 & year < 2024) %>%
  mutate(date = ISOweek2date(paste0(year, "-W", sprintf("%02d", week), "-1"))) %>%
  group_by(continent, date, disease) %>%
  summarise(weekly_cases = sum(metric, na.rm = TRUE), .groups = "drop") %>%
  arrange(continent, date) %>%
  group_by(continent, disease) %>%
  mutate(cumulative_cases = cumsum(weekly_cases)) %>%
  tidyr::fill(cumulative_cases, .direction = "down") %>%  # Fill missing values forward
  ggplot(aes(x = date, y = cumulative_cases, fill = continent)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "black") +
  facet_wrap(~disease, scales="free_y") +
  labs(
    title = "Cumulative Reported Cases by Continent Since 2017",
    x = "Date",
    y = "Cumulative Cases",
  ) +
  bbc_style() +
  theme(
    axis.title = element_text(),
    legend.position = "bottom",
    panel.spacing = unit(0.1, "lines"),
  )

finalise_plot(plot_name = cum_continent_pres,
              source = "Source: WHO FluNet / Government Health Sources", 
              save_filepath = paste0("plots/final/", "cum_continent_cases_pres.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
              width_pixels = 1100, 
              height_pixels = 700)


cum_continent_rep <- df %>%
  filter(year >= 2017 & year < 2024) %>%
  mutate(date = ISOweek2date(paste0(year, "-W", sprintf("%02d", week), "-1"))) %>%
  group_by(continent, date, disease) %>%
  summarise(weekly_cases = sum(metric, na.rm = TRUE), .groups = "drop") %>%
  arrange(continent, date) %>%
  group_by(continent, disease) %>%
  mutate(cumulative_cases = cumsum(weekly_cases)) %>%
  tidyr::fill(cumulative_cases, .direction = "down") %>%  # Fill missing values forward
  ggplot(aes(x = date, y = cumulative_cases, fill = continent)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "black") +
  facet_wrap(~disease, scales="free_y") +
  labs(
    x = "Date",
    y = "Cumulative Cases",
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(),
    legend.position = "bottom",
    panel.spacing = unit(0.1, "lines"),
  )

# ggsave(
#   filename = paste0("plots/final/", "cum_continent_cases_rep.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
#   plot = cum_continent_rep,  # This refers to the last plot generated
#   width = 6,  # Width of the plot (in inches)
#   height = 3,  # Height of the plot (in inches)
#   dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
# )


continent_abbr <- c(
  "Africa" = "AF", 
  "Asia" = "AS", 
  "Europe" = "EU", 
  "North America" = "NA", 
  "Oceania" = "OC", 
  "South America" = "SA"
)

plot_box_pres <- df %>% 
  mutate(period = factor(if_else(year < 2021, "Before 2021", "After 2021"), 
                         levels = c("After 2021", "Before 2021")),
         continent = recode(continent, !!!continent_abbr)) %>%   # Recode continents to abbreviations
  drop_na(year) %>% 
  ggplot(aes(x = continent, y = metric, fill = period)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10() +
  facet_wrap(~ disease, scales = "free_y") +
  labs(title = "Distribution of Disease Burden by Continent", x = "Continent", y = "Cases (log scale)") +
  bbc_style()

finalise_plot(plot_name = plot_box_pres,
              source = "Source: WHO FluNet / Government Health Sources", 
              save_filepath = paste0("plots/final/", "boxplot_continent_pres.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
              width_pixels = 1100, 
              height_pixels = 700)


plot_box_rep <- df %>% 
  mutate(period = factor(if_else(year < 2021, "Before 2021", "After 2021"), 
                         levels = c("After 2021", "Before 2021")),
         continent = recode(continent, !!!continent_abbr)) %>%   # Recode continents to abbreviations
  drop_na(year) %>% 
  ggplot(aes(x = continent, y = metric, fill = period)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10() +
  facet_wrap(~ disease, scales = "free_y") +
  labs(title = "Distribution of Disease Burden by Continent", x = "Continent", y = "Cases (log scale)") +
  theme_minimal()


ggsave(
  filename = paste0("plots/final/", "boxplot_continent_pres.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = plot_box_rep,  # This refers to the last plot generated
  width = 6,  # Width of the plot (in inches)
  height = 3,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)



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
  left_join(seasonality_shift_rsv, by = c("name" = "country")) %>% 
  filter(name != "Antarctica")

ggplot(world_map_data_rsv) +
  geom_sf(aes(fill = week_shift), color = "black", size = 0.1) +
  # scale_point_fill_continuous() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "gray70") +
  labs(title = "Change in Time of RSV Peak Pre- and Post- COVID-19",
       fill = "Week Shift") +
  bbc_style() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.ticks = element_line(color = "gray50", size = 0.2),
    axis.line = element_line(color = "gray50", size = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    panel.background = element_blank(),  # removes panel background
    plot.background = element_blank(),   # removes plot background
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text.x = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

finalise_plot(plot_name = last_plot(),
              source = "Source: WHO FluNet / Government Health Sources", 
              save_filepath = paste0("plots/final/", "world_map_rsv_pres.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
              width_pixels = 1000, 
              height_pixels = 500)


ggplot(world_map_data_rsv) +
  geom_sf(aes(fill = week_shift), color = "black", size = 0.1) +
  # scale_point_fill_continuous() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "gray70") +
  labs(fill = "Week Shift") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.ticks = element_line(color = "gray50", size = 0.2),
    axis.line = element_line(color = "gray50", size = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    panel.background = element_blank(),  # removes panel background
    plot.background = element_blank(),   # removes plot background
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text.x = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  filename = paste0("plots/final/", "world_map_rsv_rep.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = last_plot(),  # This refers to the last plot generated
  width = 6,  # Width of the plot (in inches)
  height = 3,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)




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
  left_join(seasonality_shift_flu, by = c("name" = "country")) %>% 
  filter(name != "Antarctica")

ggplot(world_map_data_flu) +
  geom_sf(aes(fill = week_shift), color = "black", size = 0.1) +
  # scale_point_fill_continuous() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "gray70") +
  labs(title = "Change in Time of Influenza Peak Pre- and Post- COVID-19",
       fill = "Week Shift") +
  bbc_style() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.ticks = element_line(color = "gray50", size = 0.2),
    axis.line = element_line(color = "gray50", size = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    panel.background = element_blank(),  # removes panel background
    plot.background = element_blank(),   # removes plot background
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text.x = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

finalise_plot(plot_name = last_plot(),
              source = "Source: WHO FluNet / Government Health Sources", 
              save_filepath = paste0("plots/final/", "world_map_flu_pres.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
              width_pixels = 1000, 
              height_pixels = 500)



ggplot(world_map_data_flu) +
  geom_sf(aes(fill = week_shift), color = "black", size = 0.1) +
  # scale_point_fill_continuous() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "gray70") +
  labs(fill = "Week Shift") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.ticks = element_line(color = "gray50", size = 0.2),
    axis.line = element_line(color = "gray50", size = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    panel.background = element_blank(),  # removes panel background
    plot.background = element_blank(),   # removes plot background
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text.x = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  filename = paste0("plots/final/", "world_map_flu_rep.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = last_plot(),  # This refers to the last plot generated
  width = 6,  # Width of the plot (in inches)
  height = 3,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)



plot_ridge_rep_big <- df %>%
  filter(country %in% target_countries) %>% 
  filter(year >= 2017 & year < 2025) %>% 
  mutate(country = case_when(
    country == "United States of America" ~ "USA",
    country == "United Kingdom" ~ "UK",
    TRUE ~ country
  )) %>%
  group_by(week, year, disease, country) %>%
  summarise(metric = sum(metric)) %>%
  na.omit() %>%
  ggplot(aes(x = week, y = factor(year), height = metric, fill = factor(year))) +
  geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01, size = 1) +
  labs(x = 'Week') +
  facet_grid(~ disease + country) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_blank()  # Rotates the x-axis labels for better readability
  )


ggsave(
  filename = paste0("plots/final/ridge_plot_rep_big.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = plot_ridge_rep_big,  # This refers to the last plot generated
  width = 8,  # Width of the plot (in inches)
  height = 6,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)


plot_polar_rep_big <- df %>%
  filter(country %in% target_countries) %>% 
  mutate(period = factor(if_else(year < 2021, "Before 2021", "After 2021"), 
                         levels = c("Before 2021", "After 2021"))) %>%
  mutate(country = case_when(
    country == "United States of America" ~ "USA",
    country == "United Kingdom" ~ "UK",
    TRUE ~ country
  )) %>%
  group_by(week, period, disease, country) %>%
  summarise(metric = sum(metric), .groups = "drop") %>%
  ggplot(aes(x = week, y = metric, color = period)) +
  geom_line(size = 1) +
  facet_wrap(disease ~ country, scales = "free_y") +
  labs(x = 'Week', 
       y = "Country-Specific Metric") +
  theme_minimal() + 
  theme(
    legend.position = "right",
    axis.ticks.y = element_line(),
    axis.line.y.left = element_line(),
    legend.title = element_blank(),
    axis.text.y = element_text(),
    strip.placement = "outside",  # Places facet labels outside the axis
    panel.spacing.x = unit(1.5, "lines")  # Increases spacing to mimic columns
  ) +
  coord_polar(theta = "x")


ggsave(
  filename = paste0("plots/final/plot_polar_rep_big.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = plot_polar_rep_big,  # This refers to the last plot generated
  width = 8,  # Width of the plot (in inches)
  height = 6,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)
