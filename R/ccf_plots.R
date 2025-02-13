# set working directory
# setwd("C:\\Users\\lukeb\\Downloads\\LSHTM\\TERM 2\\Data Challenge\\github\\sanofi-rsv-flu")

# packages and libraries
install.packages("readxl")
install.packages("ggthemes")
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)


# retrieve the main dataset with filters on years and disease

flu_dataset <- read_csv("csv/main_dataset.csv", 
                        col_types = cols(
                          country = col_character(),
                          source = col_character(),
                          month = col_character(),
                          disease = col_character(),
                          age = col_character(),  # not numeric in some cases
                          year = col_double(),
                          week = col_double(),
                          metric = col_double(),
                          population = col_double()
                        )) %>%
  select(country, year, week, disease, age, metric) %>%
  filter(disease == "Influenza") %>%
  filter(year >= 2017) %>%
  filter(week < 53) %>%
  select(-disease)

# collapse into weekly summaries by summing over age groups

flu_dataset <- flu_dataset %>%
  group_by(country, year, week) %>%
  summarise(cases = sum(metric, na.rm = TRUE), .groups = "drop")

# hemisphere information for included countries
inc_countries <- unique(flu_dataset$country)
southern_hemisphere <- c(
  "Argentina", "Australia", "Bolivia", "Botswana", "Brazil", "Chile", "Colombia",
  "Ecuador", "Eswatini", "Fiji", "Lesotho", "Madagascar", "Malawi", "Mauritius",
  "Mozambique", "Namibia", "New Zealand", "Paraguay", "Peru", "Papua New Guinea",
  "Rwanda", "Samoa", "Solomon Islands", "South Africa", "Tanzania", "Timor-Leste",
  "Tonga", "Uruguay", "Vanuatu", "Zambia", "Zimbabwe"
)
hemisphere <- setNames(
  ifelse(inc_countries %in% southern_hemisphere, "S", "N"), 
  inc_countries
)

# add epi_week which is the shifted week based on hemisphere
flu_dataset <- flu_dataset %>%
  mutate(epi_week = case_when(
    hemisphere[country] == "N" ~ (week + 26) %% 52,  # Shift by 26 weeks if in Northern Hemisphere
    TRUE ~ week  # Keep the same for Southern Hemisphere
  ))

# add epi_year, based on flu season and hemisphere
flu_dataset <- flu_dataset %>%
  mutate(epi_year = case_when(
    hemisphere[country] == "N" & week < 26 ~ year - 1,  # Northern Hemisphere: Before week 26 is last year's season
    TRUE ~ year  # Southern Hemisphere or Northern after week 26
  ))

# grouped dataset based on covid

flu_covid <- flu_dataset %>%
  mutate(is_covid = ifelse(year < 2021, 0, 1),
         is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(epi_week, is_covid) %>%
  summarise(cases = sum(cases)) %>%
  ungroup()

ggplot(flu_covid, aes(x=epi_week, y=cases, color=is_covid, group=is_covid)) +
  geom_line() + geom_point()


plot_ccf <- function(data, country, baseline_year, comp_years) {
  
  # Retrieve selected country data
  country_data <- data %>%
    filter(country == !!country) %>%
    select(epi_year, epi_week, cases) 
  
  # Fill in missing weeks (ensures all 52 weeks are present)
  country_data_full <- country_data %>%
    complete(epi_year, epi_week = 1:52, fill = list(cases = 0))
  
  # Ensure 'cases' is numeric
  country_data_full$cases <- as.numeric(country_data_full$cases)
  
  # Reshape data to wide format
  country_data_wide <- country_data_full %>%
    pivot_wider(names_from = epi_year, values_from = cases, values_fill = list(cases = 0))
  
  # Ensure the baseline year is available
  if (!as.character(baseline_year) %in% colnames(country_data_wide)) {
    message(paste("Skipping", country, "- Baseline year", baseline_year, "data not found"))
    return(NULL)
  }
  
  # Placeholder for CCF results
  ccf_results_list <- list()
  
  # Add the baseline year to comparison years for plotting
  all_years <- unique(c(baseline_year, comp_years))
  
  # Loop through each year
  for (comp_year in all_years) {
    if (!as.character(comp_year) %in% colnames(country_data_wide)) {
      message(paste("Skipping", country, "-", comp_year, "data not found"))
      next
    }
    
    # Compute cross-correlation function (CCF)
    ccf_result <- ccf(country_data_wide[[as.character(comp_year)]],
                      country_data_wide[[as.character(baseline_year)]],
                      lag.max = 20, plot = TRUE)
    
    # Store results in a dataframe
    ccf_df <- data.frame(
      lag = ccf_result$lag,
      correlation = ccf_result$acf,
      year = as.character(comp_year)
    )
    
    ccf_results_list[[as.character(comp_year)]] <- ccf_df
  }
  
  # Combine all CCF results
  ccf_results_df <- bind_rows(ccf_results_list)
  
  # Check if any results exist
  if (nrow(ccf_results_df) == 0) {
    message(paste("No valid data found for", country))
    return(NULL)
  }
  
  # Define color values (Baseline in grey, others in Set1)
  comp_colors <- if (length(comp_years) > 0) {
    setNames(RColorBrewer::brewer.pal(min(8, length(comp_years)), "Set1"), as.character(comp_years))
  } else {
    c()
  }
  color_values <- c(setNames("grey50", as.character(baseline_year)), comp_colors)
  
  # Define line types (Baseline is dashed, others are solid)
  line_types <- c(setNames("dashed", as.character(baseline_year)), 
                  setNames(rep("solid", length(comp_years)), as.character(comp_years)))
  
  
  ggplot(ccf_results_df, aes(x = lag, y = correlation, color = year, group = year, linetype = year)) +
    geom_line(size = 1, na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Reference line at lag 0
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = line_types) +
    labs(
      title = paste("Time-Lag Correlation:", baseline_year, "vs Comparison Years for", country),
      x = "Lag (Weeks)",
      y = "Correlation",
      caption = "Source: FLUNET/GOV | Data range: 2017 - latest available"
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.direction = "vertical",
      axis.ticks.y = element_line(),
      axis.line.y.left = element_line(),
      legend.title = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8),
      axis.text.y = element_text()
    ) +
    scale_x_continuous(breaks = seq(-20, 20, by = 5)) +
    ylim(-1, 1) # Correlation limits
}


# Example usage:
plot_ccf(flu_dataset, country = "Japan", baseline_year = 2019, comp_years = c(2022:2025))




# Add facet grid for multiple countries
plot_ccf_multi <- function(data, countries, baseline_year, comp_years) {
  
  ccf_results_list <- list()
  
  for (country in countries) {
    # Retrieve selected country data
    country_data <- data %>%
      filter(country == !!country) %>%
      select(epi_year, epi_week, cases)
    
    # Fill in missing weeks (ensures all 52 weeks are present)
    country_data_full <- country_data %>%
      complete(epi_year, epi_week = 1:52, fill = list(cases = 0))
    
    # Ensure 'cases' is numeric
    country_data_full$cases <- as.numeric(country_data_full$cases)
    
    # Reshape data to wide format
    country_data_wide <- country_data_full %>%
      pivot_wider(names_from = epi_year, values_from = cases, values_fill = list(cases = 0))
    
    # Ensure the baseline year is available
    if (!as.character(baseline_year) %in% colnames(country_data_wide)) {
      message(paste("Skipping", country, "- Baseline year", baseline_year, "data not found"))
      next
    }
    
    # Add the baseline year to comparison years for plotting
    all_years <- unique(c(baseline_year, comp_years))
    
    # Loop through each year
    for (comp_year in all_years) {
      if (!as.character(comp_year) %in% colnames(country_data_wide)) {
        message(paste("Skipping", country, "-", comp_year, "data not found"))
        next
      }
      
      # Compute cross-correlation function (CCF)
      ccf_result <- ccf(country_data_wide[[as.character(comp_year)]],
                        country_data_wide[[as.character(baseline_year)]],
                        lag.max = 20, plot = FALSE)
      
      # Store results in a dataframe
      ccf_df <- data.frame(
        lag = ccf_result$lag,
        correlation = ccf_result$acf,
        year = as.character(comp_year),
        country = country  # Add country column for facet wrapping
      )
      
      ccf_results_list[[paste(country, comp_year, sep = "_")]] <- ccf_df
    }
  }
  
  # Combine all CCF results
  ccf_results_df <- bind_rows(ccf_results_list)
  
  # Check if any results exist
  if (nrow(ccf_results_df) == 0) {
    message("No valid data found for the selected countries.")
    return(NULL)
  }
  
  # Define color values (Baseline in grey, others in Set1)
  comp_colors <- if (length(comp_years) > 0) {
    setNames(viridis::viridis(length(comp_years)), as.character(comp_years))
  } else {
    c()
  }
  color_values <- c(setNames("grey50", as.character(baseline_year)), comp_colors)
  
  # Define line types (Baseline is dashed, others are solid)
  line_types <- c(setNames("dashed", as.character(baseline_year)), 
                  setNames(rep("solid", length(comp_years)), as.character(comp_years)))
  
  ggplot(ccf_results_df, aes(x = lag, y = correlation, color = year, group = year, linetype = year)) +
    geom_line(size = 1, na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Reference line at lag 0
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = line_types) +
    labs(
      title = paste("Time-Lag Correlation:", baseline_year, "vs Comparison Years"),
      x = "Week Shift",
      y = "Correlation",
      caption = "Source: FLUNET/GOV | Data range: 2017 - latest available"
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.direction = "vertical",
      axis.ticks.y = element_line(),
      axis.line.y.left = element_line(),
      legend.title = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8),
      axis.text.y = element_text()
    ) +
    scale_x_continuous(breaks = seq(-20, 20, by = 5)) +
    ylim(-1, 1) +  # Correlation limits
    facet_wrap(~ country)  # Facet wrap for multiple countries
}


countries_rep <- c("Australia", "Chile", "Japan", "South Africa", "United Kingdom", "United States of America")
plot_ccf_facet <- plot_ccf_multi(flu_dataset, countries = countries_rep, baseline_year = 2019, comp_years = c(2022:2024))


ggsave(
  filename = paste0("plots/Influenza/Other/flu_ccf_facet.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = plot_ccf_facet,  # This refers to the last plot generated
  width = 8,  # Width of the plot (in inches)
  height = 6,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)






# updated function to compare to entire pre-2021 data rater than a single baseline year


plot_ccf_multi_agg <- function(data, countries, comp_years) {
  
  ccf_results_list <- list()
  
  for (country in countries) {
    # Retrieve selected country data
    country_data <- data %>%
      filter(country == !!country) %>%
      select(epi_year, epi_week, cases)
    
    # Fill in missing weeks (ensures all 52 weeks are present)
    country_data_full <- country_data %>%
      complete(epi_year, epi_week = 1:52, fill = list(cases = 0))
    
    # Ensure 'cases' is numeric
    country_data_full$cases <- as.numeric(country_data_full$cases)
    
    # Aggregate pre-2021 data to form the baseline
    baseline_data <- country_data_full %>%
      filter(epi_year < 2021) %>%
      group_by(epi_week) %>%
      summarise(pre2021_baseline = sum(cases, na.rm = TRUE)) %>%
      ungroup()
    
    # Reshape comparison data to wide format
    country_data_wide <- country_data_full %>%
      filter(epi_year %in% comp_years) %>%
      pivot_wider(names_from = epi_year, values_from = cases, values_fill = list(cases = 0))
    
    # Ensure there is valid baseline data
    if (nrow(baseline_data) == 0) {
      message(paste("Skipping", country, "- No pre-2021 data available"))
      next
    }
    
    # Merge baseline data with the wide-format dataset
    country_data_wide <- left_join(country_data_wide, baseline_data, by = "epi_week")
    
    # Loop through each comparison year
    for (comp_year in comp_years) {
      if (!as.character(comp_year) %in% colnames(country_data_wide)) {
        message(paste("Skipping", country, "-", comp_year, "data not found"))
        next
      }
      
      # Compute cross-correlation function (CCF)
      ccf_result <- ccf(country_data_wide[[as.character(comp_year)]],
                        country_data_wide$pre2021_baseline,
                        lag.max = 20, plot = FALSE)
      
      # Store results in a dataframe
      ccf_df <- data.frame(
        lag = ccf_result$lag,
        correlation = ccf_result$acf,
        year = as.character(comp_year),
        country = country  # Add country column for facet wrapping
      )
      
      ccf_results_list[[paste(country, comp_year, sep = "_")]] <- ccf_df
    }
  }
  
  # Combine all CCF results
  ccf_results_df <- bind_rows(ccf_results_list)
  
  # Check if any results exist
  if (nrow(ccf_results_df) == 0) {
    message("No valid data found for the selected countries.")
    return(NULL)
  }
  
  # Define color values (Baseline in grey, others in Set1)
  comp_colors <- if (length(comp_years) > 0) {
    setNames(viridis::viridis(length(comp_years)), as.character(comp_years))
  } else {
    c()
  }
  color_values <- c(setNames("grey50", "Pre-2021"), comp_colors)
  
  # Define line types (Baseline is dashed, others are solid)
  line_types <- c(setNames("dashed", "Pre-2021"), 
                  setNames(rep("solid", length(comp_years)), as.character(comp_years)))
  
  plot_ccf_facet_agg <- ggplot(ccf_results_df, aes(x = lag, y = correlation, color = year, group = year, linetype = year)) +
    geom_line(size = 2, na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = line_types) +
    labs(
      title = NULL,
      x = "Week Shift",
      y = "Correlation",
      caption = "Source: FLUNET/GOV | Data range: 2017 - latest available",
      color = "Year",  # Set legend title to "Year"
      linetype = "Year"
    ) +
    bbc_style() +
    theme(
      axis.title = element_text(size = 20),  # Slightly bigger axis titles
      axis.text = element_text(size = 18),  # Slightly bigger axis text
      legend.position = "right",  # Keep legend on the right
      legend.text = element_text(size = 18),  # Bigger legend text
      legend.title = element_text(size = 20, face = "bold"),  # "Year" title for legend
      legend.key.width = unit(1.8, "cm"),  # Adjust legend key width
      legend.spacing.y = unit(0.6, "cm"),  # Space out legend entries
      legend.direction = "vertical",  # Stack legend vertically
      axis.ticks.y = element_line(),
      axis.line.y.left = element_line(),
      panel.spacing = unit(1.2, "lines"),  # Increase spacing between plots
      strip.text = element_text(size = 20, face = "bold"),  # Bigger country names
      strip.background = element_blank(),  # Remove grey background from country names
      panel.grid.major = element_line(color = "grey90", size = 0.3),  # Faint grid lines
      panel.grid.minor = element_line(color = "grey95", size = 0.2)  # Even fainter minor grid lines
    ) +
    scale_x_continuous(breaks = seq(-20, 20, by = 5)) +
    ylim(-1, 1) +
    facet_wrap(~ country, scales = "free_y")  # Allow different y-axis scales per country if needed
  
  finalise_plot(
    plot_name = plot_ccf_facet_agg, 
    source = "FLUNET/GOV | Data range: 2017 - latest available", 
    save_filepath = "plots/Influenza/Other/flu_ccf_facet_agg.png", 
    width_pixels = 1400,  # Slightly wider to fit text and legend
    height_pixels = 950  # Increased height for better readability
  )
  
  
}

countries_rep <- c("Australia", "Chile", "Japan", "South Africa", "United Kingdom", "United States of America")
plot_ccf_facet_agg <- plot_ccf_multi_agg(flu_dataset, countries = countries_rep, comp_years = c(2022:2024))

