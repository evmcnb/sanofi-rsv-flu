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
    ccf_result <- ccf(country_data_wide[[as.character(baseline_year)]], 
                      country_data_wide[[as.character(comp_year)]], 
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
plot_ccf(flu_dataset, country = "Japan", baseline_year = 2017, comp_years = c(2022:2025))
