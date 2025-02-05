# set working directory
setwd("C:\\Users\\lukeb\\Downloads\\LSHTM\\TERM 2\\Data Challenge\\github\\sanofi-rsv-flu")

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


# plot time lag correlation for a given country and year
# compares to 2019
plot_ccf <- function(data, country, comp_years) {
  
  # Retrieve selected country
  country_data <- data %>%
    filter(country == !!country) %>%
    select(epi_year, epi_week, cases) 
  
  # merge with original data to fill missing weeks with 0 for cases
  country_data_full <- country_data %>%
    complete(epi_year, epi_week = 1:52, fill = list(cases = 0))
  
  # Ensure 'cases' is numeric (double)
  country_data_full$cases <- as.numeric(country_data_full$cases)
  
  # Reshape data to wide format for correlation calculation
  country_data_wide <- country_data_full %>%
    pivot_wider(names_from = epi_year, values_from = cases, values_fill = list(cases = 0))
  
  # Ensure 2019 data is available
  if (!"2019" %in% colnames(country_data_wide)) {
    message(paste("Skipping", country, "- 2019 data not found"))
    return(NULL)
  }
  
  # Placeholder for CCF results
  ccf_results_list <- list()
  
  # Loop through each comparison year (after 2021)
  for (comp_year in comp_years) {
    if (!as.character(comp_year) %in% colnames(country_data_wide)) {
      message(paste("Skipping", country, "-", comp_year, "data not found"))
      next
    }
    
    # Compute cross-correlation function (CCF)
    ccf_result <- ccf(country_data_wide$`2019`, country_data_wide[[as.character(comp_year)]], 
                      lag.max = 20, plot = FALSE)
    
    # Store results in a dataframe
    ccf_df <- data.frame(
      lag = ccf_result$lag,
      correlation = ccf_result$acf,
      year = as.character(comp_year)  # convert to character for plotting
    )
    
    ccf_results_list[[as.character(comp_year)]] <- ccf_df
  }
  
  # Combine all CCF results into one dataframe
  ccf_results_df <- bind_rows(ccf_results_list)
  
  # Check if results are obtained
  if (nrow(ccf_results_df) == 0) {
    message(paste("No valid data found for", country))
    return(NULL)
  }
  
  # Plot the results with multiple years overlaid
  ggplot(ccf_results_df, aes(x = lag, y = correlation, color = year, group = year)) +
    geom_line(size = 1, na.rm = TRUE, aes(color = ifelse(year == "2019", "2019", year))) +
    geom_point(size = 2, na.rm = TRUE, aes(color = ifelse(year == "2019", "2019", year))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Reference line at lag 0
    scale_color_manual(values = c("2019" = "grey50", setNames(RColorBrewer::brewer.pal(max(3, length(comp_years) - 1), "Set1")[1:(length(comp_years) - 1)], comp_years[comp_years != 2019]))) +
    labs(
      title = paste("Time-Lag Correlation, 2019 vs Post-2021 Years for", country),
      x = "Lag (Weeks)",
      y = "Correlation"
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


# plot the ccf, based on country and available years
plot_ccf(flu_dataset, country="Australia", comp_years=c(2019,2022:2024))

# this doesn't work with missing weeks (as in the week number, not cases = NA)
