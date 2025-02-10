# gather stringency data

# packages and libraries
install.packages("readxl")
install.packages("ggthemes")
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# gather data
stringency <- read_csv("csv/stringency_data.csv") %>%
  select(
    CountryName,
    Date,
    StringencyIndex_Average,
    GovernmentResponseIndex_Average,
    ContainmentHealthIndex_Average,
    EconomicSupportIndex
  ) %>%
  # remove first row
  filter(row_number() > 1) %>%
  # make year and week consistent with other data
  mutate(
    Date = ymd(Date),
    Year = year(Date),
    Week = isoweek(Date)
  ) %>%
  select(-Date)


# take averages over each week
stringency <- stringency %>%
  group_by(CountryName, Year, Week) %>%
  summarise(
    StringencyIndex_Avg = mean(StringencyIndex_Average, na.rm = TRUE),
    GovernmentResponseIndex_Avg = mean(GovernmentResponseIndex_Average, na.rm = TRUE),
    ContainmentHealthIndex_Avg = mean(ContainmentHealthIndex_Average, na.rm = TRUE),
    EconomicSupportIndex_Avg = mean(EconomicSupportIndex, na.rm = TRUE),
    .groups = "drop"
  )

# rename for consistency with main_dataset
stringency <- stringency %>%
  rename(country = CountryName, year = Year, week = Week)

# drop unwanted variables
stringency <- stringency %>%
  select(-c("GovernmentResponseIndex_Avg", "ContainmentHealthIndex_Avg", "EconomicSupportIndex_Avg"))


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
  filter(year %in% 2020:2022) %>% # UPDATE later to include more pre-COVID years
  select(-disease)

# collapse into weekly summaries by summing over age groups

flu_dataset <- flu_dataset %>%
  group_by(country, year, week) %>%
  summarise(cases = sum(metric, na.rm = TRUE), .groups = "drop")


# combine datasets together
full <- flu_dataset %>%
  left_join(stringency, by = c("country", "year", "week")) %>%
  mutate(
    # Create a new Date column combining Year and Week
    Date = as.Date(paste(year, week, 1, sep = "-"), format = "%Y-%U-%u")
  )


# try on Australia
australia_data <- full %>%
  filter(country == "Australia")

# Calculate the scaling factor based on min and max values of both variables
max_first  <- max(australia_data$StringencyIndex_Avg, na.rm = TRUE)   # Max of Stringency Index
max_second <- max(australia_data$cases, na.rm = TRUE) # Max of Cases
min_first  <- min(australia_data$StringencyIndex_Avg, na.rm = TRUE)   # Min of Stringency Index
min_second <- min(australia_data$cases, na.rm = TRUE) # Min of Cases

# Define scaling and shifting for the second axis
scale = (max_second - min_second) / (max_first - min_first)
shift = min_first - min_second

# Scaling function for secondary axis
scale_function <- function(x, scale, shift) {
  return ((x) * scale - shift)
}

# Inverse scaling function for the secondary variable
inv_scale_function <- function(x, scale, shift) {
  return ((x + shift) / scale)
}

# Create the dual axis plot
ggplot(australia_data, aes(x = Date)) +
  # Line plot for Stringency Index
  geom_line(aes(y = StringencyIndex_Avg, color = "Stringency Index"), size = 1) +
  
  # Bar plot for Daily New Cases (on right y-axis)
  geom_bar(aes(y = inv_scale_function(cases, scale, shift), fill = "Daily New Cases"), stat = "identity", alpha = 0.6, position = "identity") +
  
  scale_x_date(
    date_labels = "%b %Y",  # Format the dates as "Month Year"
    date_breaks = "3 months"  # Show a tick every 3 months
  ) +
  
  scale_y_continuous(
    name = "Stringency Index",
    limits = c(min_first, max_first),  # Set y-axis for Stringency Index
    sec.axis = sec_axis(~scale_function(., scale, shift), name = "Number of Cases")  # Secondary y-axis for cases
  ) +
  
  labs(
    title = "COVID-19 Stringency and Daily New Cases in Australia (2020-2022)",
    x = "Date (2020-2022)",
    color = "",
    fill = ""
  ) +
  
  scale_color_manual(values = c("Stringency Index" = "purple")) +
  scale_fill_manual(values = c("Daily New Cases" = "orange")) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "top"
  )




# Filter for the countries of interest
countries_of_interest <- c("Australia", "Chile", "Japan", "South Africa", "United Kingdom", "United States of America")

# Combine the stringency data with the case data by country, year, and week
combined_data <- stringency %>%
  filter(country %in% countries_of_interest) %>%
  left_join(flu_dataset, by = c("country", "year", "week"))

# Prepare the data, create Date column from year and week
combined_data <- combined_data %>%
  mutate(Date = as.Date(paste(year, week, 1, sep = "-"), format = "%Y-%U-%u"))

# Calculate per-country max and min values for StringencyIndex and cases
max_min_values <- combined_data %>%
  group_by(country) %>%
  summarise(
    max_stringency = max(StringencyIndex_Avg, na.rm = TRUE),
    min_stringency = min(StringencyIndex_Avg, na.rm = TRUE),
    max_cases = max(cases, na.rm = TRUE),
    min_cases = min(cases, na.rm = TRUE)
  )

# Merge these max/min values back into the original data
combined_data <- combined_data %>%
  left_join(max_min_values, by = "country")

# Create the scaling functions for each country
combined_data <- combined_data %>%
  mutate(
    scale = (max_cases - min_cases) / (max_stringency - min_stringency),
    shift = min_stringency - min_cases
  )

# Define scaling function for secondary axis based on country-specific scale
scale_function <- function(x, scale, shift) {
  return ((x) * scale - shift)
}

# Inverse scaling function for the secondary variable (cases)
inv_scale_function <- function(x, scale, shift) {
  return ((x + shift) / scale)
}

# Create the dual-axis plot with a bar plot for 'cases' and line plot for 'StringencyIndex_Avg', faceting by country
ggplot(combined_data, aes(x = Date)) +
  # Line plot for Stringency Index
  geom_line(aes(y = StringencyIndex_Avg, color = "Stringency Index"), size = 1) +
  
  # Bar plot for Daily New Cases (on right y-axis), scaled by country-specific factors
  geom_bar(aes(y = inv_scale_function(cases, scale, shift), fill = "Weekly New Cases"), stat = "identity", alpha = 0.6, position = "identity") +
  
  scale_x_date(
    date_labels = "%b %Y",  # Display Month and Year (e.g., Mar 2020)
    date_breaks = "3 months"  # Show a tick every 3 months
  ) +
  
  scale_y_continuous(
    name = "Stringency Index",
    limits = c(min(combined_data$min_stringency), max(combined_data$max_stringency)),  # Set y-axis for Stringency Index
    sec.axis = sec_axis(~scale_function(., combined_data$scale[1], combined_data$shift[1]), name = "Number of Cases")  # Secondary y-axis for cases, using country-specific scale
  ) +
  
  labs(
    title = "COVID-19 Stringency and Weekly Influenza Cases (2020-2022)",
    x = "Date (2020-2022)",
    color = "",
    fill = ""
  ) +
  
  scale_color_manual(values = c("Stringency Index" = "red")) +
  scale_fill_manual(values = c("Weekly New Cases" = "orange")) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  
  # Facet grid by country with free y-axis scaling for cases
  facet_wrap(~country, scales = "free_y")  # Free y-axis scales so each country can have its own scale for cases
