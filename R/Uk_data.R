library(tidyverse)
library(lubridate)


setwd("C:/Users/Evan/Documents/Code/sanofi-rsv-flu")
getwd()


flu_df <- read.csv("csv/influenza.csv") %>%
  mutate(year = factor(year)) %>% 
  select(sex, age, year, month, epiweek, date, metric_value)


flu_df %>%
  filter(age == "all") %>%  # Filter before grouping for efficiency
  group_by(epiweek, year) %>%
  summarise(
    count = sum(metric_value),
    .groups = "drop"
  ) %>%
  arrange(year, epiweek) %>%
  ggplot(aes(x = interaction(year, epiweek, sep = "-"), y = count, colour = year)) +
  geom_col(position = "dodge",
           width = 0.5,
           fill = "gray30") +
  labs(
    x = "Year-Week",
    y = "Number of Cases",
    title = "Weekly Hospitalisation Rate by Epidemiological Week and Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(dplyr)
library(ggplot2)
library(lubridate) # For extracting month

flu_df %>%
  filter(age == "all") %>%  # Filter for all ages and dates from 2015 onwards
  group_by(date) %>%  # Ensure grouping by date
  summarise(
    count = sum(metric_value, na.rm = TRUE),  # Ensure no NA values interfere
    .groups = "drop"
  ) %>%
  mutate(month = factor(month(date, label = TRUE, abbr = TRUE))) %>%  # Extract month as a factor
  ggplot(aes(x = date, y = count, colour = month, group = 1)) +  # Add group = 1 to avoid line break issues
  geom_line(size = 1) +  # Use line to show trends
  scale_colour_brewer(palette = "Paired") +  # Add a colour palette for months
  labs(
    x = "Time",
    y = "Hospital Admission Rate",
    colour = "Month",
    title = "Seasonality Trends of Influenza - Weekly Hospitalisations"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

