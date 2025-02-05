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

# grouped dataset based on covid

flu_covid <- flu_dataset %>%
  mutate(is_covid = ifelse(year < 2021, 0, 1),
         is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(epi_week, is_covid) %>%
  summarise(cases = sum(cases)) %>%
  ungroup()

ggplot(flu_covid, aes(x=epi_week, y=cases, color=is_covid, group=is_covid)) +
  geom_line() + geom_point()
