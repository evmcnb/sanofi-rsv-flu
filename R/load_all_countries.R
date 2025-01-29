library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(ggridges)

rm(list = ls())

# Change this for all data
# setwd("C:/Users/Evan/Documents/Code/sanofi-rsv-flu")
getwd()

source("R/France_data.R")
source("R/Uk_data.R")
source("R/Australia_data.R")
source("R/Denmark_data.R")
source("R/HK_data.R")
source("R/US_data.R")
source("R/Argentina_data_LL.R")
source("R/Ireland_code.R")
source("R/Finland.R")
source("R/Japan_data.R")


ba_df <- data.frame(
  country = character(),
  year = integer(),
  month = integer(),
  week = integer(),
  disease = character(),
  age = numeric(),
  metric = numeric()
)

FLU_FR_MERGE <- flu_france_data %>%
  mutate(country = "France", 
         disease = "Influenza",
         month = NA,
         age = NA,
         metric = inc) %>%
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()
  
ba_df <- rbind(ba_df, FLU_FR_MERGE)

FLU_UK_MERGE <- flu_uk_data %>% 
  mutate(country = "United Kingdom",
         disease = "Influenza",
         metric = metric_value,
         week = epiweek) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

ba_df <- rbind(ba_df, FLU_UK_MERGE)

RSV_UK_MERGE <- rsv_uk_data %>% 
  mutate(country = "United Kingdom",
         disease = "RSV",
         metric = metric_value,
         week = epiweek) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

FLU_AU_MERGE <- flu_au_data %>% 
  mutate(country = "Australia",
         disease = "Influenza",
         metric = cases,
         month = NA,
         week = epi_week,
         year = epi_year,
         age = age_group) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

ba_df <- rbind(ba_df, FLU_AU_MERGE)

FLU_DK_MERGE <- flu_dk_data %>% 
  mutate(country = "Denmark",
         disease = "Influenza",
         metric = admissions,
         month = NA,
         week = week_number,
         year = year,
         age = Age_group) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

ba_df <- rbind(ba_df, FLU_DK_MERGE)

RSV_DK_MERGE <- rsv_dk_data %>% 
  mutate(country = "Denmark",
         disease = "RSV",
         metric = cases,
         month = NA,
         age = age_group) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

FLU_HK_MERGE <- flu_hk_data %>% 
  pivot_longer(cols = starts_with("Adm"), names_to = "age", values_to = "metric") %>% 
  mutate(country = "Hong Kong",
         disease = "Influenza",
         month = NA,
         year = Year,
         week = Week) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

ARGENTINA_MERGE <- Argentina_all_data %>%
  mutate(week = epi_weeks,
         month = NA,
         country = "Argentina",
         age = age_group,
         disease = if_else(str_detect(event, regex("influenza", ignore_case = TRUE)), "Influenza", "RSV")) %>% 
  group_by(country, year, month, week, disease, age) %>% 
  summarise(metric = sum(num_cases), .groups = "drop") %>% 
  arrange(year, week) %>%
  arrange(disease) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  view()


ba_df <- rbind(ba_df, ARGENTINA_MERGE)

RSV_HK_MERGE <- flu_hk_data %>% 
  pivot_longer(cols = starts_with("Adm"), names_to = "age", values_to = "metric") %>% 
  mutate(country = "Hong Kong",
         disease = "Influenza",
         month = NA,
         year = Year,
         week = Week) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

ba_df <- rbind(ba_df, FLU_HK_MERGE)

FLU_US_MERGE <- flu_us_data %>% 
  mutate(country = "United States",
         disease = "Influenza",
         month = NA,
         year = YEAR...4,
         week = WEEK,
         age = age_group,
         metric = weekly_rate) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

ba_df <- rbind(ba_df, FLU_US_MERGE)

RSV_US_MERGE <- rsv_us_data %>%
  mutate(country = "United States",
         disease = "RSV",
         month = NA,
         year = year,
         week = week_number,
         age = Age_Category,
         metric = weekly_rate) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

ba_df <- rbind(ba_df, RSV_US_MERGE)


FLU_IR_MERGE <- flu_ireland %>%
  pivot_longer(cols = starts_with("weekly", ignore.case = TRUE), names_to = "age", values_to = "metric") %>% 
  mutate(country = "Ireland",
         disease = "Influenza",
         week = as.numeric(str_sub(week, 2)),
         month = NA) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

ba_df <- rbind(ba_df, FLU_IR_MERGE)

RSV_IR_MERGE <- rsv_ireland %>%
  pivot_longer(cols = starts_with("weekly", ignore.case = TRUE), names_to = "age", values_to = "metric") %>% 
  mutate(country = "Ireland",
         disease = "RSV",
         week = as.numeric(str_sub(week, 2)),
         month = NA) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  view()

ba_df <- rbind(ba_df, RSV_IR_MERGE)

FINLAND_MERGE <- Finland_data %>% 
  mutate(country = "Finland",
         disease = Group,
         week = NA,
         month = Month,
         year = Year,
         age = NA,
         metric = Cases) %>% 
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  arrange(disease) %>% 
  view()

ba_df <- rbind(ba_df, FINLAND_MERGE)

JAPAN_MERGE <- Japan_data %>% 
  pivot_longer(cols = ends_with("cases", ignore.case = TRUE), names_to = "disease", values_to = "metric") %>%
  mutate(disease = if_else(disease == "flu_cases", "Influenza", "RSV"),
         country = "Japan",
         year = Year,
         week = Week,
         month = NA,
         age = NA) %>%
  select(country, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>% 
  arrange(disease) %>% 
  view()

ba_df <- rbind(ba_df, JAPAN_MERGE)
  

world_population <- read_csv("csv/world_population.csv") %>%
  set_names(gsub("\\s+", "_", names(.))) %>%
  pivot_longer(cols = c(starts_with("19"), starts_with("20")), names_to = "Year", values_to = "Population")

# dk_data <- flu_dk_data %>%
#   mutate(country = "Denmark",
#          country_code = "DNK") %>%
#   full_join(rsv_dk_data, by= c("age", "year","month", "epiweek", "date")) %>% 
#   left_join(world_population %>% 
#               filter(Country_Code == "DNK") %>% 
#               select(Year, Population), 
#             by = c("year" = "Year")) %>%
#   arrange(year) %>% # Ensure rows are ordered by year
#   fill(Population, .direction = "down") %>% # Fill missing values with the closest previous value
#   view() 
#   
