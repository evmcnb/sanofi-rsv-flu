library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(ggridges)
library(rnaturalearth)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


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
source("R/Taiwan_data.R")

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
  mutate(
    country = "France",
    disease = "Influenza",
    month = NA,
    age = NA,
    metric = inc
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_FR_MERGE)

FLU_UK_MERGE <- flu_uk_data %>%
  mutate(
    country = "United Kingdom",
    disease = "Influenza",
    metric = metric_value,
    week = epiweek
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_UK_MERGE)

RSV_UK_MERGE <- rsv_uk_data %>%
  mutate(
    country = "United Kingdom",
    disease = "RSV",
    metric = metric_value,
    week = epiweek
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

FLU_AU_MERGE <- flu_au_data %>%
  mutate(
    country = "Australia",
    disease = "Influenza",
    metric = cases,
    month = NA,
    week = epi_week,
    year = epi_year,
    age = age_group
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_AU_MERGE)

FLU_DK_MERGE <- flu_dk_data %>%
  mutate(
    country = "Denmark",
    disease = "Influenza",
    metric = admissions,
    month = NA,
    week = week_number,
    year = year,
    age = Age_group
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_DK_MERGE)

RSV_DK_MERGE <- rsv_dk_data %>%
  mutate(
    country = "Denmark",
    disease = "RSV",
    metric = cases,
    month = NA,
    age = age_group
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

FLU_HK_MERGE <- flu_hk_data %>%
  pivot_longer(cols = starts_with("Adm"),
               names_to = "age",
               values_to = "metric") %>%
  mutate(
    country = "Hong Kong",
    disease = "Influenza",
    month = NA,
    year = Year,
    week = Week
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ARGENTINA_MERGE <- Argentina_all_data %>%
  mutate(
    week = epi_weeks,
    month = NA,
    country = "Argentina",
    age = age_group,
    disease = if_else(str_detect(
      event, regex("influenza", ignore_case = TRUE)
    ), "Influenza", "RSV")
  ) %>%
  group_by(country, year, month, week, disease, age) %>%
  summarise(metric = sum(num_cases), .groups = "drop") %>%
  arrange(year, week) %>%
  arrange(disease) %>%
  select(country, year, month, week, disease, age, metric) %>%
  view()


ba_df <- rbind(ba_df, ARGENTINA_MERGE)

RSV_HK_MERGE <- flu_hk_data %>%
  pivot_longer(cols = starts_with("Adm"),
               names_to = "age",
               values_to = "metric") %>%
  mutate(
    country = "Hong Kong",
    disease = "Influenza",
    month = NA,
    year = Year,
    week = Week
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_HK_MERGE)

FLU_US_MERGE <- flu_us_data %>%
  mutate(
    country = "United States",
    disease = "Influenza",
    month = NA,
    year = YEAR...4,
    week = WEEK,
    age = age_group,
    metric = weekly_rate
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_US_MERGE)

RSV_US_MERGE <- rsv_us_data %>%
  mutate(
    country = "United States",
    disease = "RSV",
    month = NA,
    year = year,
    week = week_number,
    age = Age_Category,
    metric = weekly_rate
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, RSV_US_MERGE)


FLU_IR_MERGE <- flu_ireland %>%
  pivot_longer(
    cols = starts_with("weekly", ignore.case = TRUE),
    names_to = "age",
    values_to = "metric"
  ) %>%
  mutate(
    country = "Ireland",
    disease = "Influenza",
    week = as.numeric(str_sub(week, 2)),
    month = NA
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_IR_MERGE)

RSV_IR_MERGE <- rsv_ireland %>%
  pivot_longer(
    cols = starts_with("weekly", ignore.case = TRUE),
    names_to = "age",
    values_to = "metric"
  ) %>%
  mutate(
    country = "Ireland",
    disease = "RSV",
    week = as.numeric(str_sub(week, 2)),
    month = NA
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, RSV_IR_MERGE)

FINLAND_MERGE <- Finland_data %>%
  mutate(
    country = "Finland",
    disease = Group,
    week = NA,
    month = Month,
    year = Year,
    age = NA,
    metric = Cases
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  arrange(disease) %>%
  view()

ba_df <- rbind(ba_df, FINLAND_MERGE)

JAPAN_MERGE <- Japan_data %>%
  pivot_longer(
    cols = ends_with("cases", ignore.case = TRUE),
    names_to = "disease",
    values_to = "metric"
  ) %>%
  mutate(
    disease = if_else(disease == "flu_cases", "Influenza", "RSV"),
    country = "Japan",
    year = Year,
    week = Week,
    month = NA,
    age = NA
  ) %>%
  select(country, year, month, week, disease, age, metric) %>%
  arrange(year, week) %>%
  arrange(disease) %>%
  view()

ba_df <- rbind(ba_df, JAPAN_MERGE)

TAIWAN_MERGE_FLU <- Taiwan_flu %>%
  mutate(
    country = "Taiwan",
    disease = "Influenza",
    week = NA,
    month = factor(month, labels = month.abb),
    year = year,
    age = age_group
  ) %>%
  group_by(country, year, month, week, disease, age) %>%
  summarise(metric = sum(cases)) %>%
  arrange(year, week) %>%
  arrange(disease) %>%
  view()

ba_df <- rbind(ba_df, TAIWAN_MERGE_FLU)

rm(list = setdiff(ls(), "ba_df"))

world_population <- read_csv("csv/world_population.csv") %>%
  set_names(gsub("\\s+", "_", names(.))) %>%
  pivot_longer(
    cols = c(starts_with("19"), starts_with("20")),
    names_to = "Year",
    values_to = "Population"
  )

df <- ba_df %>%
  left_join(
    world_population %>%
      select(Year, Population, Country_Name),
    by = c("year" = "Year", "country" = "Country_Name")
  ) %>%
  group_by(country) %>%  
  fill(everything(), .direction = "down") %>%  
  ungroup() %>%  
  
  mutate(
    age = case_when(
      age %in% c("all", "All", "Adm_All", "Alle", "weekly_cases", NA) ~ "All",
      TRUE ~ age  
    ),
    week = as.numeric(week),
    
    # Convert month names to abbreviations
    month = case_when(
      !is.na(month) ~ substr(month, 1, 3),
      TRUE ~ NA_character_
    ),
    
    # Ensure months are a factor in the correct order
    month = factor(month, levels = month.abb, ordered = TRUE)
  ) 

# Separate into weekly and monthly datasets
df_weekly <- df %>%
  filter(!is.na(week))  

df_monthly <- df %>%
  filter(is.na(week)) %>%  
  group_by(country, year, month) %>%
  
  # Expand each month into 4 weekly rows
  slice(rep(1, 4)) %>%  
  mutate(
    metric = metric / 4,  # Divide metric by 4
    week = (as.numeric(month) - 1) * 4 + rep(1:4, each = n() / 4)  # Generate week numbers
  ) %>%
  ungroup()

# Combine back into one dataset
df_fixed <- bind_rows(df_weekly, df_monthly)

df <- df_fixed %>% 
  mutate(
    month = case_when(
      is.na(month) & !is.na(week) ~ month.abb[pmin(ceiling(week / 4), 12)],  # Ensure it doesn't exceed 12
      TRUE ~ as.character(month)  # Convert to character before refactoring
    ),
    month = factor(month, levels = month.abb, ordered = TRUE)  # Convert back to ordered factor
  )

global_flu_season <- df %>%
  filter(disease == "Influenza") %>%
  mutate(is_covid = if_else(year < 2021, "Before", "After")) %>%
  
  # Group by country and is_covid, then calculate the mean and find the week of the mean
  group_by(country, is_covid) %>%
  reframe(
    mean_metric = mean(metric, na.rm = TRUE),
    # Calculate the mean of metric
    week_of_mean = week[which.min(abs(metric - mean(metric, na.rm = TRUE)))] %>% as.numeric(),  # Find the week of the mean and ensure it’s numeric
    .groups = "drop"
  ) %>%
  
  # Pivot to wide format so we have columns for "Before" and "After"
  pivot_wider(
    names_from = is_covid,
    values_from = c(mean_metric, week_of_mean)
  ) %>%
  
  # Calculate the difference between "After" and "Before" weeks
  mutate(
    week_of_mean_Before = coalesce(week_of_mean_Before, NA_real_),  # Ensure NAs are handled as numeric
    week_of_mean_After = coalesce(week_of_mean_After, NA_real_),    # Ensure NAs are handled as numeric
    week_diff = week_of_mean_After - week_of_mean_Before  # Calculate the difference in weeks
  ) %>%
  view()


df %>% 
  filter(disease == "Influenza") %>%
  mutate(is_covid = if_else(year < 2021, "Before", "After")) %>% 
  group_by(week, country, is_covid) %>%
  summarise(metric = sum(metric, na.rm = TRUE)) %>%  # Remove NA values while summing
  group_by(country, is_covid) %>%
  na.omit() %>% 
  mutate(median_metric = median(metric, na.rm = TRUE),
         week_of_median = week[which.min(abs(metric - median_metric))]) %>%  # Find week closest to median
  ggplot(aes(x = week, y = metric, color = factor(is_covid))) +
  geom_line(size = 1) +  # Use stat="identity" to directly use the metric values
  facet_wrap(~country, scales = "free_y") +  # Independent y-axes for each country
  geom_vline(aes(xintercept = week_of_median, color = is_covid), linetype = "dashed") +  # Add vertical line at week of median
  labs(title = "Influenza Metric by Week and Country", 
       x = "Week", 
       y = "Total Metric") +
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



# Ensure country names match (may need adjustments)
world_map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  left_join(global_flu_season, by = c("name" = "country"))

ggplot(world_map_data) +
  geom_sf(aes(fill = week_diff), color = "black", size = 0.1) +  # Add country borders
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "grey") +  
  labs(title = "Change in Week of Influenza Peak (Pre vs Post-COVID)",
       fill = "Week Difference") +
  theme_minimal()

  