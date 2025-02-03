library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(ggridges)
library(rnaturalearth)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)

rm(list = ls())

# Change this for all data
setwd("C:/Users/Evan/Documents/Code/sanofi-rsv-flu")
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
  metric = numeric(),
  source = character()
)


FLU_FR_MERGE <- flu_france_data %>%
  mutate(
    country = "France",
    disease = "Influenza",
    month = NA,
    age = NA,
    metric = inc,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_FR_MERGE)

FLU_UK_MERGE <- flu_uk_data %>%
  mutate(
    country = "United Kingdom",
    disease = "Influenza",
    metric = metric_value,
    week = epiweek,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_UK_MERGE)

RSV_UK_MERGE <- rsv_uk_data %>%
  mutate(
    country = "United Kingdom",
    disease = "RSV",
    metric = metric_value,
    week = epiweek,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, RSV_UK_MERGE)

FLU_AU_MERGE <- flu_au_data %>%
  mutate(
    country = "Australia",
    disease = "Influenza",
    metric = cases,
    month = NA,
    week = epi_week,
    year = epi_year,
    age = age_group,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
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
    age = Age_group,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_DK_MERGE)

RSV_DK_MERGE <- rsv_dk_data %>%
  mutate(
    country = "Denmark",
    disease = "RSV",
    metric = cases,
    month = NA,
    age = age_group,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, RSV_DK_MERGE)

FLU_HK_MERGE <- flu_hk_data %>%
  pivot_longer(cols = starts_with("Adm"),
               names_to = "age",
               values_to = "metric") %>%
  mutate(
    country = "Hong Kong",
    disease = "Influenza",
    month = NA,
    year = Year,
    week = Week,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, FLU_HK_MERGE)

RSV_HK_MERGE <- flu_hk_data %>%
  pivot_longer(cols = starts_with("Adm"),
               names_to = "age",
               values_to = "metric") %>%
  mutate(
    country = "Hong Kong",
    disease = "Influenza",
    month = NA,
    year = Year,
    week = Week,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>%
  view()

ba_df <- rbind(ba_df, RSV_HK_MERGE)

ARGENTINA_MERGE <- Argentina_all_data %>%
  mutate(
    week = epi_weeks,
    month = NA,
    country = "Argentina",
    age = age_group,
    disease = if_else(str_detect(event, regex("influenza", ignore_case = TRUE)), "Influenza", "RSV"),
    source = "GOV") %>%
  group_by(country, source, year, month, week, disease, age) %>%
  summarise(metric = sum(num_cases), .groups = "drop") %>%
  arrange(year, week) %>%
  arrange(disease) %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  view()


ba_df <- rbind(ba_df, ARGENTINA_MERGE)


FLU_US_MERGE <- flu_us_data %>%
  mutate(
    country = "United States of America",
    disease = "Influenza",
    month = NA,
    year = YEAR...4,
    week = WEEK,
    age = age_group,
    metric = weekly_rate,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
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
    metric = weekly_rate,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
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
    month = NA,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
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
    month = NA,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
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
    metric = Cases,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
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
    age = NA,
    source = "GOV") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
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
    age = age_group,
    source = "GOV") %>%
  group_by(country, source, year, month, week, disease, age) %>%
  summarise(metric = sum(cases)) %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  arrange(year, week) %>%
  arrange(disease) %>%
  view()

ba_df <- rbind(ba_df, TAIWAN_MERGE_FLU)

massive_flu_df <- read_csv("C:/Users/Evan/Downloads/VIW_FID.csv")

# Filter the data for the selected date
filtered_data <- massive_flu_df %>%
  group_by(COUNTRY_CODE, AGEGROUP_CODE, ISO_WEEK, ISO_YEAR, ISO_WEEKSTARTDATE, HEMISPHERE) %>% 
  summarize(cases = sum(REPORTED_CASES, na.rm = TRUE),
            RSV = sum(RSV, na.ra = TRUE)) %>%
  filter(cases < 1e7) %>% 
  filter(COUNTRY_CODE %in% c("AFG", "ALB", "ARM", "BEL", "BLR", "BRA", "BTN", "CAN", "CHL", "COL", "CRI", "CZE", "DEU", "EST", "GRC", "IRL", "ISR", "ITA", "JOR", "KAZ", "KEN", "KGZ", "LBN", "LTU", "MAR", "MEX", "MKD", "MLT", "MNE", "NOR", "POL", "PRY", "QAT", "RUS", "SVN", "TUK", "URY")) %>%
  arrange(COUNTRY_CODE, ISO_YEAR, ISO_WEEK) %>% 
  view()

world_map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(iso_a3, name)

filtered_data <- merge(filtered_data, world_map_data, by.x = "COUNTRY_CODE", by.y = "iso_a3")

filtered_data <- filtered_data %>%
  pivot_longer(cols = c("cases", "RSV"), names_to = "disease", values_to = "metric") %>%
  mutate(disease = if_else(disease == "cases", "Influenza", "RSV"),
       country = name,
       year = ISO_YEAR,
       week = ISO_WEEK,
       month = month(ISO_WEEKSTARTDATE),
       age = AGEGROUP_CODE,
       source = "FLUNET") %>%
  select(country, source, year, month, week, disease, age, metric) %>% 
  arrange(country, year, week) %>% 
  arrange(disease) %>% 
  view()


ba_df <- rbind(ba_df, filtered_data)


df <- ba_df %>% 
  mutate(
    age = case_when(
      age %in% c("all", "All", "Adm_All", "Alle", "weekly_cases", NA) ~ "All",
      TRUE ~ age  
    ),
    week = as.numeric(week),
    year = as.numeric(year),
    # Ensure months are a factor in the correct order
    month = case_when(
      !is.na(month) ~ substr(month, 1, 3),
      TRUE ~ NA_character_
    ),
    month = factor(month, levels = month.abb, ordered = TRUE)
  ) %>%
  view()



world_population <- read_csv("csv/world_population.csv") %>%
  set_names(gsub("\\s+", "_", names(.))) %>%
  pivot_longer(
    cols = c(starts_with("19"), starts_with("20")),
    names_to = "Year",
    values_to = "population"
  ) %>% 
  mutate(Year = as.numeric(Year))

world_map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(iso_a3, name, pop_est)


df <- df %>%
  left_join(world_population %>%
              select(Year, population, Country_Name),
            by = c("year" = "Year", "country" = "Country_Name")
  ) %>%
  # group_by(country) %>%  
  # fill(everything(), .direction = "down") %>%  
  # ungroup() %>%  
  mutate(
    month = case_when(
      is.na(month) & !is.na(week) ~ month.abb[pmin(ceiling(week / 4), 12)],  # Ensure it doesn't exceed 12
      TRUE ~ as.character(month)  # Convert to character before refactoring
    ),
    month = factor(month, levels = month.abb, ordered = TRUE)  # Convert back to ordered factor
  ) %>% 
  view()




df <- df %>%
  mutate(
    is_monthly = is.na(week) & !is.na(month)  # Identify monthly data
  )



# Split the dataset into weekly and monthly parts
df_weekly <- df %>% filter(!is_monthly)
df_monthly <- df %>% filter(is_monthly)

# Expand monthly data into four pseudo-weeks
df_monthly <- df_monthly %>%
  mutate(
    metric = metric / 4  # Divide the metric equally across four weeks
  ) %>%
  uncount(4, .id = "week_offset") %>%  # Create 4 copies of each row
  mutate(
    week = (as.numeric(month) - 1) * 4 + week_offset  # Assign pseudo-weeks
  ) %>%
  select(-week_offset, -is_monthly)  # Remove helper columns

# Combine back into the main dataset
df <- bind_rows(df_weekly, df_monthly) %>%
  select(-is_monthly) %>% 
  arrange(country, year, week) %>%
  view()


au_test <- df %>%
  filter(week < 53) %>% #
  filter(country == "Australia") %>%
  group_by(week) %>%
  summarize(cases = sum(metric)) %>%
  view()


write.csv(df, file="csv/main_dataset.csv", row.names = FALSE)

au_test <- df_subset %>%
  filter(week < 53) %>% 
  group_by(week) %>%
  summarize(cases = sum(metric)) %>%
  view()

au_test <- df %>%
  filter(week < 53) %>% #
  filter(country == "Australia") %>% 
  view()

flu_au_data %>% group_by(epi_week) %>% summarize(cases = sum(cases)) %>% view()
 

# global_flu_season <- df %>%
#   filter(disease == "Influenza") %>%
#   mutate(is_covid = if_else(year < 2021, "Before", "After")) %>%
#   
#   # Group by country and is_covid, then calculate the mean and find the week of the mean
#   group_by(country, is_covid) %>%
#   reframe(
#     mean_metric = mean(metric, na.rm = TRUE),
#     # Calculate the mean of metric
#     week_of_mean = week[which.min(abs(metric - mean(metric, na.rm = TRUE)))] %>% as.numeric(),  # Find the week of the mean and ensure it’s numeric
#     .groups = "drop"
#   ) %>%
#   
#   # Pivot to wide format so we have columns for "Before" and "After"
#   pivot_wider(
#     names_from = is_covid,
#     values_from = c(mean_metric, week_of_mean)
#   ) %>%
#   
#   # Calculate the difference between "After" and "Before" weeks
#   mutate(
#     week_of_mean_Before = coalesce(week_of_mean_Before, NA_real_),  # Ensure NAs are handled as numeric
#     week_of_mean_After = coalesce(week_of_mean_After, NA_real_),    # Ensure NAs are handled as numeric
#     week_diff = week_of_mean_After - week_of_mean_Before  # Calculate the difference in weeks
#   ) %>%
#   view()
# 
# 
# df %>% 
#   filter(disease == "Influenza") %>%
#   mutate(is_covid = if_else(year < 2021, "Before", "After")) %>% 
#   group_by(week, country, is_covid) %>%
#   summarise(metric = sum(metric, na.rm = TRUE)) %>%  # Remove NA values while summing
#   group_by(country, is_covid) %>%
#   na.omit() %>% 
#   mutate(median_metric = median(metric, na.rm = TRUE),
#          week_of_median = week[which.min(abs(metric - median_metric))]) %>%  # Find week closest to median
#   ggplot(aes(x = week, y = metric, color = factor(is_covid))) +
#   geom_line(size = 1) +  # Use stat="identity" to directly use the metric values
#   facet_wrap(~country, scales = "free_y") +  # Independent y-axes for each country
#   geom_vline(aes(xintercept = week_of_median, color = is_covid), linetype = "dashed") +  # Add vertical line at week of median
#   labs(title = "Influenza Metric by Week and Country", 
#        x = "Week", 
#        y = "Total Metric") +
#   theme(
#     axis.title = element_text(),
#     legend.position = "bottom",
#     axis.ticks.y = element_line(),
#     axis.line.y.left = element_line(),
#     legend.title = element_blank(),
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8),
#     axis.text.y = element_text()
#   ) +
#   coord_polar(theta = "x")
# 
# 
# 
# # Ensure country names match (may need adjustments)
# world_map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
#   left_join(global_flu_season, by = c("name" = "country"))
# 
# ggplot(world_map_data) +
#   geom_sf(aes(fill = week_diff), color = "black", size = 0.1) +  # Add country borders
#   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "grey") +  
#   labs(title = "Change in Week of Influenza Peak (Pre vs Post-COVID)",
#        fill = "Week Difference") +
#   theme_minimal()

  