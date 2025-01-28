library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(ggridges)

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
