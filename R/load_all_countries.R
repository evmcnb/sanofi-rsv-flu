library(tidyverse)
library(lubridate)
library(readxl)

rm(list = ls())

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
  pivot_longer(cols = c(starts_with("19"), starts_with("20")), names_to = "Year", values_to = "Populaion") %>% 
  view()

au_data <- flu_au_data %>%
  merge(world_population, by.x = "epi_year", by.y = "Year") %>% 
  view()
