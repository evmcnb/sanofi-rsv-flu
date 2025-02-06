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
  slice(-1) %>%
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
