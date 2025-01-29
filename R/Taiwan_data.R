## -----------------------------------------------------------------------------
## Title: Taiwan data : Area, Age, and Gender Statistical table -Influenza
## retrieved from source: https://data.cdc.gov.tw/en/dataset/aagstable-influenza#
## Description: This is a script that is pulling the influenza and RSV data from Argentina
## Author: L.Lampro - Sanofi group
## Date: 24.01.2025
## -----------------------------------------------------------------------------

#install.packages("ggthemes")

library(purrr)
library(readxl)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)

## Get the working directory

mywd <- setwd("C:/Users/icnarc246/OneDrive - ICNARC/Desktop/Trainings and personal docs/LSHTM/Data challenge/sanofi-rsv-flu/csv/Taiwan")
getwd()

Taiwan_flu <- read.csv("Age_County_Gender_487a.csv")
Taiwan_flu

## check
head(Taiwan_flu)
tail(Taiwan_flu) ## 17589 rows

## rename the variables

Taiwan_flu <- Taiwan_flu %>%
  rename(
    year= Year.of.Onset ,
    month = Month.of.Onset ,
    cases = Number.of.Confirmed.Cases,
    age_group = Age.Group
  )

## transform the month using lubridate
# Taiwan_flu$month_name <- month.name[Taiwan_flu$month]

## select the variables of interest

Taiwan_flu <- Taiwan_flu %>% 
  select(year, month, age_group, cases)
Taiwan_flu

## after 2017 as the data before 2017 are of low quality
Taiwan_flu <- Taiwan_flu %>% 
  filter(year >= 2017)

# Process the data
grp_data <- Taiwan_flu %>%
  group_by(year , month, age_group ) %>%
  summarise(num_cases = n())


summary(grp_data)


# ## plot
# grp_data <- grp_data %>%
#   mutate(is_covid = if_else(year < 2021, 
#                             "Before Lockdown", "After Lockdown"))
# 
# #View(grp_data)
# 
# grp_data <- grp_data %>%
#   group_by(month_name, is_covid, age_group) %>%
#   summarise(cases = sum(num_cases)) %>%
#   na.omit() 
# grp_data

##------------------------------------------------------------------------------
## GGplot - plot by age group

main_plot <- grp_data %>% 
  ggplot(aes(x = month, y = num_cases)) +#, color = factor(is_covid))) +
  geom_line(size = 1) +
  facet_wrap(~ age_group) +
  labs(title = 'Taiwan Flu Data', 
       subtitle = "Age stratified case data", 
       x = 'Week', 
       y = "Lab Confirmed Cases") +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  scale_x_continuous(breaks = 1:12, labels = month.abb)
main_plot

##------------------------------------------------------------------------------
## plot by year

main_plot2 <- grp_data %>% 
  ggplot(aes(x = month, y = num_cases)) +#, color = factor(is_covid))) +
  geom_line(size = 1) +
  facet_wrap(~ year) +
  labs(title = 'Taiwan Influenza Data', 
       subtitle = "Stratified by year", 
       x = 'Week', 
       y = "Influenza cases") +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  scale_x_continuous(breaks = 1:12, labels = month.abb)

main_plot2
