## -----------------------------------------------------------------------------
## Title: Argentina flu and bronchio
## Description: This is a script that is pulling the influenza and RSV data from Argentina
## Author:
## Date:
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

mywd <- setwd("./sanofi-rsv-flu/csv/Argentina")
getwd()

## Get all the files all at once:

#1. List all CSV files in a directory
file_list <- list.files(path = mywd, pattern = "*.csv", full.names = TRUE)

#2. Read all files into a list of data frames
data_list <- lapply(file_list, read.csv)

#3. Optionally, combine all data frames into one (if they have the same structure)
combined_data <- do.call(rbind, data_list)

## check
head(combined_data)
tail(combined_data) ## 2866665  rows

## Translate the headers to English

combined_data <- combined_data %>%
  rename(
    dept_id = departamento_id,
    dept_name = departamento_nombre,
    prov_id = provincia_id,
    prov_name = provincia_nombre,
    year = anio,
    epi_weeks = semanas_epidemiologicas,
    event = evento_nombre,
    age_group_id = grupo_edad_id,
    age_group = grupo_edad_desc,
    num_cases = cantidad_casos
  )

tail(combined_data) ## 2866665  rows

## check the distinct events -- make sure that we only have influenza and RSV
combined_data %>%
  distinct(event)

## result indicates that there are more than the resp illnesses that those we care about
## filter to those with Influenza and bronchiolitis.
## There is a precedent set to use Bronchiolitis as a stand in for RSV
## Keeping all bronchiolitis unspecified 

Argentina_all_data <- combined_data %>% 
  filter(str_detect(event, regex("Bronquiolitis|influenza", ignore_case = TRUE)) ) %>%
  filter(!str_detect(event, regex("ambulatorios", ignore_case = TRUE)))
  
## check

head(Argentina_all_data)
tail(Argentina_all_data) ## 2164687 rows of patients

## check that it worked

Argentina_all_data %>%
  distinct(event)

## LL REVISIT this:
# Write the data to a CSV file
# write.csv(flu_bronchio_data, "flu_bronchio_data.csv", row.names = TRUE)
## I sorted the data and made sure manually that it seems ok. this is because i wasnt able to do view() bcs of the large file.

summary(Argentina_all_data)
head(Argentina_all_data)

# Process the combined data
grp_data <- Argentina_data %>%
  group_by(year , epi_weeks, age_group ) %>%
  summarise(num_cases = n(), .groups = "drop")

## plot
grp_data <- grp_data %>%
  mutate(is_covid = if_else(year < 2021, 
                            "Before Lockdown", "After Lockdown"))
#View(grp_data)

grp_data <- grp_data %>%
  group_by(epi_weeks, is_covid, age_group) %>%
  summarise(cases = sum(num_cases)) %>%
  na.omit() 

## ggplot
main_plot <- grp_data %>% 
  ggplot(aes(x = epi_weeks, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  facet_wrap(~age_group) +
  labs(title = 'Argentina RSV and Flu Data', subtitle = "Age stratified case data", x = 'Week', y = "Lab Confirmed Cases") +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
main_plot

## now views by RSV or Influenza
## split into 2 objects, one for bronchio and one for flu

flu_Argentina <- Argentina_all_data %>% 
  filter(str_detect(event, regex("influenza", ignore_case = TRUE)) )

bronchio_Argentina <- Argentina_all_data %>% 
  filter(str_detect(event, regex("Bronquio", ignore_case = TRUE)) )


##------------------------------------------------------------------------------
# Process the FLU data
grp_flu <- flu_Argentina %>%
  group_by(year , epi_weeks, age_group ) %>%
  summarise(num_cases = n(), .groups = "drop")
grp_flu

## plot
grp_flu <- grp_flu %>%
  mutate(is_covid = if_else(year < 2021, 
                            "Before Lockdown", "After Lockdown"))
#View(grp_data)

grp_flu <- grp_flu %>%
  group_by(epi_weeks, is_covid, age_group) %>%
  summarise(cases = sum(num_cases)) %>%
  na.omit()
grp_flu

##------------------------------------------------------------------------------
## only Flu plot

flu_plot <- grp_flu %>% 
  ggplot(aes(x = epi_weeks, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  facet_wrap(~age_group) +
  labs(title = 'Argentina Flu Data', subtitle = "Age stratified case data", x = 'Week', y = "Lab Confirmed Cases") +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
flu_plot

##------------------------------------------------------------------------------
# Process the Bronchio data
grp_bronchio <- bronchio_Argentina %>%
  group_by(year , epi_weeks, age_group ) %>%
  summarise(num_cases = n(), .groups = "drop")
grp_bronchio

## plot
grp_bronchio <- grp_bronchio %>%
  mutate(is_covid = if_else(year < 2021, 
                            "Before Lockdown", "After Lockdown"))
#View(grp_data)

grp_bronchio <- grp_bronchio %>%
  group_by(epi_weeks, is_covid, age_group) %>%
  summarise(cases = sum(num_cases)) %>%
  na.omit()
grp_bronchio

##------------------------------------------------------------------------------
## only bronchio plot

bronchio_plot <- grp_bronchio %>% 
  ggplot(aes(x = epi_weeks, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  facet_wrap(~age_group) +
  labs(title = 'Argentina bronchio Data', subtitle = "Age stratified case data", x = 'Week', y = "Lab Confirmed Cases") +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
bronchio_plot












