## This is a script that is pulling the influenza and RSV data from Argentina
library(dplyr)
library(stringr)

## Get the working directory

mywd <- setwd("C:/Users/icnarc246/OneDrive - ICNARC/Desktop/Trainings and personal docs/LSHTM/Data challenge/sanofi-rsv-flu/csv/Argentina")
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

Argentina_data <- combined_data %>% 
  filter(str_detect(event, regex("Bronquiolitis|influenza", ignore_case = TRUE)) ) %>%
  filter(!str_detect(event, regex("ambulatorios", ignore_case = TRUE)))
  
## check

head(Argentina_data)
tail(Argentina_data) ## 2164687 rows of patients

## check that it worked

Argentina_data %>%
  distinct(event)

## LL REVISIT this: 
# check for duplicates : 
# Argentina_data %>%
#   group_by(across(everything())) %>%
#   filter(n() > 1) %>%
#   ungroup()



# Write the data to a CSV file
# write.csv(flu_bronchio_data, "flu_bronchio_data.csv", row.names = TRUE)

## I sorted the data and made sure manually that it seems ok. this is because i wasnt able to do view() bcs of the large file.

summary(Argentina_data)


















