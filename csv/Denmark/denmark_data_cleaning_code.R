install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")

library(readxl)
library(dplyr)
library(tidyr)

flu_denmark <- read_excel("/Users/onayomirosenior-patten/Documents/GitHub/sanofi-rsv-flu/csv/Denmark/flu_denmark.xlsx", sheet = 1)

rsv_denmark <- read.csv("/Users/onayomirosenior-patten/Documents/GitHub/sanofi-rsv-flu/csv/Denmark/rsv_denmark.csv")

# ==============================================================================
# RSV Data
# ==============================================================================

# Separate week_sort into year and week
transformed_data <- rsv_denmark %>%
  # Step 1: Separate `week_sort` into `year` and `week`
  separate(week_sort, into = c("Year_Label", "Year", "week_Label", "Week"), sep = " ") %>%
  
  # Step 2: Remove the extra labels (Year_Label and week_Label)
  select(-Year_Label, -week_Label) %>%
  
  # Step 3: Rearrange columns to match the target template
  select(Year, Week, value, type, Season, Age.Group)

# Assign the correct year to the 'year' column
transformed_data <- transformed_data %>%
  # Step 1: Separate `Season` into `1` and `2`
  separate(Season, into = c("Season_1", "Season_2"), sep = "/") %>%
  
  # Step 2: Assign the correct year based on the value in `Year`
  mutate(Year = ifelse(Year == 1, as.integer(Season_1), as.integer(Season_2))) %>%
  
  # Step 3: Drop the temporary `Season_1` and `Season_2` columns
  select(-Season_1, -Season_2)

# View the transformed dataset
View(transformed_data)

# Assign a country code to the dataset
coded_data <- transformed_data %>%
  mutate(country = "Denmark") %>%
  mutate(country_code = "DNK")

# Standardise the structure of the dataset
if (!require(ISOweek)) install.packages("ISOweek")
library(ISOweek)

# Add a new `month` column
coded_data <- coded_data %>%
  rename(epiweek = Week) %>%  # Step 1: Rename `week` to `epiweek`
  
  mutate(
    epiweek = as.numeric(epiweek),  # Ensure `epiweek` is numeric
    date = ISOweek2date(paste0(Year, "-W", sprintf("%02d", epiweek), "-1")),  # Convert epiweek to a date
    month = format(date, "%m")  # Extract the month as a two-digit number
  ) %>%
  select(-date)  # Remove the temporary `date` column if not needed

# Rearrange and rename columns
rsv_data <- coded_data %>%
  rename(year = Year) %>%
  rename(age = Age.Group) %>%
  select(country, country_code, year, epiweek, month, age, type)

# Filter for `cases` only
clean_rsv_denmark <- filter(rsv_data, type == "cases")

write_csv(clean_rsv_denmark, "/Users/onayomirosenior-patten/Documents/GitHub/sanofi-rsv-flu/csv/Denmark/clean_rsv_denmark.csv")

# ==============================================================================
# Flu Data
# ==============================================================================
# Separate `Week` into year and week
flu_transformed <- flu_denmark %>%
  # Step 1: Separate `Week` into `year` and `week`
  separate(Week, into = c("year", "week"), sep = "-") %>%
  
  # Step 2: Remove the extra labels (Season)
  select(-Season, -Number_of_tests, -Number_of_deaths, -Positivity_rate, -Tests_per_100k_citizens, -Region_code, -Timestamp)

# Combine the rows with the same `year`, `week`, `Age_group` and `Gender`
flu_combined_data <- flu_transformed %>%
  # Step 1: Group by columns that should remain unique
  group_by(year, week, Age_group, Gender) %>%
  
  # Step 2: Summarize by summing numeric columns
  summarise(
    Number_of_citizens = sum(Number_of_citizens, na.rm = TRUE),
    Number_of_confirmed_cases = sum(Number_of_confirmed_cases, na.rm = TRUE),
    Number_of_new_admissions = sum(Number_of_new_admissions, na.rm = TRUE),
    Confirmed_cases_per_100k_citizens = sum(Confirmed_cases_per_100k_citizens, na.rm = TRUE),
    New_admissions_per_100k_citizens = sum(New_admissions_per_100k_citizens, na.rm = TRUE),
    .groups = "drop"  # Ensures the result is not grouped after summarising
  )

flu_nogender <- filter(flu_combined_data, Gender == "Alle") %>%
  select(-Gender) %>%
  mutate(country = "Denmark") %>%
  mutate(country_code = "DNK")

write_csv(flu_nogender, "/Users/onayomirosenior-patten/Documents/GitHub/sanofi-rsv-flu/csv/Denmark/clean_flu_denmark.csv")
