

flu_us_data <- read_excel("csv/US/influenza.xlsx") %>%
  set_names(gsub("\\s+", "_", names(.))) %>% 
  filter(VIRUS_TYPE_CATEGORY == "Overall") %>%
  mutate(
    age_group = case_when(
      AGE_CATEGORY %in% c("0-< 1 yr", "< 18") ~ "Infant",                 # Ages 0-<1 or <18
      AGE_CATEGORY %in% c("0-4 yr", "1-4 yr", "5-11  yr", "5-17 yr") ~ "Adolescent", # Ages 1-17
      AGE_CATEGORY %in% c("18-29 yr", "18-49 yr", "30-39 yr", "40-49 yr", "50-64 yr") ~ "Adult", # Ages 18-64
      AGE_CATEGORY %in% c("65-74 yr", "75-84 yr", ">= 65 yr", ">= 75", ">= 85") ~ "Elderly", # Ages 65+
      AGE_CATEGORY == "Overall" ~ "All",                                     # Keep "Overall" as a separate category
      TRUE ~ NA_character_                                                    # Mark unexpected values as NA
    ),
    age_group = factor(age_group, levels = c("Infant", "Adolescent", "Adult", "Elderly", "All"))
  ) %>%
  group_by(YEAR...4, WEEK, age_group) %>% 
  summarise(
    weekly_rate = sum(WEEKLY_RATE),
    .groups = "drop"
  ) %>%
  arrange(YEAR...4, WEEK) %>% 
  view()


rsv_us_data <- read_csv("csv/US/RSV.csv")  %>%
  set_names(gsub("\\s+", "_", names(.))) %>%
  mutate(
    year = year(Week_ending_date),
    week_number = week(Week_ending_date)
  ) %>% 
  group_by(year, week_number, Age_Category) %>%
  summarise(
    weekly_rate = sum(Rate),
    year = year,
    week_number = week_number,
    .groups = "drop"
  ) %>%
  arrange(year, week_number) %>%
  distinct() %>% 
  view()
