

flu_us_data <- read_excel("csv/US/influenza.xlsx") %>%
  set_names(gsub("\\s+", "_", names(flu_us_data))) %>% 
  filter(VIRUS_TYPE_CATEGORY == "Overall") %>% 
  group_by(YEAR...4, WEEK, AGE_CATEGORY) %>% 
  summarise(
    weekly_rate = sum(WEEKLY_RATE),
    .groups = "drop"
  ) %>%
  arrange(YEAR...4, WEEK)

rsv_us_data <- read_csv("csv/US/RSV.csv") 

rsv_us_data <- rsv_us_data %>%
  set_names(gsub("\\s+", "_", names(rsv_us_data))) %>%
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
  distinct()
