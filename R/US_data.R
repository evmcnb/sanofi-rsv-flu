

# flu_us_data <- read_excel("csv/US/influenza.xlsx") %>%
#   set_names(gsub("\\s+", "_", names(.))) %>% 
#   filter(VIRUS_TYPE_CATEGORY == "Overall") %>%
#   filter(!AGE_CATEGORY %in% c("0-4 yr", "0-<1 yr", "1-4 yr", "12-17 yr", "18-29 yr", "18-49 yr", "30-39 yr", "40-49 yr", "5-11 yr", "5-17 yr", "50-64 yr", "65-74 yr", "75-84 yr", "< 18", ">= 18", ">= 65 yr", ">= 75", ">= 85")) %>%  
#   group_by(YEAR...4, WEEK, AGE_CATEGORY) %>% 
#   summarise(
#     weekly_rate = sum(WEEKLY_RATE),
#     .groups = "drop"
#   ) %>%
#   arrange(YEAR...4, WEEK) %>% 
#   view()

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
