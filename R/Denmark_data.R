

flu_dk_data <- read_excel("csv/Denmark/flu_denmark.xlsx") %>% 
  mutate(
    year = substr(Season, 1, 4),  # Extract the first 4 characters as the year
    week_number = gsub(".*-U(.*)", "\\1", Week)  # Extract the week number after "U"
  ) %>%
  group_by(week_number, year, Age_group) %>%
  summarise(admissions = sum(Number_of_new_admissions), .groups = "drop") %>% 
  view()

rsv_dk_data <- read_csv("csv/Denmark/rsv_denmark.csv") %>% 
  mutate(
    season_number = gsub("Year (\\d+) .*", "\\1", week_sort),  # Extract the year (first digit)
    week = gsub(".*week (\\d+)", "\\1", week_sort),
    start_year = as.numeric(substr(Season, 1, 4)),
    year = ifelse(season_number == 1, start_year, start_year + 1)
  ) %>% 
  rename(age_group = 5) %>% 
  select(-start_year) %>%
  group_by(week, year, age_group) %>%
  summarise(cases = floor(sum(value)), .groups = "drop") %>% 
  view()
