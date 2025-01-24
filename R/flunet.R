massive_flu_df <- read_csv("C:/Users/Evan/Downloads/VIW_FID.csv")

# Filter the data for the selected date
filtered_data <- massive_flu_df %>%
  group_by(COUNTRY_CODE, AGEGROUP_CODE, MMWR_WEEK, MMWR_YEAR) %>% 
  summarize(inpatients = sum(INPATIENTS, na.rm = TRUE)) %>%
  filter(COUNTRY_CODE == "GBR") %>%
  arrange(MMWR_YEAR, MMWR_WEEK) %>% 
  view()
