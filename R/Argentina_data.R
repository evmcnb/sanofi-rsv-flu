## This is a script that is pulling the influenza and RSV data from Argentina


flu_uk_data <- read.csv("csv/UK/influenza.csv") %>%
  mutate(year = factor(year)) %>% 
  select(sex, age, year, month, epiweek, date, metric_value)

rsv_uk_data <- read.csv("csv/UK/RSV.csv") %>%
  mutate(year = factor(year)) %>% 
  select(sex, age, year, month, epiweek, date, metric_value)