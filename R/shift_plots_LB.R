# Post-COVID shift plots

# set working directory
setwd("C:\\Users\\lukeb\\Downloads\\LSHTM\\TERM 2\\Data Challenge\\github\\sanofi-rsv-flu")

# packages and libraries
install.packages("readxl")
install.packages("ggthemes")
library(tidyverse)
library(readxl)
library(ggthemes)

# source all the weekly countries - can introduce France and Japan later
source("R/France_data.R")
source("R/Uk_data.R")
source("R/Australia_data.R")
source("R/Denmark_data.R")
source("R/HK_data.R")
source("R/US_data.R")
source("R/Argentina_data_LL.R")
source("R/Ireland_code.R")
source("R/Taiwan_data.R")




### Argentina

# combine age groups to obtain weekly cases
flu_shift <- Argentina_all_data %>%
  select(year, epi_weeks, event, age_group, num_cases) %>%
  filter(event == "Influenza") %>%
  arrange(year, epi_weeks) %>%
  group_by(year, epi_weeks) %>%
  summarise(cases = sum(num_cases), .groups = "drop")

# carry out a time lag correlation / cross-correlation to best estimate the week shift
# initially compare to 2019 then we can extend this to 'pre-covid'
flu_shift <- flu_shift %>%
  filter(year > 2021 | year == 2019) %>%
  select(year, epi_weeks, cases)

# convert into wide format for correlation analysis
flu_shift_wide <- flu_shift %>% tidyr::pivot_wider(names_from = year, values_from = cases)

# find which post-2021 years are actually in the dataset
years_to_analyse <- unique(flu_shift$year[flu_shift$year > 2021])

# calculate lag for each year
lag_results <- lapply(years_to_analyse, function(year) {
  ccf_result <- ccf(flu_shift_wide$`2019`, flu_shift_wide[[as.character(year)]], lag.max = 20, plot = TRUE) # cap the shift at 20wks either side
  best_lag <- ccf_result$lag[which.max(ccf_result$acf)] # find which lag gives highest correlation
  data.frame(year = year, shift = best_lag)
})

# combine the results from each year
lag_data <- do.call(rbind, lag_results)

# plot the results using common aesthetic
ggplot(lag_data, aes(x = year, y = shift)) +
  geom_line(aes(group = 1), color = "skyblue", size = 1) +
  geom_point(color = "skyblue", size = 3) +
  labs(title = "Estimated Seasonality Shift in Weeks (Compared to 2019)",
       x = "Year",
       y = "Shift in Weeks") +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    axis.ticks.y = element_line(),
    axis.line.y.left = element_line(),
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.y = element_text()
  ) +
  scale_x_continuous(breaks = seq(min(lag_data$year), max(lag_data$year), by = 1)) +  # integer years on the x-axis
  ylim(-10, 10)  # set y-axis limits from -10 to +10



### Australia

# combine age groups to obtain weekly cases
flu_shift <- flu_au_data %>%
  arrange(epi_year, epi_week) %>%
  group_by(epi_year, epi_week) %>%
  summarise(cases = sum(cases), .groups = "drop")

# carry out a time lag correlation / cross-correlation to best estimate the week shift
# initially compare to 2019 then we can extend this to 'pre-covid'
flu_shift <- flu_shift %>%
  filter(epi_year > 2021 | epi_year == 2019) %>%
  select(epi_year, epi_week, cases)

# convert into wide format for correlation analysis
flu_shift_wide <- flu_shift %>% tidyr::pivot_wider(names_from = epi_year, values_from = cases)

# find which post-2021 years are actually in the dataset
years_to_analyse <- unique(flu_shift$epi_year[flu_shift$epi_year > 2021])

# calculate lag for each year
lag_results <- lapply(years_to_analyse, function(year) {
  ccf_result <- ccf(flu_shift_wide$`2019`, flu_shift_wide[[as.character(year)]], lag.max = 20) # cap the shift at 20wks either side
  best_lag <- ccf_result$lag[which.max(ccf_result$acf)] # find which lag gives highest correlation
  data.frame(year = year, shift = best_lag)
})

# combine the results from each year
lag_data <- do.call(rbind, lag_results)

# plot the results using common aesthetic
ggplot(lag_data, aes(x = year, y = shift)) +
  geom_line(aes(group = 1), color = "skyblue", size = 1) +
  geom_point(color = "skyblue", size = 3) +
  labs(title = "Estimated Seasonality Shift in Weeks (Compared to 2019)",
       x = "Year",
       y = "Shift in Weeks") +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    axis.ticks.y = element_line(),
    axis.line.y.left = element_line(),
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.y = element_text()
  ) +
  scale_x_continuous(breaks = seq(min(lag_data$year), max(lag_data$year), by = 1)) +  # integer years on the x-axis
  ylim(-10, 10)  # set y-axis limits from -10 to +10


### Denmark



### France

# combine age groups to obtain weekly cases
flu_shift <- flu_france_data %>%
  arrange(year, week) %>%
  group_by(year, week) %>%
  summarise(cases = sum(inc), .groups = "drop")

# carry out a time lag correlation / cross-correlation to best estimate the week shift
# initially compare to 2019 then we can extend this to 'pre-covid'
flu_shift <- flu_shift %>%
  filter(year %in% c(2022, 2023, 2024) | year == 2019) %>% # removed 2025 due to current missing data / NAs
  select(year, week, cases)

# convert into wide format for correlation analysis
flu_shift_wide <- flu_shift %>% tidyr::pivot_wider(names_from = year, values_from = cases)

# find which post-2021 years are actually in the dataset
years_to_analyse <- unique(flu_shift$year[flu_shift$year > 2021])

# calculate lag for each year
lag_results <- lapply(years_to_analyse, function(year) {
  ccf_result <- ccf(flu_shift_wide$`2019`, flu_shift_wide[[as.character(year)]], lag.max = 20) # cap the shift at 20wks either side
  best_lag <- ccf_result$lag[which.max(ccf_result$acf)] # find which lag gives highest correlation
  data.frame(year = year, shift = best_lag)
})

# combine the results from each year
lag_data <- do.call(rbind, lag_results)

# plot the results using common aesthetic
ggplot(lag_data, aes(x = year, y = shift)) +
  geom_line(aes(group = 1), color = "skyblue", size = 1) +
  geom_point(color = "skyblue", size = 3) +
  labs(title = "Estimated Seasonality Shift in Weeks (Compared to 2019)",
       x = "Year",
       y = "Shift in Weeks") +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    axis.ticks.y = element_line(),
    axis.line.y.left = element_line(),
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.y = element_text()
  ) +
  scale_x_discrete(breaks = unique(lag_data$year)) +  # integer years on the x-axis
  ylim(-10, 10)  # set y-axis limits from -10 to +10


### Hong Kong



### Ireland



### Taiwan




### UK


### USA