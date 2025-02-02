
# Flu data ----------------------------------------------------------------

flu_france_data <- read.csv("csv/France/influenza.csv") %>%
  mutate(date = week,
    year = substr(week, 1, 4),
    week = substr(week, 5, 6)
    )




# Luke week shift plots ---------------------------------------------------------


ggplot(flu_shift, aes(x=week, y=cases, group = year, color=year)) + geom_line()

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
  scale_x_discrete(breaks = unique(lag_data$year)) +  # integer years on the x-axis
  ylim(-10, 10)  # set y-axis limits from -10 to +10
