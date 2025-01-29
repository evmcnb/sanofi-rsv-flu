
# Flu data ----------------------------------------------------------------

flu_uk_data <- read.csv("csv/UK/influenza.csv") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(month = factor(month(date, label = TRUE, abbr = TRUE))) %>%
  select(sex, age, year, month, epiweek, date, metric_value)

rsv_uk_data <- read.csv("csv/UK/RSV.csv") %>%
  mutate(month = factor(month(date, label = TRUE, abbr = TRUE))) %>%
  select(sex, age, year, month, epiweek, date, metric_value)




uk_data <- flu_uk_data %>% 
  mutate(flu_r10k = metric_value / 10,
         country = "United Kingdom",
         country_code = "GBR") %>%
  full_join(rsv_uk_data, by= c("age", "year","month", "epiweek", "date")) %>% 
  mutate(rsv_r10k = metric_value.y / 10) %>%
  mutate(
    age_group = case_when(
      age %in% c("00-04") ~ "infant",                 # Ages 0-4
      age %in% c("05-14") ~ "adolescent",             # Ages 5-14
      age %in% c("15-44", "45-54", "55-64") ~ "adult", # Ages 15-64
      age %in% c("65-74", "75-84", "75+", "85", "85+") ~ "elderly", # Ages 65+
      age == "all" ~ "all",                           # Keep "all" as a separate category
      TRUE ~ NA_character_                            # Mark unexpected values as NA
    ),
    age_group = factor(age_group, levels = c("infant", "adolescent", "adult", "elderly", "all"))
  ) %>%
  group_by(country, country_code, month, year, epiweek, age_group) %>% 
  summarize(
    flu_r10k = sum(flu_r10k, na.rm = TRUE),
    rsv_r10k = sum(rsv_r10k, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  select(country, country_code, year, epiweek, month, age_group, flu_r10k, rsv_r10k) %>% 
  view()



# Graphics --------------------------------------------------------------------

# Bar graph over all the weeks 

flu_uk_data %>%
  filter(age == "all") %>%  # Filter before grouping for efficiency
  group_by(epiweek, year) %>%
  summarise(
    count = sum(metric_value),
    .groups = "drop"
  ) %>%
  arrange(year, epiweek) %>%
  ggplot(aes(x = interaction(year, epiweek, sep = "-"), y = count, colour = as.factor(year))) +
  geom_col(position = "dodge",
           width = 0.5,
           fill = "gray30") +
  labs(
    x = "Year-Week",
    y = "Number of Cases",
    title = "Weekly Hospitalisation Rate by Epidemiological Week and Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Line graph of years coloured by the month
flu_uk_data %>%
  filter(age == "all") %>%  # Filter for all ages and dates from 2015 onwards
  group_by(date) %>%  # Ensure grouping by date
  summarise(
    count = sum(metric_value, na.rm = TRUE),  # Ensure no NA values interfere
    .groups = "drop"
  ) %>%
  mutate(month = factor(month(date, label = TRUE, abbr = TRUE))) %>%  # Extract month as a factor
  ggplot(aes(x = date, y = count, colour = month, group = 1)) +  # Add group = 1 to avoid line break issues
  geom_line(size = 1) +  # Use line to show trends
  scale_colour_brewer(palette = "Paired") +  # Add a colour palette for months
  labs(
    x = "Time",
    y = "Hospital Admission Rate  ",
    colour = "Month",
    title = "Seasonality Trends of Influenza - Weekly Hospitalisations"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# Density ridge plot - may need to center the data as hard to see trends
flu_uk_data %>% 
  filter(age == "all") %>% 
  group_by(epiweek, year) %>%
  summarise(cases = sum(metric_value)) %>%
  ggplot(aes(x = epiweek, y = factor(year), height = cases, fill = factor(year))) +
  geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01) +
  labs(title = 'Flu Cases by Year and Week', x = 'Epi Week', y = 'Year') +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotates the x-axis labels for better readability
  )

flu_uk_data %>%
  filter(age == "all") %>%
  group_by(year, epiweek) %>%
  summarise(cases = sum(metric_value, na.rm = TRUE)) %>% 
  mutate(
    Adjusted_Week = epiweek + 26,  # Shift all weeks forward by 26
    Adjusted_Year = if_else(Adjusted_Week > 52, year + 1, year),  # Increment year if week > 52
    Adjusted_Week = if_else(Adjusted_Week > 52, Adjusted_Week - 52, Adjusted_Week)  # Wrap weeks > 52
  ) %>%
  ggplot(aes(x = Adjusted_Week, y = factor(Adjusted_Year), height = cases, fill = factor(Adjusted_Year))) +
  geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01) +
  labs(
    title = 'UK Influenza Case Density by Flu Season',
    x = 'Flu Season Week (Centred around Week 1)'
  ) +
  scale_x_continuous(
    breaks = c(1, 13, 26, 39),  # Key flu season weeks
    labels = c("Week 26", "Week 39", "Week 1", "Week 13")
  ) +
  scale_y_discrete(
    labels = function(x) paste0(as.numeric(x) - 1, "/", x)  # Convert years to "2014/2015" format
  ) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    axis.title.x = element_text(),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(size = 10)
  )

ggsave(
  filename = "plots/Other/flu_uk_case_density.png",  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = last_plot(),  # This refers to the last plot generated
  width = 7,  # Width of the plot (in inches)
  height = 7,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)


# Bar graphcs - cases per month
flu_uk_data %>%
  group_by(year, month) %>%
  summarise(total_cases = sum(metric_value)) %>%
  ggplot(aes(x = month, y = total_cases, fill = factor(year))) +
  geom_col(position = "dodge") +
  labs(title = 'UK Influenza Data', x = 'Year') +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Also might need centering - good thing about the polar plots this isnt needed. year 7 = 2021
flu_uk_data %>%
  filter(age == "all") %>%
  mutate(is_covid = if_else(year < 2021, 0, 1),
         is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>% 
  group_by(epiweek, is_covid) %>%
  summarise(cases = sum(metric_value)) %>% 
  ggplot(aes(x = epiweek, y = cases, color = factor(is_covid))) +
  geom_line() +
  labs(title = 'UK Influenza Data', x = 'Week') +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Same as above but in polar coordinates. Very clear plot - defo final
flu_uk_data %>%
  filter(age == "all") %>%
  mutate(is_covid = if_else(year < 2021, 0, 1),
         is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(epiweek, is_covid) %>%
  summarise(cases = sum(metric_value)) %>%
  ggplot(aes(x = epiweek, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  labs(title = 'UK Influenza Data', subtitle = "Before Lockdown is defined as any data prior to 2021", x = 'Week', y = "Cumulative Hosptialization Rate per 10,000") +
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text(),
    legend.position = "bottom",
    axis.ticks.y = element_line(),
    axis.line.y.left = element_line(),
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.y = element_text()
  ) +
  coord_polar(theta = "x")

ggsave(
  filename = "plots/Polar/flu_uk_polar_plot.png",  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = last_plot(),  # This refers to the last plot generated
  width = 6,  # Width of the plot (in inches)
  height = 6,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)

# Unfortunately not very useful
flu_uk_data %>%
  mutate(is_covid = if_else(year < 2021, 0, 1),
         is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(epiweek, is_covid, age) %>%
  summarise(cases = sum(metric_value)) %>%
  na.omit() %>% 
  ggplot(aes(x = epiweek, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  facet_wrap(~age) +
  labs(title = 'UK Influenza Data', subtitle = "Age stratified case data. Before Lockdown is defined as any data prior to 2021", x = 'Week', y = "Lab Confirmed Cases") +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )


# Similar to above but not aggregated 
flu_uk_data %>%
  filter(age == "all") %>%
  group_by(epiweek, year) %>%
  summarise(cases = sum(metric_value)) %>%
  ggplot(aes(x = epiweek, y = cases, color = factor(year))) +
  geom_line() +
  labs(title = 'UK Influenza Data', x = 'Week') +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  coord_polar(theta = "x")


