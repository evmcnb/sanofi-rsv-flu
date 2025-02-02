file_path = "csv/Australia/influenza.xlsx"

# Read all sheets
all_data <- excel_sheets(file_path) %>%
  set_names() %>%  # Use sheet names for better tracking
  map_df(~ read_excel(file_path, sheet = .x) %>%
           mutate(sheet_name = .x))  # Add a column to track the sheet source

# Process the combined data
flu_au_data <- all_data %>%
  rename(date = 1, age_group = 3) %>% 
  mutate(date = dmy(date)) %>%
  mutate(epi_year = isoyear(date),
         epi_week = isoweek(date)) %>%
  group_by(date, epi_year, epi_week, age_group) %>%
  summarise(cases = n(), .groups = "drop")

rm(list = c("all_data", "file_path"))


rsv_au_data <- read_excel("csv/Australia/RSV.xlsx")



# Plots -------------------------------------------------------------------


# Great plot showing seasonality differences
flu_au_data %>%
  mutate(is_covid = if_else((epi_year) < 2021, 0, 1),
         is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(epi_week, is_covid) %>%
  summarise(cases = sum(cases)) %>%
  na.omit() %>% 
  ggplot(aes(x = epi_week, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  labs(title = 'AU Influenza Data', subtitle = "Before Lockdown is defined as any data prior to 2021", x = 'Week', y = "Lab Confirmed Cases") +
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

# ggsave(
#   filename = "plots/Polar/flu_au_polar_plot_new.png",  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
#   plot = last_plot(),  # This refers to the last plot generated
#   width = 6,  # Width of the plot (in inches)
#   height = 6,  # Height of the plot (in inches)
#   dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
# )


# Another great plot showing seasonality changes across all age groups. 
flu_au_data %>%
  mutate(is_covid = if_else((epi_year) < 2021, 0, 1),
         is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(epi_week, is_covid, age_group) %>%
  summarise(cases = sum(cases)) %>%
  na.omit() %>% 
  ggplot(aes(x = epi_week, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  facet_wrap(~age_group) +
  labs(title = 'AU Influenza Data', subtitle = "Age stratified case data. Before Lockdown is defined as any data prior to 2021", x = 'Week', y = "Lab Confirmed Cases") +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# ggsave(
#   filename = "plots/Age/flu_au_age_facet.png",  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
#   plot = last_plot(),  # This refers to the last plot generated
#   width = 10,  # Width of the plot (in inches)
#   height = 6,  # Height of the plot (in inches)
#   dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
# )


# Polar plot with all the years of data
flu_au_data %>%
  group_by(epi_week, epi_year) %>%
  summarise(cases = sum(cases)) %>%
  na.omit() %>% 
  ggplot(aes(x = epi_week, y = cases, color = factor(epi_year))) +
  geom_line() +
  labs(title = 'AU Influenza Data', x = 'Week') +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  coord_polar(theta = "x")

# Pretty good
flu_au_data %>% 
  filter(epi_year > 2014) %>% 
  group_by(epi_week, epi_year) %>%
  summarise(cases = sum(cases)) %>%
  na.omit() %>% 
  ggplot(aes(x = epi_week, y = factor(epi_year), height = cases, fill = factor(epi_year))) +
  geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01, size = 1) +
  labs(title = 'Australia Influenza Case Density by Year', x = 'Week') +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    axis.title.x = element_text(),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text()  # Rotates the x-axis labels for better readability
  )

# ggsave(
#   filename = "plots/Other/flu_au_case_density2.png",  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
#   plot = last_plot(),  # This refers to the last plot generated
#   width = 6,  # Width of the plot (in inches)
#   height = 6,  # Height of the plot (in inches)
#   dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
# )



# # Trying to animate stuff... can ignore ----------------------------------------------
# 
# library(gganimate)

# flu_au_data %>%
#   arrange(epi_year, epi_week) %>%
#   group_by(epi_year, epi_week) %>% 
#   summarise(cases = sum(cases)) %>%
#   mutate(order = epi_year+epi_week) %>% 
#   na.omit() %>%
#   ggplot(aes(x = epi_week, y = cases, color = factor(epi_year))) +
#   geom_line(stat="identity") + # use stat="identity" to draw a single line
#   labs(title = 'AU Influenza Data', x = 'Week') +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   ) +
#   transition_reveal(order) +
#   coord_polar(theta = "x")
  
# 
# flu_au_data %>%
#   arrange(epi_year, epi_week) %>%
#   group_by(epi_year, epi_week) %>%
#   summarise(cases = sum(cases)) %>%
#   mutate(order = epi_year+epi_week) %>%
#   na.omit() %>%
#   ggplot(aes(x = epi_week, y = cases, color = factor(epi_year))) +
#   geom_line(stat="identity") + 
#   labs(title = 'AU Influenza Data', x = 'Week') +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   ) +
#   transition_reveal(order) + # reveal each year one at a time
#   ease_aes("linear") + 
#   coord_polar(theta = "x")
# 
# 
# library(tidyverse)
# library(gganimate)
# library(av)
# 
# flu_au_animation <- flu_au_data %>%
#   arrange(epi_year, epi_week) %>%
#   group_by(epi_year, epi_week) %>%
#   summarise(cases = sum(cases), .groups = "drop") %>%
#   mutate(order = row_number()) %>%  # Sequential order for plotting
#   na.omit() %>%
#   ggplot(aes(
#     x = epi_week,
#     y = cases,
#     group = epi_year,
#     color = factor(epi_year),
#   )) +
#   geom_line() +
#   labs(
#     title = "AU Influenza Data",
#     subtitle = "",
#     x = "Week",
#     y = "Cases"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   ) +
#   coord_polar(theta = "x") +
#   transition_reveal(order) +
#   ease_aes("cubic-in-out")
# 
# animate(
#   flu_au_animation,
#   renderer = av_renderer("flu_animation.mp4"), # Specify the output file name
#   width = 1024, height = 1024, res = 150, fps = 20, duration = 20
# )




# Luke week shift plots ---------------------------------------------------------

# UPDATE variable names!

# combine age groups to obtain weekly cases
aus_lb <- flu_au_data %>%
  arrange(epi_year, epi_week) %>%
  group_by(epi_year, epi_week) %>%
  summarise(cases = sum(cases), .groups = "drop")

# carry out a time lag correlation / cross-correlation to best estimate the week shift
# initially compare to 2019 then we can extend this to "pre-covid"
aus_lb2 <- aus_lb %>%
  filter(epi_year %in% c(2019, 2022, 2023)) %>%
  select(epi_year, epi_week, cases)

# convert into wide format for correlation analysis
aus_wide <- aus_lb2 %>% tidyr::pivot_wider(names_from = epi_year, values_from = cases)
# calculate correlation for each shift
ccf_2022 <- ccf(aus_wide$`2019`, aus_wide$`2022`, lag.max = 20, plot = TRUE); # cap the shift at 20 weeks either side
ccf_2023 <- ccf(aus_wide$`2019`, aus_wide$`2023`, lag.max = 20, plot = TRUE);
# find the peak correlation and corresponding lag
lag_2022 <- ccf_2022$lag[which.max(ccf_2022$acf)]  # best shift for 2022
lag_2023 <- ccf_2023$lag[which.max(ccf_2023$acf)]

# create a lag dataframe
lag_data <- data.frame(
  year = c(2022, 2023),
  shift_in_weeks = c(lag_2022, lag_2023)
)

# plot the results
ggplot(lag_data, aes(x = year, y = shift_in_weeks)) +
  geom_line(aes(group = 1), color = "skyblue", size = 1) +
  geom_point(color = "skyblue", size = 3) +
  labs(title = "Estimated Seasonality Shift in Weeks (Compared to 2019)",
       x = "Year",
       y = "Shift in Weeks") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2022, 2023, by = 1)) +  # Integer years on the x-axis
  ylim(-10, 10) +  # Set y-axis limits from -10 to +10
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
