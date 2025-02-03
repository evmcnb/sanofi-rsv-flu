massive_flu_df <- read_csv("C:/Users/Evan/Downloads/VIW_FID.csv")

# Filter the data for the selected date
filtered_data <- massive_flu_df %>%
  group_by(COUNTRY_CODE, AGEGROUP_CODE, ISO_WEEK, ISO_YEAR, ISO_WEEKSTARTDATE, HEMISPHERE) %>% 
  summarize(cases = sum(REPORTED_CASES, na.rm = TRUE),
            RSV = sum(RSV, na.rm = TRUE)) %>%
  filter(cases < 1e7) %>% 
  arrange(COUNTRY_CODE, ISO_YEAR, ISO_WEEK) %>%
  mutate(smooth_case = {
    smoothed <- ksmooth(ISO_WEEKSTARTDATE, cases, "normal", bandwidth = 3)
    smoothed$y[match(ISO_WEEKSTARTDATE, smoothed$x)]  # Extract corresponding smoothed value for each week
  }) %>%
  view()


uk_standard_data <- uk_data %>% 
  filter(age_group == "all") %>% 
  mutate(COUNTRY_CODE = country_code, 
         ISO_WEEK = epiweek,
         ISO_YEAR = year,
         cases = flu_r10k * 10000,
         HEMISPHERE = "NH",
         ISO_WEEKSTARTDATE = NA,
         AGE_GROUPCODE = age_group) %>% 
  select(COUNTRY_CODE, AGE_GROUPCODE, ISO_WEEK, ISO_YEAR, ISO_WEEKSTARTDATE, HEMISPHERE, cases) %>% 
  arrange(COUNTRY_CODE, ISO_YEAR, ISO_WEEK) %>% 
  view()

fr_standard_data <- flu_france_data %>% 
  mutate(COUNTRY_CODE = "-99", 
         ISO_WEEK = as.numeric(week),
         ISO_YEAR = as.numeric(year),
         cases = inc,
         HEMISPHERE = "NH",
         ISO_WEEKSTARTDATE = NA,
         AGE_GROUPCODE = "All") %>% 
  select(COUNTRY_CODE, AGE_GROUPCODE, ISO_WEEK, ISO_YEAR, ISO_WEEKSTARTDATE, HEMISPHERE, cases) %>% 
  arrange(COUNTRY_CODE, ISO_YEAR, ISO_WEEK) %>% 
  view()



filtered_data <- rbind(filtered_data, uk_standard_data)
filtered_data <- rbind(filtered_data, fr_standard_data)

filtered_data %>%
  filter(cases < 9000000) %>% 
  filter(ISO_YEAR > 2015) %>%
  mutate(is_covid = if_else(ISO_YEAR < 2021, 0, 1),
         is_covid = factor(is_covid, levels = c(0,1), labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(COUNTRY_CODE, is_covid) %>%
  summarise(has_data = mean(cases) > 5, .groups = "drop") %>%
  group_by(COUNTRY_CODE) %>%
  filter(n_distinct(is_covid) == 2) %>%   # Keep only countries with both Before & After data
  pull(COUNTRY_CODE) -> valid_countries

excluded_countries <- c("MDA", "X10", "BIH", "GEO", "ISL", "LAO", "PRK", "X9", "MOZ", "NPL", "NER", "ROU", "SDN", "PCN", "CUB", "NRU")

filtered_data %>%
  filter(COUNTRY_CODE %in% valid_countries & !COUNTRY_CODE %in% excluded_countries) %>%
  mutate(is_covid = if_else(ISO_YEAR < 2021, 0, 1),
         is_covid = factor(is_covid, levels = c(0,1), labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(ISO_WEEK, is_covid, COUNTRY_CODE) %>%
  summarise(cases = sum(cases), .groups = "drop") %>% 
  ggplot(aes(x = ISO_WEEK, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  facet_wrap(~COUNTRY_CODE, scales = "free_y") +
  labs(title = 'World Influenza Data', 
       subtitle = "Before Lockdown is defined as any data prior to 2021.", 
       x = 'Week', y = "Lab Confirmed Cases") +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

filtered_data %>%
  filter(COUNTRY_CODE %in% valid_countries & !COUNTRY_CODE %in% excluded_countries) %>%
  mutate(is_covid = if_else(ISO_YEAR < 2021, 0, 1),
         is_covid = factor(is_covid, levels = c(0,1), labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(ISO_WEEK, is_covid, COUNTRY_CODE) %>%
  summarise(cases = sum(RSV, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = ISO_WEEK, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  facet_wrap(~COUNTRY_CODE, scales = "free_y") +
  labs(title = 'World RSV Data', 
       subtitle = "Before Lockdown is defined as any data prior to 2021.", 
       x = 'Week', y = "Lab Confirmed Cases") +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

flu_median <- median(filtered_data$cases, na.rm = TRUE)
rsv_median <- median(filtered_data$RSV, na.rm = TRUE)

flu_mean <- mean(filtered_data$cases, na.rm = TRUE)
rsv_mean <- mean(filtered_data$RSV, na.rm = TRUE)

scale_factor_median <- flu_median / rsv_median
scale_factor_mean <- flu_mean / rsv_mean

list(
  median_ratio = scale_factor_median,
  mean_ratio = scale_factor_mean
)

scale_factor <- 20

filtered_data %>%
  filter(COUNTRY_CODE %in% valid_countries & !COUNTRY_CODE %in% excluded_countries) %>%
  pivot_longer(cols = c("cases", "RSV"), names_to = "disease", values_to = "cases") %>%
  group_by(ISO_WEEKSTARTDATE, disease, COUNTRY_CODE) %>%
  summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  filter(cases > 10) %>% 
  ggplot(aes(x = ISO_WEEKSTARTDATE, y = cases, color = factor(disease))) +
  geom_line(data = . %>% filter(disease == "cases"), aes(y = cases), size = 1) +
  geom_line(data = . %>% filter(disease == "RSV"), aes(y = cases * scale_factor), size = 1) +
  facet_wrap(~COUNTRY_CODE, scales = "free_y") +
  scale_y_continuous(
    name = "Lab Confirmed Influenza Cases",
    sec.axis = sec_axis(~ . / scale_factor, name = "Lab Confirmed RSV Cases")
  ) +
  labs(
    title = 'World Influenza vs RSV Data', 
    subtitle = "Before Lockdown is defined as any data prior to 2021.", 
    x = 'Week'
  ) +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )




library(rnaturalearth)
library(rnaturalearthdata)
library(ggcorrplot)

seasonality_shift <- filtered_data %>%
  filter(COUNTRY_CODE %in% valid_countries & !COUNTRY_CODE %in% excluded_countries) %>%
  filter(age)
  mutate(is_covid = if_else(ISO_YEAR < 2021, "Before", "After")) %>%
  group_by(COUNTRY_CODE, is_covid, ISO_WEEK) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  group_by(COUNTRY_CODE, is_covid) %>%
  filter(total_cases == max(total_cases)) %>%
  summarise(peak_week = first(ISO_WEEK), .groups = "drop") %>%
  pivot_wider(names_from = is_covid, values_from = peak_week) %>%
  mutate(
    raw_shift = After - Before,
    week_shift = ifelse(
      abs(raw_shift) > 26,  # If shift is more than half a year, wrap around
      ifelse(raw_shift > 0, raw_shift - 52, raw_shift + 52),
      raw_shift
    )
  )

write_csv(seasonality_shift, file = "csv/flu_net_shift.csv")

world_map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  left_join(seasonality_shift, by = c("iso_a3" = "COUNTRY_CODE"))

ggplot(world_map_data) +
  geom_sf(aes(fill = week_shift), color = "black", size = 0.1) +
  # scale_point_fill_continuous() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "gray70") +
  labs(title = "Change in Time of Influenza Peak Pre- and Post- COVID-19",
       fill = "Week Shift") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.ticks = element_line(color = "gray50", size = 0.2),
    axis.line = element_line(color = "gray50", size = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    panel.background = element_blank(),  # removes panel background
    plot.background = element_blank(),   # removes plot background
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text.x = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  ) + 
  theme_fivethirtyeight()

ggsave(
  filename = "plots/Other/world_map.png",  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = last_plot(),  # This refers to the last plot generated
  width = 10,  # Width of the plot (in inches)
  height = 7,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)
library(leaflet)
library(dplyr)

# Ensure 'week_shift' is numeric
world_map_data <- world_map_data %>%
  mutate(week_shift = as.numeric(week_shift))

# Create the leaflet map
leaflet(world_map_data) %>%
  addTiles() %>%  # Add base tiles (default OpenStreetMap)
  addPolygons(
    fillColor = ~colorNumeric("Blues", week_shift)(week_shift),  # Colour based on 'week_shift'
    color = "black",  # Border color
    weight = 0.5,  # Border weight
    opacity = 1,  # Border opacity
    fillOpacity = 0.7,  # Fill opacity
    popup = ~paste("Country: ", name, "<br>Week Shift: ", week_shift, "</br>"),  # Display country name and week shift in popup
    label = ~paste("Country: ", name, "<br>Week Shift: ", week_shift, "</br>"),  # Add labels for hover
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric("Blues", world_map_data$week_shift),
    values = world_map_data$week_shift,
    title = "Week Shift",
    opacity = 1
  ) %>%
  setView(lng = 0, lat = 20, zoom = 2)  # Set the initial view of the map


seasonality_variance <- filtered_data %>%
  filter(COUNTRY_CODE %in% valid_countries) %>%
  mutate(is_covid = if_else(ISO_YEAR < 2021, "Before", "After")) %>%
  group_by(COUNTRY_CODE, is_covid, ISO_WEEK) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  group_by(COUNTRY_CODE, is_covid) %>%
  summarise(week_sd = sd(total_cases, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = is_covid, values_from = week_sd) %>%
  mutate(sd_ratio = After / Before)

ggplot(seasonality_variance, aes(x = Before, y = After, label = COUNTRY_CODE)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(vjust = -0.5, hjust = 1) +
  labs(title = "Change in Flu Seasonality Spread",
       x = "Spread Before COVID",
       y = "Spread After COVID") +
  theme_minimal()

seasonality_correlation <- filtered_data %>%
  filter(COUNTRY_CODE %in% valid_countries) %>%
  mutate(is_covid = if_else(ISO_YEAR < 2021, "Before", "After")) %>%
  group_by(COUNTRY_CODE, is_covid, ISO_WEEK) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = is_covid, values_from = total_cases) %>%
  drop_na(Before, After) %>%  # Remove countries missing all data for either period
  group_by(COUNTRY_CODE) %>%
  summarise(correlation = cor(Before, After, use = "complete.obs"), .groups = "drop")


world_map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  left_join(seasonality_correlation, by = c("iso_a3" = "COUNTRY_CODE"))

ggplot(world_map_data) +
  geom_sf(aes(fill = correlation), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "grey") +
  labs(title = "Correlation between Before vs After COVID",
       fill = "Week Shift") +
  theme_minimal()


seasonality_correlation <- filtered_data %>%
  filter(ISO_YEAR >= 2015) %>%
  mutate(is_covid = if_else(ISO_YEAR < 2021, "Before", "After")) %>%
  group_by(COUNTRY_CODE, is_covid, ISO_WEEK) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = is_covid, values_from = total_cases) %>%
  drop_na(Before, After) %>%  # Remove countries missing all data for either period
  group_by(COUNTRY_CODE, ISO_WEEK) %>%
  summarise(correlation = cor(Before, After, use = "complete.obs"), .groups = "drop")

# Plot correlation heatmap
ggplot(seasonality_correlation, aes(x = month, y = COUNTRY_CODE, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "grey") +
  labs(title = "Monthly Correlation of Flu Seasonality (Before vs After COVID)",
       x = "Month",
       y = "Country",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

