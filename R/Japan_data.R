# create file names for each year

file_names <- paste0("csv/Japan/", 2015:2024, ".csv")

# read all datasets,create a Year column and combine
Japan_data <- do.call(rbind, lapply(file_names, function(file) {
  data <- read.csv(file)
  data$Year <- gsub("^.*/|\\.csv$", "", file) # extract year from filename
  return(data)
}))

Japan_data <- Japan_data %>%
  mutate(
    flu_cases = as.numeric(flu_cases),
    Year = as.numeric(Year)
  ) %>%
  mutate(
    flu_cases = ifelse(is.na(flu_cases), 0, flu_cases),
    Year = ifelse(is.na(Year), 0, Year)
  )


# Clean up
rm(list=c("file_names"))



# Graphics ----------------------------------------------------------------

Japan_data %>%
  mutate(is_covid = if_else((Year) < 2021, 0, 1),
         is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(Week, is_covid) %>% 
  summarise(cases = sum(flu_cases)) %>% 
  ggplot(aes(x = Week, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  labs(title = 'Japan Influenza Data', subtitle = "Before Lockdown is defined as any data prior to 2021", x = 'Week', y = "Lab Confirmed Cases") +
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
  filename = "plots/Polar/flu_jp_polar_plot.png",  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = last_plot(),  # This refers to the last plot generated
  width = 6,  # Width of the plot (in inches)
  height = 6,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)


Japan_data %>% 
  ggplot(aes(x = Week, y = factor(Year), height = flu_cases, fill = factor(Year))) +
  geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01) +
  labs(title = 'Japan Influenza Case Density by Year', x = 'Week') +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    axis.title.x = element_text(),
    strip.text.x = element_text(size = 8)
  )

# Centered around the flu season plot
Japan_data %>%
  mutate(
    Adjusted_Week = Week + 26,  # Shift all weeks forward by 26
    Adjusted_Year = if_else(Adjusted_Week > 52, Year + 1, Year),  # Increment year if week > 52
    Adjusted_Week = if_else(Adjusted_Week > 52, Adjusted_Week - 52, Adjusted_Week)  # Wrap weeks > 52
  ) %>%
  ggplot(aes(x = Adjusted_Week, y = factor(Adjusted_Year), height = flu_cases, fill = factor(Adjusted_Year))) +
  geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01) +
  labs(
    title = 'Japan Influenza Case Density by Flu Season',
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
  filename = "plots/Other/flu_jp_case_density.png",  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = last_plot(),  # This refers to the last plot generated
  width = 7,  # Width of the plot (in inches)
  height = 7,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)




# RSV Plots ---------------------------------------------------------------

Japan_data %>%
  mutate(is_covid = if_else((Year) < 2021, 0, 1),
         is_covid = factor(is_covid, labels = c("Before Lockdown", "After Lockdown"))) %>%
  group_by(Week, is_covid) %>% 
  summarise(cases = sum(RSV_cases)) %>% 
  ggplot(aes(x = Week, y = cases, color = factor(is_covid))) +
  geom_line(size = 1) +
  labs(title = 'Japan RSV Data', subtitle = "Before Lockdown is defined as any data prior to 2021", x = 'Week', y = "Lab Confirmed Cases") +
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

Japan_data %>% 
  ggplot(aes(x = Week, y = factor(Year), height = RSV_cases, fill = factor(Year))) +
  geom_density_ridges(stat = "identity", scale = 1, rel_min_height = 0.01) +
  labs(title = 'Japan RSV Case Density by Year', x = 'Week') +
  theme_fivethirtyeight() + 
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    axis.title.x = element_text(),
    strip.text.x = element_text(size = 8)
  )

