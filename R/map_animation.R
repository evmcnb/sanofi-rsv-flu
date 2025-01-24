library(tidyverse)
library(sf)
library(rnaturalearth)
library(gganimate)
library(transformr)

# Load data
massive_flu_df <- read_csv("C:/Users/Evan/Downloads/VIW_FID.csv")

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Preprocess the data
filtered_data <- massive_flu_df %>%
  filter(MMWR_YEAR >= 2015) %>%
  group_by(COUNTRY_CODE, MMWR_YEAR, MMWR_WEEK) %>%
  summarize(cases = sum(RSV, na.rm = TRUE), .groups = "drop") 

# Join the map data with the filtered data
map_data <- world %>%
  left_join(filtered_data, by = c("adm0_a3" = "COUNTRY_CODE")) %>%
  mutate(
    cases = replace_na(cases, 0),  # Replace missing cases with 0
    case_prop = cases / pop_est * 100  # Cases per 100k population
  ) %>%
  arrange(MMWR_YEAR, MMWR_WEEK)  # Arrange by year and week

# Create the plot
animated_plot <- ggplot(map_data) +
  geom_sf(aes(fill = case_prop), colour = "black") +  # Map boundaries
  scale_fill_viridis_c(
    option = "plasma", 
    na.value = "grey"  # Grey for countries with no data
  ) +
  labs(
    title = "Flu Cases Worldwide: Year {closest_state}",
    fill = "Cases per 100k",
    subtitle = "Epidemiological Week: {frame_along}"
  ) +
  theme_minimal() +
  transition_states(
    states = interaction(MMWR_YEAR, MMWR_WEEK, sep = "-"),  # Combine year and week for transitions
    transition_length = 1,
    state_length = 0
  ) +
  enter_fade() +
  exit_fade()

# Render and save animation
anim_save(
  filename = "flu_cases_animation.mp4",
  animation = animated_plot,
  fps = 20,  # Increased FPS
  renderer = av_renderer(),  # Render as MP4
  width = 1200, height = 720
)
