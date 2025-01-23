library(tidyverse)
library(gganimate)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# MASSIVE FLU DATABASE  ---------------------------------------------------


massive_flu_df <- read_csv("C:/Users/Evan/Downloads/VIW_FID.csv")

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter the data for the selected date
filtered_data <- massive_flu_df %>%
  filter(MMWR_YEAR == "2021" & MMWR_WEEK == '40') %>% 
  group_by(COUNTRY_CODE) %>% 
  summarize(cases = sum(RSV, na.rm = TRUE)) %>% 
  view()

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join the map data with the filtered data
map_data <- world %>%
  left_join(filtered_data, by = c("adm0_a3" = "COUNTRY_CODE")) %>%  # Join using ISO codes
  mutate(case_prop = cases/pop_est * 100)

# Plot the map for the selected date
ggplot(map_data) +
  geom_sf(aes(fill = case_prop), colour = "black") + # Map boundaries
  scale_fill_viridis_c(option = "plasma", na.value = "grey") + # Colour scale
  labs(
    title = paste("Flu Cases 2021"),
    fill = "Cases"
  ) +
  theme_minimal()
