
# Flu data ----------------------------------------------------------------

flu_france_data <- read.csv("csv/France/influenza.csv") %>%
  mutate(date = week,
    year = substr(week, 1, 4),
    week = substr(week, 5, 6)
    )