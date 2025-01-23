

flu_hk_data <- read.csv("csv/HK/influenza.csv") %>%
  select(Week, Year, H1, H3, B, AandB, starts_with("Adm_"), starts_with("SevereCase"))
