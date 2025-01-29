ireland_data <- read.csv("csv/Ireland/Respiratory_Virus_Hub_Open_Data.csv")

# Separate the year and week column
ireland_data <- ireland_data %>%
  separate(week, into = c("year", "week"), sep = " ") %>%
  select(-floor, -ceiling, -extract_date, -total_cases, -Total_0_14, -Total_15_24, -Total_25_44, -Total_45_64, -Total_65_, -ObjectId)

# Separate into flu and rsv
flu_ireland <- filter(ireland_data, disease == "Influenza") %>% select(-disease)
rsv_ireland <- filter(ireland_data, disease == 'RSV') %>% select(-disease)

# write_csv(flu_ireland, "/Users/onayomirosenior-patten/Documents/GitHub/sanofi-rsv-flu/csv/Ireland/clean_flu_ireland.csv")
# write_csv(rsv_ireland, "/Users/onayomirosenior-patten/Documents/GitHub/sanofi-rsv-flu/csv/Ireland/clean_rsv_ireland.csv")