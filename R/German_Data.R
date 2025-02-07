#1. List all CSV files in a directory
flu_file_list <- list.files(path = "csv/Germany/flu", pattern = "*.csv", full.names = TRUE)

#2. Read all files into a list of data frames
flu_data_list <- lapply(flu_file_list, read_csv)

#1. List all CSV files in a directory
rsv_file_list <- list.files(path = "csv/Germany/rsv", pattern = "*.csv", full.names = TRUE)

#2. Read all files into a list of data frames
rsv_data_list <- lapply(rsv_file_list, read_csv)


German_data <- data.frame(
  country = character(),
  continent = character(),
  year = integer(),
  month = integer(),
  week = integer(),
  disease = character(),
  age = numeric(),
  metric = numeric(),
  source = character()
)

i = 0

for (item in flu_data_list) {
  FLU_GERM_MERGE <- item %>%
    pivot_longer(-Week,
                 names_to = "age",
                 values_to = "metric") %>%
    mutate(
      country = "Germany",
      continent = "Europe",
      disease = "Influenza",
      month = NA,
      age = age,
      metric = replace_na(metric, 0),
      source = "GOV",
      week = Week,
      year = 2016 + i) %>%
    select(country, continent, source, year, month, week, disease, age, metric) %>% 
    arrange(year, week)
  
  German_data <- rbind(German_data, FLU_GERM_MERGE)
  
  i = i + 1
}


i = 0
for (item in rsv_data_list) {
  RSV_GERM_MERGE <- item %>%
    pivot_longer(-Week,
                 names_to = "age",
                 values_to = "metric") %>%
    mutate(
      country = "Germany",
      continent = "Europe",
      disease = "RSV",
      month = NA,
      age = age,
      metric = replace_na(metric, 0),
      source = "GOV",
      week = Week,
      year = 2016 + i) %>%
    select(country, continent, source, year, month, week, disease, age, metric) %>% 
    arrange(year, week)
  
  German_data <- rbind(German_data, RSV_GERM_MERGE)
  
  i = i + 1
}

