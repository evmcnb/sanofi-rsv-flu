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
