# need this for separate
library(tidyr)

# split Time column accordingly
cases_201019 <- read.csv("csv/Finland/fact_ttr_cases 2010 -2019.csv", sep = ";") |>
  separate(Time, into = c("Month", "Year"), sep = " ")
cases_202025 <- read.csv("csv/Finland/fact_ttr_cases 2020-2025.csv", sep = ";") |>
  separate(Time, into = c("Month", "Year"), sep = " ")

colnames(cases_201019) <- c("Month", "Year", "Group", "Cases")
colnames(cases_202025) <- c("Month", "Year", "Group", "Cases")

# remove the yearly summaries
cases_201019 <- cases_201019[cases_201019$Month != "Year", ]
cases_202025 <- cases_202025[cases_202025$Month != "Year", ]
Finland_data <- rbind(cases_201019, cases_202025)
rm(list = c("cases_201019", "cases_202025"))

Fin_flu <- Finland_data[Finland_data$Group == "Influenza", ]
Fin_rsv <- Finland_data[Finland_data$Group == "RSV", ]

# libraries for plotting
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)

# convert month and date to combined format
Finland_data <- Finland_data |> mutate(Date = dmy(paste("01", Month, Year)))

# plot the data
ggplot(Finland_data, aes(x = Date, y = Cases, color = Group)) +
  geom_line() +
  labs(title = "Seasonality of RSV and Influenza in Finland 2010-2025", x = "Date", y = "Cases") +
  scale_x_date(breaks = seq(min(Finland_data$Date), max(Finland_data$Date), by = "2 year"),
               labels = date_format("%Y")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# work on difference in peaks. adjust data to allow for peaks being on either side of NYE
Finland_data <- Finland_data |> mutate(Year = as.numeric(Year)) |>
  mutate(Epi_year = ifelse(month(Date) >= 7, Year, Year - 1))

# use function to find peak
# NB: this data only has months and would benefit from a granular, weekly division
get_peak_month <- function(data, disease) {
  data %>%
    filter(Group == disease, month(Date) %in% c(9:12, 1:5)) %>%  # Focus on Sept-May
    group_by(Epi_year) %>%
    slice_max(Cases, n = 1, with_ties = FALSE) %>%  # Find peak within season
    select(Epi_year, Date) %>%
    rename(!!paste0(disease, "_Peak") := Date)  # Rename for clarity
}

# get peak months for each disease
flu_peaks <- get_peak_month(Finland_data, "Influenza")
rsv_peaks <- get_peak_month(Finland_data, "RSV")

# REWRITE THIS
# Merge peak data
peak_diff <- inner_join(flu_peaks, rsv_peaks, by = "Epi_year") %>%
  mutate(Difference = as.numeric(difftime(Influenza_Peak, RSV_Peak, units = "days")))  # Compute difference in days

# Plot difference over epidemic seasons - issue in 2020 from lack of data/cases
# improve plot to indicate direction (+/-) clearly
ggplot(peak_diff, aes(x = Epi_year, y = Difference)) +
  geom_line() +
  geom_point(size = 3, color = "blue") +
  labs(title = "Difference in RSV and Influenza Peaks by Season", x = "Epidemic season", y = "Days between peaks") +
  theme_minimal()
