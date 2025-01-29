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

Fin_flu <- Finland_data[Finland_data$Group=="Influenza",]
Fin_rsv <- Finland_data[Finland_data$Group=="RSV",]

# libraries for plotting
library(ggplot2) ; library(lubridate) ; library(dplyr)

# convert month and date to combined format
Finland_data <- Finland_data |> mutate(Date = dmy(paste("01", Month, Year)))

# plot the data
ggplot(Finland_data, aes(x = Date, y = Cases, color = Group)) +
  geom_line() + 
  labs(title = "Seasonality of RSV and Influenza in Finland 2010-2025",
       x = "Date",
       y = "Cases") +
  theme_minimal()

# work on difference in peaks. adjust data to allow for peaks being on either side of NYE
Finland_data <- Finland_data |> mutate(Year = as.numeric(Year)) |>
  mutate(Epi_year = ifelse(month(Date) >= 7, Year, Year-1))

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

# Plot difference over epidemic seasons
ggplot(peak_diff, aes(x = Epi_year, y = Difference)) +
  geom_line() + 
  geom_point(size = 3, color = "blue") +
  labs(title = "Timing Difference Between Influenza and RSV Peaks (Epidemic Seasons)",
       x = "Epidemic Season",
       y = "Days Between Peaks") +
  theme_minimal()


# Merge with original data for visualization
plot_data <- Finland_data %>% select(Date, Cases, Group) 

# Create combined plot
ggplot() +
  # Main time series plot for RSV and Influenza
  geom_line(data = plot_data, aes(x = Date, y = Cases, color = Group), linewidth = 1) +
  
  # Line plot for peak difference with secondary axis
  geom_line(data = peak_diff, aes(x = as.Date(paste0(Epi_year, "-01-01")), y = Difference * 10), 
            color = "black", linewidth = 1) +
  
  scale_y_continuous(
    name = "Cases",
    sec.axis = sec_axis(~ . / 10, name = "Days Between Peaks")  # Adjust scale for visibility
  ) +
  # Horizontal dashed line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  
  labs(title = "Seasonality of RSV and Influenza with Peak Timing Difference",
       x = "Date",
       color = "Disease") +
  theme_minimal()

