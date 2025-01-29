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
