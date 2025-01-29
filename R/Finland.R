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
head(cases_201019)
head(cases_202025)

Finland_data <- rbind(cases_201019, cases_202025)

rm(list = c("cases_201019", "cases_202025"))
