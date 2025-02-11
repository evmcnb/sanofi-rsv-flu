# gather stringency data

# packages and libraries
install.packages("readxl")
install.packages("ggthemes")
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(zoo)

# gather data
stringency <- read_csv("csv/stringency_data.csv") %>%
  select(
    CountryName,
    Date,
    StringencyIndex_Average,
    GovernmentResponseIndex_Average,
    ContainmentHealthIndex_Average,
    EconomicSupportIndex
  ) %>%
  # remove first row
  filter(row_number() > 1) %>%
  # make year and week consistent with other data
  mutate(
    CountryName = ifelse(CountryName == "United States", "United States of America", CountryName),
    Date = ymd(Date),
    Year = year(Date),
    Week = isoweek(Date)
  ) %>%
  select(-Date)


# take averages over each week
stringency <- stringency %>%
  group_by(CountryName, Year, Week) %>%
  summarise(
    StringencyIndex_Avg = mean(StringencyIndex_Average, na.rm = TRUE),
    GovernmentResponseIndex_Avg = mean(GovernmentResponseIndex_Average, na.rm = TRUE),
    ContainmentHealthIndex_Avg = mean(ContainmentHealthIndex_Average, na.rm = TRUE),
    EconomicSupportIndex_Avg = mean(EconomicSupportIndex, na.rm = TRUE),
    .groups = "drop"
  )

# rename for consistency with main_dataset
stringency <- stringency %>%
  rename(country = CountryName, year = Year, week = Week)

# drop unwanted variables
stringency <- stringency %>%
  select(-c("GovernmentResponseIndex_Avg", "ContainmentHealthIndex_Avg", "EconomicSupportIndex_Avg"))


# create new stringency dataset with rolling average for a smoother looking graph
stringency_ra <- stringency %>%
  arrange(country, year, week) %>%  # Ensure correct ordering
  group_by(country) %>%  # Rolling average within each country
  mutate(StringencyIndex_Avg = rollmean(StringencyIndex_Avg, k = 5, fill = 0, align = "right")) %>%
  ungroup()


flu_dataset <- read_csv("csv/main_dataset.csv", 
                        col_types = cols(
                          country = col_character(),
                          source = col_character(),
                          month = col_character(),
                          disease = col_character(),
                          age = col_character(),  # not numeric in some cases
                          year = col_double(),
                          week = col_double(),
                          metric = col_double(),
                          population = col_double()
                        )) %>%
  select(country, year, week, disease, age, metric) %>%
  filter(disease == "Influenza") %>%
  filter(year %in% 2020:2022) %>% # UPDATE later to include more pre-COVID years
  select(-disease)

# collapse into weekly summaries by summing over age groups

flu_dataset <- flu_dataset %>%
  group_by(country, year, week) %>%
  summarise(cases = sum(metric, na.rm = TRUE), .groups = "drop")


# combine datasets together
full <- flu_dataset %>%
  left_join(stringency, by = c("country", "year", "week")) %>% # use rolling average if needed
  mutate(
    # Create a new Date column combining Year and Week
    Date = as.Date(paste(year, week, 1, sep = "-"), format = "%Y-%U-%u")
  )


# try on Australia
australia_data <- full %>%
  filter(country == "Australia")

# Calculate the scaling factor based on min and max values of both variables
max_first  <- max(australia_data$StringencyIndex_Avg, na.rm = TRUE)   # Max of Stringency Index
max_second <- max(australia_data$cases, na.rm = TRUE) # Max of Cases
min_first  <- min(australia_data$StringencyIndex_Avg, na.rm = TRUE)   # Min of Stringency Index
min_second <- min(australia_data$cases, na.rm = TRUE) # Min of Cases

# Define scaling and shifting for the second axis
scale = (max_second - min_second) / (max_first - min_first)
shift = min_first - min_second

# Scaling function for secondary axis
scale_function <- function(x, scale, shift) {
  return ((x) * scale - shift)
}

# Inverse scaling function for the secondary variable
inv_scale_function <- function(x, scale, shift) {
  return ((x + shift) / scale)
}

# Create the dual axis plot
ggplot(australia_data, aes(x = Date)) +
  # Line plot for Stringency Index
  geom_line(aes(y = StringencyIndex_Avg, color = "Stringency Index"), size = 1) +
  
  # Bar plot for Daily New Cases (on right y-axis)
  geom_bar(aes(y = inv_scale_function(cases, scale, shift), fill = "Daily New Cases"), stat = "identity", alpha = 0.6, position = "identity") +
  
  scale_x_date(
    date_labels = "%b %Y",  # Format the dates as "Month Year"
    date_breaks = "3 months"  # Show a tick every 3 months
  ) +
  
  scale_y_continuous(
    name = "Stringency Index",
    limits = c(min_first, max_first),  # Set y-axis for Stringency Index
    sec.axis = sec_axis(~scale_function(., scale, shift), name = "Number of Cases")  # Secondary y-axis for cases
  ) +
  
  labs(
    title = "COVID-19 Stringency and Daily New Cases in Australia (2020-2022)",
    x = "Date (2020-2022)",
    color = "",
    fill = ""
  ) +
  
  scale_color_manual(values = c("Stringency Index" = "purple")) +
  scale_fill_manual(values = c("Daily New Cases" = "orange")) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "top"
  )




# Filter for the countries of interest
countries_of_interest <- c("Australia", "Chile", "Japan", "South Africa", "United Kingdom", "United States of America")
countries_of_interest <- unique(flu_dataset$country)
# Combine the stringency data with the case data by country, year, and week
combined_data <- stringency %>%
  filter(country %in% countries_of_interest) %>%
  left_join(flu_dataset, by = c("country", "year", "week"))

# Prepare the data, create Date column from year and week
combined_data <- combined_data %>%
  mutate(Date = as.Date(paste(year, week, 1, sep = "-"), format = "%Y-%U-%u"))

# Calculate per-country max and min values for StringencyIndex and cases
max_min_values <- combined_data %>%
  group_by(country) %>%
  summarise(
    max_stringency = max(StringencyIndex_Avg, na.rm = TRUE),
    min_stringency = min(StringencyIndex_Avg, na.rm = TRUE),
    max_cases = max(cases, na.rm = TRUE),
    min_cases = min(cases, na.rm = TRUE)
  )

# Merge these max/min values back into the original data
combined_data <- combined_data %>%
  left_join(max_min_values, by = "country")

# Create the scaling functions for each country
combined_data <- combined_data %>%
  mutate(
    scale = (max_cases - min_cases) / (max_stringency - min_stringency),
    shift = min_stringency - min_cases
  )

# Define scaling function for secondary axis based on country-specific scale
scale_function <- function(x, scale, shift) {
  return ((x) * scale - shift)
}

# Inverse scaling function for the secondary variable (cases)
inv_scale_function <- function(x, scale, shift) {
  return ((x + shift) / scale)
}

# Create the dual-axis plot with a bar plot for 'cases' and line plot for 'StringencyIndex_Avg', faceting by country
string_case_plot <- ggplot(combined_data, aes(x = Date)) +
  # Line plot for Stringency Index (Now in Blue)
  geom_line(aes(y = StringencyIndex_Avg, color = "Stringency Index"), size = 1.2) +
  
  # Bar plot for Weekly New Cases (Now in Soft Gray)
  geom_bar(aes(y = inv_scale_function(cases, scale, shift), fill = "Weekly New Cases"), 
           stat = "identity", alpha = 0.5, position = "identity") +
  
  scale_x_date(
    date_labels = "%b %Y",  # Display Month and Year (e.g., Mar 2020)
    date_breaks = "6 months"  # Show a tick every 4 months
  ) +
  
  scale_y_continuous(
    name = "Stringency Index",
    limits = c(min(combined_data$min_stringency), max(combined_data$max_stringency)),  
    sec.axis = sec_axis(~scale_function(., combined_data$scale[1], combined_data$shift[1]), 
                        name = "Number of Cases")  
  ) +
  
  labs(
    title = "Stringency Index and Weekly Influenza Cases (2020-2022)",
    x = NULL,
    color = "",
    fill = ""
  ) +
  
  # New color scheme
  scale_color_manual(values = c("Stringency Index" = "#1f78b4")) +  # Blue for Stringency Index
  scale_fill_manual(values = c("Weekly New Cases" = "#E69F00")) +  # Improved orange for cases
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),  # Rotate and enlarge x-axis labels
    axis.title.y = element_text(size = 12),  # Enlarge y-axis title for readability
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 11)  # Slightly enlarge legend text for clarity
  ) +
  
  # Facet grid by country with free y-axis scaling for cases
  facet_wrap(~country, scales = "free_y")  



ggsave(
  filename = paste0("plots/Influenza/Other/stringency_cases.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = string_case_plot,  # This refers to the last plot generated
  width = 8,  # Width of the plot (in inches)
  height = 6,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)


# plot stringency vs shift

ccf_shift_data <- function(data, countries, hemisphere, baseline_year = 2019, comp_years) {
  
  # Initialize list to store results
  lag_results <- list()
  
  for (country in countries) {
    
    # Retrieve selected country data
    country_data <- data %>%
      filter(country == !!country) %>%
      select(epi_year, epi_week, cases)
    
    # Fill in missing weeks (ensures all 52 weeks are present)
    country_data_full <- country_data %>%
      complete(epi_year, epi_week = 1:52, fill = list(cases = 0))
    
    # Ensure 'cases' is numeric
    country_data_full$cases <- as.numeric(country_data_full$cases)
    
    # Reshape data to wide format
    country_data_wide <- country_data_full %>%
      pivot_wider(names_from = epi_year, values_from = cases, values_fill = list(cases = 0))
    
    # Ensure the baseline year is available
    if (!as.character(baseline_year) %in% colnames(country_data_wide)) {
      message(paste("Skipping", country, "- Baseline year", baseline_year, "data not found"))
      next
    }
    
    # Add the baseline year to comparison years for plotting
    years_to_analyse <- unique(c(baseline_year, comp_years))
    
    # Loop through each comparison year
    for (comp_year in years_to_analyse) {
      
      if (!as.character(comp_year) %in% colnames(country_data_wide)) {
        message(paste("Skipping", country, "-", comp_year, "data not found"))
        next
      }
      
      # Compute cross-correlation function (CCF)
      ccf_result <- ccf(country_data_wide[[as.character(comp_year)]],
                        country_data_wide[[as.character(baseline_year)]],
                        lag.max = 20, plot = FALSE)
      
      # Find the lag with the highest correlation (best lag)
      best_lag <- ccf_result$lag[which.max(ccf_result$acf)]  # Find lag with highest correlation
      
      # Store results in a dataframe
      lag_results[[paste(country, comp_year, sep = "_")]] <- data.frame(
        country = country,
        year = comp_year,
        shift = best_lag
      )
    }
  }
  
  # Combine all lag results into a single dataframe
  lag_data <- do.call(rbind, lag_results)
  
  # Return the final dataframe with lag results
  return(lag_data)
}


southern_hemisphere <- c(
  "Argentina", "Australia", "Bolivia", "Botswana", "Brazil", "Chile", "Colombia",
  "Ecuador", "Eswatini", "Fiji", "Lesotho", "Madagascar", "Malawi", "Mauritius",
  "Mozambique", "Namibia", "New Zealand", "Paraguay", "Peru", "Papua New Guinea",
  "Rwanda", "Samoa", "Solomon Islands", "South Africa", "Tanzania", "Timor-Leste",
  "Tonga", "Uruguay", "Vanuatu", "Zambia", "Zimbabwe"
)
countries_rep <- c("Australia", "Chile", 
                    "Japan", "South Africa",
                    "United Kingdom", "United States of America")
countries_rep <- unique(flu_dataset$country)
hemisphere_rep <- setNames(
  ifelse(countries_rep %in% southern_hemisphere, "S", "N"), 
  countries_rep
)

# obtain lag data
lag_data <- ccf_shift_data(flu_dataset, countries_rep, hemisphere_rep, comp_years = 2022:2024)



# calculate average stringency for each country (2020-2022)
avg_stringency <- stringency %>%
  filter(year %in% 2020:2022) %>%
  group_by(country) %>%
  summarise(avg_stringency = mean(StringencyIndex_Avg, na.rm = TRUE))

# calculate average shift in seasonality for post-2021 (from lag_data)
avg_shift <- lag_data %>%
  filter(year > 2021) %>%
  group_by(country) %>%
  summarise(avg_shift = mean(shift, na.rm = TRUE))

# Step 3: Merge the average stringency data with the average shift data
merged_data_clean <- na.omit(merged_data[, c("avg_stringency", "avg_shift")])

# Calculate the correlation on complete data
correlation <- cor(merged_data_clean$avg_stringency, merged_data_clean$avg_shift)

# Step 4: Create the scatterplot
correlation <- cor(merged_data$avg_stringency, merged_data$avg_shift, na.rm=TRUE)

ggplot(merged_data, aes(x = avg_stringency, y = avg_shift, color = country)) +
  geom_point(size = 3, shape = 16, alpha = 0.7) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid", size = 1) +  # Add line of best fit
  labs(
    title = "Average Weekly Stringency vs. Average Seasonality Shift (Post-2021)",
    x = "Average Stringency (2020-2022)",
    y = "Average Shift in Seasonality (Post-2021)",
    caption = paste("Source: FLUNET/GOV | Correlation: ", round(correlation, 2))  # Add correlation to the caption
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",  # Remove the legend
    plot.title = element_text(hjust = 0.5, size = 14),  # Center title and increase size
    plot.caption = element_text(hjust = 0, size = 8),  # Align caption left and reduce size
    plot.margin = margin(10, 10, 10, 10)  # Reduce margin space
  ) +
  coord_cartesian(xlim = c(min(merged_data$avg_stringency) - 5, max(merged_data$avg_stringency) + 5), 
                  ylim = c(min(merged_data$avg_shift) - 2, max(merged_data$avg_shift) + 2))  # Adjust axis limits to fit data better

