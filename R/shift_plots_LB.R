# Post-COVID shift plots

# set working directory
setwd("C:\\Users\\lukeb\\Downloads\\LSHTM\\TERM 2\\Data Challenge\\github\\sanofi-rsv-flu")

# packages and libraries
install.packages("readxl")
install.packages("ggthemes")
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggplot2)

# retrieve the main dataset with filters on years and disease

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
  filter(year > 2018) %>% # UPDATE later to include more pre-COVID years
  filter(year < 2025) %>% # avoid incomplete year issue
  filter(week < 53) %>% # avoid issue of 52 vs 53 weeks - use imputing instead as week 53 may have cases!
  select(-disease)

# collapse into weekly summaries by summing over age groups

flu_dataset <- flu_dataset %>%
  group_by(country, year, week) %>%
  summarise(cases = sum(metric, na.rm = TRUE), .groups = "drop")


# create a function to plot seasonality shift for given data, countries, and hemispheres

plot_seasonality_shift <- function(data, countries, hemisphere) {
  
  # initialise lag results
  lag_results <- list()
  
  # run through each country in 'countries'
  for (country in countries) {
    
    country_hemisphere <- hemisphere[[country]]
    
    # retrieve the country information and shift weeks based on hemisphere
    country_data <- data %>%
      filter(country == !!country) %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "North" ~ (week + 26) %% 52,  # Shift by 26 weeks if North
        TRUE ~ week  # Keep same if South
      )) %>%
      arrange(year, epi_week)
    
    # convert into wide format for correlation analysis
    country_data_wide <- country_data %>% tidyr::pivot_wider(names_from = year, values_from = cases)
    
    # identify post-2021 years for comparison
    years_to_analyse <- unique(country_data$year[country_data$year > 2021])
    
    # perform time lag correlation for each year - set plot = TRUE to see the ccf plot
    for (comp_year in years_to_analyse) {
      ccf_result <- ccf(country_data_wide$`2019`, country_data_wide[[as.character(comp_year)]], lag.max = 20) # cap the shift at 20wks either side
      best_lag <- ccf_result$lag[which.max(ccf_result$acf)] # find which lag gives highest correlation
      
      # store results into a data frame
      lag_results[[paste(country, comp_year, sep = "_")]] <- data.frame(
        country = country,
        year = comp_year,
        shift = best_lag
      )
    }
  }
  
  # combine all results into a single dataframe
  lag_data <- do.call(rbind, lag_results)
  
  # plot the results
  ggplot(lag_data, aes(x = year, y = shift, color = country, group = country)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = "Estimated Seasonality Shift in Weeks (Compared to 2019)",
         x = "Year",
         y = "Shift in Weeks") +
    theme_fivethirtyeight() +  # Apply theme
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      axis.ticks.y = element_line(),
      axis.line.y.left = element_line(),
      legend.title = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8),
      axis.text.y = element_text()
    ) +
    scale_x_continuous(breaks = unique(lag_data$year)) +  # integer years on x-axis
    ylim(-20, 20)  # set y-axis limit
}


chosen_countries <- c(
  "Argentina",   
  "Australia",   
  "France",     
  "Hong Kong",  
  "Ireland",    
  "UK",         
  "USA"         
)
hemisphere_info <- c(
  "Argentina" = "South", 
  "Australia" = "South", 
  "France" = "North", 
  "Hong Kong" = "North",
  "Ireland" = "North", 
  "UK" = "North",
  "USA" = "North"
)

plot_seasonality_shift(flu_dataset, chosen_countries, hemisphere_info)

# Denmark has severe missing values in 2024
# UK and USA don't show up
# Hong Kong has extreme shift
# Issues with Taiwan - only goes up to 48 weeks with some missing values

tw <- flu_dataset[flu_dataset$country == "Taiwan",]
summary(tw)
sum(is.na(tw$cases))
range(tw$week)
ggplot(tw, aes(x = week, y = cases, color = year, group = year)) +
  geom_line() +
  geom_point() +
  labs(title = "Taiwan Data", x = "Week", y = "Cases")
