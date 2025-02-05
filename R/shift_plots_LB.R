# Post-COVID shift plots

# set working directory
setwd("C:\\Users\\lukeb\\Downloads\\LSHTM\\TERM 2\\Data Challenge\\github\\sanofi-rsv-flu")

# packages and libraries
install.packages("readxl")
install.packages("ggthemes")
install.packages("WaveletComp")
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(zoo) # library for rolling average
library(WaveletComp) # library for wavelet transform analysis
library(boot) # For bootstrap resampling

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
    
    # skip countries not in the dataset
    if (!(country %in% unique(data$country))) {
      message(paste("Skipping", country, "- not in dataset"))
      next
    }
    
    country_hemisphere <- hemisphere[[country]]
    
    # retrieve the country information and shift weeks based on hemisphere
    country_data <- data %>%
      filter(country == !!country) %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "N" ~ (week + 26) %% 52,  # Shift by 26 weeks if North
        TRUE ~ week  # Keep same if South
      )) %>%
      arrange(year, epi_week)
    
    # identify years with exactly 52 weeks
    valid_years <- country_data %>%
      group_by(year) %>%
      summarise(week_count = n()) %>%
      filter(week_count == 52) %>%
      pull(year)
    
    # check 2019 has 52 weeks
    if (!(2019 %in% valid_years)) {
      message(paste("Skipping", country, "- 2019 does not have 52 weeks"))
      next
    }
    
    # filter to only include valid years
    country_data <- country_data %>%
      filter(year %in% valid_years)
    
    # convert into wide format for correlation analysis
    country_data_wide <- country_data %>% tidyr::pivot_wider(names_from = year, values_from = cases)
    
    # identify post-2021 years for comparison
    years_to_analyse <- valid_years[valid_years > 2021]
    
    # perform time lag correlation for each year - set plot = TRUE to see the ccf plot
    for (comp_year in years_to_analyse) {
      
      if (!(as.character(comp_year) %in% colnames(country_data_wide))) {
        message(paste("Skipping", country, comp_year, "- data missing"))
        next
      }
      
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
    geom_line(size = 1, na.rm = TRUE) +
    geom_point(size = 3, na.rm = TRUE) +
    labs(title = "Estimated Seasonality Shift in Weeks (Compared to 2019)",
         x = "Year",
         y = "Shift in Weeks") +
    theme_fivethirtyeight() +  # Apply theme
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right",
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


# new approach which should handle different length years better
plot_seasonality_shift1 <- function(data, countries, hemisphere) {
  
  # initialize lag results
  lag_results <- list()
  
  # run through each country in 'countries'
  for (country in countries) {
    
    country_hemisphere <- hemisphere[[country]]
    
    # retrieve the country information and shift weeks based on hemisphere
    country_data <- data %>%
      filter(country == !!country) %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "N" ~ (week + 26) %% 52,  # Shift by 26 weeks if North
        TRUE ~ week  # Keep same if South
      )) %>%
      arrange(year, epi_week)
    
    # Ensure data is numeric and remove NAs for 'cases' column
    country_data <- country_data %>%
      mutate(cases = as.numeric(cases)) %>%
      drop_na(cases)  # Drop rows where cases are NA
    
    # Check if 2019 exists and has complete data for the required weeks
    if (!2019 %in% country_data$year) next
    
    # Convert into wide format for correlation analysis
    country_data_wide <- country_data %>%
      tidyr::pivot_wider(names_from = year, values_from = cases)
    
    # Ensure that the 'year' column exists after pivot_wider
    if (!"2019" %in% colnames(country_data_wide)) next
    
    # Get valid years for the analysis (i.e., years with data)
    valid_years <- unique(country_data$year)
    
    # Perform time lag correlation for each year
    for (comp_year in valid_years[valid_years > 2021]) {
      
      # Ensure numeric and remove NAs from both columns
      ref_2019 <- as.numeric(country_data_wide$`2019`)
      comp_year_data <- as.numeric(country_data_wide[[as.character(comp_year)]])
      
      # Skip if data is missing or not usable
      if (length(ref_2019) == 0 | length(comp_year_data) == 0) next
      
      # Compute cross-correlation function (ccf) - ignore NA values
      ccf_result <- tryCatch({
        ccf(ref_2019, comp_year_data, lag.max = 20, plot = FALSE)
      }, error = function(e) {
        # If ccf fails (due to missing data or any other issue), skip the country-year
        return(NULL)
      })
      
      # If ccf failed (e.g., due to NA values), skip this iteration
      if (is.null(ccf_result)) next
      
      # Find the best lag (highest correlation)
      best_lag <- ccf_result$lag[which.max(ccf_result$acf)]
      
      # Store results
      lag_results[[paste(country, comp_year, sep = "_")]] <- data.frame(
        country = country,
        year = as.numeric(comp_year),
        shift = best_lag
      )
    }
  }
  
  # Combine all results into a single dataframe
  lag_data <- do.call(rbind, lag_results)
  
  # Plot the results
  ggplot(lag_data, aes(x = year, y = shift, color = country, group = country)) +
    geom_line(size = 1, na.rm = TRUE) +
    geom_point(size = 3, na.rm = TRUE) +
    labs(title = "Estimated Seasonality Shift in Weeks (Compared to 2019)",
         x = "Year",
         y = "Shift in Weeks") +
    theme_fivethirtyeight() +  # Apply theme
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right",
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

# more systematic approach
chosen_countries <- unique(flu_dataset$country)
southern_hemisphere <- c(
  "Argentina", "Australia", "Bolivia", "Botswana", "Brazil", "Chile", "Colombia",
  "Ecuador", "Eswatini", "Fiji", "Lesotho", "Madagascar", "Malawi", "Mauritius",
  "Mozambique", "Namibia", "New Zealand", "Paraguay", "Peru", "Papua New Guinea",
  "Rwanda", "Samoa", "Solomon Islands", "South Africa", "Tanzania", "Timor-Leste",
  "Tonga", "Uruguay", "Vanuatu", "Zambia", "Zimbabwe"
)
hemisphere_info <- setNames(
  ifelse(chosen_countries %in% southern_hemisphere, "S", "N"), 
  chosen_countries
)

# produce the plot
plot_seasonality_shift(flu_dataset, chosen_countries, hemisphere_info)

# produce the plot using (hopefully) more robust method
plot_seasonality_shift1(flu_dataset, chosen_countries, hemisphere_info)

# select countries for report
countries_rep <- c("Argentina", "Australia", "Denmark", "France", "Hong Kong",
                   "Ireland", "Taiwan", "United Kingdom", "United States of America")
hemisphere_rep <- setNames(
  ifelse(countries_rep %in% southern_hemisphere, "S", "N"), 
  countries_rep
)
plot_seasonality_shift1(flu_dataset, countries_rep, hemisphere_rep)


# Week shift using rolling average -----------------------------------------------



# define the function to compute and plot seasonality shifts using rolling averages
plot_shift_rolling_average <- function(data, countries, hemisphere, window_size = 3) {
  
  # initialise peak shift results
  peak_results <- list()
  
  # iterate through each country in 'countries'
  for (country in countries) {
    
    # skip countries not in the dataset
    if (!(country %in% unique(data$country))) {
      message(paste("Skipping", country, "- not in dataset"))
      next
    }
    
    # retrieve the country information and shift weeks based on hemisphere
    country_data <- data %>%
      filter(country == !!country) %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "N" ~ (week + 26) %% 52,  # Shift by 26 weeks if North
        TRUE ~ week  # Keep same if South
      )) %>%
      arrange(year, epi_week)
    
    # compute rolling average for smoothing
    data_smoothed <- country_data %>%
      group_by(year) %>%
      arrange(epi_week) %>%
      mutate(rolling_cases = rollmean(cases, k = window_size, fill = NA, align = "center")) %>%
      ungroup()
    
    # function to find the peak week for a given year
    find_peak_week <- function(data, year) {
      peak_week <- data %>%
        filter(year == !!year) %>%
        filter(rolling_cases == max(rolling_cases, na.rm = TRUE)) %>%
        pull(week) %>%
        unique()
      
      if (length(peak_week) == 0) {
        return(NA)  # Return NA if no valid peak found
      } else {
        return(min(peak_week))  # Take the first peak if multiple
      }
    }
    
    # identify all valid years in the dataset
    valid_years <- unique(data_smoothed$year)
    
    # ensure 2019 is in dataset
    if (!(2019 %in% valid_years)) {
      message(paste("Skipping", country, "- 2019 data missing"))
      next
    }
    
    # compute the peak week for 2019
    peak_2019 <- find_peak_week(data_smoothed, 2019)
    
    if (is.na(peak_2019)) {
      message(paste("Skipping", country, "- No valid peak in 2019"))
      next
    }
    
    # identify post-2021 years for comparison
    years_to_analyse <- valid_years[valid_years > 2021]
    
    # compute peak shifts for each year
    for (comp_year in years_to_analyse) {
      peak_comp <- find_peak_week(data_smoothed, comp_year)
      
      if (!is.na(peak_comp)) {
        peak_shift <- peak_comp - peak_2019
        
        # store results
        peak_results[[paste(country, comp_year, sep = "_")]] <- data.frame(
          country = country,
          year = comp_year,
          shift = peak_shift
        )
      }
    }
  }
  
  # combine all results into a single dataframe
  peak_data <- do.call(rbind, peak_results)
  
  # plot the results
  ggplot(peak_data, aes(x = year, y = shift, color = country, group = country)) +
    geom_line(size = 1, na.rm = TRUE) +
    geom_point(size = 3, na.rm = TRUE) +
    labs(title = "Estimated Seasonality Shift in Weeks (Compared to 2019)",
         x = "Year",
         y = "Shift in Weeks") +
    theme_fivethirtyeight() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right",
      axis.ticks.y = element_line(),
      axis.line.y.left = element_line(),
      legend.title = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8),
      axis.text.y = element_text()
    ) +
    scale_x_continuous(breaks = unique(peak_data$year)) + 
    ylim(-20, 20)  
}

plot_shift_rolling_average(flu_dataset, chosen_countries, hemisphere_info, window_size=5)

# plotting select countries for report

plot_shift_rolling_average(flu_dataset, countries_rep, hemisphere_rep)


# Week shift using bootstrapping -----------------------------------------------


# Function to find the peak week for a given year
find_peak_week <- function(data, year) {
  peak_week <- data %>%
    filter(year == !!year) %>%
    filter(cases == max(cases, na.rm = TRUE)) %>%
    pull(epi_week)
  return(peak_week[1])  # Ensure we take the first peak in case of ties
}

# Bootstrap function to estimate shift and confidence interval
bootstrap_shift <- function(data, indices, peak_2019) {
  sampled_data <- data[indices, ]  # Bootstrap sample
  peak_sample <- find_peak_week(sampled_data, sampled_data$year[1])  # Find peak for sample
  shift <- peak_sample - peak_2019  # Calculate shift compared to 2019
  return(shift)
}

# calculate peak shifts and perform bootstrap sampling with 90% confidence intervals
plot_shift_bootstrap <- function(data, countries, hemisphere) {
  
  peak_results <- list()
  
  # loop through each country
  for (country in countries) {
    
    # check it exists in the dataset
    if (!(country %in% unique(data$country))) {
      message(paste("Skipping", country, "- not in dataset"))
      next
    }
    
    # retrieve country data and shift week based on hemisphere
    country_data <- data %>%
      filter(country == !!country) %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "N" ~ (week + 26) %% 52,  # Shift by 26 weeks if North
        TRUE ~ week  # Keep same if South
      )) %>%
      arrange(year, epi_week)
    
    # Find peak week for 2019 (the baseline year)
    peak_2019 <- find_peak_week(country_data, 2019)
    
    # Skip if peak for 2019 is not found
    if (is.na(peak_2019)) {
      message(paste("Skipping", country, "- 2019 peak not found"))
      next
    }
    
    # Filter data for years after 2021
    years_to_analyse <- unique(country_data$year[country_data$year > 2021])
    
    # Prepare an empty data frame to store peak shift results
    country_peak_shifts <- data.frame(country = character(), year = integer(), shift = numeric(), 
                                      lower_ci = numeric(), upper_ci = numeric())
    
    # Perform bootstrap for each year after 2021
    for (year in years_to_analyse) {
      
      # Filter data for the specific year
      year_data <- country_data %>%
        filter(year == !!year)
      
      # Check if there's enough data to perform bootstrapping (at least 30 data points)
      if (nrow(year_data) < 30) {
        message(paste("Skipping", country, year, "- not enough data for bootstrapping"))
        next
      }
      
      # Bootstrap sampling
      set.seed(123)  # For reproducibility
      bootstrap_results <- boot(data = year_data, statistic = bootstrap_shift, R = 1000, 
                                peak_2019 = peak_2019)
      
      # Calculate 90% confidence interval for the shift
      ci <- boot.ci(bootstrap_results, type = "perc", conf = 0.90)$percent[4:5]
      
      # Store the results for the country and year
      country_peak_shifts <- rbind(country_peak_shifts, 
                                   data.frame(country = country, year = year, 
                                              shift = mean(bootstrap_results$t), 
                                              lower_ci = ci[1], upper_ci = ci[2]))
    }
    
    # Append country results to the list
    peak_results[[country]] <- country_peak_shifts
  }
  
  # Combine all results into a single dataframe
  peak_results_df <- do.call(rbind, peak_results)
  
  # Plot the results with confidence intervals
  ggplot(peak_results_df, aes(x = year, y = shift, color = country, group = country)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
    labs(title = "Peak Week Shift with 90% Confidence Intervals",
         x = "Year", y = "Peak Week Shift (Compared to 2019)") +
    theme_fivethirtyeight() +  # Apply theme
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right",   # Positioning of the legend
      axis.ticks.y = element_line(),
      axis.line.y.left = element_line(),
      legend.title = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8),
      axis.text.y = element_text()
    ) +
    scale_x_continuous(breaks = unique(peak_results_df$year)) +  # integer years on x-axis
    ylim(-20, 20)  # set y-axis limit
}



plot_shift_bootstrap(flu_dataset, countries_rep, hemisphere_rep)


# Week shift using wavelet transform analysis -----------------------------------


# Struggling to get this to work

plot_seasonality_shift_wavelet <- function(data, countries, hemisphere) {
  
  shift_results <- list()
  
  for (country in countries) {
    
    if (!(country %in% unique(data$country))) {
      message(paste("Skipping", country, "- not in dataset"))
      next
    }
    
    country_data <- data %>%
      filter(country == !!country) %>%
      arrange(year, week)
    
    # Normalize weeks based on hemisphere
    country_data <- country_data %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "N" ~ (week + 26) %% 52,
        TRUE ~ week
      ))
    
    if (!2019 %in% unique(country_data$year)) {
      message(paste("Skipping", country, "- missing 2019 data"))
      next
    }
    
    years_to_analyse <- unique(country_data$year[country_data$year > 2021 & country_data$year < 2025])
    
    # Convert data into time series format and interpolate missing values
    country_data_ts <- country_data %>%
      group_by(year) %>%
      arrange(epi_week) %>%
      mutate(
        time = row_number(),
        cases = na.fill(cases, "extend")  # More robust interpolation
      ) %>%
      ungroup()
    
    # Function to apply wavelet transform & detect peak week
    find_peak_wavelet <- function(year_data) {
      suppressMessages({
        wavelet_result <- analyze.wavelet(
          data.frame(time = year_data$time, cases = year_data$cases),
          loess.span = 0, dt = 1, dj = 1/12, 
          lowerPeriod = 20, upperPeriod = 60, make.pval = FALSE  # Seasonal periods
        )
      })
      
      # Find the closest period to **52 weeks**
      period_index <- which.min(abs(wavelet_result$Period - 52))
      
      # Validate period_index
      if (length(period_index) == 0 || period_index < 1 || period_index > length(wavelet_result$Period)) {
        message(paste("Skipping", country, year_data$year[1], "- no valid period index"))
        return(NA)
      }
      
      # Extract the power spectrum safely
      if (is.null(dim(wavelet_result$Power.avg))) {
        message(paste("Skipping", country, year_data$year[1], "- Power.avg is not a matrix"))
        return(NA)
      }
      
      power_spectrum <- wavelet_result$Power.avg[period_index, , drop = FALSE]  # Ensure matrix format
      
      # Identify the **peak week** where power is highest
      peak_week_index <- which.max(power_spectrum)
      
      # Ensure valid index
      if (length(peak_week_index) == 0 || peak_week_index > nrow(year_data)) {
        message(paste("Skipping", country, year_data$year[1], "- peak detection failed"))
        return(NA)
      }
      
      peak_week <- year_data$week[peak_week_index]
      
      return(peak_week)
    }
    
    # Find peak for 2019
    peak_2019 <- find_peak_wavelet(country_data_ts %>% filter(year == 2019))
    
    if (is.na(peak_2019)) {
      message(paste("Skipping", country, "- 2019 peak not found"))
      next
    }
    
    for (comp_year in years_to_analyse) {
      
      peak_comp_year <- find_peak_wavelet(country_data_ts %>% filter(year == comp_year))
      
      if (is.na(peak_comp_year)) {
        message(paste("Skipping", country, comp_year, "- peak not found"))
        next
      }
      
      shift_value <- peak_comp_year - peak_2019
      
      shift_results[[paste(country, comp_year, sep = "_")]] <- data.frame(
        country = country,
        year = comp_year,
        shift = shift_value
      )
    }
  }
  
  # Only plot if results exist
  if (length(shift_results) > 0) {
    shift_data <- do.call(rbind, shift_results)
    
    ggplot(shift_data, aes(x = year, y = shift, color = country, group = country)) +
      geom_line(size = 1, na.rm = TRUE) +
      geom_point(size = 3, na.rm = TRUE) +
      labs(title = "Estimated Seasonality Shift (Wavelet Transform)",
           x = "Year",
           y = "Shift in Weeks") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "right",
        axis.ticks.y = element_line(),
        axis.line.y.left = element_line(),
        legend.title = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text()
      ) +
      scale_x_continuous(breaks = unique(shift_data$year)) +
      ylim(-20, 20)
  } else {
    message("No valid data to plot.")
  }
}s


plot_seasonality_shift_wavelet(flu_dataset, countries_rep, hemisphere_rep)
