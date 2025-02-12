# Post-COVID shift plots

# set working directory
setwd("C:\\Users\\lukeb\\Downloads\\LSHTM\\TERM 2\\Data Challenge\\github\\sanofi-rsv-flu")

# packages and libraries
install.packages("readxl")
install.packages("ggthemes")
install.packages("WaveletComp")
install.packages("plotly")
install.packages("gt")
install.packages("gtExtras")
install.packages("rlang")
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(zoo) # library for rolling average
library(WaveletComp) # library for wavelet transform analysis
library(boot) # For bootstrap resampling
library(plotly)
library(gt)
library(gtExtras)
library(kableExtra)
library(scales)

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

ccf_shift_data <- function(data, countries, hemisphere) {
  
  # Initialize list to store results
  lag_results <- list()
  
  for (country in countries) {
    
    # Skip if country not in dataset
    if (!(country %in% unique(data$country))) {
      message(paste("Skipping", country, "- not in dataset"))
      next
    }
    
    # Retrieve hemisphere and shift weeks if necessary
    country_data <- data %>%
      filter(country == !!country) %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "N" ~ (week + 26) %% 52,  # Shift by 26 weeks if North
        TRUE ~ week  # Keep same if South
      )) %>%
      arrange(year, epi_week)
    
    # Identify years with exactly 52 weeks
    valid_years <- country_data %>%
      group_by(year) %>%
      summarise(week_count = n()) %>%
      filter(week_count == 52) %>%
      pull(year)
    
    # Ensure 2019 is present
    if (!(2019 %in% valid_years)) {
      message(paste("Skipping", country, "- 2019 does not have 52 weeks"))
      next
    }
    
    # Filter to only valid years
    country_data <- country_data %>%
      filter(year %in% valid_years)
    
    # Convert into wide format for correlation analysis
    country_data_wide <- country_data %>% tidyr::pivot_wider(names_from = year, values_from = cases)
    
    # Identify post-2021 years for comparison
    years_to_analyse <- valid_years[valid_years > 2021]
    
    # Perform time lag correlation
    for (comp_year in years_to_analyse) {
      
      if (!(as.character(comp_year) %in% colnames(country_data_wide))) {
        message(paste("Skipping", country, comp_year, "- data missing"))
        next
      }
      
      ccf_result <- ccf(country_data_wide$`2019`, country_data_wide[[as.character(comp_year)]], lag.max = 20, plot = FALSE) 
      best_lag <- ccf_result$lag[which.max(ccf_result$acf)]  # Find lag with highest correlation
      
      # Store results
      lag_results[[paste(country, comp_year, sep = "_")]] <- data.frame(
        country = country,
        year = comp_year,
        shift = best_lag
      )
    }
  }
  
  # Combine results into a dataframe
  lag_data <- do.call(rbind, lag_results)
  return(lag_data)
}

plot_seasonality_shift <- function(lag_data) {
  ggplot(lag_data, aes(x = year, y = shift, color = country, group = country)) +
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
    scale_x_continuous(breaks = unique(lag_data$year)) +  
    ylim(-20, 20)  
}

# alternative method
ccf_shift_data1 <- function(data, countries, hemisphere) {
  
  # Initialize list to store results
  lag_results <- list()
  
  for (country in countries) {
    
    country_hemisphere <- hemisphere[[country]]
    
    # Retrieve country data and adjust weeks based on hemisphere
    country_data <- data %>%
      filter(country == !!country) %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "N" ~ (week + 26) %% 52,  
        TRUE ~ week  
      )) %>%
      arrange(year, epi_week)
    
    # Ensure 'cases' column is numeric and remove NA values
    country_data <- country_data %>%
      mutate(cases = as.numeric(cases)) %>%
      drop_na(cases)
    
    # Check if 2019 exists in data
    if (!2019 %in% country_data$year) next
    
    # Convert into wide format for correlation analysis
    country_data_wide <- country_data %>%
      tidyr::pivot_wider(names_from = year, values_from = cases)
    
    # Ensure 2019 column exists
    if (!"2019" %in% colnames(country_data_wide)) next
    
    # Get valid years for analysis
    valid_years <- unique(country_data$year)
    
    # Perform time lag correlation for each post-2021 year
    for (comp_year in valid_years[valid_years > 2021]) {
      
      ref_2019 <- as.numeric(country_data_wide$`2019`)
      comp_year_data <- as.numeric(country_data_wide[[as.character(comp_year)]])
      
      # Skip if data is missing
      if (length(ref_2019) == 0 | length(comp_year_data) == 0) next
      
      # Compute cross-correlation function (ccf), handling errors
      ccf_result <- tryCatch({
        ccf(ref_2019, comp_year_data, lag.max = 20, plot = FALSE)
      }, error = function(e) return(NULL))
      
      # Skip if ccf failed
      if (is.null(ccf_result)) next
      
      # Find best lag (highest correlation)
      best_lag <- ccf_result$lag[which.max(ccf_result$acf)]
      
      # Store results
      lag_results[[paste(country, comp_year, sep = "_")]] <- data.frame(
        country = country,
        year = as.numeric(comp_year),
        shift = best_lag
      )
    }
  }
  
  # Combine all results into a dataframe
  lag_data <- do.call(rbind, lag_results)
  return(lag_data)
}

plot_seasonality_shift1 <- function(lag_data) {
  ggplot(lag_data, aes(x = year, y = shift, color = country, group = country)) +
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
    scale_x_continuous(breaks = unique(lag_data$year)) +  
    ylim(-20, 20)  
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
shift_data <- ccf_shift_data(flu_dataset, chosen_countries, hemisphere_info)
plot_seasonality_shift(shift_data)

# produce the plot using (hopefully) more robust method
shift_data1 <- ccf_shift_data1(flu_dataset, chosen_countries, hemisphere_info)
plot_seasonality_shift1(shift_data1)

# select countries for report
countries_rep <- c("Argentina", "Australia", "Denmark", "France", "Germany", "Hong Kong",
                   "Ireland", "Japan", "Taiwan", "United Kingdom", "United States of America")
hemisphere_rep <- setNames(
  ifelse(countries_rep %in% southern_hemisphere, "S", "N"), 
  countries_rep
)

shift_data_rep <- ccf_shift_data1(flu_dataset, countries_rep, hemisphere_rep)
plot_seasonality_shift1(shift_data_rep)

flu_dataset %>%
  filter(country == "Chile") %>%
  ggplot(aes(x = week, y = cases, group = year, color = factor(year))) +
  geom_line(size = 1, alpha = 0.7) +
  scale_color_viridis_d() +  # Colorblind-friendly palette
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +  # Show week numbers at intervals
  labs(title = "Flu Cases by Week",
       x = "Week",
       y = "Cases",
       color = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom")



# Interactive plot ----------------------------------------------------------
# using this to pick out problematic countries


plot_seasonality_shift_int <- function(lag_data) {
  p <- ggplot(lag_data, aes(x = year, y = shift, 
                            color = country, group = country, 
                            text = paste("Country:", country, "<br>Year:", year, "<br>Shift:", shift))) +
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
    scale_x_continuous(breaks = unique(lag_data$year)) +  
    ylim(-20, 20)  
  
  ggplotly(p, tooltip = "text")  # Convert ggplot to interactive plotly plot
}


plot_seasonality_shift_int(shift_data)


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

# Function to calculate peak shifts and bootstrap results
bootstrap_data <- function(data, countries, hemisphere) {
  
  peak_results <- list()
  
  # Loop through each country
  for (country in countries) {
    
    # Check if the country exists in the dataset
    if (!(country %in% unique(data$country))) {
      message(paste("Skipping", country, "- not in dataset"))
      next
    }
    
    # Retrieve country data and shift week based on hemisphere
    country_data <- data %>%
      filter(country == !!country) %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "N" ~ (week + 26) %% 52,  # Shift by 26 weeks if North
        TRUE ~ week  # Keep the same if South
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
      bootstrap_results <- boot(data = year_data, statistic = bootstrap_shift, R = 100, 
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
  
  # Combine all results into a single data frame
  peak_results_df <- do.call(rbind, peak_results)
  
  return(peak_results_df)
}



# select countries for bootstrap
countries_boot <- c("Australia", "Chile", 
                   "Japan", "South Africa",
                   "United Kingdom", "United States of America") # omitted South America (Chile)
hemisphere_boot <- setNames(
  ifelse(countries_boot %in% southern_hemisphere, "S", "N"), 
  countries_boot
)

shift_data_boot <- bootstrap_data(flu_dataset, countries_boot, hemisphere_boot)


# work on plots


# set colours to match continent cumulative plots
country_colors <- c(
  "Australia" = "#5A9BD5",
  "Chile" = "#F27C8B",
  "Japan" = "#F5D300",
  "United Kingdom" = "#7AC943",
  "United States of America" = "#66C2A5",
  "South Africa" = "#F4A4A4"
)

plot_boot_facet <- ggplot(shift_data_boot, aes(x = year, y = shift, group = country, color = country)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_line(size = 0.8, linetype = "solid") +  # Changed to solid lines
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = country), alpha = 0.2, color = NA) +  # Add ribbons for error intervals
  labs(
    title = "Peak Week Shift by Country",
    x = "Year", 
    y = "Peak Week Shift (Compared to 2019)"
  ) +
  facet_wrap(~ country, scales = "fixed", ncol = 3) +  # Y scale fixed across facets
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  scale_color_manual(values = country_colors) +  # Using the continent colors mapped to countries
  scale_fill_manual(values = country_colors) +  # Matching fill colors for ribbons
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


ggsave(
  filename = paste0("plots/Influenza/Other/flu_boot_facet.png"),  # Name of the file (you can change the extension to .jpg, .pdf, etc.)
  plot = plot_boot_facet,  # This refers to the last plot generated
  width = 8,  # Width of the plot (in inches)
  height = 6,  # Height of the plot (in inches)
  dpi = 300  # Resolution (dots per inch) - 300 is good for print quality
)



# produce summary table instead
shift_data_boot1 <- shift_data_boot %>%
  mutate(
    shift = round(shift, 2),
    lower_ci = round(lower_ci, 2),
    upper_ci = round(upper_ci, 2)
  ) %>%
  filter(year == 2023)


kable(shift_data_boot1, format = "html", col.names = c("Country", "Year", "Median Shift", "Lower CI", "Upper CI"), row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
  column_spec(3, color = "white", background = "#5A9BD5") %>%  # Highlight shift column
  column_spec(4:5, background = "#F5F5F5") %>%  # Light gray for CI columns
  row_spec(0, bold = TRUE, color = "white", background = "#333333")  # Header formatting




# UPDATE to pre-2021 aggregated data rather than just 2019 --------------------




# Function to find the average peak week for pre-2021 years
find_avg_peak_week <- function(data) {
  pre_2021_data <- data %>% filter(year < 2021)
  week_totals <- pre_2021_data %>% group_by(epi_week) %>% summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")
  peak_week <- week_totals %>% filter(total_cases == max(total_cases, na.rm = TRUE)) %>% pull(epi_week)
  return(peak_week)
}


# Bootstrap function to estimate shift compared to pre-2021 average
bootstrap_shift_agg <- function(data, indices, avg_peak_pre2021) {
  sampled_data <- data[indices, ]  # Bootstrap sample
  peak_sample <- find_peak_week(sampled_data, sampled_data$year[1])  # Find peak for sample
  shift <- peak_sample - avg_peak_pre2021  # Compare shift to pre-2021 avg
  return(shift)
}

bootstrap_data_agg <- function(data, countries, hemisphere) {
  
  peak_results <- list()
  
  for (country in countries) {
    
    if (!(country %in% unique(data$country))) {
      message(paste("Skipping", country, "- not in dataset"))
      next
    }
    
    country_data <- data %>%
      filter(country == !!country) %>%
      mutate(epi_week = case_when(
        hemisphere[country] == "N" ~ (week + 26) %% 52,  # Adjust for North/South
        TRUE ~ week
      )) %>%
      arrange(year, epi_week)
    
    # Find pre-2021 average peak week
    avg_peak_pre2021 <- find_avg_peak_week(country_data)
    
    if (is.na(avg_peak_pre2021)) {
      message(paste("Skipping", country, "- Pre-2021 peak not found"))
      next
    }
    
    years_to_analyse <- unique(country_data$year[country_data$year > 2021])
    
    country_peak_shifts <- data.frame(country = character(), year = integer(), shift = numeric(), 
                                      lower_ci = numeric(), upper_ci = numeric())
    
    for (year in years_to_analyse) {
      
      year_data <- country_data %>% filter(year == !!year)
      
      if (nrow(year_data) < 30) {
        message(paste("Skipping", country, year, "- Not enough data"))
        next
      }
      
      set.seed(123)
      bootstrap_results <- boot(data = year_data, statistic = bootstrap_shift_agg, R = 2500, 
                                avg_peak_pre2021 = avg_peak_pre2021)
      
      ci <- boot.ci(bootstrap_results, type = "perc", conf = 0.90)$percent[4:5]
      
      country_peak_shifts <- rbind(country_peak_shifts, 
                                   data.frame(country = country, year = year, 
                                              shift = mean(bootstrap_results$t), 
                                              lower_ci = ci[1], upper_ci = ci[2]))
    }
    
    peak_results[[country]] <- country_peak_shifts
  }
  
  peak_results_df <- do.call(rbind, peak_results)
  return(peak_results_df)
}

shift_data_boot_agg <- bootstrap_data_agg(flu_dataset, countries_boot, hemisphere_boot)


plot_boot_facet_agg <- ggplot(shift_data_boot_agg, aes(x = year, y = shift, group = country, color = country)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = country), alpha = 0.2, color = NA) +
  labs(
    title = NULL,
    x = "Year", 
    y = "Peak Week Shift (Compared to Pre-2021 Average)",
    caption = "Source: FLUNET/GOV | Data range: 2017 - latest available"
  ) +
  facet_wrap(~ country, scales = "fixed", ncol = 3) +
  bbc_style() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",  # Removes all legends
    axis.ticks.y = element_line(),
    axis.line.y.left = element_line()
  ) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  scale_color_manual(values = country_colors) +
  scale_fill_manual(values = country_colors)


finalise_plot(plot_name = plot_boot_facet_agg,
              source = "FLUNET/GOV | Data range: 2017 - latest available",
              save_filepath = "plots/Influenza/Other/flu_boot_agg.png",  
              width_pixels = 1100, 
              height_pixels = 700)


# produce summary table instead
shift_data_boot_agg1 <- shift_data_boot_agg %>%
  mutate(
    shift = round(shift, 2),
    lower_ci = round(lower_ci, 2),
    upper_ci = round(upper_ci, 2)
  ) %>%
  filter(year == 2023)


kable(shift_data_boot_agg1, format = "html", col.names = c("Country", "Year", "Median Shift", "Lower CI", "Upper CI"), row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
  column_spec(3, color = "white", background = "#5A9BD5") %>%  # Highlight shift column
  column_spec(4:5, background = "#F5F5F5") %>%  # Light gray for CI columns
  row_spec(0, bold = TRUE, color = "white", background = "#333333")  # Header formatting

