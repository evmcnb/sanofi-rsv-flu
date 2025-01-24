# directory path for datasets
path <- "csv/Japan/"

# create file names for each year
years <- 2015:2024
file_names <- paste0(path, years, ".csv")

# read all datasets,create a Year column and combine
Japan_data <- do.call(rbind, lapply(file_names, function(file) {
  data <- read.csv(file)
  data$Year <- gsub("^.*/|\\.csv$", "", file) # extract year from filename
  return(data)
}))

