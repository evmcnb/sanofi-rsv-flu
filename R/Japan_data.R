# create file names for each year

file_names <- paste0("csv/Japan/", 2015:2024, ".csv")

# read all datasets,create a Year column and combine
Japan_data <- do.call(rbind, lapply(file_names, function(file) {
  data <- read.csv(file)
  data$Year <- gsub("^.*/|\\.csv$", "", file) # extract year from filename
  return(data)
}))


# Clean up
rm(list=c("file_names"))
s