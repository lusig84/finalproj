#test
library(readr)  # For read_csv function

# Get a list of all CSV files in the current directory
csv_files <- list.files(pattern = "\\.csv$")

# Specify the files to exclude
exclude_files <- c("Gender_NJ.csv", "Race-Ethnicity_NJ.csv")

# Filter the list to exclude specific files
csv_files <- setdiff(csv_files, exclude_files)

# Read each remaining file, skip the first 2 lines, and add the county column
data_list <- lapply(csv_files, function(file) {
# Extract the county name from the file name
county_name <- gsub("_.*", "", file)  # Extract everything before the first "_"
  
# Read the CSV file, skipping the first 2 lines
data <- read_csv(file, skip = 2)
  
# Add the county name as a new column
data$County <- county_name
  
  return(data)
})

# View one of the processed datasets to confirm
print(data_list[[1]])


# Combine all datasets in the list into a single data frame
combined_data <- do.call(rbind, data_list)

# View the combined dataset
print(head(combined_data))

# Optionally, save the combined dataset to a CSV file
write_csv(combined_data, "combined_data.csv")



#- do a function adn get rid of the first too lines locally
 # - then combine all data sets together
