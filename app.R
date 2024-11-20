#cleaning data for just Atlantic County table
raw_data <- suppressWarnings(readLines("AtlanticCounty_Age.csv")) # Read the CSV file line by line, ignoring any warnings (e.g., incomplete last line).
data_lines <- raw_data[-c(1:2)] # Remove the first two lines of extra info from the data.
header <- unlist(strsplit(data_lines[1], ","))  # Extract the first line as header
header <- gsub(" ", "_", header) # Replace spaces in the header names with underscores for consistency.
AtlanticCounty_Age <- read.csv(text = paste(data_lines[-1], collapse = "\n"), header = FALSE) # Read the remaining lines as a CSV, treating the header separately.
View(AtlanticCounty_Age)
colnames(AtlanticCounty_Age) <- header # Assign the cleaned header names to the data frame's columns.
AtlanticCounty_Age[AtlanticCounty_Age == "No Data"] <- NA # Replace "No Data" entries in the data frame with `NA` (missing data).
AtlanticCounty_Age <- AtlanticCounty_Age[-19, ] # Remove row 19 from the data frame, which contains unwanted data.
AtlanticCounty_Age <- AtlanticCounty_Age[, -ncol(AtlanticCounty_Age)] # Remove the last column of the data frame cause its problematic.
AtlanticCounty_Age$Year <- as.integer(AtlanticCounty_Age$Year) # Convert the `Year` column to integers, replacing invalid entries with `NA`.
cols_to_numeric <- setdiff(names(AtlanticCounty_Age), "Year")  # Identify all column names except "Year" for numeric conversion.
AtlanticCounty_Age[cols_to_numeric] <- lapply(AtlanticCounty_Age[cols_to_numeric], as.numeric) # Convert all selected columns to numeric, ensuring they can be used for calculations.

library(readr)  
# For read_csv function

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
