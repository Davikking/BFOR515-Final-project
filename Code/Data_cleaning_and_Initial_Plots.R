#Christoph Schwartz
#9/11/2024
#BFOR515

# ==================================================
# Section 1: Setup and Initialization
# Description: Load necessary libraries, if needed create global variables etc.
# ==================================================
library(tidyverse)
library(stringr)
library(rpart)
library(rpart.plot)
library(tidygeocoder)

# ==================================================
# Section 2: Data Import 
# Description: Import data sets, and investigate them, check for missing information
# clean the data and merge data sets so we are only doing computations on one large data frame
# this portion of code was used for basic cleaning such as renaming colums that were the same
# and ensuring data is in the correct format
# ==================================================


NYC_data <- read.csv("C:\\Users\\Christoph\\Documents\\515\\Project\\Datasets\\NewYorkCity.csv")
NYS_data <- read.csv("C:\\Users\\Christoph\\Documents\\515\\Project\\Datasets\\NewYorkState.csv")

glimpse(NYC_data)
glimpse(NYS_data)

#if 'ACTION' contains only empty strings or NA, you can handle both:
NYC_cleaned <- NYC_data[!is.na(NYC_data$ACTION) & NYC_data$ACTION != "", ]
glimpse(NYC_cleaned)
# Rename NYS columns to match NYC columns
colnames(NYS_data)[colnames(NYS_data) == "FACILITY"] <- "DBA"
colnames(NYS_data)[colnames(NYS_data) == "ZIP.CODE"] <- "ZIPCODE"
colnames(NYS_data)[colnames(NYS_data) == "ADDRESS"] <- "STREET"
colnames(NYS_data)[colnames(NYS_data) == "LAST.INSPECTED"] <- "INSPECTION.DATE"
colnames(NYC_cleaned)[colnames(NYC_cleaned) == "BORO"] <- "MUNICIPALITY"

# Convert INSPECTION DATE to Date type
NYC_cleaned$INSPECTION.DATE <- as.Date(NYC_cleaned$INSPECTION.DATE, format="%m/%d/%Y")
NYS_data$INSPECTION.DATE <- as.Date(NYS_data$INSPECTION.DATE, format="%m/%d/%Y")

# Split Location1 into Latitude and Longitude in the NYS dataset
NYS_data$Latitude <- as.numeric(str_extract(NYS_data$Location1, "(?<=\\().*?(?=,)"))
NYS_data$Longitude <- as.numeric(str_extract(NYS_data$Location1, "(?<=, ).*?(?=\\))"))

NYS_data <- NYS_data %>%
  mutate(TOTAL...CRITICAL.VIOLATIONS = ifelse(VIOLATIONS == "No violations found." & is.na(TOTAL...CRITICAL.VIOLATIONS), 0, TOTAL...CRITICAL.VIOLATIONS),
         TOTAL..CRIT...NOT.CORRECTED = ifelse(VIOLATIONS == "No violations found." & is.na(TOTAL..CRIT...NOT.CORRECTED), 0, TOTAL..CRIT...NOT.CORRECTED),
         TOTAL...NONCRITICAL.VIOLATIONS = ifelse(VIOLATIONS == "No violations found." & is.na(TOTAL...NONCRITICAL.VIOLATIONS), 0, TOTAL...NONCRITICAL.VIOLATIONS)
         )


# ==================================================
# Section 3: Detailed data cleaning and combination of data sets
# Description: This section of code aims to make the two data sets more comparable
# based on the criteria given by New York City health and safety department
# I created a function for the rest of the New York State data that computes the 
# score and grade for restaurants outside New York city 
# ==================================================

# Function to calculate score and grade based on critical violations
calculate_score_and_grade <- function(critical_violations) {
  # Initialize score
  score <- 0
  # Add 5 points for each critical violation
  score <- score + (critical_violations * 5)
  #print(score)
  # Assign letter grade based on score
  if (score >= 0 && score <= 13) {
    grade <- "A"
  } else if (score >= 14 && score <= 27) {
    grade <- "B"
  } else if (score >= 28) {
    grade <- "C"
  } else {
    grade <- NA
  }
  
  return(data.frame(score = score, grade = grade))
}

NYS_data <- NYS_data %>%
  mutate(
    TOTAL...CRITICAL.VIOLATIONS = as.numeric(TOTAL...CRITICAL.VIOLATIONS),  # Convert to numeric if needed
    score_grade = purrr::pmap_df(list(TOTAL...CRITICAL.VIOLATIONS), calculate_score_and_grade)
  )

# Split the score and grade into separate columns
NYS_data <- NYS_data %>%
  mutate(
    SCORE = score_grade$score,
    GRADE = score_grade$grade
  ) %>%
  select(-score_grade)  
##nyc_df_combined <- NYC_cleaned %>%
  #group_by(DBA, INSPECTION.DATE) %>%  # Group by restaurant and inspection date
  #summarise(
    #TOTAL...CRITICAL.VIOLATIONS = sum(CRITICAL.FLAG == "Critical", na.rm = TRUE),  # Count critical violations
    #TOTAL...NONCRITICAL.VIOLATIONS = sum(CRITICAL.FLAG == "Not Critical", na.rm = TRUE),  # Count non-critical violations
    #SCORE = mean(SCORE, na.rm = TRUE),  # Sum the scores
    #GRADE = min(GRADE, na.rm = TRUE),  # Get the worst (lowest) grade
    #VIOLATION.DESCRIPTION = paste(VIOLATION.DESCRIPTION, collapse = "; ")  # Concatenate all descriptions
  #) %>%
 # ungroup()  # Ungroup after summarizing
# Group and merge violations


NYC_concat <- NYC_cleaned %>%
  group_by(DBA, STREET, INSPECTION.DATE, Latitude, Longitude, MUNICIPALITY, BUILDING) %>% # Group by restaurant, address, and date
  summarize(
    TOTAL...CRITICAL.VIOLATIONS = sum(CRITICAL.FLAG == "Critical", na.rm = TRUE),  # Count critical violations
    Violations = paste(VIOLATION.DESCRIPTION, collapse = "; "), # Concatenate violations
    Violation_Codes = paste(VIOLATION.CODE, collapse = "; "),   # Concatenate violation codes
    TOTAL...NONCRITICAL.VIOLATIONS = sum(CRITICAL.FLAG == "Not Critical", na.rm = TRUE),  # Count non-critical violations
    SCORE = mean(SCORE, na.rm = TRUE),  # Sum the scores
    GRADE = min(GRADE, na.rm = TRUE),  # Get the worst (lowest) grade
    .groups = "drop" # Drop grouping structure after summarizing
  )

# View the merged data
print(NYC_concat)

nyc_df_filtered <- nyc_df_combined %>%
  filter(is.finite(SCORE))  # This will retain only rows with valid numeric scores

nys_df_filtered <- NYS_data %>%
  select(DBA, INSPECTION.DATE, TOTAL...CRITICAL.VIOLATIONS, TOTAL...NONCRITICAL.VIOLATIONS, SCORE)

NYS_Full_dF <- full_join(nyc_df_filtered, nys_df_filtered, by = c("DBA", "INSPECTION.DATE", "SCORE", "TOTAL...CRITICAL.VIOLATIONS", "TOTAL...NONCRITICAL.VIOLATIONS"))
# ==================================================
# Section 3.1: Finding restaurant cuisine type 
# This section is dedicated to exporting the restaurant names from 
# the data set into a separate CSV file to feed to a python web scrapper script
# this script searches trip advisor for the cuisine type to then give me a list I can add back 
# to the data set for clustering analysis. 
# ==================================================
# Extract unique restaurant names and addresses
if ("DBA" %in% colnames(NYS_data) && "STREET" %in% colnames(NYS_data)) {  # Replace 'address' if the column name is different
  restaurant_info <- NYS_data %>%
    select(DBA, STREET) %>%      # Select the DBA and address columns
    distinct()                    # Get unique combinations
  
  # Save to new CSV file
  write.csv(restaurant_info, "restaurant_names_addresses.csv", row.names = FALSE)  # Save without row names
  print("Restaurant names and addresses extracted and saved to 'restaurant_names_addresses.csv'.")
} else {
  print("Columns 'DBA' or 'address' not found in the dataset.")
}

# ==================================================
# Section 4: Initial plots
# This section I began plotting number of critical violations per year 
# and non critical violations per year since 2010 to see any obvious patterns in the base data 
# as well as the combined data 
# ==================================================
# Step 1: Extract the year from the INSPECTION.DATE column
NYC_concat <- NYC_concat %>%
  mutate(year = format(as.Date(INSPECTION.DATE, format="%Y-%m-%d"), "%Y"))

# Step 2: Filter data for the year 2010 and after
NYC_concat <- NYC_concat %>%
  filter(as.numeric(year) >= 2010)

# Step 3: Summarize the total scores per year
nyc_violations_per_year <- NYC_concat %>%
  group_by(year) %>%
  summarise(total_score = sum(SCORE, na.rm = TRUE))  # Sum the scores


ggplot(nyc_violations_per_year, aes(x = as.numeric(year), y = total_score)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Use fill for bar color
  labs(title = "Total Score from Violations per Year in NYC Restaurants (2010 and after)",
       x = "Year",
       y = "Total Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

# Step 1: Extract the year from the INSPECTION.DATE column
nys_df_filtered <- nys_df_filtered %>%
  mutate(year = format(as.Date(INSPECTION.DATE, format="%Y-%m-%d"), "%Y"))

# Step 2: Filter data for the year 2010 and after
nys_df_filtered <- nys_df_filtered %>%
  filter(as.numeric(year) >= 2010)

# Step 3: Summarize the total critical violations per year
nys_violations_per_year <- nys_df_filtered %>%
  group_by(year) %>%
  summarise(total_violations = sum(TOTAL...CRITICAL.VIOLATIONS, na.rm = TRUE))  # Sum the critical violations

# Step 4: Create a plot using ggplot2
ggplot(nys_violations_per_year, aes(x = as.numeric(year), y = total_violations)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Use fill for bar color
  labs(title = "Total Score from Violations per Year in NYS Restaurants outside of NYC (2010 and after)",
       x = "Year",
       y = "Total Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

# Step 1: Extract the year from the INSPECTION.DATE column
NYS_Full_dF <- NYS_Full_dF %>%
  mutate(year = format(as.Date(INSPECTION.DATE, format="%Y-%m-%d"), "%Y"))

# Step 2: Filter data for the year 2010 and after
NYS_Full_dF <- NYS_Full_dF %>%
  filter(as.numeric(year) >= 2010)

# Step 3: Summarize the total critical violations per year
ny_violations_per_year <- NYS_Full_dF %>%
  group_by(year) %>%
  summarise(total_violations = sum(TOTAL...CRITICAL.VIOLATIONS, na.rm = TRUE))  # Sum the critical violations


# Step 4: Create a plot using ggplot2
ggplot(nys_violations_per_year, aes(x = as.numeric(year), y = total_violations)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Use fill for bar color
  labs(title = "Total Violations per Year in NYS Restaurants (2010 and after)",
       x = "Year",
       y = "Total Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

# Summarize violations per county
violations_by_county <- NYS_data %>%
  group_by(COUNTY) %>%
  summarize(total_violations = sum(TOTAL...CRITICAL.VIOLATIONS, na.rm = TRUE))

# Plot the bar chart
ggplot(violations_by_county, aes(x = reorder(COUNTY, -total_violations), y = total_violations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip for easier reading if there are many counties
  labs(title = "Total Violations per County", x = "County", y = "Total Violations") +
  theme_minimal()
# Summarize violations per county
violations_by_county <- NYC_concat %>%
  group_by(MUNICIPALITY) %>%
  summarize(total_violations = sum(TOTAL...CRITICAL.VIOLATIONS, na.rm = TRUE))

# Plot the bar chart
ggplot(violations_by_county, aes(x = reorder(MUNICIPALITY, -total_violations), y = total_violations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip for easier reading if there are many counties
  labs(title = "Total Violations per Boro", x = "County", y = "Total Violations") +
  theme_minimal()

ggplot(NYS_data, aes(y = TOTAL...CRITICAL.VIOLATIONS, x = INSPECTION.DATE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Total violations by year", 
       x = "Total violations", 
       y = "year")

lr_model <- lm(TOTAL...CRITICAL.VIOLATIONS ~ INSPECTION.DATE, data = NYS_data)
# View the summary of the model to see coefficients, R-squared, p-values, etc.
summary(lr_model)

