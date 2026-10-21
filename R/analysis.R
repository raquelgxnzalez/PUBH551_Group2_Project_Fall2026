# Group Project: Depression, Comorbidity, and Multiple Linear Regression
# Load necessary libraries
library(tidyverse)
# Question 2. Data audit, cleaning, and analytic cohort
# 2a. Standardization
# Import raw CSV file from data folder
raw_data <- read.csv("../data/PUBH551_Group_Project_Depression_Data_2005_2025.csv")
# Identify the 5-missing value representations in raw data by quickly looking at every 
# unique response
raw_data %>% 
  # Select the text columns
  select(where(is.character)) %>%
  # Do not include the variables containing "id", "date", and "USD" as those will have an 
  # abundance of unique values and will be hard to sift through
  select(-matches("_id"), -matches("date"), -matches("usd")) %>%
  # Use map() to return a list of each unique response from every variable
  map(~ unique(.x))
# 5 missing value representations = "", ".", "NA", "N/A" , and "missing"
# Replace the missing value representations with standard NA values across all columns
# Use mutate(across(where(is.character))) to modify all columns stored as text/characters
# ifelse(.x %in% ) to check each value in the column and identify whether the current value 
# (.x) matches any of the 5 specific missing value strings identified in audit and replace 
# with standard NA value
na_data <- raw_data %>%
  mutate(across(where(is.character), ~ ifelse(.x %in% c("", ".", "NA", "N/A", "missing"), NA, .x)))

# Standardize the condition column to fix inconsistent capitalization, whitespace, underscores, and missing-value representations
data <- na_data %>%
  # Use mutate() to modify column inside the dataset
  mutate(
    comorbid_condition = comorbid_condition %>%
      # str_to_lower() converts every letter to lowercase
      str_replace_all("_", " ") %>%
      # str_squish() removes leading/trailing spaces, turns double-spaces into single
      str_squish() %>%
      str_to_lower() 
    # str_replace_all() to replace all underscores with normal spaces
  )
# 2b. Cleaning Numeric Fields