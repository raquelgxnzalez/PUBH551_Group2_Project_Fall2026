# Group Project: Depression, Comorbidity, and Multiple Linear Regression
# Load necessary libraries
library(tidyverse)
# Question 2. Data audit, cleaning, and analytic cohort
# 2a. Standardization
# Import raw CSV file from data folder
raw_data <- read_csv("../data/PUBH551_Group_Project_Depression_Data_2005_2025.csv", 
                     col_types = cols(.default = col_character())) 
# Added col_types = col(.default) per step 2b instruction
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

# Standardize the dataset to fix inconsistent capitalization, whitespace, underscores, and missing-value representations
data <- na_data %>%
  # Use mutate() to modify column inside the dataset
  mutate(across(
    # Exclude the often case-sensitive ID codes from cleaning using !c()
    where(is.character) & !c(record_id, participant_id),
    # Apply changes to every item in list
    ~ .x %>%
      # str_to_lower() converts every letter to lowercase
      str_to_lower() %>%
      # str_squish() removes leading/trailing spaces
      str_squish() %>%
      # str_replace_all() to replace all underscores with normal spaces
      str_replace_all("_", " ")
  ))
# 2b. Cleaning Numeric Fields
# Retroactively add cols(.default = col_character())) to the CSV import step
# to have deliberate control of column type
# Use across() and parse_number() to clean numeric fields discussed above
data <- data %>%
  mutate(across(
    c(annual_household_income_usd, area_deprivation_index_percentile, 
      sleep_duration_hours, physical_activity_min_week, bmi_kg_m2, 
      systolic_bp_mmHg, total_cholesterol_mg_dL, healthcare_visits_past_year, 
      phq9_score),
    parse_number
    )
  ) 
# 2c. Removing Duplicates
# Use distinct() to remove all duplicate rows
data <- data %>%
  distinct()
# Identify rows where either prevalence or mortality values are missing
missing_vars <- data %>%
  # Use filter() to identify rows of NAs in each prevalence/mortality column
  filter(is.na(condition_prevalence_per_100k) | 
           is.na(condition_mortality_per_100k)) %>%
  # Select key columns (record_id), comorbid_condition) in addition to 
  # prevalence/mortality to create a df for easy scanning
  select(record_id, comorbid_condition, 
         condition_prevalence_per_100k, condition_mortality_per_100k) %>%
  as.data.frame()
# Print table of missing variables with key identifier columns
print(missing_vars)
# Use summary() to identify the suspicious value in total_cholesterol_mg_dL
summary(data$total_cholesterol_mg_dL)
# 2e. Complete-Case Analysis for Primary MLR
# Perform a complete-case analysis for the primary MLR
analytic_data <- data %>%
  # Filter the dataset for our group's chosen condition subset
  filter(comorbid_condition == "insert here") %>%
  # Restrict to complete cases on the 6 primary MLR variables only
  drop_na(
    phq9_score,
    physical_activity_min_week,
    age_years,
    sex,
    annual_household_income_usd,
    smoking_status
  )
# Export the cleaned analytic CSV for analysis into the data folder
write_csv(analytic_data, "../data/cleaned_analytic_data.csv")