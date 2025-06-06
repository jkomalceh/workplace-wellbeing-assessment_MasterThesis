# Create Sample Data for Testing
# This script generates a simulated dataset with the same structure as pilotdata_4comp.csv

library(tidyverse)
set.seed(12345)  # For reproducibility

# Function to generate sample data
create_sample_data <- function(n = 500) {
  
  # Demographics
  age_cat <- sample(c("<25", "25-34", "35-44", "45-54", ">=55"), 
                    n, replace = TRUE, prob = c(0.1, 0.25, 0.3, 0.25, 0.1))
  
  sex_cat <- sample(c("M", "F"), n, replace = TRUE, prob = c(0.48, 0.52))
  
  LanguageCode <- sample(c("nl", "du", "fr"), n, replace = TRUE, 
                        prob = c(0.4, 0.3, 0.3))
  
  ismobile <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7))
  
  Name_Company <- sample(paste0("Company_", LETTERS[1:10]), n, replace = TRUE)
  
  Version_survey <- sample(c("v1", "v2"), n, replace = TRUE, prob = c(0.6, 0.4))
  
  # Function to generate ordinal data with some missingness
  generate_ordinal <- function(n, prob_na = 0.1, probs = c(0.1, 0.2, 0.4, 0.2, 0.1)) {
    vals <- sample(1:5, n, replace = TRUE, prob = probs)
    # Introduce missing values
    na_idx <- sample(1:n, size = floor(n * prob_na))
    vals[na_idx] <- NA
    return(vals)
  }
  
  # Psychosocial variables
  veerkracht_calc <- rnorm(n, mean = 3.5, sd = 0.8)
  veerkracht_calc[sample(1:n, size = floor(n * 0.05))] <- NA
  
  psy_rating_pace <- generate_ordinal(n, 0.05)
  psy_rating_emotional <- generate_ordinal(n, 0.08)
  psy_rating_sphere <- generate_ordinal(n, 0.06)
  psy_work_life <- generate_ordinal(n, 0.07)
  
  # Ergonomics variables
  erg_capac <- generate_ordinal(n, 0.09)
  erg_rating_posture <- generate_ordinal(n, 0.10)
  erg_rating_repeat <- generate_ordinal(n, 0.08)
  erg_rating_sitting <- generate_ordinal(n, 0.07)
  erg_rating_loads <- generate_ordinal(n, 0.12)
  erg_rating_physical <- generate_ordinal(n, 0.11)
  
  # Safety variables
  saf_satisfaction <- generate_ordinal(n, 0.06, probs = c(0.05, 0.15, 0.3, 0.35, 0.15))
  saf_rating_workinvolv <- generate_ordinal(n, 0.09)
  saf_rating_leadengage <- generate_ordinal(n, 0.10)
  
  # Hygiene variables
  hyg_satisfaction <- generate_ordinal(n, 0.08)
  hyg_rating_tools <- generate_ordinal(n, 0.15)
  hyg_rating_low_temp <- generate_ordinal(n, 0.18)
  hyg_rating_high_temp <- generate_ordinal(n, 0.17)
  hyg_rating_noise <- generate_ordinal(n, 0.14)
  hyg_rating_substances <- generate_ordinal(n, 0.20)
  
  # Environmental domain (only for 60% of companies)
  has_env <- Name_Company %in% sample(unique(Name_Company), 6)
  
  mil_rating_leadengage <- ifelse(has_env, generate_ordinal(n, 0.25), -999)
  mil_satisfaction <- ifelse(has_env, generate_ordinal(n, 0.23), -999)
  mil_rating_contrib <- ifelse(has_env, generate_ordinal(n, 0.26), -999)
  mil_rating_workinvolv <- ifelse(has_env, generate_ordinal(n, 0.24), -999)
  
  # Frequency/exposure variables
  hyg_freq_exp_substances <- generate_ordinal(n, 0.22)
  hyg_freq_exp_tools <- generate_ordinal(n, 0.20)
  hyg_freq_exp_high_temp <- generate_ordinal(n, 0.19)
  hyg_freq_exp_low_temp <- generate_ordinal(n, 0.21)
  hyg_freq_exp_noise <- generate_ordinal(n, 0.18)
  erg_freq_exp_sitting <- generate_ordinal(n, 0.16)
  erg_freq_exp_posture <- generate_ordinal(n, 0.17)
  erg_freq_exp_physical <- generate_ordinal(n, 0.15)
  
  # Additional auxiliary variables
  shift <- sample(c("Day", "Night", "Rotating"), n, replace = TRUE, 
                  prob = c(0.7, 0.1, 0.2))
  sleep <- rnorm(n, mean = 7, sd = 1.2)
  sleep[sleep < 3] <- 3
  sleep[sleep > 10] <- 10
  health_not_ok <- sample(0:1, n, replace = TRUE, prob = c(0.8, 0.2))
  
  # Create the dataframe
  sample_data <- data.frame(
    # Demographics
    age_cat, sex_cat, LanguageCode, ismobile, Name_Company, Version_survey,
    
    # Psychosocial
    veerkracht_calc, psy_rating_pace, psy_rating_emotional, 
    psy_rating_sphere, psy_work_life,
    
    # Ergonomics
    erg_capac, erg_rating_posture, erg_rating_repeat, erg_rating_sitting,
    erg_rating_loads, erg_rating_physical,
    
    # Safety
    saf_satisfaction, saf_rating_workinvolv, saf_rating_leadengage,
    
    # Hygiene
    hyg_satisfaction, hyg_rating_tools, hyg_rating_low_temp,
    hyg_rating_high_temp, hyg_rating_noise, hyg_rating_substances,
    
    # Environmental
    mil_rating_leadengage, mil_satisfaction, mil_rating_contrib, mil_rating_workinvolv,
    
    # Frequency/exposure
    hyg_freq_exp_substances, hyg_freq_exp_tools, hyg_freq_exp_high_temp,
    hyg_freq_exp_low_temp, hyg_freq_exp_noise, erg_freq_exp_sitting,
    erg_freq_exp_posture, erg_freq_exp_physical,
    
    # Additional
    shift, sleep, health_not_ok
  )
  
  return(sample_data)
}

# Generate sample data
cat("Generating sample data...\n")
sample_data <- create_sample_data(n = 500)

# Save to data/raw directory
output_path <- "data/raw/pilotdata_4comp_SAMPLE.csv"
write.table(sample_data, 
            file = output_path, 
            sep = ";", 
            row.names = FALSE,
            quote = FALSE)

cat("Sample data created successfully!\n")
cat("File saved as:", output_path, "\n")
cat("Dimensions:", nrow(sample_data), "rows x", ncol(sample_data), "columns\n")

# Display basic summary
cat("\nMissing data summary:\n")
missing_summary <- sample_data %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Percent = round(Missing_Count / nrow(sample_data) * 100, 1)) %>%
  filter(Missing_Count > 0) %>%
  arrange(desc(Missing_Percent))

print(missing_summary, n = 10)

cat("\nNOTE: This is simulated data for testing purposes only.\n")
cat("Replace 'pilotdata_4comp_SAMPLE.csv' with your actual 'pilotdata_4comp.csv' file.\n")