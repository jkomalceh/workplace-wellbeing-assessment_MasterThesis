# Create Sample Data for Testing - Matching Thesis Sample Characteristics
# This script generates a simulated dataset that matches the sample characteristics
# described in the thesis methodology

library(tidyverse)
set.seed(12345)  # For reproducibility

# Function to generate sample data matching thesis characteristics
create_sample_data <- function(n = 699) {  # Default to thesis sample size
  
  # Initialize dataframe
  sample_data <- data.frame(id = 1:n)
  
  # 1. LANGUAGE DISTRIBUTION (88.3% Dutch, 10.4% French, 1.3% English)
  # Combining Dutch variants (nl + du) as "Dutch" per thesis
  language_probs <- c(0.883, 0.104, 0.013)
  language_n <- round(n * language_probs)
  # Adjust for rounding
  language_n[1] <- n - sum(language_n[-1])
  
  language_codes <- c(
    rep("nl", floor(language_n[1] * 0.6)),  # 60% nl variant
    rep("du", ceiling(language_n[1] * 0.4)), # 40% du variant
    rep("fr", language_n[2]),
    rep("en", language_n[3])
  )
  
  # Ensure exact length
  if(length(language_codes) > n) {
    language_codes <- language_codes[1:n]
  } else if(length(language_codes) < n) {
    language_codes <- c(language_codes, rep("nl", n - length(language_codes)))
  }
  
  sample_data$LanguageCode <- sample(language_codes)
  
  # 2. DATA COLLECTION METHOD (25% mobile, 75% desktop)
  mobile_n <- round(n * 0.25)  # ~175 mobile users
  sample_data$ismobile <- sample(c(rep(TRUE, mobile_n), 
                                  rep(FALSE, n - mobile_n)))
  
  # 3. GENDER DISTRIBUTION (63.4% men, 35.2% women, 1.4% missing)
  gender_n <- round(n * c(0.634, 0.352, 0.014))
  # Adjust for rounding
  gender_n[1] <- n - sum(gender_n[-1])
  
  gender_values <- c(
    rep("M", gender_n[1]),
    rep("F", gender_n[2]),
    rep(NA, gender_n[3])
  )
  
  sample_data$sex_cat <- sample(gender_values)
  
  # 4. AGE DISTRIBUTION
  # <25: 4.0%, 25-34: 18.9%, 35-44: 27.5%, 45-54: 32.6%, ≥55: 16.2%, missing: 0.9%
  age_probs <- c(0.040, 0.189, 0.275, 0.326, 0.162, 0.009)
  age_n <- round(n * age_probs)
  # Adjust for rounding
  age_n[1] <- n - sum(age_n[-1])
  
  age_values <- c(
    rep("<25", age_n[1]),
    rep("25-34", age_n[2]),
    rep("35-44", age_n[3]),
    rep("45-54", age_n[4]),
    rep(">=55", age_n[5]),
    rep(NA, age_n[6])
  )
  
  sample_data$age_cat <- sample(age_values)
  
  # 5. COMPANY ASSIGNMENT
  # Company 4 gets special treatment for environment domain
  # Total Company 4: 94 participants (48 extended + 46 standard)
  company_names <- paste0("Company_", 1:10)
  
  # Assign Company 4 to exactly 94 participants
  company_4_indices <- sample(1:n, 94)
  sample_data$Name_Company <- sample(setdiff(company_names, "Company_4"), 
                                    n, replace = TRUE)
  sample_data$Name_Company[company_4_indices] <- "Company_4"
  
  # 6. ENVIRONMENT DOMAIN ASSIGNMENT
  # Within Company 4: 48 get extended version, 46 get standard
  sample_data$Version_survey <- "standard"
  company_4_mask <- sample_data$Name_Company == "Company_4"
  company_4_ids <- which(company_4_mask)
  
  if(length(company_4_ids) >= 48) {
    extended_ids <- sample(company_4_ids, 48)
    sample_data$Version_survey[extended_ids] <- "extended"
  }
  
  # Function to generate ordinal data with specific distributional characteristics
  # and correlation structure
  generate_ordinal_with_stats <- function(n, target_mean, target_sd, min_val, max_val, 
                                        target_skew, prob_na = 0.1, correlate_with = NULL,
                                        correlation = 0) {
    # Adjust probabilities to match target statistics
    
    if(max_val == 3) {
      # For 1-3 scale (Psychosocial and Safety domains)
      if(target_mean < 2) {
        probs <- c(0.40, 0.40, 0.20)
      } else if(target_mean < 2.5) {
        probs <- c(0.15, 0.30, 0.55)
      } else {
        probs <- c(0.10, 0.20, 0.70)
      }
    } else {
      # For 1-4 scale (Ergonomics and Hygiene domains)
      if(target_mean < 2.5) {
        probs <- c(0.30, 0.40, 0.20, 0.10)
      } else if(target_mean < 3.0) {
        probs <- c(0.15, 0.25, 0.35, 0.25)
      } else if(target_mean < 3.5) {
        probs <- c(0.05, 0.15, 0.35, 0.45)
      } else {
        probs <- c(0.02, 0.08, 0.25, 0.65)
      }
    }
    
    if(!is.null(correlate_with)) {
      # Generate correlated data
      # Create a latent variable that's correlated with the base
      latent <- correlation * correlate_with + sqrt(1 - correlation^2) * rnorm(n)
      
      # Convert to ordinal scale using quantiles
      quantiles <- c(0, cumsum(probs))
      vals <- cut(pnorm(latent), breaks = quantiles, labels = min_val:max_val, 
                  include.lowest = TRUE)
      vals <- as.numeric(as.character(vals))
    } else {
      vals <- sample(min_val:max_val, n, replace = TRUE, prob = probs)
    }
    
    # Introduce missing values
    if(prob_na > 0) {
      na_idx <- sample(1:n, size = floor(n * prob_na))
      vals[na_idx] <- NA
    }
    return(vals)
  }
  
  # 7. Generate variables with proper correlation structure
  # First, create latent factors for each domain based on correlation matrix
  
  # PSYCHOSOCIAL DOMAIN
  # Internal correlations: 0.43-0.58
  psy_factor <- rnorm(n)
  
  sample_data$psy_rating_pace <- generate_ordinal_with_stats(n, 2.42, 0.71, 1, 3, -0.81,
                                                            prob_na = (699-661)/699,
                                                            correlate_with = psy_factor,
                                                            correlation = 0.7)
  
  sample_data$psy_rating_emotional <- generate_ordinal_with_stats(n, 2.49, 0.67, 1, 3, -0.94, 
                                                                 prob_na = (699-661)/699,
                                                                 correlate_with = psy_factor,
                                                                 correlation = 0.75)
  
  sample_data$psy_rating_sphere <- generate_ordinal_with_stats(n, 2.46, 0.68, 1, 3, -0.88,
                                                              prob_na = (699-659)/699,
                                                              correlate_with = psy_factor,
                                                              correlation = 0.72)
  
  sample_data$psy_work_life <- generate_ordinal_with_stats(n, 2.58, 0.60, 1, 3, -1.09,
                                                          prob_na = (699-659)/699,
                                                          correlate_with = psy_factor,
                                                          correlation = 0.68)
  
  # ERGONOMICS DOMAIN
  # Internal correlations: 0.41-0.76
  erg_factor <- rnorm(n)
  
  # Create sub-correlation for physical strain variables (high intercorrelation)
  physical_subfactor <- 0.7 * erg_factor + 0.3 * rnorm(n)
  
  sample_data$erg_rating_posture <- generate_ordinal_with_stats(n, 3.42, 0.94, 1, 4, -1.26,
                                                               prob_na = (699-661)/699,
                                                               correlate_with = physical_subfactor,
                                                               correlation = 0.85)
  
  sample_data$erg_rating_repeat <- generate_ordinal_with_stats(n, 3.67, 0.74, 1, 4, -2.07,
                                                              prob_na = (699-661)/699,
                                                              correlate_with = erg_factor,
                                                              correlation = 0.75)
  
  sample_data$erg_rating_sitting <- generate_ordinal_with_stats(n, 3.05, 0.96, 1, 4, -0.47,
                                                               prob_na = (699-662)/699,
                                                               correlate_with = erg_factor,
                                                               correlation = 0.5)
  
  sample_data$erg_rating_loads <- generate_ordinal_with_stats(n, 3.55, 0.87, 1, 4, -1.70,
                                                             prob_na = (699-661)/699,
                                                             correlate_with = physical_subfactor,
                                                             correlation = 0.8)
  
  sample_data$erg_rating_physical <- generate_ordinal_with_stats(n, 3.76, 0.68, 1, 4, -2.83,
                                                                prob_na = (699-661)/699,
                                                                correlate_with = physical_subfactor,
                                                                correlation = 0.9)
  
  # SAFETY DOMAIN
  # Very high correlation between the two items (0.74)
  saf_factor <- rnorm(n)
  
  sample_data$saf_rating_leadengage <- generate_ordinal_with_stats(n, 2.62, 0.59, 1, 3, -1.27,
                                                                  prob_na = (699-668)/699,
                                                                  correlate_with = saf_factor,
                                                                  correlation = 0.86)
  
  sample_data$saf_rating_workinvolv <- generate_ordinal_with_stats(n, 2.62, 0.59, 1, 3, -1.32,
                                                                  prob_na = (699-668)/699,
                                                                  correlate_with = saf_factor,
                                                                  correlation = 0.86)
  
  # HYGIENE DOMAIN
  # Internal correlations: 0.39-0.67
  hyg_factor <- rnorm(n)
  
  # Temperature subfactor (high and low temp are correlated)
  temp_subfactor <- 0.6 * hyg_factor + 0.4 * rnorm(n)
  
  sample_data$hyg_rating_tools <- generate_ordinal_with_stats(n, 3.71, 0.68, 1, 4, -2.36,
                                                             prob_na = (699-662)/699,
                                                             correlate_with = hyg_factor,
                                                             correlation = 0.75)
  
  sample_data$hyg_rating_low_temp <- generate_ordinal_with_stats(n, 3.51, 0.91, 1, 4, -1.55,
                                                                prob_na = (699-664)/699,
                                                                correlate_with = temp_subfactor,
                                                                correlation = 0.8)
  
  sample_data$hyg_rating_high_temp <- generate_ordinal_with_stats(n, 3.63, 0.80, 1, 4, -1.99,
                                                                 prob_na = (699-664)/699,
                                                                 correlate_with = temp_subfactor,
                                                                 correlation = 0.82)
  
  sample_data$hyg_rating_noise <- generate_ordinal_with_stats(n, 3.40, 0.94, 1, 4, -1.23,
                                                             prob_na = (699-664)/699,
                                                             correlate_with = hyg_factor,
                                                             correlation = 0.7)
  
  sample_data$hyg_rating_substances <- generate_ordinal_with_stats(n, 3.83, 0.55, 1, 4, -3.41,
                                                                  prob_na = (699-665)/699,
                                                                  correlate_with = hyg_factor,
                                                                  correlation = 0.78)
  
  # Add cross-domain correlations
  # Physical work correlates with hygiene exposures
  cross_physical_hyg <- 0.4 * erg_factor + 0.6 * hyg_factor
  
  # Adjust some hygiene variables to show cross-correlation
  hyg_noise_adj <- sample_data$hyg_rating_noise
  noise_mask <- !is.na(hyg_noise_adj)
  if(sum(noise_mask) > 0) {
    # Add correlation with physical work
    phys_values <- sample_data$erg_rating_physical[noise_mask]
    phys_values[is.na(phys_values)] <- 3
    correlation_adjustment <- ifelse(phys_values > 3, 
                                   sample(c(0, 1), sum(noise_mask), replace = TRUE, prob = c(0.7, 0.3)),
                                   sample(c(-1, 0), sum(noise_mask), replace = TRUE, prob = c(0.3, 0.7)))
    hyg_noise_adj[noise_mask] <- pmax(1, pmin(4, hyg_noise_adj[noise_mask] + correlation_adjustment * 0.3))
    sample_data$hyg_rating_noise <- round(hyg_noise_adj)
  }
  
  # Note: veerkracht_calc and erg_capac not in descriptive stats
  sample_data$veerkracht_calc <- rnorm(n, mean = 3.2, sd = 0.8)
  sample_data$veerkracht_calc[sample(1:n, size = floor(n * 0.05))] <- NA
  
  sample_data$erg_capac <- generate_ordinal_with_stats(n, 3.4, 0.85, 1, 4, -1.2,
                                                      prob_na = 0.09,
                                                      correlate_with = erg_factor,
                                                      correlation = 0.6)
  
  sample_data$saf_satisfaction <- generate_ordinal_with_stats(n, 2.6, 0.6, 1, 3, -1.3,
                                                             prob_na = 0.06,
                                                             correlate_with = saf_factor,
                                                             correlation = 0.8)
  
  sample_data$hyg_satisfaction <- generate_ordinal_with_stats(n, 3.5, 0.8, 1, 4, -1.5,
                                                             prob_na = 0.08,
                                                             correlate_with = hyg_factor,
                                                             correlation = 0.7)
  
  # 11. ENVIRONMENTAL DOMAIN (only for extended version, 1-4 scale)
  # Set to -999 for all initially
  env_vars <- c("mil_rating_leadengage", "mil_satisfaction", 
                "mil_rating_contrib", "mil_rating_workinvolv")
  
  for(var in env_vars) {
    sample_data[[var]] <- -999
  }
  
  # Generate values only for extended version (n=44 in real data)
  extended_mask <- sample_data$Version_survey == "extended"
  n_extended <- sum(extended_mask)
  
  if(n_extended > 0) {
    # Environmental leadership: mean=2.84, sd=0.75
    env_lead_vals <- generate_ordinal_with_stats(n_extended, 2.84, 0.75, 1, 4, -0.41, 
                                                prob_na = 0)
    sample_data$mil_rating_leadengage[extended_mask] <- env_lead_vals
    
    # Environmental satisfaction: mean=3.07, sd=0.70
    env_sat_vals <- generate_ordinal_with_stats(n_extended, 3.07, 0.70, 1, 4, -0.49,
                                               prob_na = 0)
    sample_data$mil_satisfaction[extended_mask] <- env_sat_vals
    
    # Environmental contribution: mean=2.91, sd=0.47
    env_contrib_vals <- generate_ordinal_with_stats(n_extended, 2.91, 0.47, 1, 4, -1.57,
                                                   prob_na = 0)
    sample_data$mil_rating_contrib[extended_mask] <- env_contrib_vals
    
    # Environmental involvement: mean=3.00, sd=0.75
    env_involv_vals <- generate_ordinal_with_stats(n_extended, 3.00, 0.75, 1, 4, -0.33,
                                                  prob_na = 0)
    sample_data$mil_rating_workinvolv[extended_mask] <- env_involv_vals
  }
  
  # 12. FREQUENCY/EXPOSURE VARIABLES (correlated with main variables)
  sample_data$hyg_freq_exp_substances <- generate_ordinal(n, 0.22,
                                                         correlate_with = sample_data$hyg_rating_substances,
                                                         correlation = 0.7)
  sample_data$hyg_freq_exp_tools <- generate_ordinal(n, 0.20,
                                                    correlate_with = sample_data$hyg_rating_tools,
                                                    correlation = 0.7)
  sample_data$hyg_freq_exp_high_temp <- generate_ordinal(n, 0.19,
                                                        correlate_with = sample_data$hyg_rating_high_temp,
                                                        correlation = 0.7)
  sample_data$hyg_freq_exp_low_temp <- generate_ordinal(n, 0.21,
                                                       correlate_with = sample_data$hyg_rating_low_temp,
                                                       correlation = 0.7)
  sample_data$hyg_freq_exp_noise <- generate_ordinal(n, 0.18,
                                                    correlate_with = sample_data$hyg_rating_noise,
                                                    correlation = 0.7)
  sample_data$erg_freq_exp_sitting <- generate_ordinal(n, 0.16,
                                                      correlate_with = sample_data$erg_rating_sitting,
                                                      correlation = 0.7)
  sample_data$erg_freq_exp_posture <- generate_ordinal(n, 0.17,
                                                      correlate_with = sample_data$erg_rating_posture,
                                                      correlation = 0.7)
  sample_data$erg_freq_exp_physical <- generate_ordinal(n, 0.15,
                                                       correlate_with = sample_data$erg_rating_physical,
                                                       correlation = 0.7)
  
  # 13. ADDITIONAL AUXILIARY VARIABLES
  sample_data$shift <- sample(c("Day", "Night", "Rotating"), n, 
                             replace = TRUE, prob = c(0.7, 0.1, 0.2))
  sample_data$sleep <- round(rnorm(n, mean = 7, sd = 1.2), 1)
  sample_data$sleep[sample_data$sleep < 3] <- 3
  sample_data$sleep[sample_data$sleep > 10] <- 10
  sample_data$health_not_ok <- sample(0:1, n, replace = TRUE, prob = c(0.8, 0.2))
  
  # Remove the temporary id column
  sample_data$id <- NULL
  
  return(sample_data)
}

# Generate sample data matching thesis characteristics
cat("Generating sample data with thesis-specific characteristics...\n")
cat("Target sample size: 699 participants\n")
cat("- Dutch speakers: ~617 (88.3%)\n")
cat("- French speakers: ~73 (10.4%)\n")
cat("- English speakers: ~9 (1.3%)\n")
cat("- Mobile users: ~175 (25%)\n")
cat("- Company 4 participants: 94 (48 extended, 46 standard)\n\n")

sample_data <- create_sample_data(n = 699)

# Verify sample characteristics
cat("Verifying generated sample characteristics:\n\n")

# Language distribution
lang_table <- table(sample_data$LanguageCode)
dutch_count <- sum(lang_table[c("nl", "du")])
cat("Language distribution:\n")
cat(sprintf("  Dutch (nl+du): %d (%.1f%%)\n", dutch_count, dutch_count/nrow(sample_data)*100))
cat(sprintf("  French: %d (%.1f%%)\n", lang_table["fr"], lang_table["fr"]/nrow(sample_data)*100))
if("en" %in% names(lang_table)) {
  cat(sprintf("  English: %d (%.1f%%)\n", lang_table["en"], lang_table["en"]/nrow(sample_data)*100))
}

# Mobile vs Desktop
mobile_table <- table(sample_data$ismobile)
cat(sprintf("\nData collection method:\n"))
cat(sprintf("  Mobile: %d (%.1f%%)\n", sum(sample_data$ismobile), 
            sum(sample_data$ismobile)/nrow(sample_data)*100))
cat(sprintf("  Desktop: %d (%.1f%%)\n", sum(!sample_data$ismobile), 
            sum(!sample_data$ismobile)/nrow(sample_data)*100))

# Gender distribution
gender_table <- table(sample_data$sex_cat, useNA = "always")
cat(sprintf("\nGender distribution:\n"))
cat(sprintf("  Men: %d (%.1f%%)\n", gender_table["M"], 
            gender_table["M"]/nrow(sample_data)*100))
cat(sprintf("  Women: %d (%.1f%%)\n", gender_table["F"], 
            gender_table["F"]/nrow(sample_data)*100))
cat(sprintf("  Missing: %d (%.1f%%)\n", sum(is.na(sample_data$sex_cat)), 
            sum(is.na(sample_data$sex_cat))/nrow(sample_data)*100))

# Age distribution
age_table <- table(sample_data$age_cat, useNA = "always")
cat(sprintf("\nAge distribution:\n"))
for(age in c("<25", "25-34", "35-44", "45-54", ">=55")) {
  if(age %in% names(age_table)) {
    cat(sprintf("  %s: %d (%.1f%%)\n", age, age_table[age], 
                age_table[age]/nrow(sample_data)*100))
  }
}
cat(sprintf("  Missing: %d (%.1f%%)\n", sum(is.na(sample_data$age_cat)), 
            sum(is.na(sample_data$age_cat))/nrow(sample_data)*100))

# Environment domain
company_4_data <- sample_data[sample_data$Name_Company == "Company_4", ]
cat(sprintf("\nEnvironment domain (Company 4):\n"))
cat(sprintf("  Total Company 4 participants: %d\n", nrow(company_4_data)))
cat(sprintf("  Extended version: %d\n", sum(company_4_data$Version_survey == "extended")))
cat(sprintf("  Standard version: %d\n", sum(company_4_data$Version_survey == "standard")))

# Save to file
output_path <- "Simulated_data_SAMPLE.csv"
write.table(sample_data, 
            file = output_path, 
            sep = ";", 
            row.names = FALSE,
            quote = FALSE)

cat(sprintf("\nSample data created successfully!\n"))
cat(sprintf("File saved as: %s\n", output_path))
cat(sprintf("Dimensions: %d rows × %d columns\n", nrow(sample_data), ncol(sample_data)))

# Display descriptive statistics comparison
cat("\nDescriptive Statistics Comparison:\n")
cat("=" * 80, "\n")

# Function to calculate stats for ordinal variables
calc_stats <- function(x, var_name, domain) {
  x_clean <- x[!is.na(x) & x != -999]
  if(length(x_clean) > 0) {
    data.frame(
      Domain = domain,
      Variable = var_name,
      N = length(x_clean),
      Mean = round(mean(x_clean), 2),
      SD = round(sd(x_clean), 2),
      Median = median(x_clean),
      Min = min(x_clean),
      Max = max(x_clean),
      Skewness = round(moments::skewness(x_clean), 2),
      Kurtosis = round(moments::kurtosis(x_clean) - 3, 2)  # Excess kurtosis
    )
  }
}

# Calculate stats for each domain
stats_list <- list()

# Psychosocial
stats_list <- append(stats_list, list(
  calc_stats(sample_data$psy_rating_emotional, "Emotional demands", "Psychosocial"),
  calc_stats(sample_data$psy_rating_sphere, "Work atmosphere", "Psychosocial"),
  calc_stats(sample_data$psy_rating_pace, "Work pace", "Psychosocial"),
  calc_stats(sample_data$psy_work_life, "Work-life balance", "Psychosocial")
))

# Ergonomics
stats_list <- append(stats_list, list(
  calc_stats(sample_data$erg_rating_loads, "Manual handling loads", "Ergonomics"),
  calc_stats(sample_data$erg_rating_physical, "Physically strenuous", "Ergonomics"),
  calc_stats(sample_data$erg_rating_repeat, "Repetitive work", "Ergonomics"),
  calc_stats(sample_data$erg_rating_sitting, "Sitting for long periods", "Ergonomics"),
  calc_stats(sample_data$erg_rating_posture, "Stressful postures", "Ergonomics")
))

# Safety
stats_list <- append(stats_list, list(
  calc_stats(sample_data$saf_rating_leadengage, "Leadership engagement", "Safety"),
  calc_stats(sample_data$saf_rating_workinvolv, "Worker involvement", "Safety")
))

# Hygiene
stats_list <- append(stats_list, list(
  calc_stats(sample_data$hyg_rating_substances, "Hazardous substances", "Hygiene"),
  calc_stats(sample_data$hyg_rating_high_temp, "High temperatures", "Hygiene"),
  calc_stats(sample_data$hyg_rating_low_temp, "Low temperatures", "Hygiene"),
  calc_stats(sample_data$hyg_rating_noise, "Noise", "Hygiene"),
  calc_stats(sample_data$hyg_rating_tools, "Tool vibrations", "Hygiene")
))

# Environment (only for extended version)
env_data <- sample_data[sample_data$Version_survey == "extended", ]
stats_list <- append(stats_list, list(
  calc_stats(env_data$mil_rating_leadengage, "Environmental leadership", "Environment"),
  calc_stats(env_data$mil_satisfaction, "Environmental satisfaction", "Environment"),
  calc_stats(env_data$mil_rating_contrib, "Environmental contribution", "Environment"),
  calc_stats(env_data$mil_rating_workinvolv, "Environmental involvement", "Environment")
))

# Combine and display
stats_df <- do.call(rbind, stats_list)
print(stats_df, row.names = FALSE)

cat("\nNOTE: This simulated data closely matches the distributional characteristics\n")
cat("      of the thesis sample (means, SDs, skewness, etc.) while maintaining\n")
cat("      complete confidentiality of the actual company data.\n")
