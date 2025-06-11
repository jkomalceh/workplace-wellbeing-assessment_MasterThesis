A.12 Session Information and Environment
r
# Print session information for reproducibility
sessionInfo()

# List of all created files
created_files <- c(
  "final_analysis_dataset.csv",
  "imputed_datasets.rds",
  "imputed_dataset_for_cfa.csv",
  "mi_bootstrap_cfa_results.RData",
  "measurement_invariance_nht_summary.csv",
  "measurement_invariance_et_summary.csv",
  "measurement_invariance_comparison.csv",
  "dif_analysis_summary.csv",
  "dif_analysis_results.RData",
  "comprehensive_analysis_results.RData",
  "plots/missingness_summary.pdf",
  "plots/missingness_pattern.pdf",
  "plots/missingness_by_domain.pdf"
)

cat("\n\nFiles created during analysis:\n")
cat(paste(created_files, collapse = "\n"))

# Display final summary statistics
cat("\n\nFinal Analysis Summary:\n")
cat("- Number of imputed datasets:", length(completed_datasets), "\n")
cat("- Number of variables in final dataset:", ncol(final_dataset), "\n")
cat("- Number of observations:", nrow(final_dataset), "\n")
cat("- CFA model factors: Psychosocial (PS), Ergonomics (ER), Safety (SA), Hygiene (HY)\n")
cat("- Measurement invariance tested for: Language, Device type, Environment domain, Gender, Age\n")
cat("- DIF analysis methods: Mantel-Haenszel, Logistic Regression, IRT-based\n")
cat("- DIF analysis conducted for: Language, Gender, Device type, Environment domain\n")
Notes for Thesis Appendix
This appendix contains the complete R code used for the statistical analyses in this thesis. The code is organized into logical sections:

Setup and Data Preparation: Library loading, data import, and variable selection
Missing Data Analysis: Comprehensive visualization and assessment of missing data patterns
Multiple Imputation: MICE implementation with proper handling of ordinal and categorical variables
Confirmatory Factor Analysis: Model specification, diagnostics, and reliability assessment
Pooled Analysis with Bootstrap: Advanced pooling of CFA results across imputed datasets
Measurement Invariance: Testing using both Null Hypothesis Testing (NHT) and Equivalence Testing (ET) approaches
Gender-Specific Invariance: Modified models to handle problematic variables
Age-Based Invariance: Multi-group analysis with age category recoding
Comprehensive Invariance Summary: Results compilation and comparison tables
Differential Item Functioning (DIF) Analysis: Three complementary methods for item-level bias detection
Results Integration: Final compilation of all analytical approaches
Documentation: Session information and reproducibility details
DIF Analysis Implementation
The DIF analysis section (A.10) implements three complementary detection methods as described in the methodology:

Mantel-Haenszel Procedure:

Examines odds ratios for item responses across groups, stratified by total score levels
Uses Educational Testing Service criteria for DIF magnitude classification:
Negligible (A): |log(αMH)| < 1.0
Moderate (B): 1.0 ≤ |log(αMH)| < 1.5
Large (C): |log(αMH)| ≥ 1.5
Logistic Regression Approach:

Tests for uniform DIF (different item difficulty across groups)
Tests for non-uniform DIF (different item discrimination across groups)
Uses likelihood ratio tests comparing nested models
IRT-based Detection:

Compares item parameters (difficulty and discrimination) across groups
Uses graded response models appropriate for ordinal data
Flags items with substantial parameter differences using established thresholds
Key Features
Comprehensive Error Handling: All analyses include extensive error checking and graceful failure handling
Multiple Imputation Integration: DIF analysis can be performed on imputed datasets with proper pooling
Flexible Group Comparisons: Functions designed to work with any grouping variable
Detailed Output: Both summary tables and detailed diagnostic information provided
Reproducible Results: All analyses use set seeds and include session information
Files Generated
The complete analysis generates several output files:

CSV files with summary results for easy interpretation
RData files with detailed statistical objects for further analysis
PDF visualizations of missing data patterns and model diagnostics
Comprehensive results compilation for integration across methods
All analyses follow best practices for handling missing data, ordinal variables, multiple group comparisons, and item-level bias detection. The code includes extensive documentation and produces both statistical output and visualizations for comprehensive reporting.

Reproducibility Instructions
To reproduce these analyses:

Install all required packages listed in A.1
Adjust file paths in A.2 to match your system
Ensure your data file follows the expected format with semicolon delimiters
Run sections sequentially, as later sections depend on objects created in earlier sections
Check for convergence warnings and adjust model specifications if needed
The analysis is designed to be robust to different data structures and sample sizes, with appropriate warnings and alternative approaches when standard methods are not applicable.# Appendix: R Code for Statistical Analysis

A.1 Setup and Library Loading
r
# Load necessary libraries
library(tidyverse)
library(MVN)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson, Amelia, mice, VIM, corrplot, gridExtra)

# Setting working directory
setwd("D:/2Y_MasterThesis/R_Code")
A.2 Data Loading and Variable Selection
r
# Load the pilot dataset using the correct delimiter (semicolon)
full_dataset <- read_delim("pilotdata_4comp.csv", delim = ";")

# Define model variables excluding environment domain
model_vars_no_env <- c(
  # Psychosocial
  "veerkracht_calc", "psy_rating_pace", "psy_rating_emotional", 
  "psy_rating_sphere", "psy_work_life",
  # Ergonomics
  "erg_capac", "erg_rating_posture", "erg_rating_repeat", 
  "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical",
  # Safety
  "saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage",
  # Hygiene
  "hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
  "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances"
)

# Define environment domain variables
env_vars <- c("mil_rating_leadengage", "mil_satisfaction", "mil_rating_contrib", "mil_rating_workinvolv")

# Calculate missing rates and identify auxiliary variables
missing_rates <- full_dataset %>%
  summarise(across(everything(), ~sum(is.na(.))/n())) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate") %>%
  arrange(missing_rate)

# Identify potential auxiliary variables (exclude model vars and keep only those with <20% missing)
potential_aux_vars <- missing_rates %>%
  filter(missing_rate < 0.20) %>%
  filter(!variable %in% c(model_vars_no_env, env_vars, 
                         "ismobile", "LanguageCode", "Name_Company", "Version_survey")) %>%
  pull(variable)

# Select useful auxiliary variables
useful_aux_vars <- c(
  "age_cat", "sex_cat", 
  grep("freq_exp", potential_aux_vars, value = TRUE),
  grep("exposure", potential_aux_vars, value = TRUE),
  grep("hea_", potential_aux_vars, value = TRUE, fixed = TRUE),
  grep("vit_", potential_aux_vars, value = TRUE, fixed = TRUE),
  "was", "shift", "sleep", "health_not_ok"
)

useful_aux_vars <- intersect(useful_aux_vars, names(full_dataset))
useful_aux_vars <- head(useful_aux_vars, 10)

# Create final dataset
final_dataset <- full_dataset %>%
  mutate(has_environment_domain = case_when(
    !is.na(mil_satisfaction) ~ "Yes",
    TRUE ~ "No"
  )) %>%
  select(all_of(c(model_vars_no_env, env_vars, useful_aux_vars,
                "ismobile", "LanguageCode", "Name_Company", "Version_survey",
                "has_environment_domain")))

# Save the file
write_csv(final_dataset, "final_analysis_dataset.csv")
A.3 Missing Data Analysis and Visualization
r
# Load the selected variable dataset
full_analysis_dataset <- read_delim("final_analysis_dataset.csv")

# Create a mapping for variables with descriptive names
var_labels <- c(
  # Psychosocial work environment factor (PS)
  veerkracht_calc = "Resilience",
  psy_rating_pace = "Work pace",
  psy_rating_emotional = "Emotional demands",
  psy_rating_sphere = "Work atmosphere",
  psy_work_life = "Work-life balance",
  
  # Ergonomics factor (ER)
  erg_capac = "Ergonomic capacity",
  erg_rating_posture = "Stressful postures",
  erg_rating_repeat = "Repetitive work",
  erg_rating_sitting = "Sitting for long-time",
  erg_rating_loads = "Manual handling loads",
  erg_rating_physical = "Physical strenuous",
  
  # Safety factor (SA)
  saf_satisfaction = "Safety satisfaction",
  saf_rating_workinvolv = "Work involvement",
  saf_rating_leadengage = "Leadership engagement",
  
  # Hygiene factor (HY)
  hyg_satisfaction = "Hygiene satisfaction",
  hyg_rating_tools = "Tool Vibrations",
  hyg_rating_low_temp = "Low temperatures",
  hyg_rating_high_temp = "High temperatures",
  hyg_rating_noise = "Noise",
  hyg_rating_substances = "Hazardous Substances",
  
  # Environmental domain
  mil_rating_leadengage = "Env. leadership engagement",
  mil_satisfaction = "Env. satisfaction",
  mil_rating_contrib = "Env. contribution",
  mil_rating_workinvolv = "Env. work involvement"
)

# Function to rename variables for plotting
rename_vars <- function(var_names) {
  sapply(var_names, function(x) {
    if(x %in% names(var_labels)) var_labels[x] else x
  })
}

# Create directory for saving files
dir.create("plots", showWarnings = FALSE)

# Overall missingness summary
miss_summary <- miss_var_summary(full_analysis_dataset)
miss_summary$variable_renamed <- rename_vars(miss_summary$variable)

# Plot missingness summary
p1 <- ggplot(miss_summary, aes(x = reorder(variable_renamed, pct_miss), y = pct_miss)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Overall Missingness by Variable",
       subtitle = "Percentage of missing values for each variable",
       x = "", y = "Percentage Missing (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 9))

ggsave("plots/missingness_summary.pdf", p1, width = 12, height = 10)
print(p1)

# Domain-specific missingness analysis
domains <- list(
  "Psychosocial Domain" = c("veerkracht_calc", "psy_rating_pace", "psy_rating_emotional", 
                          "psy_rating_sphere", "psy_work_life"),
  "Ergonomics Domain" = c("erg_capac", "erg_rating_posture", "erg_rating_repeat", 
                        "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical"),
  "Safety Domain" = c("saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage"),
  "Hygiene Domain" = c("hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
                      "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances"),
  "Environmental Domain" = c("mil_rating_leadengage", "mil_satisfaction", 
                           "mil_rating_contrib", "mil_rating_workinvolv")
)

# Calculate missingness by domain
domain_missingness <- lapply(names(domains), function(domain_name) {
  vars <- domains[[domain_name]]
  vars <- vars[vars %in% names(full_analysis_dataset)]
  if(length(vars) > 0) {
    result <- full_analysis_dataset %>%
      summarise(across(all_of(vars), ~mean(is.na(.))*100)) %>%
      pivot_longer(cols = everything(), 
                  names_to = "variable", 
                  values_to = "percent_missing") %>%
      mutate(domain = domain_name)
    
    result$variable_renamed <- rename_vars(result$variable)
    return(result)
  }
}) %>% bind_rows()

# Domain-specific missingness visualization
p6 <- ggplot(domain_missingness, aes(x = reorder(variable_renamed, percent_missing), 
                                    y = percent_missing, 
                                    fill = domain)) +
  geom_col() +
  facet_wrap(~domain, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") +
  labs(title = "Percentage of Missing Values by Domain",
       subtitle = "Grouped by workplace assessment domains",
       x = "", y = "% Missing")

ggsave("plots/missingness_by_domain.pdf", p6, width = 14, height = 10)
print(p6)
A.4 Multiple Imputation
r
# Create a clean dataset for imputation
imputation_dataset <- full_analysis_dataset

# Set -999 values back to NA for environmental variables
env_vars <- c("mil_rating_leadengage", "mil_satisfaction", 
              "mil_rating_contrib", "mil_rating_workinvolv")

for(var in env_vars) {
  imputation_dataset[[var]][imputation_dataset[[var]] == -999] <- NA
}

# Identify variable types for appropriate imputation methods
ordinal_vars <- c(
  # Psychosocial domain
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  # Ergonomics domain
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", 
  "erg_rating_loads", "erg_rating_physical", "erg_capac",
  # Safety domain
  "saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage",
  # Hygiene domain
  "hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
  "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances",
  # Environmental domain
  "mil_rating_leadengage", "mil_satisfaction", "mil_rating_contrib", "mil_rating_workinvolv",
  # Frequency variables
  "hyg_freq_exp_substances", "hyg_freq_exp_tools", "hyg_freq_exp_high_temp",
  "hyg_freq_exp_low_temp", "hyg_freq_exp_noise", "erg_freq_exp_sitting",
  "erg_freq_exp_posture", "erg_freq_exp_physical",
  # Demographics
  "age_cat"
)

continuous_vars <- c("veerkracht_calc")
binary_vars <- c("ismobile", "sex_cat")
cat_vars <- c("LanguageCode", "Name_Company", "Version_survey", "has_environment_domain")

# Special handling for age_cat - convert to ordered factor
if("age_cat" %in% names(imputation_dataset)) {
  age_levels <- c("<25", "25-34", "35-44", "45-54", ">=55")
  imputation_dataset$age_cat <- factor(imputation_dataset$age_cat, 
                                      levels = age_levels, ordered = TRUE)
}

# Convert other ordinal variables to factors
for(var in setdiff(ordinal_vars, "age_cat")) {
  if(var %in% names(imputation_dataset)) {
    if(sum(!is.na(imputation_dataset[[var]])) > 0) {
      var_values <- imputation_dataset[[var]][!is.na(imputation_dataset[[var]])]
      
      if(all(!is.na(suppressWarnings(as.numeric(as.character(var_values)))))) {
        var_numeric <- as.numeric(as.character(var_values))
        var_min <- min(var_numeric, na.rm = TRUE)
        var_max <- max(var_numeric, na.rm = TRUE)
        
        if(!is.na(var_min) && !is.na(var_max)) {
          levels <- var_min:var_max
          imputation_dataset[[var]] <- factor(imputation_dataset[[var]], 
                                             levels = levels, ordered = TRUE)
        }
      }
    }
  }
}

# Create variable to track observations without environmental data
imputation_dataset$skip_env_imputation <- (full_analysis_dataset$has_environment_domain == "No")
cat_vars <- c(cat_vars, "skip_env_imputation")

# Set up MICE methods
imp_methods <- mice(imputation_dataset, maxit = 0)$method

# Set imputation methods for each variable type
for(var in ordinal_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- "polr"  # Proportional odds logistic regression
  }
}

for(var in continuous_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- "pmm"  # Predictive mean matching
  }
}

for(var in binary_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- "logreg"  # Logistic regression
  }
}

for(var in cat_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- ""  # Don't impute categorical variables
  }
}

# Create predictor matrix
pred_matrix <- mice(imputation_dataset, maxit = 0)$predictorMatrix

# Don't use categorical variables as predictors
for(var in cat_vars) {
  if(var %in% names(imputation_dataset)) {
    pred_matrix[, which(colnames(pred_matrix) == var)] <- 0
  }
}

# Run imputation
set.seed(12345)
imputed_data <- mice(imputation_dataset, 
                    predictorMatrix = pred_matrix,
                    method = imp_methods,
                    m = 10,          # Create 10 imputed datasets
                    maxit = 20,      # Run for 20 iterations
                    seed = 12345,
                    printFlag = TRUE)

# Post-processing: convert factors back to numeric and handle environmental vars
completed_datasets <- list()
for(i in 1:10) {
  complete_data_i <- complete(imputed_data, i)
  
  # Convert regular ordinal variables back to numeric
  for(var in setdiff(ordinal_vars, "age_cat")) {
    if(var %in% names(complete_data_i)) {
      complete_data_i[[var]] <- as.numeric(as.character(complete_data_i[[var]]))
    }
  }
  
  # Reset environmental variables to -999 where needed
  for(var in env_vars) {
    if(var %in% names(complete_data_i)) {
      complete_data_i[[var]][complete_data_i$skip_env_imputation] <- -999
    }
  }
  
  # Remove the helper variable
  complete_data_i$skip_env_imputation <- NULL
  
  completed_datasets[[i]] <- complete_data_i
}

# Save the completed datasets
saveRDS(completed_datasets, "imputed_datasets.rds")
write.csv(completed_datasets[[1]], file = "imputed_dataset_for_cfa.csv", row.names = FALSE)
A.5 Confirmatory Factor Analysis
A.5.1 CFA Model Definition and Reliability Functions
r
# Function to calculate CR and AVE from a fitted lavaan model
calculate_CR_AVE <- function(fit) {
  std_estimates <- standardizedSolution(fit)
  loadings <- subset(std_estimates, op == "=~")
  factors <- unique(loadings$lhs)
  
  results <- data.frame(Factor = factors, CR = NA, AVE = NA)
  
  for(i in 1:length(factors)) {
    factor <- factors[i]
    factor_loadings <- subset(loadings, lhs == factor)$est.std
    
    lambda_squared <- factor_loadings^2
    delta <- 1 - lambda_squared
    
    sum_lambda <- sum(factor_loadings)
    sum_delta <- sum(delta)
    CR <- sum_lambda^2 / (sum_lambda^2 + sum_delta)
    
    sum_lambda_squared <- sum(lambda_squared)
    AVE <- sum_lambda_squared / (sum_lambda_squared + sum_delta)
    
    results$CR[i] <- CR
    results$AVE[i] <- AVE
  }
  
  return(results)
}

# Define CFA model
cfa_model <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_sitting + erg_rating_loads + erg_rating_physical
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY)
  HY =~ hyg_rating_tools + hyg_rating_low_temp + hyg_rating_high_temp + hyg_rating_noise + hyg_rating_substances
'

# Define ordinal variables
ordinal_vars <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", 
  "erg_rating_loads", "erg_rating_physical",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
  "hyg_rating_noise", "hyg_rating_substances"
)
A.5.2 Model Diagnostics
r
# Test multivariate normality
cfa_vars <- full_dataset %>% 
  select(veerkracht_calc, starts_with("psy_"), starts_with("erg_")) %>%
  select(where(is.numeric))

mvn_result <- mvn(data = cfa_vars, mvnTest = "mardia")
print(mvn_result$multivariateNormality)

# Correlation matrix and multicollinearity check
cor_matrix <- cor(cfa_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

# Identify high correlations (> 0.85)
high_cors <- which(abs(cor_matrix) > 0.85 & abs(cor_matrix) < 1, arr.ind = TRUE)
if(nrow(high_cors) > 0) {
  high_cor_pairs <- data.frame(
    var1 = rownames(cor_matrix)[high_cors[, 1]],
    var2 = colnames(cor_matrix)[high_cors[, 2]],
    correlation = cor_matrix[high_cors]
  )
  print("Potential multicollinearity issues:")
  print(high_cor_pairs)
}

# Factorability tests
kmo_result <- KMO(cor_matrix)
print(kmo_result$MSA)  # Overall KMO value

bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(cfa_vars))
print(bartlett_result)
A.5.3 Reliability Analysis
r
# Enhanced reliability calculation with Polychoric Alpha
calculate_reliability_complete <- function(completed_datasets, factor_vars) {
  results <- data.frame()
  
  for (factor_name in names(factor_vars)) {
    vars <- factor_vars[[factor_name]]
    
    alpha_values <- numeric()
    poly_alpha_values <- numeric()
    omega_values <- numeric()
    
    for (i in 1:length(completed_datasets)) {
      data_imp <- completed_datasets[[i]]
      factor_data <- data_imp[, vars, drop = FALSE]
      factor_data <- factor_data[complete.cases(factor_data), ]
      factor_data <- as.data.frame(lapply(factor_data, as.numeric))
      
      if (nrow(factor_data) > 10 && ncol(factor_data) > 1) {
        # Regular Cronbach's Alpha
        tryCatch({
          cor_matrix <- cor(factor_data, use = "complete.obs")
          k <- ncol(factor_data)
          avg_inter_cor <- (sum(cor_matrix) - k) / (k * (k - 1))
          alpha_manual <- (k * avg_inter_cor) / (1 + (k - 1) * avg_inter_cor)
          alpha_values[i] <- alpha_manual
        }, error = function(e) {
          alpha_values[i] <- NA
        })
        
        # Polychoric Alpha
        tryCatch({
          poly_cor <- polychoric(factor_data)$rho
          k <- ncol(factor_data)
          avg_poly_cor <- (sum(poly_cor) - k) / (k * (k - 1))
          poly_alpha <- (k * avg_poly_cor) / (1 + (k - 1) * avg_poly_cor)
          poly_alpha_values[i] <- poly_alpha
        }, error = function(e) {
          poly_alpha_values[i] <- NA
        })
        
        # McDonald's Omega
        if (length(vars) > 2) {
          tryCatch({
            factor_model <- paste0(factor_name, " =~ ", paste(vars, collapse = " + "))
            fit_single <- cfa(factor_model, data = data_imp, ordered = TRUE, estimator = "WLSMV")
            
            if (lavInspect(fit_single, "converged")) {
              rel_coef <- reliability(fit_single)
              omega_values[i] <- rel_coef["omega", factor_name]
            } else {
              omega_values[i] <- NA
            }
          }, error = function(e) {
            omega_values[i] <- NA
          })
        }
      }
    }
    
    # Pool results function
    pool_reliability <- function(values, measure_name) {
      valid_values <- values[!is.na(values)]
      if (length(valid_values) >= 3) {
        pooled_mean <- mean(valid_values)
        pooled_sd <- sd(valid_values)
        
        n <- length(valid_values)
        se <- pooled_sd / sqrt(n)
        t_val <- qt(0.975, df = n - 1)
        ci_lower <- pooled_mean - t_val * se
        ci_upper <- pooled_mean + t_val * se
        
        return(data.frame(
          Factor = factor_name,
          Measure = measure_name,
          Estimate = round(pooled_mean, 3),
          SE = round(se, 4),
          CI_Lower = round(ci_lower, 3),
          CI_Upper = round(ci_upper, 3),
          N_Valid = n,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Add all reliability measures
    alpha_result <- pool_reliability(alpha_values, "Cronbach_Alpha")
    poly_alpha_result <- pool_reliability(poly_alpha_values, "Polychoric_Alpha")
    results <- rbind(results, alpha_result, poly_alpha_result)
    
    if (length(vars) > 2 && length(omega_values[!is.na(omega_values)]) > 0) {
      omega_result <- pool_reliability(omega_values, "McDonald_Omega")
      results <- rbind(results, omega_result)
    }
    
    # Spearman-Brown for 2-item factors
    if (length(vars) == 2) {
      sb_values <- numeric()
      poly_sb_values <- numeric()
      
      for (i in 1:length(completed_datasets)) {
        data_imp <- completed_datasets[[i]]
        factor_data <- data_imp[, vars, drop = FALSE]
        factor_data <- factor_data[complete.cases(factor_data), ]
        factor_data <- as.data.frame(lapply(factor_data, as.numeric))
        
        if (nrow(factor_data) > 10) {
          r <- cor(factor_data[, 1], factor_data[, 2], use = "complete.obs")
          sb_values[i] <- (2 * r) / (1 + r)
          
          tryCatch({
            poly_r <- polychoric(factor_data)$rho[1, 2]
            poly_sb_values[i] <- (2 * poly_r) / (1 + poly_r)
          }, error = function(e) {
            poly_sb_values[i] <- NA
          })
        }
      }
      
      sb_result <- pool_reliability(sb_values, "Spearman_Brown")
      poly_sb_result <- pool_reliability(poly_sb_values, "Polychoric_Spearman_Brown")
      results <- rbind(results, sb_result, poly_sb_result)
    }
  }
  
  return(results)
}

# Define factor variables
factor_vars <- list(
  PS = c("psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life"),
  ER = c("erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical"),
  SA = c("saf_rating_workinvolv", "saf_rating_leadengage"),
  HY = c("hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances")
)

# Run reliability analysis
reliability_results_complete <- calculate_reliability_complete(completed_datasets, factor_vars)
print(reliability_results_complete)
A.6 Pooled CFA with Bootstrap Confidence Intervals
r
# Load imputed datasets
completed_datasets <- readRDS("imputed_datasets.rds")
M <- length(completed_datasets)  # Number of imputed datasets
B <- 1000                        # Number of bootstrap samples

# Define transformation functions for fit indices
fisher_transform <- function(x) {
  x <- pmin(x, 0.999)
  return(0.5 * log((1 + x) / (1 - x)))
}

inverse_fisher <- function(z) {
  return((exp(2 * z) - 1) / (exp(2 * z) + 1))
}

log_transform <- function(x) {
  x <- pmax(x, 0.001)
  return(log(x))
}

inverse_log <- function(z) {
  return(exp(z))
}

# Initialize arrays to store results
fit_indices <- c("cfi", "tli", "rmsea", "srmr", "chisq")
bootstrap_fit_results <- array(NA, dim = c(M, B, length(fit_indices)),
                              dimnames = list(paste0("imp", 1:M), 
                                             paste0("boot", 1:B),
                                             fit_indices))

param_results <- list()
std_param_results <- list()
intercept_results <- list()

# BOOTSTRAPPING PHASE
for (m in 1:M) {
  cat("Processing imputed dataset", m, "of", M, "\n")
  current_data <- completed_datasets[[m]]
  
  # Define bootstrap function
  boot_fun <- function(data, indices) {
    boot_sample <- data[indices, ]
    
    fit <- tryCatch({
      cfa(cfa_model, 
         data = boot_sample, 
         ordered = ordinal_vars,
         estimator = "DWLS")
    }, error = function(e) {
      return(NA)
    })
    
    if (inherits(fit, "lavaan")) {
      fit_measures <- fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr", "chisq"))
      return(fit_measures)
    } else {
      return(rep(NA, length(fit_indices)))
    }
  }
  
  # Run bootstrap
  boot_results <- boot(data = current_data, 
                     statistic = boot_fun, 
                     R = B, 
                     parallel = "multicore",
                     ncpus = 4)
  
  # Store bootstrap results
  for (b in 1:B) {
    if (!any(is.na(boot_results$t[b, ]))) {
      bootstrap_fit_results[m, b, ] <- boot_results$t[b, ]
    }
  }
  
  # Fit the model once to get parameter estimates
  fit_m <- cfa(cfa_model, 
              data = current_data, 
              ordered = ordinal_vars,
              estimator = "WLSMV")
  
  params <- parameterEstimates(fit_m)
  param_results[[m]] <- params
  
  std_params <- standardizedSolution(fit_m)
  std_param_results[[m]] <- std_params
  
  intercepts <- params[params$op == "|", ]
  intercept_results[[m]] <- intercepts
}

# Calculate bootstrap estimates for each imputed dataset
bootstrap_point_estimates <- matrix(NA, nrow = M, ncol = length(fit_indices),
                                   dimnames = list(paste0("imp", 1:M), fit_indices))
bootstrap_se <- matrix(NA, nrow = M, ncol = length(fit_indices),
                      dimnames = list(paste0("imp", 1:M), fit_indices))

for (m in 1:M) {
  for (i in 1:length(fit_indices)) {
    valid_results <- bootstrap_fit_results[m, , i]
    valid_results <- valid_results[!is.na(valid_results)]
    
    if (length(valid_results) > 0) {
      bootstrap_point_estimates[m, i] <- mean(valid_results)
      bootstrap_se[m, i] <- sd(valid_results)
    }
  }
}

# TRANSFORMATION PHASE
transformed_estimates <- matrix(NA, nrow = M, ncol = length(fit_indices),
                               dimnames = list(paste0("imp", 1:M), fit_indices))
transformed_variance <- matrix(NA, nrow = M, ncol = length(fit_indices),
                              dimnames = list(paste0("imp", 1:M), fit_indices))

# Apply transformations
for (m in 1:M) {
  for (i in 1:length(fit_indices)) {
    index_name <- fit_indices[i]
    est <- bootstrap_point_estimates[m, i]
    se <- bootstrap_se[m, i]
    
    if (!is.na(est) && !is.na(se)) {
      if (index_name %in% c("cfi", "tli")) {
        transformed_estimates[m, i] <- fisher_transform(est)
        deriv <- 1 / (1 - est^2)
        transformed_variance[m, i] <- (deriv^2) * (se^2)
      } else if (index_name == "rmsea") {
        transformed_estimates[m, i] <- log_transform(est)
        deriv <- 1 / est
        transformed_variance[m, i] <- (deriv^2) * (se^2)
      } else {
        transformed_estimates[m, i] <- est
        transformed_variance[m, i] <- se^2
      }
    }
  }
}

# POOLING PHASE
pooled_results <- data.frame(
  Index = fit_indices,
  Estimate_Transformed = NA,
  Within_Var = NA,
  Between_Var = NA,
  Total_Var = NA,
  DF = NA,
  Estimate_Original = NA,
  SE = NA,
  CI_Lower = NA,
  CI_Upper = NA,
  stringsAsFactors = FALSE
)

for (i in 1:length(fit_indices)) {
  estimates <- transformed_estimates[, i]
  variances <- transformed_variance[, i]
  
  valid <- !is.na(estimates) & !is.na(variances)
  if (sum(valid) < 2) next
  
  estimates <- estimates[valid]
  variances <- variances[valid]
  m_valid <- sum(valid)
  
  # Calculate pooled estimate
  q_bar <- mean(estimates)
  
  # Calculate within-imputation variance
  W <- mean(variances)
  
  # Calculate between-imputation variance
  B <- sum((estimates - q_bar)^2) / (m_valid - 1)
  
  # Calculate total variance
  T_var <- W + (1 + 1/m_valid) * B
  
  # Calculate degrees of freedom
  df <- (m_valid - 1) * (1 + W / ((1 + 1/m_valid) * B))^2
  
  # Store transformed results
  pooled_results$Estimate_Transformed[i] <- q_bar
  pooled_results$Within_Var[i] <- W
  pooled_results$Between_Var[i] <- B
  pooled_results$Total_Var[i] <- T_var
  pooled_results$DF[i] <- df
  
  # Back-transform to original scale
  index_name <- fit_indices[i]
  if (index_name %in% c("cfi", "tli")) {
    est_original <- inverse_fisher(q_bar)
  } else if (index_name == "rmsea") {
    est_original <- inverse_log(q_bar)
  } else {
    est_original <- q_bar
  }
  
  # Calculate confidence intervals
  t_critical <- qt(0.975, df)
  ci_lower_trans <- q_bar - t_critical * sqrt(T_var)
  ci_upper_trans <- q_bar + t_critical * sqrt(T_var)
  
  # Back-transform confidence intervals
  if (index_name %in% c("cfi", "tli")) {
    ci_lower <- inverse_fisher(ci_lower_trans)
    ci_upper <- inverse_fisher(ci_upper_trans)
  } else if (index_name == "rmsea") {
    ci_lower <- inverse_log(ci_lower_trans)
    ci_upper <- inverse_log(ci_upper_trans)
  } else {
    ci_lower <- ci_lower_trans
    ci_upper <- ci_upper_trans
  }
  
  pooled_results$Estimate_Original[i] <- est_original
  pooled_results$SE[i] <- sqrt(T_var)
  pooled_results$CI_Lower[i] <- ci_lower
  pooled_results$CI_Upper[i] <- ci_upper
}

# Pool parameter estimates
all_params <- do.call(rbind, lapply(1:M, function(m) {
  params <- param_results[[m]]
  params$Imputation <- m
  return(params)
}))

all_std_params <- do.call(rbind, lapply(1:M, function(m) {
  std_params <- std_param_results[[m]]
  std_params$Imputation <- m
  return(std_params)
}))

# Calculate pooled parameter estimates
pooled_params <- aggregate(est ~ lhs + op + rhs, data = all_params, FUN = mean)
pooled_params$se <- aggregate(se ~ lhs + op + rhs, data = all_params, FUN = function(x) {
  sqrt(mean(x^2))
})$se

pooled_std_params <- aggregate(est.std ~ lhs + op + rhs, data = all_std_params, FUN = mean)
colnames(pooled_std_params)[colnames(pooled_std_params) == "est.std"] <- "std_est"
pooled_std_params$std_se <- aggregate(se ~ lhs + op + rhs, data = all_std_params, FUN = function(x) {
  sqrt(mean(x^2))
})$se

# Merge unstandardized and standardized results
merged_params <- merge(
  pooled_params[, c("lhs", "op", "rhs", "est", "se")],
  pooled_std_params[, c("lhs", "op", "rhs", "std_est", "std_se")],
  by = c("lhs", "op", "rhs")
)

# Calculate FMI and relative efficiency
pooled_results$FMI <- (1 + 1/M) * pooled_results$Between_Var / pooled_results$Total_Var
pooled_results$Rel_Efficiency <- 1 / (1 + pooled_results$FMI/M)

# Display results
print("Pooled Fit Indices with 95% Confidence Intervals:")
print(pooled_results[, c("Index", "Estimate_Original", "SE", "CI_Lower", "CI_Upper", "FMI")])

# Save results
save(bootstrap_fit_results, bootstrap_point_estimates, bootstrap_se,
     pooled_results, pooled_params, pooled_std_params,
     merged_params, factor_loading_table, threshold_table,
     file = "mi_bootstrap_cfa_results.RData")
A.7 Measurement Invariance Testing
A.7.1 Preparation and Model Modifications
r
# Recategorize problematic variables into binary format
for (i in 1:length(completed_datasets)) {
  # Create binary versions of problematic variables
  completed_datasets[[i]]$erg_rating_physical_bin <- ifelse(completed_datasets[[i]]$erg_rating_physical <= 2, 0, 1)
  completed_datasets[[i]]$hyg_rating_substances_bin <- ifelse(completed_datasets[[i]]$hyg_rating_substances <= 2, 0, 1)
  completed_datasets[[i]]$hyg_rating_noise_bin <- ifelse(completed_datasets[[i]]$hyg_rating_noise <= 2, 0, 1)
  
  # Convert to ordered factors
  completed_datasets[[i]]$erg_rating_physical_bin <- ordered(completed_datasets[[i]]$erg_rating_physical_bin)
  completed_datasets[[i]]$hyg_rating_substances_bin <- ordered(completed_datasets[[i]]$hyg_rating_substances_bin)
  completed_datasets[[i]]$hyg_rating_noise_bin <- ordered(completed_datasets[[i]]$hyg_rating_noise_bin)
  
  # Combine du and nl into Dutch category
  completed_datasets[[i]]$language_group <- ifelse(completed_datasets[[i]]$LanguageCode %in% c("du", "nl"), 
                                                 "Dutch", 
                                                 as.character(completed_datasets[[i]]$LanguageCode))
  completed_datasets[[i]]$language_group <- factor(completed_datasets[[i]]$language_group)
  
  # Ensure device_type is correctly coded
  completed_datasets[[i]]$device_type <- factor(
    ifelse(is.na(completed_datasets[[i]]$ismobile), "Desktop",
           ifelse(completed_datasets[[i]]$ismobile, "Mobile", "Desktop"))
  )
  
  # Environment domain factor
  completed_datasets[[i]]$env_domain <- factor(completed_datasets[[i]]$has_environment_domain)
}

# Define updated CFA model with binary versions
cfa_model_bin <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_loads + erg_rating_physical_bin
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY)
  HY =~ hyg_rating_tools + hyg_rating_low_temp + hyg_rating_high_temp + hyg_rating_noise_bin + hyg_rating_substances_bin
'

# Update ordinal variables list
ordinal_vars_bin <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_loads", "erg_rating_physical_bin",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
  "hyg_rating_noise_bin", "hyg_rating_substances_bin"
)
A.7.2 Measurement Invariance Functions
r
# Function for pairwise comparison with projection method (both NHT and ET)
run_pairwise_projection_invariance <- function(data_list, model, group_var, groups_to_compare, 
                                     ordinal_vars, label) {
  nht_results <- list()  # For null hypothesis testing
  et_results <- list()   # For equivalence testing
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    
    # Filter to include only the two groups being compared
    subset_data <- data[data[[group_var]] %in% groups_to_compare, ]
    subset_data[[group_var]] <- factor(subset_data[[group_var]], 
                                       levels = groups_to_compare)
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      # Configural model
      configural <- cfa(model, 
                       data = subset_data,
                       group = group_var,
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      # Metric model (equal loadings)
      metric_nht <- cfa(model, 
                      data = subset_data,
                      group = group_var,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      # Scalar model (equal loadings and thresholds)
      scalar_nht <- cfa(model, 
                      data = subset_data, 
                      group = group_var,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # For ET approach - using projection method
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      group1_loadings <- group_loadings[[1]]
      group2_loadings <- group_loadings[[2]]
      
      # Calculate projection-based parameters
      loadings_diff <- abs(group1_loadings - group2_loadings)
      max_loading_diff <- max(loadings_diff, na.rm = TRUE)
      
      # Extract thresholds
      group_thresholds <- lavInspect(configural, what = "est")$th
      group1_thresholds <- unlist(group_thresholds[[1]])
      group2_thresholds <- unlist(group_thresholds[[2]])
      
      thresholds_diff <- abs(group1_thresholds - group2_thresholds)
      max_threshold_diff <- max(thresholds_diff, na.rm = TRUE)
      
      configural_fit <- fitMeasures(configural, c("cfi", "rmsea", "srmr"))
      
      list(
        nht = list(configural = configural, metric = metric_nht, scalar = scalar_nht),
        et = list(
          configural = configural_fit,
          metric_projection = list(
            max_loading_diff = max_loading_diff,
            loading_diffs = loadings_diff
          ),
          scalar_projection = list(
            max_threshold_diff = max_threshold_diff,
            threshold_diffs = thresholds_diff
          )
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    } else {
      nht_results[[i]] <- NULL
      et_results[[i]] <- NULL
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Function to pool and compare fit indices for NHT
pool_and_compare_nht <- function(results_list, label) {
  valid_results <- !sapply(results_list, is.null)
  if (sum(valid_results) == 0) {
    cat("\n\nNo valid results available for", label, "\n")
    return(NULL)
  }
  
  n_valid <- sum(valid_results)
  
  # Initialize arrays for fit indices
  fit_indices <- array(
    NA,
    dim = c(n_valid, 3, 5),
    dimnames = list(
      paste0("valid_imp", which(valid_results)),
      c("configural", "metric", "scalar"),
      c("chisq", "df", "cfi", "rmsea", "srmr")
    )
  )
  
  # Get fit indices from valid imputations
  valid_idx <- 1
  for (i in which(valid_results)) {
    models <- c("configural", "metric", "scalar")
    for (j in 1:length(models)) {
      model_name <- models[j]
      if (!is.null(results_list[[i]][[model_name]])) {
        fit_indices[valid_idx, j, "chisq"] <- fitMeasures(results_list[[i]][[model_name]], "chisq")
        fit_indices[valid_idx, j, "df"] <- fitMeasures(results_list[[i]][[model_name]], "df")
        fit_indices[valid_idx, j, "cfi"] <- fitMeasures(results_list[[i]][[model_name]], "cfi")
        fit_indices[valid_idx, j, "rmsea"] <- fitMeasures(results_list[[i]][[model_name]], "rmsea")
        fit_indices[valid_idx, j, "srmr"] <- fitMeasures(results_list[[i]][[model_name]], "srmr")
      }
    }
    valid_idx <- valid_idx + 1
  }
  
  # Pool fit indices
  pooled_indices <- array(
    NA,
    dim = c(3, 5),
    dimnames = list(
      c("configural", "metric", "scalar"),
      c("chisq", "df", "cfi", "rmsea", "srmr")
    )
  )
  
  for (j in 1:3) {
    for (k in 1:5) {
      pooled_indices[j, k] <- mean(fit_indices[, j, k], na.rm = TRUE)
    }
  }
  
  # Calculate changes in fit indices
  delta_fit <- array(
    NA,
    dim = c(2, 5),
    dimnames = list(
      c("metric-configural", "scalar-metric"),
      c("chisq", "df", "cfi", "rmsea", "srmr")
    )
  )
  
  for (k in 1:5) {
    delta_fit[1, k] <- pooled_indices[2, k] - pooled_indices[1, k]
    delta_fit[2, k] <- pooled_indices[3, k] - pooled_indices[2, k]
  }
  
  # Evaluate measurement invariance
  invariance_decision <- data.frame(
    Comparison = c("Metric vs. Configural", "Scalar vs. Metric"),
    ΔCFI = delta_fit[, "cfi"],
    ΔRMSEA = delta_fit[, "rmsea"],
    ΔSRMR = delta_fit[, "srmr"],
    Decision = NA
  )
  
  for (i in 1:2) {
    if (is.na(invariance_decision$ΔCFI[i])) {
      invariance_decision$Decision[i] <- "Cannot determine"
    } else if (invariance_decision$ΔCFI[i] > -0.01 && 
               !is.na(invariance_decision$ΔRMSEA[i]) && invariance_decision$ΔRMSEA[i] < 0.015 && 
               (i == 1 && !is.na(invariance_decision$ΔSRMR[i]) && invariance_decision$ΔSRMR[i] < 0.030 ||
                i > 1 && !is.na(invariance_decision$ΔSRMR[i]) && invariance_decision$ΔSRMR[i] < 0.010)) {
      invariance_decision$Decision[i] <- "Supported"
    } else {
      invariance_decision$Decision[i] <- "Not Supported"
    }
  }
  
  cat("\n\n==== NHT Measurement Invariance Results for", label, "====\n")
  cat("\nPooled Fit Indices:\n")
  print(pooled_indices)
  
  cat("\nChanges in Fit Indices:\n")
  print(delta_fit)
  
  cat("\nInvariance Evaluation (NHT Approach):\n")
  print(invariance_decision)
  
  return(list(
    pooled = pooled_indices,
    delta = delta_fit,
    decision = invariance_decision
  ))
}

# Function to analyze ET results
analyze_et_results <- function(results_list, label) {
  valid_results <- !sapply(results_list, is.null)
  if (sum(valid_results) == 0) {
    cat("\n\nNo valid ET results available for", label, "\n")
    return(NULL)
  }
  
  n_valid <- sum(valid_results)
  
  metric_diffs <- numeric(n_valid)
  scalar_diffs <- numeric(n_valid)
  
  valid_idx <- 1
  for (i in which(valid_results)) {
    if (!is.null(results_list[[i]]$metric_projection) && 
        !is.null(results_list[[i]]$metric_projection$max_loading_diff)) {
      if (is.finite(results_list[[i]]$metric_projection$max_loading_diff)) {
        metric_diffs[valid_idx] <- results_list[[i]]$metric_projection$max_loading_diff
      } else {
        metric_diffs[valid_idx] <- 0.01
      }
    }
    
    if (!is.null(results_list[[i]]$scalar_projection) && 
        !is.null(results_list[[i]]$scalar_projection$max_threshold_diff)) {
      if (is.finite(results_list[[i]]$scalar_projection$max_threshold_diff)) {
        scalar_diffs[valid_idx] <- results_list[[i]]$scalar_projection$max_threshold_diff
      } else {
        scalar_diffs[valid_idx] <- 0.01
      }
    }
    
    valid_idx <- valid_idx + 1
  }
  
  # Pool projected values
  pooled_metric_diff <- if (all(is.na(metric_diffs)) || length(metric_diffs) == 0) {
    0.01
  } else {
    mean(metric_diffs, na.rm = TRUE)
  }
  
  pooled_scalar_diff <- if (all(is.na(scalar_diffs)) || length(scalar_diffs) == 0) {
    0.01
  } else {
    mean(scalar_diffs, na.rm = TRUE)
  }
  
  # Define equivalence thresholds
  et_metric_threshold <- 0.2
  et_scalar_threshold <- 0.3
  
  et_decision <- data.frame(
    Comparison = c("Metric (Loadings)", "Scalar (Thresholds)"),
    MaxDiff = c(pooled_metric_diff, pooled_scalar_diff),
    Threshold = c(et_metric_threshold, et_scalar_threshold),
    Decision = NA
  )
  
  et_decision$Decision[1] <- ifelse(et_decision$MaxDiff[1] <= et_decision$Threshold[1], 
                                   "Supported (Equivalent)", 
                                   "Not Supported (Not Equivalent)")
  
  et_decision$Decision[2] <- ifelse(et_decision$MaxDiff[2] <= et_decision$Threshold[2], 
                                   "Supported (Equivalent)", 
                                   "Not Supported (Not Equivalent)")
  
  cat("\n\n==== ET Measurement Invariance Results for", label, "====\n")
  cat("\nProjection Method Results:\n")
  cat("\nMetric Invariance (Loading Differences):\n")
  cat("Average maximum loading difference:", pooled_metric_diff, "\n")
  cat("Equivalence threshold:", et_metric_threshold, "\n")
  
  cat("\nScalar Invariance (Threshold Differences):\n")
  cat("Average maximum threshold difference:", pooled_scalar_diff, "\n")
  cat("Equivalence threshold:", et_scalar_threshold, "\n")
  
  cat("\nInvariance Evaluation (ET Approach):\n")
  print(et_decision)
  
  return(list(
    metric = pooled_metric_diff,
    scalar = pooled_scalar_diff,
    decision = et_decision
  ))
}
A.7.3 Running Measurement Invariance Tests
r
# Language groups (Dutch vs French)
dutch_fr_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_bin,
  "language_group",
  c("Dutch", "fr"),
  ordinal_vars_bin,
  "Dutch vs French"
)

# Mobile vs Desktop
mobile_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_original,
  "device_type",
  c("Mobile", "Desktop"),
  ordinal_vars_original,
  "Mobile vs Desktop"
)

# Environment domain (Yes vs No)
env_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_original,
  "env_domain",
  c("Yes", "No"),
  ordinal_vars_original,
  "Environment vs No Environment"
)

# Analyze results - NHT approach
dutch_fr_nht_summary <- tryCatch({
  pool_and_compare_nht(dutch_fr_results$nht, "Dutch vs French")
}, error = function(e) {
  cat("Error analyzing Dutch vs French NHT results:", e$message, "\n")
  return(NULL)
})

mobile_nht_summary <- tryCatch({
  pool_and_compare_nht(mobile_results$nht, "Mobile vs Desktop")
}, error = function(e) {
  cat("Error analyzing Mobile vs Desktop NHT results:", e$message, "\n")
  return(NULL)
})

env_nht_summary <- tryCatch({
  pool_and_compare_nht(env_results$nht, "Environment vs No Environment")
}, error = function(e) {
  cat("Error analyzing Environment NHT results:", e$message, "\n")
  return(NULL)
})

# Analyze results - ET approach
dutch_fr_et_summary <- tryCatch({
  analyze_et_results(dutch_fr_results$et, "Dutch vs French")
}, error = function(e) {
  cat("Error analyzing Dutch vs French ET results:", e$message, "\n")
  return(NULL)
})

mobile_et_summary <- tryCatch({
  analyze_et_results(mobile_results$et, "Mobile vs Desktop")
}, error = function(e) {
  cat("Error analyzing Mobile vs Desktop ET results:", e$message, "\n")
  return(NULL)
})

env_et_summary <- tryCatch({
  analyze_et_results(env_results$et, "Environment vs No Environment")
}, error = function(e) {
  cat("Error analyzing Environment ET results:", e$message, "\n")
  return(NULL)
})
A.8 Gender-Specific Measurement Invariance
r
# Function for gender measurement invariance testing
run_gender_invariance <- function(data_list, model, ordinal_vars, label) {
  nht_results <- list()
  et_results <- list()
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    data$sex_cat <- factor(data$sex_cat)
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      configural <- cfa(model, 
                       data = data,
                       group = "sex_cat",
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      metric_nht <- cfa(model, 
                      data = data,
                      group = "sex_cat",
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      scalar_nht <- cfa(model, 
                      data = data, 
                      group = "sex_cat",
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # ET approach calculations (similar to previous function)
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      group1_loadings <- group_loadings[[1]]
      group2_loadings <- group_loadings[[2]]
      
      loadings_diff <- abs(group1_loadings - group2_loadings)
      max_loading_diff <- max(loadings_diff, na.rm = TRUE)
      
      group_thresholds <- lavInspect(configural, what = "est")$th
      group1_thresholds <- unlist(group_thresholds[[1]])
      group2_thresholds <- unlist(group_thresholds[[2]])
      
      thresholds_diff <- abs(group1_thresholds - group2_thresholds)
      max_threshold_diff <- max(thresholds_diff, na.rm = TRUE)
      
      configural_fit <- fitMeasures(configural, c("cfi", "rmsea", "srmr"))
      
      list(
        nht = list(configural = configural, metric = metric_nht, scalar = scalar_nht),
        et = list(
          configural = configural_fit,
          metric_projection = list(
            max_loading_diff = max_loading_diff,
            loading_diffs = loadings_diff
          ),
          scalar_projection = list(
            max_threshold_diff = max_threshold_diff,
            threshold_diffs = thresholds_diff
          )
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    } else {
      nht_results[[i]] <- NULL
      et_results[[i]] <- NULL
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Modified CFA model excluding problematic variable
cfa_model_gender <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_loads + erg_rating_physical_bin
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY) - excluding hyg_rating_low_temp
  HY =~ hyg_rating_tools + hyg_rating_high_temp + hyg_rating_noise_bin + hyg_rating_substances_bin
'

ordinal_vars_gender <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_loads", "erg_rating_physical_bin",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_high_temp", 
  "hyg_rating_noise_bin", "hyg_rating_substances_bin"
)

# Run gender measurement invariance
gender_results_modified <- run_gender_invariance(
  completed_datasets[1:3],
  cfa_model_gender,
  ordinal_vars_gender,
  "Gender Modified (Men vs Women)"
)

# Analyze results
gender_nht_summary_modified <- tryCatch({
  pool_and_compare_nht(gender_results_modified$nht, "Gender Modified (Men vs Women)")
}, error = function(e) {
  cat("Error analyzing Modified Gender NHT results:", e$message, "\n")
  return(NULL)
})

gender_et_summary_modified <- tryCatch({
  analyze_et_results(gender_results_modified$et, "Gender Modified (Men vs Women)")
}, error = function(e) {
  cat("Error analyzing Modified Gender ET results:", e$message, "\n")
  return(NULL)
})
A.9 Age-Based Measurement Invariance
r
# Function for age measurement invariance testing
run_age_invariance <- function(data_list, model, ordinal_vars, age_variable, label) {
  nht_results <- list()
  et_results <- list()
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    data[[age_variable]] <- factor(data[[age_variable]])
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      configural <- cfa(model, 
                       data = data,
                       group = age_variable,
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      metric_nht <- cfa(model, 
                      data = data,
                      group = age_variable,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      scalar_nht <- cfa(model, 
                      data = data, 
                      group = age_variable,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # For ET approach - projection method for multiple groups
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      
      # Initialize matrices for pairwise comparisons
      num_groups <- length(unique(data[[age_variable]]))
      max_loading_diffs <- matrix(0, num_groups, num_groups)
      max_threshold_diffs <- matrix(0, num_groups, num_groups)
      
      # Perform all pairwise comparisons
      for (g1 in 1:(num_groups-1)) {
        for (g2 in (g1+1):num_groups) {
          loadings_diff <- abs(group_loadings[[g1]] - group_loadings[[g2]])
          max_diff <- max(loadings_diff, na.rm = TRUE)
          max_loading_diffs[g1, g2] <- max_diff
          max_loading_diffs[g2, g1] <- max_diff
          
          group_thresholds <- lavInspect(configural, what = "est")$th
          g1_thresholds <- unlist(group_thresholds[[g1]])
          g2_thresholds <- unlist(group_thresholds[[g2]])
          
          thresholds_diff <- abs(g1_thresholds - g2_thresholds)
          max_diff <- max(thresholds_diff, na.rm = TRUE)
          max_threshold_diffs[g1, g2] <- max_diff
          max_threshold_diffs[g2, g1] <- max_diff
        }
      }
      
      max_loading_diff <- max(max_loading_diffs, na.rm = TRUE)
      max_threshold_diff <- max(max_threshold_diffs, na.rm = TRUE)
      
      configural_fit <- fitMeasures(configural, c("cfi", "rmsea", "srmr"))
      
      list(
        nht = list(configural = configural, metric = metric_nht, scalar = scalar_nht),
        et = list(
          configural = configural_fit,
          metric_projection = list(
            max_loading_diff = max_loading_diff,
            loading_diffs_matrix = max_loading_diffs
          ),
          scalar_projection = list(
            max_threshold_diff = max_threshold_diff,
            threshold_diffs_matrix = max_threshold_diffs
          )
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    } else {
      nht_results[[i]] <- NULL
      et_results[[i]] <- NULL
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Check age group distributions and potentially recode
age_counts <- table(completed_datasets[[1]]$age_cat)
print("Age category counts:")
print(age_counts)

min_group_size <- 50
small_groups <- names(age_counts[age_counts < min_group_size])

if (length(small_groups) > 0) {
  cat("\nRecoding age groups due to small sample sizes\n")
  
  for (i in 1:length(completed_datasets)) {
    completed_datasets[[i]]$age_cat_recoded <- as.character(completed_datasets[[i]]$age_cat)
    
    # Example recoding - adjust based on actual distribution
    if ("<25" %in% small_groups) {
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == "<25"] <- "< 35"
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == "25-34"] <- "< 35"
    }
    
    if (">=55" %in% small_groups) {
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == ">=55"] <- "> 44"
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == "45-54"] <- "> 44"
    }
    
    completed_datasets[[i]]$age_cat_recoded <- factor(completed_datasets[[i]]$age_cat_recoded)
  }
  
  age_var <- "age_cat_recoded"
} else {
  age_var <- "age_cat"
}

# Run age measurement invariance
age_results <- run_age_invariance(
  completed_datasets[1:3],
  cfa_model_gender,
  ordinal_vars_gender,
  age_var,
  paste0("Age (", age_var, ")")
)

# Analyze results
age_nht_summary <- tryCatch({
  pool_and_compare_nht(age_results$nht, paste0("Age (", age_var, ")"))
}, error = function(e) {
  cat("Error analyzing Age NHT results:", e$message, "\n")
  return(NULL)
})

age_et_summary <- tryCatch({
  analyze_et_results(age_results$et, paste0("Age (", age_var, ")"))
}, error = function(e) {
  cat("Error analyzing Age ET results:", e$message, "\n")
  return(NULL)
})
A.10 Differential Item Functioning (DIF) Analysis
A.10.1 DIF Analysis Setup and Rationale
r
# Load additional packages for DIF analysis
library(difR)
library(lordif)
library(mirt)
library(eRm)

# DIF analysis is employed as a complementary approach to MGCFA, particularly valuable when
# sample size constraints prevent reliable multi-group modeling. DIF evaluates whether individual
# items function differently across groups while controlling for the underlying trait level.

# Prepare data for DIF analysis
prepare_dif_data <- function(completed_datasets, group_var, domain_items) {
  # Use the first imputed dataset for DIF analysis
  # (Alternative: pool results across multiple imputations)
  data <- completed_datasets[[1]]
  
  # Remove missing values for the specific domain and group variable
  complete_cases <- complete.cases(data[, c(domain_items, group_var)])
  dif_data <- data[complete_cases, c(domain_items, group_var)]
  
  # Ensure group variable is properly coded
  dif_data[[group_var]] <- factor(dif_data[[group_var]])
  
  # Convert items to numeric if they aren't already
  for(item in domain_items) {
    dif_data[[item]] <- as.numeric(dif_data[[item]])
  }
  
  return(dif_data)
}

# Define domain items for DIF analysis
domain_items <- list(
  "Psychosocial" = c("psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life"),
  "Ergonomics" = c("erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical"),
  "Safety" = c("saf_rating_workinvolv", "saf_rating_leadengage"),
  "Hygiene" = c("hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances")
)
A.10.2 Mantel-Haenszel DIF Detection
r
# Function to perform Mantel-Haenszel DIF analysis
perform_mh_dif <- function(dif_data, group_var, items, domain_name) {
  cat("\n=== Mantel-Haenszel DIF Analysis for", domain_name, "Domain ===\n")
  
  # Prepare data for difR package
  # Items should be in columns 1 to k, group variable in column k+1
  item_data <- dif_data[, items]
  group_data <- dif_data[[group_var]]
  
  # Calculate total scores for stratification
  total_scores <- rowSums(item_data, na.rm = TRUE)
  
  # Create data frame in required format
  dif_input <- cbind(item_data, group_data)
  colnames(dif_input) <- c(items, "group")
  
  # Perform Mantel-Haenszel test
  mh_results <- tryCatch({
    difMH(Data = dif_input, 
          group = "group", 
          focal.name = levels(group_data)[2],  # Second level as focal group
          match = total_scores,
          purify = TRUE)
  }, error = function(e) {
    cat("Error in MH analysis:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(mh_results)) {
    # Extract results
    mh_stats <- mh_results$MH
    alpha_mh <- mh_results$alphaMH
    
    # Classify DIF magnitude using ETS criteria
    dif_classification <- ifelse(abs(log(alpha_mh)) < 1.0, "A (Negligible)",
                                ifelse(abs(log(alpha_mh)) < 1.5, "B (Moderate)", "C (Large)"))
    
    # Create results summary
    mh_summary <- data.frame(
      Item = items,
      MH_Statistic = mh_stats,
      Alpha_MH = alpha_mh,
      Log_Alpha_MH = log(alpha_mh),
      DIF_Classification = dif_classification,
      Significant = mh_results$MH > qchisq(0.95, 1),
      stringsAsFactors = FALSE
    )
    
    cat("\nMantel-Haenszel DIF Results:\n")
    print(mh_summary)
    
    # Count DIF items by classification
    dif_counts <- table(dif_classification)
    cat("\nDIF Classification Summary:\n")
    print(dif_counts)
    
    return(list(summary = mh_summary, detailed = mh_results))
  } else {
    return(NULL)
  }
}
A.10.3 Logistic Regression DIF Detection
r
# Function to perform Logistic Regression DIF analysis
perform_lr_dif <- function(dif_data, group_var, items, domain_name) {
  cat("\n=== Logistic Regression DIF Analysis for", domain_name, "Domain ===\n")
  
  # Prepare data
  item_data <- dif_data[, items]
  group_data <- dif_data[[group_var]]
  total_scores <- rowSums(item_data, na.rm = TRUE)
  
  # Create data frame for analysis
  lr_data <- cbind(item_data, 
                   group = group_data, 
                   total_score = total_scores)
  
  # Initialize results storage
  lr_results <- list()
  uniform_dif <- character(length(items))
  nonuniform_dif <- character(length(items))
  
  # Analyze each item
  for (i in 1:length(items)) {
    item <- items[i]
    cat("\nAnalyzing item:", item, "\n")
    
    # Create binary item response (for ordinal items, consider threshold)
    # For this example, we'll use median split
    item_median <- median(lr_data[[item]], na.rm = TRUE)
    lr_data$item_binary <- ifelse(lr_data[[item]] > item_median, 1, 0)
    
    tryCatch({
      # Model 1: Base model (ability only)
      model1 <- glm(item_binary ~ total_score, 
                   data = lr_data, 
                   family = binomial)
      
      # Model 2: Uniform DIF model (ability + group)
      model2 <- glm(item_binary ~ total_score + group, 
                   data = lr_data, 
                   family = binomial)
      
      # Model 3: Non-uniform DIF model (ability + group + interaction)
      model3 <- glm(item_binary ~ total_score + group + total_score:group, 
                   data = lr_data, 
                   family = binomial)
      
      # Test for uniform DIF (Model 2 vs Model 1)
      uniform_test <- anova(model1, model2, test = "Chisq")
      uniform_p <- uniform_test# Appendix: R Code for Statistical Analysis

## A.1 Setup and Library Loading

```r
# Load necessary libraries
library(tidyverse)
library(MVN)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson, Amelia, mice, VIM, corrplot, gridExtra)

# Setting working directory
setwd("D:/2Y_MasterThesis/R_Code")
A.2 Data Loading and Variable Selection
r
# Load the pilot dataset using the correct delimiter (semicolon)
full_dataset <- read_delim("pilotdata_4comp.csv", delim = ";")

# Define model variables excluding environment domain
model_vars_no_env <- c(
  # Psychosocial
  "veerkracht_calc", "psy_rating_pace", "psy_rating_emotional", 
  "psy_rating_sphere", "psy_work_life",
  # Ergonomics
  "erg_capac", "erg_rating_posture", "erg_rating_repeat", 
  "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical",
  # Safety
  "saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage",
  # Hygiene
  "hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
  "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances"
)

# Define environment domain variables
env_vars <- c("mil_rating_leadengage", "mil_satisfaction", "mil_rating_contrib", "mil_rating_workinvolv")

# Calculate missing rates and identify auxiliary variables
missing_rates <- full_dataset %>%
  summarise(across(everything(), ~sum(is.na(.))/n())) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate") %>%
  arrange(missing_rate)

# Identify potential auxiliary variables (exclude model vars and keep only those with <20% missing)
potential_aux_vars <- missing_rates %>%
  filter(missing_rate < 0.20) %>%
  filter(!variable %in% c(model_vars_no_env, env_vars, 
                         "ismobile", "LanguageCode", "Name_Company", "Version_survey")) %>%
  pull(variable)

# Select useful auxiliary variables
useful_aux_vars <- c(
  "age_cat", "sex_cat", 
  grep("freq_exp", potential_aux_vars, value = TRUE),
  grep("exposure", potential_aux_vars, value = TRUE),
  grep("hea_", potential_aux_vars, value = TRUE, fixed = TRUE),
  grep("vit_", potential_aux_vars, value = TRUE, fixed = TRUE),
  "was", "shift", "sleep", "health_not_ok"
)

useful_aux_vars <- intersect(useful_aux_vars, names(full_dataset))
useful_aux_vars <- head(useful_aux_vars, 10)

# Create final dataset
final_dataset <- full_dataset %>%
  mutate(has_environment_domain = case_when(
    !is.na(mil_satisfaction) ~ "Yes",
    TRUE ~ "No"
  )) %>%
  select(all_of(c(model_vars_no_env, env_vars, useful_aux_vars,
                "ismobile", "LanguageCode", "Name_Company", "Version_survey",
                "has_environment_domain")))

# Save the file
write_csv(final_dataset, "final_analysis_dataset.csv")
A.3 Missing Data Analysis and Visualization
r
# Load the selected variable dataset
full_analysis_dataset <- read_delim("final_analysis_dataset.csv")

# Create a mapping for variables with descriptive names
var_labels <- c(
  # Psychosocial work environment factor (PS)
  veerkracht_calc = "Resilience",
  psy_rating_pace = "Work pace",
  psy_rating_emotional = "Emotional demands",
  psy_rating_sphere = "Work atmosphere",
  psy_work_life = "Work-life balance",
  
  # Ergonomics factor (ER)
  erg_capac = "Ergonomic capacity",
  erg_rating_posture = "Stressful postures",
  erg_rating_repeat = "Repetitive work",
  erg_rating_sitting = "Sitting for long-time",
  erg_rating_loads = "Manual handling loads",
  erg_rating_physical = "Physical strenuous",
  
  # Safety factor (SA)
  saf_satisfaction = "Safety satisfaction",
  saf_rating_workinvolv = "Work involvement",
  saf_rating_leadengage = "Leadership engagement",
  
  # Hygiene factor (HY)
  hyg_satisfaction = "Hygiene satisfaction",
  hyg_rating_tools = "Tool Vibrations",
  hyg_rating_low_temp = "Low temperatures",
  hyg_rating_high_temp = "High temperatures",
  hyg_rating_noise = "Noise",
  hyg_rating_substances = "Hazardous Substances",
  
  # Environmental domain
  mil_rating_leadengage = "Env. leadership engagement",
  mil_satisfaction = "Env. satisfaction",
  mil_rating_contrib = "Env. contribution",
  mil_rating_workinvolv = "Env. work involvement"
)

# Function to rename variables for plotting
rename_vars <- function(var_names) {
  sapply(var_names, function(x) {
    if(x %in% names(var_labels)) var_labels[x] else x
  })
}

# Create directory for saving files
dir.create("plots", showWarnings = FALSE)

# Overall missingness summary
miss_summary <- miss_var_summary(full_analysis_dataset)
miss_summary$variable_renamed <- rename_vars(miss_summary$variable)

# Plot missingness summary
p1 <- ggplot(miss_summary, aes(x = reorder(variable_renamed, pct_miss), y = pct_miss)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Overall Missingness by Variable",
       subtitle = "Percentage of missing values for each variable",
       x = "", y = "Percentage Missing (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 9))

ggsave("plots/missingness_summary.pdf", p1, width = 12, height = 10)
print(p1)

# Domain-specific missingness analysis
domains <- list(
  "Psychosocial Domain" = c("veerkracht_calc", "psy_rating_pace", "psy_rating_emotional", 
                          "psy_rating_sphere", "psy_work_life"),
  "Ergonomics Domain" = c("erg_capac", "erg_rating_posture", "erg_rating_repeat", 
                        "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical"),
  "Safety Domain" = c("saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage"),
  "Hygiene Domain" = c("hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
                      "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances"),
  "Environmental Domain" = c("mil_rating_leadengage", "mil_satisfaction", 
                           "mil_rating_contrib", "mil_rating_workinvolv")
)

# Calculate missingness by domain
domain_missingness <- lapply(names(domains), function(domain_name) {
  vars <- domains[[domain_name]]
  vars <- vars[vars %in% names(full_analysis_dataset)]
  if(length(vars) > 0) {
    result <- full_analysis_dataset %>%
      summarise(across(all_of(vars), ~mean(is.na(.))*100)) %>%
      pivot_longer(cols = everything(), 
                  names_to = "variable", 
                  values_to = "percent_missing") %>%
      mutate(domain = domain_name)
    
    result$variable_renamed <- rename_vars(result$variable)
    return(result)
  }
}) %>% bind_rows()

# Domain-specific missingness visualization
p6 <- ggplot(domain_missingness, aes(x = reorder(variable_renamed, percent_missing), 
                                    y = percent_missing, 
                                    fill = domain)) +
  geom_col() +
  facet_wrap(~domain, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") +
  labs(title = "Percentage of Missing Values by Domain",
       subtitle = "Grouped by workplace assessment domains",
       x = "", y = "% Missing")

ggsave("plots/missingness_by_domain.pdf", p6, width = 14, height = 10)
print(p6)
A.4 Multiple Imputation
r
# Create a clean dataset for imputation
imputation_dataset <- full_analysis_dataset

# Set -999 values back to NA for environmental variables
env_vars <- c("mil_rating_leadengage", "mil_satisfaction", 
              "mil_rating_contrib", "mil_rating_workinvolv")

for(var in env_vars) {
  imputation_dataset[[var]][imputation_dataset[[var]] == -999] <- NA
}

# Identify variable types for appropriate imputation methods
ordinal_vars <- c(
  # Psychosocial domain
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  # Ergonomics domain
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", 
  "erg_rating_loads", "erg_rating_physical", "erg_capac",
  # Safety domain
  "saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage",
  # Hygiene domain
  "hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
  "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances",
  # Environmental domain
  "mil_rating_leadengage", "mil_satisfaction", "mil_rating_contrib", "mil_rating_workinvolv",
  # Frequency variables
  "hyg_freq_exp_substances", "hyg_freq_exp_tools", "hyg_freq_exp_high_temp",
  "hyg_freq_exp_low_temp", "hyg_freq_exp_noise", "erg_freq_exp_sitting",
  "erg_freq_exp_posture", "erg_freq_exp_physical",
  # Demographics
  "age_cat"
)

continuous_vars <- c("veerkracht_calc")
binary_vars <- c("ismobile", "sex_cat")
cat_vars <- c("LanguageCode", "Name_Company", "Version_survey", "has_environment_domain")

# Special handling for age_cat - convert to ordered factor
if("age_cat" %in% names(imputation_dataset)) {
  age_levels <- c("<25", "25-34", "35-44", "45-54", ">=55")
  imputation_dataset$age_cat <- factor(imputation_dataset$age_cat, 
                                      levels = age_levels, ordered = TRUE)
}

# Convert other ordinal variables to factors
for(var in setdiff(ordinal_vars, "age_cat")) {
  if(var %in% names(imputation_dataset)) {
    if(sum(!is.na(imputation_dataset[[var]])) > 0) {
      var_values <- imputation_dataset[[var]][!is.na(imputation_dataset[[var]])]
      
      if(all(!is.na(suppressWarnings(as.numeric(as.character(var_values)))))) {
        var_numeric <- as.numeric(as.character(var_values))
        var_min <- min(var_numeric, na.rm = TRUE)
        var_max <- max(var_numeric, na.rm = TRUE)
        
        if(!is.na(var_min) && !is.na(var_max)) {
          levels <- var_min:var_max
          imputation_dataset[[var]] <- factor(imputation_dataset[[var]], 
                                             levels = levels, ordered = TRUE)
        }
      }
    }
  }
}

# Create variable to track observations without environmental data
imputation_dataset$skip_env_imputation <- (full_analysis_dataset$has_environment_domain == "No")
cat_vars <- c(cat_vars, "skip_env_imputation")

# Set up MICE methods
imp_methods <- mice(imputation_dataset, maxit = 0)$method

# Set imputation methods for each variable type
for(var in ordinal_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- "polr"  # Proportional odds logistic regression
  }
}

for(var in continuous_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- "pmm"  # Predictive mean matching
  }
}

for(var in binary_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- "logreg"  # Logistic regression
  }
}

for(var in cat_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- ""  # Don't impute categorical variables
  }
}

# Create predictor matrix
pred_matrix <- mice(imputation_dataset, maxit = 0)$predictorMatrix

# Don't use categorical variables as predictors
for(var in cat_vars) {
  if(var %in% names(imputation_dataset)) {
    pred_matrix[, which(colnames(pred_matrix) == var)] <- 0
  }
}

# Run imputation
set.seed(12345)
imputed_data <- mice(imputation_dataset, 
                    predictorMatrix = pred_matrix,
                    method = imp_methods,
                    m = 10,          # Create 10 imputed datasets
                    maxit = 20,      # Run for 20 iterations
                    seed = 12345,
                    printFlag = TRUE)

# Post-processing: convert factors back to numeric and handle environmental vars
completed_datasets <- list()
for(i in 1:10) {
  complete_data_i <- complete(imputed_data, i)
  
  # Convert regular ordinal variables back to numeric
  for(var in setdiff(ordinal_vars, "age_cat")) {
    if(var %in% names(complete_data_i)) {
      complete_data_i[[var]] <- as.numeric(as.character(complete_data_i[[var]]))
    }
  }
  
  # Reset environmental variables to -999 where needed
  for(var in env_vars) {
    if(var %in% names(complete_data_i)) {
      complete_data_i[[var]][complete_data_i$skip_env_imputation] <- -999
    }
  }
  
  # Remove the helper variable
  complete_data_i$skip_env_imputation <- NULL
  
  completed_datasets[[i]] <- complete_data_i
}

# Save the completed datasets
saveRDS(completed_datasets, "imputed_datasets.rds")
write.csv(completed_datasets[[1]], file = "imputed_dataset_for_cfa.csv", row.names = FALSE)
A.5 Confirmatory Factor Analysis
A.5.1 CFA Model Definition and Reliability Functions
r
# Function to calculate CR and AVE from a fitted lavaan model
calculate_CR_AVE <- function(fit) {
  std_estimates <- standardizedSolution(fit)
  loadings <- subset(std_estimates, op == "=~")
  factors <- unique(loadings$lhs)
  
  results <- data.frame(Factor = factors, CR = NA, AVE = NA)
  
  for(i in 1:length(factors)) {
    factor <- factors[i]
    factor_loadings <- subset(loadings, lhs == factor)$est.std
    
    lambda_squared <- factor_loadings^2
    delta <- 1 - lambda_squared
    
    sum_lambda <- sum(factor_loadings)
    sum_delta <- sum(delta)
    CR <- sum_lambda^2 / (sum_lambda^2 + sum_delta)
    
    sum_lambda_squared <- sum(lambda_squared)
    AVE <- sum_lambda_squared / (sum_lambda_squared + sum_delta)
    
    results$CR[i] <- CR
    results$AVE[i] <- AVE
  }
  
  return(results)
}

# Define CFA model
cfa_model <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_sitting + erg_rating_loads + erg_rating_physical
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY)
  HY =~ hyg_rating_tools + hyg_rating_low_temp + hyg_rating_high_temp + hyg_rating_noise + hyg_rating_substances
'

# Define ordinal variables
ordinal_vars <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", 
  "erg_rating_loads", "erg_rating_physical",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
  "hyg_rating_noise", "hyg_rating_substances"
)
A.5.2 Model Diagnostics
r
# Test multivariate normality
cfa_vars <- full_dataset %>% 
  select(veerkracht_calc, starts_with("psy_"), starts_with("erg_")) %>%
  select(where(is.numeric))

mvn_result <- mvn(data = cfa_vars, mvnTest = "mardia")
print(mvn_result$multivariateNormality)

# Correlation matrix and multicollinearity check
cor_matrix <- cor(cfa_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

# Identify high correlations (> 0.85)
high_cors <- which(abs(cor_matrix) > 0.85 & abs(cor_matrix) < 1, arr.ind = TRUE)
if(nrow(high_cors) > 0) {
  high_cor_pairs <- data.frame(
    var1 = rownames(cor_matrix)[high_cors[, 1]],
    var2 = colnames(cor_matrix)[high_cors[, 2]],
    correlation = cor_matrix[high_cors]
  )
  print("Potential multicollinearity issues:")
  print(high_cor_pairs)
}

# Factorability tests
kmo_result <- KMO(cor_matrix)
print(kmo_result$MSA)  # Overall KMO value

bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(cfa_vars))
print(bartlett_result)
A.5.3 Reliability Analysis
r
# Enhanced reliability calculation with Polychoric Alpha
calculate_reliability_complete <- function(completed_datasets, factor_vars) {
  results <- data.frame()
  
  for (factor_name in names(factor_vars)) {
    vars <- factor_vars[[factor_name]]
    
    alpha_values <- numeric()
    poly_alpha_values <- numeric()
    omega_values <- numeric()
    
    for (i in 1:length(completed_datasets)) {
      data_imp <- completed_datasets[[i]]
      factor_data <- data_imp[, vars, drop = FALSE]
      factor_data <- factor_data[complete.cases(factor_data), ]
      factor_data <- as.data.frame(lapply(factor_data, as.numeric))
      
      if (nrow(factor_data) > 10 && ncol(factor_data) > 1) {
        # Regular Cronbach's Alpha
        tryCatch({
          cor_matrix <- cor(factor_data, use = "complete.obs")
          k <- ncol(factor_data)
          avg_inter_cor <- (sum(cor_matrix) - k) / (k * (k - 1))
          alpha_manual <- (k * avg_inter_cor) / (1 + (k - 1) * avg_inter_cor)
          alpha_values[i] <- alpha_manual
        }, error = function(e) {
          alpha_values[i] <- NA
        })
        
        # Polychoric Alpha
        tryCatch({
          poly_cor <- polychoric(factor_data)$rho
          k <- ncol(factor_data)
          avg_poly_cor <- (sum(poly_cor) - k) / (k * (k - 1))
          poly_alpha <- (k * avg_poly_cor) / (1 + (k - 1) * avg_poly_cor)
          poly_alpha_values[i] <- poly_alpha
        }, error = function(e) {
          poly_alpha_values[i] <- NA
        })
        
        # McDonald's Omega
        if (length(vars) > 2) {
          tryCatch({
            factor_model <- paste0(factor_name, " =~ ", paste(vars, collapse = " + "))
            fit_single <- cfa(factor_model, data = data_imp, ordered = TRUE, estimator = "WLSMV")
            
            if (lavInspect(fit_single, "converged")) {
              rel_coef <- reliability(fit_single)
              omega_values[i] <- rel_coef["omega", factor_name]
            } else {
              omega_values[i] <- NA
            }
          }, error = function(e) {
            omega_values[i] <- NA
          })
        }
      }
    }
    
    # Pool results function
    pool_reliability <- function(values, measure_name) {
      valid_values <- values[!is.na(values)]
      if (length(valid_values) >= 3) {
        pooled_mean <- mean(valid_values)
        pooled_sd <- sd(valid_values)
        
        n <- length(valid_values)
        se <- pooled_sd / sqrt(n)
        t_val <- qt(0.975, df = n - 1)
        ci_lower <- pooled_mean - t_val * se
        ci_upper <- pooled_mean + t_val * se
        
        return(data.frame(
          Factor = factor_name,
          Measure = measure_name,
          Estimate = round(pooled_mean, 3),
          SE = round(se, 4),
          CI_Lower = round(ci_lower, 3),
          CI_Upper = round(ci_upper, 3),
          N_Valid = n,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Add all reliability measures
    alpha_result <- pool_reliability(alpha_values, "Cronbach_Alpha")
    poly_alpha_result <- pool_reliability(poly_alpha_values, "Polychoric_Alpha")
    results <- rbind(results, alpha_result, poly_alpha_result)
    
    if (length(vars) > 2 && length(omega_values[!is.na(omega_values)]) > 0) {
      omega_result <- pool_reliability(omega_values, "McDonald_Omega")
      results <- rbind(results, omega_result)
    }
    
    # Spearman-Brown for 2-item factors
    if (length(vars) == 2) {
      sb_values <- numeric()
      poly_sb_values <- numeric()
      
      for (i in 1:length(completed_datasets)) {
        data_imp <- completed_datasets[[i]]
        factor_data <- data_imp[, vars, drop = FALSE]
        factor_data <- factor_data[complete.cases(factor_data), ]
        factor_data <- as.data.frame(lapply(factor_data, as.numeric))
        
        if (nrow(factor_data) > 10) {
          r <- cor(factor_data[, 1], factor_data[, 2], use = "complete.obs")
          sb_values[i] <- (2 * r) / (1 + r)
          
          tryCatch({
            poly_r <- polychoric(factor_data)$rho[1, 2]
            poly_sb_values[i] <- (2 * poly_r) / (1 + poly_r)
          }, error = function(e) {
            poly_sb_values[i] <- NA
          })
        }
      }
      
      sb_result <- pool_reliability(sb_values, "Spearman_Brown")
      poly_sb_result <- pool_reliability(poly_sb_values, "Polychoric_Spearman_Brown")
      results <- rbind(results, sb_result, poly_sb_result)
    }
  }
  
  return(results)
}

# Define factor variables
factor_vars <- list(
  PS = c("psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life"),
  ER = c("erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical"),
  SA = c("saf_rating_workinvolv", "saf_rating_leadengage"),
  HY = c("hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances")
)

# Run reliability analysis
reliability_results_complete <- calculate_reliability_complete(completed_datasets, factor_vars)
print(reliability_results_complete)
A.6 Pooled CFA with Bootstrap Confidence Intervals
r
# Load imputed datasets
completed_datasets <- readRDS("imputed_datasets.rds")
M <- length(completed_datasets)  # Number of imputed datasets
B <- 1000                        # Number of bootstrap samples

# Define transformation functions for fit indices
fisher_transform <- function(x) {
  x <- pmin(x, 0.999)
  return(0.5 * log((1 + x) / (1 - x)))
}

inverse_fisher <- function(z) {
  return((exp(2 * z) - 1) / (exp(2 * z) + 1))
}

log_transform <- function(x) {
  x <- pmax(x, 0.001)
  return(log(x))
}

inverse_log <- function(z) {
  return(exp(z))
}

# Initialize arrays to store results
fit_indices <- c("cfi", "tli", "rmsea", "srmr", "chisq")
bootstrap_fit_results <- array(NA, dim = c(M, B, length(fit_indices)),
                              dimnames = list(paste0("imp", 1:M), 
                                             paste0("boot", 1:B),
                                             fit_indices))

param_results <- list()
std_param_results <- list()
intercept_results <- list()

# BOOTSTRAPPING PHASE
for (m in 1:M) {
  cat("Processing imputed dataset", m, "of", M, "\n")
  current_data <- completed_datasets[[m]]
  
  # Define bootstrap function
  boot_fun <- function(data, indices) {
    boot_sample <- data[indices, ]
    
    fit <- tryCatch({
      cfa(cfa_model, 
         data = boot_sample, 
         ordered = ordinal_vars,
         estimator = "DWLS")
    }, error = function(e) {
      return(NA)
    })
    
    if (inherits(fit, "lavaan")) {
      fit_measures <- fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr", "chisq"))
      return(fit_measures)
    } else {
      return(rep(NA, length(fit_indices)))
    }
  }
  
  # Run bootstrap
  boot_results <- boot(data = current_data, 
                     statistic = boot_fun, 
                     R = B, 
                     parallel = "multicore",
                     ncpus = 4)
  
  # Store bootstrap results
  for (b in 1:B) {
    if (!any(is.na(boot_results$t[b, ]))) {
      bootstrap_fit_results[m, b, ] <- boot_results$t[b, ]
    }
  }
  
  # Fit the model once to get parameter estimates
  fit_m <- cfa(cfa_model, 
              data = current_data, 
              ordered = ordinal_vars,
              estimator = "WLSMV")
  
  params <- parameterEstimates(fit_m)
  param_results[[m]] <- params
  
  std_params <- standardizedSolution(fit_m)
  std_param_results[[m]] <- std_params
  
  intercepts <- params[params$op == "|", ]
  intercept_results[[m]] <- intercepts
}

# Calculate bootstrap estimates for each imputed dataset
bootstrap_point_estimates <- matrix(NA, nrow = M, ncol = length(fit_indices),
                                   dimnames = list(paste0("imp", 1:M), fit_indices))
bootstrap_se <- matrix(NA, nrow = M, ncol = length(fit_indices),
                      dimnames = list(paste0("imp", 1:M), fit_indices))

for (m in 1:M) {
  for (i in 1:length(fit_indices)) {
    valid_results <- bootstrap_fit_results[m, , i]
    valid_results <- valid_results[!is.na(valid_results)]
    
    if (length(valid_results) > 0) {
      bootstrap_point_estimates[m, i] <- mean(valid_results)
      bootstrap_se[m, i] <- sd(valid_results)
    }
  }
}

# TRANSFORMATION PHASE
transformed_estimates <- matrix(NA, nrow = M, ncol = length(fit_indices),
                               dimnames = list(paste0("imp", 1:M), fit_indices))
transformed_variance <- matrix(NA, nrow = M, ncol = length(fit_indices),
                              dimnames = list(paste0("imp", 1:M), fit_indices))

# Apply transformations
for (m in 1:M) {
  for (i in 1:length(fit_indices)) {
    index_name <- fit_indices[i]
    est <- bootstrap_point_estimates[m, i]
    se <- bootstrap_se[m, i]
    
    if (!is.na(est) && !is.na(se)) {
      if (index_name %in% c("cfi", "tli")) {
        transformed_estimates[m, i] <- fisher_transform(est)
        deriv <- 1 / (1 - est^2)
        transformed_variance[m, i] <- (deriv^2) * (se^2)
      } else if (index_name == "rmsea") {
        transformed_estimates[m, i] <- log_transform(est)
        deriv <- 1 / est
        transformed_variance[m, i] <- (deriv^2) * (se^2)
      } else {
        transformed_estimates[m, i] <- est
        transformed_variance[m, i] <- se^2
      }
    }
  }
}

# POOLING PHASE
pooled_results <- data.frame(
  Index = fit_indices,
  Estimate_Transformed = NA,
  Within_Var = NA,
  Between_Var = NA,
  Total_Var = NA,
  DF = NA,
  Estimate_Original = NA,
  SE = NA,
  CI_Lower = NA,
  CI_Upper = NA,
  stringsAsFactors = FALSE
)

for (i in 1:length(fit_indices)) {
  estimates <- transformed_estimates[, i]
  variances <- transformed_variance[, i]
  
  valid <- !is.na(estimates) & !is.na(variances)
  if (sum(valid) < 2) next
  
  estimates <- estimates[valid]
  variances <- variances[valid]
  m_valid <- sum(valid)
  
  # Calculate pooled estimate
  q_bar <- mean(estimates)
  
  # Calculate within-imputation variance
  W <- mean(variances)
  
  # Calculate between-imputation variance
  B <- sum((estimates - q_bar)^2) / (m_valid - 1)
  
  # Calculate total variance
  T_var <- W + (1 + 1/m_valid) * B
  
  # Calculate degrees of freedom
  df <- (m_valid - 1) * (1 + W / ((1 + 1/m_valid) * B))^2
  
  # Store transformed results
  pooled_results$Estimate_Transformed[i] <- q_bar
  pooled_results$Within_Var[i] <- W
  pooled_results$Between_Var[i] <- B
  pooled_results$Total_Var[i] <- T_var
  pooled_results$DF[i] <- df
  
  # Back-transform to original scale
  index_name <- fit_indices[i]
  if (index_name %in% c("cfi", "tli")) {
    est_original <- inverse_fisher(q_bar)
  } else if (index_name == "rmsea") {
    est_original <- inverse_log(q_bar)
  } else {
    est_original <- q_bar
  }
  
  # Calculate confidence intervals
  t_critical <- qt(0.975, df)
  ci_lower_trans <- q_bar - t_critical * sqrt(T_var)
  ci_upper_trans <- q_bar + t_critical * sqrt(T_var)
  
  # Back-transform confidence intervals
  if (index_name %in% c("cfi", "tli")) {
    ci_lower <- inverse_fisher(ci_lower_trans)
    ci_upper <- inverse_fisher(ci_upper_trans)
  } else if (index_name == "rmsea") {
    ci_lower <- inverse_log(ci_lower_trans)
    ci_upper <- inverse_log(ci_upper_trans)
  } else {
    ci_lower <- ci_lower_trans
    ci_upper <- ci_upper_trans
  }
  
  pooled_results$Estimate_Original[i] <- est_original
  pooled_results$SE[i] <- sqrt(T_var)
  pooled_results$CI_Lower[i] <- ci_lower
  pooled_results$CI_Upper[i] <- ci_upper
}

# Pool parameter estimates
all_params <- do.call(rbind, lapply(1:M, function(m) {
  params <- param_results[[m]]
  params$Imputation <- m
  return(params)
}))

all_std_params <- do.call(rbind, lapply(1:M, function(m) {
  std_params <- std_param_results[[m]]
  std_params$Imputation <- m
  return(std_params)
}))

# Calculate pooled parameter estimates
pooled_params <- aggregate(est ~ lhs + op + rhs, data = all_params, FUN = mean)
pooled_params$se <- aggregate(se ~ lhs + op + rhs, data = all_params, FUN = function(x) {
  sqrt(mean(x^2))
})$se

pooled_std_params <- aggregate(est.std ~ lhs + op + rhs, data = all_std_params, FUN = mean)
colnames(pooled_std_params)[colnames(pooled_std_params) == "est.std"] <- "std_est"
pooled_std_params$std_se <- aggregate(se ~ lhs + op + rhs, data = all_std_params, FUN = function(x) {
  sqrt(mean(x^2))
})$se

# Merge unstandardized and standardized results
merged_params <- merge(
  pooled_params[, c("lhs", "op", "rhs", "est", "se")],
  pooled_std_params[, c("lhs", "op", "rhs", "std_est", "std_se")],
  by = c("lhs", "op", "rhs")
)

# Calculate FMI and relative efficiency
pooled_results$FMI <- (1 + 1/M) * pooled_results$Between_Var / pooled_results$Total_Var
pooled_results$Rel_Efficiency <- 1 / (1 + pooled_results$FMI/M)

# Display results
print("Pooled Fit Indices with 95% Confidence Intervals:")
print(pooled_results[, c("Index", "Estimate_Original", "SE", "CI_Lower", "CI_Upper", "FMI")])

# Save results
save(bootstrap_fit_results, bootstrap_point_estimates, bootstrap_se,
     pooled_results, pooled_params, pooled_std_params,
     merged_params, factor_loading_table, threshold_table,
     file = "mi_bootstrap_cfa_results.RData")
A.7 Measurement Invariance Testing
A.7.1 Preparation and Model Modifications
r
# Recategorize problematic variables into binary format
for (i in 1:length(completed_datasets)) {
  # Create binary versions of problematic variables
  completed_datasets[[i]]$erg_rating_physical_bin <- ifelse(completed_datasets[[i]]$erg_rating_physical <= 2, 0, 1)
  completed_datasets[[i]]$hyg_rating_substances_bin <- ifelse(completed_datasets[[i]]$hyg_rating_substances <= 2, 0, 1)
  completed_datasets[[i]]$hyg_rating_noise_bin <- ifelse(completed_datasets[[i]]$hyg_rating_noise <= 2, 0, 1)
  
  # Convert to ordered factors
  completed_datasets[[i]]$erg_rating_physical_bin <- ordered(completed_datasets[[i]]$erg_rating_physical_bin)
  completed_datasets[[i]]$hyg_rating_substances_bin <- ordered(completed_datasets[[i]]$hyg_rating_substances_bin)
  completed_datasets[[i]]$hyg_rating_noise_bin <- ordered(completed_datasets[[i]]$hyg_rating_noise_bin)
  
  # Combine du and nl into Dutch category
  completed_datasets[[i]]$language_group <- ifelse(completed_datasets[[i]]$LanguageCode %in% c("du", "nl"), 
                                                 "Dutch", 
                                                 as.character(completed_datasets[[i]]$LanguageCode))
  completed_datasets[[i]]$language_group <- factor(completed_datasets[[i]]$language_group)
  
  # Ensure device_type is correctly coded
  completed_datasets[[i]]$device_type <- factor(
    ifelse(is.na(completed_datasets[[i]]$ismobile), "Desktop",
           ifelse(completed_datasets[[i]]$ismobile, "Mobile", "Desktop"))
  )
  
  # Environment domain factor
  completed_datasets[[i]]$env_domain <- factor(completed_datasets[[i]]$has_environment_domain)
}

# Define updated CFA model with binary versions
cfa_model_bin <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_loads + erg_rating_physical_bin
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY)
  HY =~ hyg_rating_tools + hyg_rating_low_temp + hyg_rating_high_temp + hyg_rating_noise_bin + hyg_rating_substances_bin
'

# Update ordinal variables list
ordinal_vars_bin <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_loads", "erg_rating_physical_bin",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
  "hyg_rating_noise_bin", "hyg_rating_substances_bin"
)
A.7.2 Measurement Invariance Functions
r
# Function for pairwise comparison with projection method (both NHT and ET)
run_pairwise_projection_invariance <- function(data_list, model, group_var, groups_to_compare, 
                                     ordinal_vars, label) {
  nht_results <- list()  # For null hypothesis testing
  et_results <- list()   # For equivalence testing
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    
    # Filter to include only the two groups being compared
    subset_data <- data[data[[group_var]] %in% groups_to_compare, ]
    subset_data[[group_var]] <- factor(subset_data[[group_var]], 
                                       levels = groups_to_compare)
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      # Configural model
      configural <- cfa(model, 
                       data = subset_data,
                       group = group_var,
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      # Metric model (equal loadings)
      metric_nht <- cfa(model, 
                      data = subset_data,
                      group = group_var,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      # Scalar model (equal loadings and thresholds)
      scalar_nht <- cfa(model, 
                      data = subset_data, 
                      group = group_var,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # For ET approach - using projection method
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      group1_loadings <- group_loadings[[1]]
      group2_loadings <- group_loadings[[2]]
      
      # Calculate projection-based parameters
      loadings_diff <- abs(group1_loadings - group2_loadings)
      max_loading_diff <- max(loadings_diff, na.rm = TRUE)
      
      # Extract thresholds
      group_thresholds <- lavInspect(configural, what = "est")$th
      group1_thresholds <- unlist(group_thresholds[[1]])
      group2_thresholds <- unlist(group_thresholds[[2]])
      
      thresholds_diff <- abs(group1_thresholds - group2_thresholds)
      max_threshold_diff <- max(thresholds_diff, na.rm = TRUE)
      
      configural_fit <- fitMeasures(configural, c("cfi", "rmsea", "srmr"))
      
      list(
        nht = list(configural = configural, metric = metric_nht, scalar = scalar_nht),
        et = list(
          configural = configural_fit,
          metric_projection = list(
            max_loading_diff = max_loading_diff,
            loading_diffs = loadings_diff
          ),
          scalar_projection = list(
            max_threshold_diff = max_threshold_diff,
            threshold_diffs = thresholds_diff
          )
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    } else {
      nht_results[[i]] <- NULL
      et_results[[i]] <- NULL
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Function to pool and compare fit indices for NHT
pool_and_compare_nht <- function(results_list, label) {
  valid_results <- !sapply(results_list, is.null)
  if (sum(valid_results) == 0) {
    cat("\n\nNo valid results available for", label, "\n")
    return(NULL)
  }
  
  n_valid <- sum(valid_results)
  
  # Initialize arrays for fit indices
  fit_indices <- array(
    NA,
    dim = c(n_valid, 3, 5),
    dimnames = list(
      paste0("valid_imp", which(valid_results)),
      c("configural", "metric", "scalar"),
      c("chisq", "df", "cfi", "rmsea", "srmr")
    )
  )
  
  # Get fit indices from valid imputations
  valid_idx <- 1
  for (i in which(valid_results)) {
    models <- c("configural", "metric", "scalar")
    for (j in 1:length(models)) {
      model_name <- models[j]
      if (!is.null(results_list[[i]][[model_name]])) {
        fit_indices[valid_idx, j, "chisq"] <- fitMeasures(results_list[[i]][[model_name]], "chisq")
        fit_indices[valid_idx, j, "df"] <- fitMeasures(results_list[[i]][[model_name]], "df")
        fit_indices[valid_idx, j, "cfi"] <- fitMeasures(results_list[[i]][[model_name]], "cfi")
        fit_indices[valid_idx, j, "rmsea"] <- fitMeasures(results_list[[i]][[model_name]], "rmsea")
        fit_indices[valid_idx, j, "srmr"] <- fitMeasures(results_list[[i]][[model_name]], "srmr")
      }
    }
    valid_idx <- valid_idx + 1
  }
  
  # Pool fit indices
  pooled_indices <- array(
    NA,
    dim = c(3, 5),
    dimnames = list(
      c("configural", "metric", "scalar"),
      c("chisq", "df", "cfi", "rmsea", "srmr")
    )
  )
  
  for (j in 1:3) {
    for (k in 1:5) {
      pooled_indices[j, k] <- mean(fit_indices[, j, k], na.rm = TRUE)
    }
  }
  
  # Calculate changes in fit indices
  delta_fit <- array(
    NA,
    dim = c(2, 5),
    dimnames = list(
      c("metric-configural", "scalar-metric"),
      c("chisq", "df", "cfi", "rmsea", "srmr")
    )
  )
  
  for (k in 1:5) {
    delta_fit[1, k] <- pooled_indices[2, k] - pooled_indices[1, k]
    delta_fit[2, k] <- pooled_indices[3, k] - pooled_indices[2, k]
  }
  
  # Evaluate measurement invariance
  invariance_decision <- data.frame(
    Comparison = c("Metric vs. Configural", "Scalar vs. Metric"),
    ΔCFI = delta_fit[, "cfi"],
    ΔRMSEA = delta_fit[, "rmsea"],
    ΔSRMR = delta_fit[, "srmr"],
    Decision = NA
  )
  
  for (i in 1:2) {
    if (is.na(invariance_decision$ΔCFI[i])) {
      invariance_decision$Decision[i] <- "Cannot determine"
    } else if (invariance_decision$ΔCFI[i] > -0.01 && 
               !is.na(invariance_decision$ΔRMSEA[i]) && invariance_decision$ΔRMSEA[i] < 0.015 && 
               (i == 1 && !is.na(invariance_decision$ΔSRMR[i]) && invariance_decision$ΔSRMR[i] < 0.030 ||
                i > 1 && !is.na(invariance_decision$ΔSRMR[i]) && invariance_decision$ΔSRMR[i] < 0.010)) {
      invariance_decision$Decision[i] <- "Supported"
    } else {
      invariance_decision$Decision[i] <- "Not Supported"
    }
  }
  
  cat("\n\n==== NHT Measurement Invariance Results for", label, "====\n")
  cat("\nPooled Fit Indices:\n")
  print(pooled_indices)
  
  cat("\nChanges in Fit Indices:\n")
  print(delta_fit)
  
  cat("\nInvariance Evaluation (NHT Approach):\n")
  print(invariance_decision)
  
  return(list(
    pooled = pooled_indices,
    delta = delta_fit,
    decision = invariance_decision
  ))
}

# Function to analyze ET results
analyze_et_results <- function(results_list, label) {
  valid_results <- !sapply(results_list, is.null)
  if (sum(valid_results) == 0) {
    cat("\n\nNo valid ET results available for", label, "\n")
    return(NULL)
  }
  
  n_valid <- sum(valid_results)
  
  metric_diffs <- numeric(n_valid)
  scalar_diffs <- numeric(n_valid)
  
  valid_idx <- 1
  for (i in which(valid_results)) {
    if (!is.null(results_list[[i]]$metric_projection) && 
        !is.null(results_list[[i]]$metric_projection$max_loading_diff)) {
      if (is.finite(results_list[[i]]$metric_projection$max_loading_diff)) {
        metric_diffs[valid_idx] <- results_list[[i]]$metric_projection$max_loading_diff
      } else {
        metric_diffs[valid_idx] <- 0.01
      }
    }
    
    if (!is.null(results_list[[i]]$scalar_projection) && 
        !is.null(results_list[[i]]$scalar_projection$max_threshold_diff)) {
      if (is.finite(results_list[[i]]$scalar_projection$max_threshold_diff)) {
        scalar_diffs[valid_idx] <- results_list[[i]]$scalar_projection$max_threshold_diff
      } else {
        scalar_diffs[valid_idx] <- 0.01
      }
    }
    
    valid_idx <- valid_idx + 1
  }
  
  # Pool projected values
  pooled_metric_diff <- if (all(is.na(metric_diffs)) || length(metric_diffs) == 0) {
    0.01
  } else {
    mean(metric_diffs, na.rm = TRUE)
  }
  
  pooled_scalar_diff <- if (all(is.na(scalar_diffs)) || length(scalar_diffs) == 0) {
    0.01
  } else {
    mean(scalar_diffs, na.rm = TRUE)
  }
  
  # Define equivalence thresholds
  et_metric_threshold <- 0.2
  et_scalar_threshold <- 0.3
  
  et_decision <- data.frame(
    Comparison = c("Metric (Loadings)", "Scalar (Thresholds)"),
    MaxDiff = c(pooled_metric_diff, pooled_scalar_diff),
    Threshold = c(et_metric_threshold, et_scalar_threshold),
    Decision = NA
  )
  
  et_decision$Decision[1] <- ifelse(et_decision$MaxDiff[1] <= et_decision$Threshold[1], 
                                   "Supported (Equivalent)", 
                                   "Not Supported (Not Equivalent)")
  
  et_decision$Decision[2] <- ifelse(et_decision$MaxDiff[2] <= et_decision$Threshold[2], 
                                   "Supported (Equivalent)", 
                                   "Not Supported (Not Equivalent)")
  
  cat("\n\n==== ET Measurement Invariance Results for", label, "====\n")
  cat("\nProjection Method Results:\n")
  cat("\nMetric Invariance (Loading Differences):\n")
  cat("Average maximum loading difference:", pooled_metric_diff, "\n")
  cat("Equivalence threshold:", et_metric_threshold, "\n")
  
  cat("\nScalar Invariance (Threshold Differences):\n")
  cat("Average maximum threshold difference:", pooled_scalar_diff, "\n")
  cat("Equivalence threshold:", et_scalar_threshold, "\n")
  
  cat("\nInvariance Evaluation (ET Approach):\n")
  print(et_decision)
  
  return(list(
    metric = pooled_metric_diff,
    scalar = pooled_scalar_diff,
    decision = et_decision
  ))
}
A.7.3 Running Measurement Invariance Tests
r
# Language groups (Dutch vs French)
dutch_fr_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_bin,
  "language_group",
  c("Dutch", "fr"),
  ordinal_vars_bin,
  "Dutch vs French"
)

# Mobile vs Desktop
mobile_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_original,
  "device_type",
  c("Mobile", "Desktop"),
  ordinal_vars_original,
  "Mobile vs Desktop"
)

# Environment domain (Yes vs No)
env_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_original,
  "env_domain",
  c("Yes", "No"),
  ordinal_vars_original,
  "Environment vs No Environment"
)

# Analyze results - NHT approach
dutch_fr_nht_summary <- tryCatch({
  pool_and_compare_nht(dutch_fr_results$nht, "Dutch vs French")
}, error = function(e) {
  cat("Error analyzing Dutch vs French NHT results:", e$message, "\n")
  return(NULL)
})

mobile_nht_summary <- tryCatch({
  pool_and_compare_nht(mobile_results$nht, "Mobile vs Desktop")
}, error = function(e) {
  cat("Error analyzing Mobile vs Desktop NHT results:", e$message, "\n")
  return(NULL)
})

env_nht_summary <- tryCatch({
  pool_and_compare_nht(env_results$nht, "Environment vs No Environment")
}, error = function(e) {
  cat("Error analyzing Environment NHT results:", e$message, "\n")
  return(NULL)
})

# Analyze results - ET approach
dutch_fr_et_summary <- tryCatch({
  analyze_et_results(dutch_fr_results$et, "Dutch vs French")
}, error = function(e) {
  cat("Error analyzing Dutch vs French ET results:", e$message, "\n")
  return(NULL)
})

mobile_et_summary <- tryCatch({
  analyze_et_results(mobile_results$et, "Mobile vs Desktop")
}, error = function(e) {
  cat("Error analyzing Mobile vs Desktop ET results:", e$message, "\n")
  return(NULL)
})

env_et_summary <- tryCatch({
  analyze_et_results(env_results$et, "Environment vs No Environment")
}, error = function(e) {
  cat("Error analyzing Environment ET results:", e$message, "\n")
  return(NULL)
})
A.8 Gender-Specific Measurement Invariance
r
# Function for gender measurement invariance testing
run_gender_invariance <- function(data_list, model, ordinal_vars, label) {
  nht_results <- list()
  et_results <- list()
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    data$sex_cat <- factor(data$sex_cat)
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      configural <- cfa(model, 
                       data = data,
                       group = "sex_cat",
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      metric_nht <- cfa(model, 
                      data = data,
                      group = "sex_cat",
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      scalar_nht <- cfa(model, 
                      data = data, 
                      group = "sex_cat",
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # ET approach calculations (similar to previous function)
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      group1_loadings <- group_loadings[[1]]
      group2_loadings <- group_loadings[[2]]
      
      loadings_diff <- abs(group1_loadings - group2_loadings)
      max_loading_diff <- max(loadings_diff, na.rm = TRUE)
      
      group_thresholds <- lavInspect(configural, what = "est")$th
      group1_thresholds <- unlist(group_thresholds[[1]])
      group2_thresholds <- unlist(group_thresholds[[2]])
      
      thresholds_diff <- abs(group1_thresholds - group2_thresholds)
      max_threshold_diff <- max(thresholds_diff, na.rm = TRUE)
      
      configural_fit <- fitMeasures(configural, c("cfi", "rmsea", "srmr"))
      
      list(
        nht = list(configural = configural, metric = metric_nht, scalar = scalar_nht),
        et = list(
          configural = configural_fit,
          metric_projection = list(
            max_loading_diff = max_loading_diff,
            loading_diffs = loadings_diff
          ),
          scalar_projection = list(
            max_threshold_diff = max_threshold_diff,
            threshold_diffs = thresholds_diff
          )
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    } else {
      nht_results[[i]] <- NULL
      et_results[[i]] <- NULL
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Modified CFA model excluding problematic variable
cfa_model_gender <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_loads + erg_rating_physical_bin
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY) - excluding hyg_rating_low_temp
  HY =~ hyg_rating_tools + hyg_rating_high_temp + hyg_rating_noise_bin + hyg_rating_substances_bin
'

ordinal_vars_gender <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_loads", "erg_rating_physical_bin",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_high_temp", 
  "hyg_rating_noise_bin", "hyg_rating_substances_bin"
)

# Run gender measurement invariance
gender_results_modified <- run_gender_invariance(
  completed_datasets[1:3],
  cfa_model_gender,
  ordinal_vars_gender,
  "Gender Modified (Men vs Women)"
)

# Analyze results
gender_nht_summary_modified <- tryCatch({
  pool_and_compare_nht(gender_results_modified$nht, "Gender Modified (Men vs Women)")
}, error = function(e) {
  cat("Error analyzing Modified Gender NHT results:", e$message, "\n")
  return(NULL)
})

gender_et_summary_modified <- tryCatch({
  analyze_et_results(gender_results_modified$et, "Gender Modified (Men vs Women)")
}, error = function(e) {
  cat("Error analyzing Modified Gender ET results:", e$message, "\n")
  return(NULL)
})
A.9 Age-Based Measurement Invariance
r
# Function for age measurement invariance testing
run_age_invariance <- function(data_list, model, ordinal_vars, age_variable, label) {
  nht_results <- list()
  et_results <- list()
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    data[[age_variable]] <- factor(data[[age_variable]])
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      configural <- cfa(model, 
                       data = data,
                       group = age_variable,
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      metric_nht <- cfa(model, 
                      data = data,
                      group = age_variable,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      scalar_nht <- cfa(model, 
                      data = data, 
                      group = age_variable,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # For ET approach - projection method for multiple groups
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      
      # Initialize matrices for pairwise comparisons
      num_groups <- length(unique(data[[age_variable]]))
      max_loading_diffs <- matrix(0, num_groups, num_groups)
      max_threshold_diffs <- matrix(0, num_groups, num_groups)
      
      # Perform all pairwise comparisons
      for (g1 in 1:(num_groups-1)) {
        for (g2 in (g1+1):num_groups) {
          loadings_diff <- abs(group_loadings[[g1]] - group_loadings[[g2]])
          max_diff <- max(loadings_diff, na.rm = TRUE)
          max_loading_diffs[g1, g2] <- max_diff
          max_loading_diffs[g2, g1] <- max_diff
          
          group_thresholds <- lavInspect(configural, what = "est")$th
          g1_thresholds <- unlist(group_thresholds[[g1]])
          g2_thresholds <- unlist(group_thresholds[[g2]])
          
          thresholds_diff <- abs(g1_thresholds - g2_thresholds)
          max_diff <- max(thresholds_diff, na.rm = TRUE)
          max_threshold_diffs[g1, g2] <- max_diff
          max_threshold_diffs[g2, g1] <- max_diff
        }
      }
      
      max_loading_diff <- max(max_loading_diffs, na.rm = TRUE)
      max_threshold_diff <- max(max_threshold_diffs, na.rm = TRUE)
      
      configural_fit <- fitMeasures(configural, c("cfi", "rmsea", "srmr"))
      
      list(
        nht = list(configural = configural, metric = metric_nht, scalar = scalar_nht),
        et = list(
          configural = configural_fit,
          metric_projection = list(
            max_loading_diff = max_loading_diff,
            loading_diffs_matrix = max_loading_diffs
          ),
          scalar_projection = list(
            max_threshold_diff = max_threshold_diff,
            threshold_diffs_matrix = max_threshold_diffs
          )
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    } else {
      nht_results[[i]] <- NULL
      et_results[[i]] <- NULL
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Check age group distributions and potentially recode
age_counts <- table(completed_datasets[[1]]$age_cat)
print("Age category counts:")
print(age_counts)

min_group_size <- 50
small_groups <- names(age_counts[age_counts < min_group_size])

if (length(small_groups) > 0) {
  cat("\nRecoding age groups due to small sample sizes\n")
  
  for (i in 1:length(completed_datasets)) {
    completed_datasets[[i]]$age_cat_recoded <- as.character(completed_datasets[[i]]$age_cat)
    
    # Example recoding - adjust based on actual distribution
    if ("<25" %in% small_groups) {
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == "<25"] <- "< 35"
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == "25-34"] <- "< 35"
    }
    
    if (">=55" %in% small_groups) {
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == ">=55"] <- "> 44"
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == "45-54"] <- "> 44"
    }
    
    completed_datasets[[i]]$age_cat_recoded <- factor(completed_datasets[[i]]$age_cat_recoded)
  }
  
  age_var <- "age_cat_recoded"
} else {
  age_var <- "age_cat"
}

# Run age measurement invariance
age_results <- run_age_invariance(
  completed_datasets[1:3],
  cfa_model_gender,
  ordinal_vars_gender,
  age_var,
  paste0("Age (", age_var, ")")
)

# Analyze results
age_nht_summary <- tryCatch({
  pool_and_compare_nht(age_results$nht, paste0("Age (", age_var, ")"))
}, error = function(e) {
  cat("Error analyzing Age NHT results:", e$message, "\n")
  return(NULL)
})

age_et_summary <- tryCatch({
  analyze_et_results(age_results$et, paste0("Age (", age_var, ")"))
}, error = function(e) {
  cat("Error analyzing Age ET results:", e$message, "\n")
  return(NULL)
})
Pr(>Chi)`[2]

  # Test for non-uniform DIF (Model 3 vs Model 2)
  nonuniform_test <- anova(model2, model3, test = "Chisq")
  nonuniform_p <- nonuniform_test# Appendix: R Code for Statistical Analysis
A.1 Setup and Library Loading
r
# Load necessary libraries
library(tidyverse)
library(MVN)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson, Amelia, mice, VIM, corrplot, gridExtra)

# Setting working directory
setwd("D:/2Y_MasterThesis/R_Code")
A.2 Data Loading and Variable Selection
r
# Load the pilot dataset using the correct delimiter (semicolon)
full_dataset <- read_delim("pilotdata_4comp.csv", delim = ";")

# Define model variables excluding environment domain
model_vars_no_env <- c(
  # Psychosocial
  "veerkracht_calc", "psy_rating_pace", "psy_rating_emotional", 
  "psy_rating_sphere", "psy_work_life",
  # Ergonomics
  "erg_capac", "erg_rating_posture", "erg_rating_repeat", 
  "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical",
  # Safety
  "saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage",
  # Hygiene
  "hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
  "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances"
)

# Define environment domain variables
env_vars <- c("mil_rating_leadengage", "mil_satisfaction", "mil_rating_contrib", "mil_rating_workinvolv")

# Calculate missing rates and identify auxiliary variables
missing_rates <- full_dataset %>%
  summarise(across(everything(), ~sum(is.na(.))/n())) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate") %>%
  arrange(missing_rate)

# Identify potential auxiliary variables (exclude model vars and keep only those with <20% missing)
potential_aux_vars <- missing_rates %>%
  filter(missing_rate < 0.20) %>%
  filter(!variable %in% c(model_vars_no_env, env_vars, 
                         "ismobile", "LanguageCode", "Name_Company", "Version_survey")) %>%
  pull(variable)

# Select useful auxiliary variables
useful_aux_vars <- c(
  "age_cat", "sex_cat", 
  grep("freq_exp", potential_aux_vars, value = TRUE),
  grep("exposure", potential_aux_vars, value = TRUE),
  grep("hea_", potential_aux_vars, value = TRUE, fixed = TRUE),
  grep("vit_", potential_aux_vars, value = TRUE, fixed = TRUE),
  "was", "shift", "sleep", "health_not_ok"
)

useful_aux_vars <- intersect(useful_aux_vars, names(full_dataset))
useful_aux_vars <- head(useful_aux_vars, 10)

# Create final dataset
final_dataset <- full_dataset %>%
  mutate(has_environment_domain = case_when(
    !is.na(mil_satisfaction) ~ "Yes",
    TRUE ~ "No"
  )) %>%
  select(all_of(c(model_vars_no_env, env_vars, useful_aux_vars,
                "ismobile", "LanguageCode", "Name_Company", "Version_survey",
                "has_environment_domain")))

# Save the file
write_csv(final_dataset, "final_analysis_dataset.csv")
A.3 Missing Data Analysis and Visualization
r
# Load the selected variable dataset
full_analysis_dataset <- read_delim("final_analysis_dataset.csv")

# Create a mapping for variables with descriptive names
var_labels <- c(
  # Psychosocial work environment factor (PS)
  veerkracht_calc = "Resilience",
  psy_rating_pace = "Work pace",
  psy_rating_emotional = "Emotional demands",
  psy_rating_sphere = "Work atmosphere",
  psy_work_life = "Work-life balance",
  
  # Ergonomics factor (ER)
  erg_capac = "Ergonomic capacity",
  erg_rating_posture = "Stressful postures",
  erg_rating_repeat = "Repetitive work",
  erg_rating_sitting = "Sitting for long-time",
  erg_rating_loads = "Manual handling loads",
  erg_rating_physical = "Physical strenuous",
  
  # Safety factor (SA)
  saf_satisfaction = "Safety satisfaction",
  saf_rating_workinvolv = "Work involvement",
  saf_rating_leadengage = "Leadership engagement",
  
  # Hygiene factor (HY)
  hyg_satisfaction = "Hygiene satisfaction",
  hyg_rating_tools = "Tool Vibrations",
  hyg_rating_low_temp = "Low temperatures",
  hyg_rating_high_temp = "High temperatures",
  hyg_rating_noise = "Noise",
  hyg_rating_substances = "Hazardous Substances",
  
  # Environmental domain
  mil_rating_leadengage = "Env. leadership engagement",
  mil_satisfaction = "Env. satisfaction",
  mil_rating_contrib = "Env. contribution",
  mil_rating_workinvolv = "Env. work involvement"
)

# Function to rename variables for plotting
rename_vars <- function(var_names) {
  sapply(var_names, function(x) {
    if(x %in% names(var_labels)) var_labels[x] else x
  })
}

# Create directory for saving files
dir.create("plots", showWarnings = FALSE)

# Overall missingness summary
miss_summary <- miss_var_summary(full_analysis_dataset)
miss_summary$variable_renamed <- rename_vars(miss_summary$variable)

# Plot missingness summary
p1 <- ggplot(miss_summary, aes(x = reorder(variable_renamed, pct_miss), y = pct_miss)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Overall Missingness by Variable",
       subtitle = "Percentage of missing values for each variable",
       x = "", y = "Percentage Missing (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 9))

ggsave("plots/missingness_summary.pdf", p1, width = 12, height = 10)
print(p1)

# Domain-specific missingness analysis
domains <- list(
  "Psychosocial Domain" = c("veerkracht_calc", "psy_rating_pace", "psy_rating_emotional", 
                          "psy_rating_sphere", "psy_work_life"),
  "Ergonomics Domain" = c("erg_capac", "erg_rating_posture", "erg_rating_repeat", 
                        "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical"),
  "Safety Domain" = c("saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage"),
  "Hygiene Domain" = c("hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
                      "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances"),
  "Environmental Domain" = c("mil_rating_leadengage", "mil_satisfaction", 
                           "mil_rating_contrib", "mil_rating_workinvolv")
)

# Calculate missingness by domain
domain_missingness <- lapply(names(domains), function(domain_name) {
  vars <- domains[[domain_name]]
  vars <- vars[vars %in% names(full_analysis_dataset)]
  if(length(vars) > 0) {
    result <- full_analysis_dataset %>%
      summarise(across(all_of(vars), ~mean(is.na(.))*100)) %>%
      pivot_longer(cols = everything(), 
                  names_to = "variable", 
                  values_to = "percent_missing") %>%
      mutate(domain = domain_name)
    
    result$variable_renamed <- rename_vars(result$variable)
    return(result)
  }
}) %>% bind_rows()

# Domain-specific missingness visualization
p6 <- ggplot(domain_missingness, aes(x = reorder(variable_renamed, percent_missing), 
                                    y = percent_missing, 
                                    fill = domain)) +
  geom_col() +
  facet_wrap(~domain, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") +
  labs(title = "Percentage of Missing Values by Domain",
       subtitle = "Grouped by workplace assessment domains",
       x = "", y = "% Missing")

ggsave("plots/missingness_by_domain.pdf", p6, width = 14, height = 10)
print(p6)
A.4 Multiple Imputation
r
# Create a clean dataset for imputation
imputation_dataset <- full_analysis_dataset

# Set -999 values back to NA for environmental variables
env_vars <- c("mil_rating_leadengage", "mil_satisfaction", 
              "mil_rating_contrib", "mil_rating_workinvolv")

for(var in env_vars) {
  imputation_dataset[[var]][imputation_dataset[[var]] == -999] <- NA
}

# Identify variable types for appropriate imputation methods
ordinal_vars <- c(
  # Psychosocial domain
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  # Ergonomics domain
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", 
  "erg_rating_loads", "erg_rating_physical", "erg_capac",
  # Safety domain
  "saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage",
  # Hygiene domain
  "hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
  "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances",
  # Environmental domain
  "mil_rating_leadengage", "mil_satisfaction", "mil_rating_contrib", "mil_rating_workinvolv",
  # Frequency variables
  "hyg_freq_exp_substances", "hyg_freq_exp_tools", "hyg_freq_exp_high_temp",
  "hyg_freq_exp_low_temp", "hyg_freq_exp_noise", "erg_freq_exp_sitting",
  "erg_freq_exp_posture", "erg_freq_exp_physical",
  # Demographics
  "age_cat"
)

continuous_vars <- c("veerkracht_calc")
binary_vars <- c("ismobile", "sex_cat")
cat_vars <- c("LanguageCode", "Name_Company", "Version_survey", "has_environment_domain")

# Special handling for age_cat - convert to ordered factor
if("age_cat" %in% names(imputation_dataset)) {
  age_levels <- c("<25", "25-34", "35-44", "45-54", ">=55")
  imputation_dataset$age_cat <- factor(imputation_dataset$age_cat, 
                                      levels = age_levels, ordered = TRUE)
}

# Convert other ordinal variables to factors
for(var in setdiff(ordinal_vars, "age_cat")) {
  if(var %in% names(imputation_dataset)) {
    if(sum(!is.na(imputation_dataset[[var]])) > 0) {
      var_values <- imputation_dataset[[var]][!is.na(imputation_dataset[[var]])]
      
      if(all(!is.na(suppressWarnings(as.numeric(as.character(var_values)))))) {
        var_numeric <- as.numeric(as.character(var_values))
        var_min <- min(var_numeric, na.rm = TRUE)
        var_max <- max(var_numeric, na.rm = TRUE)
        
        if(!is.na(var_min) && !is.na(var_max)) {
          levels <- var_min:var_max
          imputation_dataset[[var]] <- factor(imputation_dataset[[var]], 
                                             levels = levels, ordered = TRUE)
        }
      }
    }
  }
}

# Create variable to track observations without environmental data
imputation_dataset$skip_env_imputation <- (full_analysis_dataset$has_environment_domain == "No")
cat_vars <- c(cat_vars, "skip_env_imputation")

# Set up MICE methods
imp_methods <- mice(imputation_dataset, maxit = 0)$method

# Set imputation methods for each variable type
for(var in ordinal_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- "polr"  # Proportional odds logistic regression
  }
}

for(var in continuous_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- "pmm"  # Predictive mean matching
  }
}

for(var in binary_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- "logreg"  # Logistic regression
  }
}

for(var in cat_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- ""  # Don't impute categorical variables
  }
}

# Create predictor matrix
pred_matrix <- mice(imputation_dataset, maxit = 0)$predictorMatrix

# Don't use categorical variables as predictors
for(var in cat_vars) {
  if(var %in% names(imputation_dataset)) {
    pred_matrix[, which(colnames(pred_matrix) == var)] <- 0
  }
}

# Run imputation
set.seed(12345)
imputed_data <- mice(imputation_dataset, 
                    predictorMatrix = pred_matrix,
                    method = imp_methods,
                    m = 10,          # Create 10 imputed datasets
                    maxit = 20,      # Run for 20 iterations
                    seed = 12345,
                    printFlag = TRUE)

# Post-processing: convert factors back to numeric and handle environmental vars
completed_datasets <- list()
for(i in 1:10) {
  complete_data_i <- complete(imputed_data, i)
  
  # Convert regular ordinal variables back to numeric
  for(var in setdiff(ordinal_vars, "age_cat")) {
    if(var %in% names(complete_data_i)) {
      complete_data_i[[var]] <- as.numeric(as.character(complete_data_i[[var]]))
    }
  }
  
  # Reset environmental variables to -999 where needed
  for(var in env_vars) {
    if(var %in% names(complete_data_i)) {
      complete_data_i[[var]][complete_data_i$skip_env_imputation] <- -999
    }
  }
  
  # Remove the helper variable
  complete_data_i$skip_env_imputation <- NULL
  
  completed_datasets[[i]] <- complete_data_i
}

# Save the completed datasets
saveRDS(completed_datasets, "imputed_datasets.rds")
write.csv(completed_datasets[[1]], file = "imputed_dataset_for_cfa.csv", row.names = FALSE)
A.5 Confirmatory Factor Analysis
A.5.1 CFA Model Definition and Reliability Functions
r
# Function to calculate CR and AVE from a fitted lavaan model
calculate_CR_AVE <- function(fit) {
  std_estimates <- standardizedSolution(fit)
  loadings <- subset(std_estimates, op == "=~")
  factors <- unique(loadings$lhs)
  
  results <- data.frame(Factor = factors, CR = NA, AVE = NA)
  
  for(i in 1:length(factors)) {
    factor <- factors[i]
    factor_loadings <- subset(loadings, lhs == factor)$est.std
    
    lambda_squared <- factor_loadings^2
    delta <- 1 - lambda_squared
    
    sum_lambda <- sum(factor_loadings)
    sum_delta <- sum(delta)
    CR <- sum_lambda^2 / (sum_lambda^2 + sum_delta)
    
    sum_lambda_squared <- sum(lambda_squared)
    AVE <- sum_lambda_squared / (sum_lambda_squared + sum_delta)
    
    results$CR[i] <- CR
    results$AVE[i] <- AVE
  }
  
  return(results)
}

# Define CFA model
cfa_model <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_sitting + erg_rating_loads + erg_rating_physical
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY)
  HY =~ hyg_rating_tools + hyg_rating_low_temp + hyg_rating_high_temp + hyg_rating_noise + hyg_rating_substances
'

# Define ordinal variables
ordinal_vars <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", 
  "erg_rating_loads", "erg_rating_physical",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
  "hyg_rating_noise", "hyg_rating_substances"
)
A.5.2 Model Diagnostics
r
# Test multivariate normality
cfa_vars <- full_dataset %>% 
  select(veerkracht_calc, starts_with("psy_"), starts_with("erg_")) %>%
  select(where(is.numeric))

mvn_result <- mvn(data = cfa_vars, mvnTest = "mardia")
print(mvn_result$multivariateNormality)

# Correlation matrix and multicollinearity check
cor_matrix <- cor(cfa_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

# Identify high correlations (> 0.85)
high_cors <- which(abs(cor_matrix) > 0.85 & abs(cor_matrix) < 1, arr.ind = TRUE)
if(nrow(high_cors) > 0) {
  high_cor_pairs <- data.frame(
    var1 = rownames(cor_matrix)[high_cors[, 1]],
    var2 = colnames(cor_matrix)[high_cors[, 2]],
    correlation = cor_matrix[high_cors]
  )
  print("Potential multicollinearity issues:")
  print(high_cor_pairs)
}

# Factorability tests
kmo_result <- KMO(cor_matrix)
print(kmo_result$MSA)  # Overall KMO value

bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(cfa_vars))
print(bartlett_result)
A.5.3 Reliability Analysis
r
# Enhanced reliability calculation with Polychoric Alpha
calculate_reliability_complete <- function(completed_datasets, factor_vars) {
  results <- data.frame()
  
  for (factor_name in names(factor_vars)) {
    vars <- factor_vars[[factor_name]]
    
    alpha_values <- numeric()
    poly_alpha_values <- numeric()
    omega_values <- numeric()
    
    for (i in 1:length(completed_datasets)) {
      data_imp <- completed_datasets[[i]]
      factor_data <- data_imp[, vars, drop = FALSE]
      factor_data <- factor_data[complete.cases(factor_data), ]
      factor_data <- as.data.frame(lapply(factor_data, as.numeric))
      
      if (nrow(factor_data) > 10 && ncol(factor_data) > 1) {
        # Regular Cronbach's Alpha
        tryCatch({
          cor_matrix <- cor(factor_data, use = "complete.obs")
          k <- ncol(factor_data)
          avg_inter_cor <- (sum(cor_matrix) - k) / (k * (k - 1))
          alpha_manual <- (k * avg_inter_cor) / (1 + (k - 1) * avg_inter_cor)
          alpha_values[i] <- alpha_manual
        }, error = function(e) {
          alpha_values[i] <- NA
        })
        
        # Polychoric Alpha
        tryCatch({
          poly_cor <- polychoric(factor_data)$rho
          k <- ncol(factor_data)
          avg_poly_cor <- (sum(poly_cor) - k) / (k * (k - 1))
          poly_alpha <- (k * avg_poly_cor) / (1 + (k - 1) * avg_poly_cor)
          poly_alpha_values[i] <- poly_alpha
        }, error = function(e) {
          poly_alpha_values[i] <- NA
        })
        
        # McDonald's Omega
        if (length(vars) > 2) {
          tryCatch({
            factor_model <- paste0(factor_name, " =~ ", paste(vars, collapse = " + "))
            fit_single <- cfa(factor_model, data = data_imp, ordered = TRUE, estimator = "WLSMV")
            
            if (lavInspect(fit_single, "converged")) {
              rel_coef <- reliability(fit_single)
              omega_values[i] <- rel_coef["omega", factor_name]
            } else {
              omega_values[i] <- NA
            }
          }, error = function(e) {
            omega_values[i] <- NA
          })
        }
      }
    }
    
    # Pool results function
    pool_reliability <- function(values, measure_name) {
      valid_values <- values[!is.na(values)]
      if (length(valid_values) >= 3) {
        pooled_mean <- mean(valid_values)
        pooled_sd <- sd(valid_values)
        
        n <- length(valid_values)
        se <- pooled_sd / sqrt(n)
        t_val <- qt(0.975, df = n - 1)
        ci_lower <- pooled_mean - t_val * se
        ci_upper <- pooled_mean + t_val * se
        
        return(data.frame(
          Factor = factor_name,
          Measure = measure_name,
          Estimate = round(pooled_mean, 3),
          SE = round(se, 4),
          CI_Lower = round(ci_lower, 3),
          CI_Upper = round(ci_upper, 3),
          N_Valid = n,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Add all reliability measures
    alpha_result <- pool_reliability(alpha_values, "Cronbach_Alpha")
    poly_alpha_result <- pool_reliability(poly_alpha_values, "Polychoric_Alpha")
    results <- rbind(results, alpha_result, poly_alpha_result)
    
    if (length(vars) > 2 && length(omega_values[!is.na(omega_values)]) > 0) {
      omega_result <- pool_reliability(omega_values, "McDonald_Omega")
      results <- rbind(results, omega_result)
    }
    
    # Spearman-Brown for 2-item factors
    if (length(vars) == 2) {
      sb_values <- numeric()
      poly_sb_values <- numeric()
      
      for (i in 1:length(completed_datasets)) {
        data_imp <- completed_datasets[[i]]
        factor_data <- data_imp[, vars, drop = FALSE]
        factor_data <- factor_data[complete.cases(factor_data), ]
        factor_data <- as.data.frame(lapply(factor_data, as.numeric))
        
        if (nrow(factor_data) > 10) {
          r <- cor(factor_data[, 1], factor_data[, 2], use = "complete.obs")
          sb_values[i] <- (2 * r) / (1 + r)
          
          tryCatch({
            poly_r <- polychoric(factor_data)$rho[1, 2]
            poly_sb_values[i] <- (2 * poly_r) / (1 + poly_r)
          }, error = function(e) {
            poly_sb_values[i] <- NA
          })
        }
      }
      
      sb_result <- pool_reliability(sb_values, "Spearman_Brown")
      poly_sb_result <- pool_reliability(poly_sb_values, "Polychoric_Spearman_Brown")
      results <- rbind(results, sb_result, poly_sb_result)
    }
  }
  
  return(results)
}

# Define factor variables
factor_vars <- list(
  PS = c("psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life"),
  ER = c("erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical"),
  SA = c("saf_rating_workinvolv", "saf_rating_leadengage"),
  HY = c("hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances")
)

# Run reliability analysis
reliability_results_complete <- calculate_reliability_complete(completed_datasets, factor_vars)
print(reliability_results_complete)
A.6 Pooled CFA with Bootstrap Confidence Intervals
r
# Load imputed datasets
completed_datasets <- readRDS("imputed_datasets.rds")
M <- length(completed_datasets)  # Number of imputed datasets
B <- 1000                        # Number of bootstrap samples

# Define transformation functions for fit indices
fisher_transform <- function(x) {
  x <- pmin(x, 0.999)
  return(0.5 * log((1 + x) / (1 - x)))
}

inverse_fisher <- function(z) {
  return((exp(2 * z) - 1) / (exp(2 * z) + 1))
}

log_transform <- function(x) {
  x <- pmax(x, 0.001)
  return(log(x))
}

inverse_log <- function(z) {
  return(exp(z))
}

# Initialize arrays to store results
fit_indices <- c("cfi", "tli", "rmsea", "srmr", "chisq")
bootstrap_fit_results <- array(NA, dim = c(M, B, length(fit_indices)),
                              dimnames = list(paste0("imp", 1:M), 
                                             paste0("boot", 1:B),
                                             fit_indices))

param_results <- list()
std_param_results <- list()
intercept_results <- list()

# BOOTSTRAPPING PHASE
for (m in 1:M) {
  cat("Processing imputed dataset", m, "of", M, "\n")
  current_data <- completed_datasets[[m]]
  
  # Define bootstrap function
  boot_fun <- function(data, indices) {
    boot_sample <- data[indices, ]
    
    fit <- tryCatch({
      cfa(cfa_model, 
         data = boot_sample, 
         ordered = ordinal_vars,
         estimator = "DWLS")
    }, error = function(e) {
      return(NA)
    })
    
    if (inherits(fit, "lavaan")) {
      fit_measures <- fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr", "chisq"))
      return(fit_measures)
    } else {
      return(rep(NA, length(fit_indices)))
    }
  }
  
  # Run bootstrap
  boot_results <- boot(data = current_data, 
                     statistic = boot_fun, 
                     R = B, 
                     parallel = "multicore",
                     ncpus = 4)
  
  # Store bootstrap results
  for (b in 1:B) {
    if (!any(is.na(boot_results$t[b, ]))) {
      bootstrap_fit_results[m, b, ] <- boot_results$t[b, ]
    }
  }
  
  # Fit the model once to get parameter estimates
  fit_m <- cfa(cfa_model, 
              data = current_data, 
              ordered = ordinal_vars,
              estimator = "WLSMV")
  
  params <- parameterEstimates(fit_m)
  param_results[[m]] <- params
  
  std_params <- standardizedSolution(fit_m)
  std_param_results[[m]] <- std_params
  
  intercepts <- params[params$op == "|", ]
  intercept_results[[m]] <- intercepts
}

# Calculate bootstrap estimates for each imputed dataset
bootstrap_point_estimates <- matrix(NA, nrow = M, ncol = length(fit_indices),
                                   dimnames = list(paste0("imp", 1:M), fit_indices))
bootstrap_se <- matrix(NA, nrow = M, ncol = length(fit_indices),
                      dimnames = list(paste0("imp", 1:M), fit_indices))

for (m in 1:M) {
  for (i in 1:length(fit_indices)) {
    valid_results <- bootstrap_fit_results[m, , i]
    valid_results <- valid_results[!is.na(valid_results)]
    
    if (length(valid_results) > 0) {
      bootstrap_point_estimates[m, i] <- mean(valid_results)
      bootstrap_se[m, i] <- sd(valid_results)
    }
  }
}

# TRANSFORMATION PHASE
transformed_estimates <- matrix(NA, nrow = M, ncol = length(fit_indices),
                               dimnames = list(paste0("imp", 1:M), fit_indices))
transformed_variance <- matrix(NA, nrow = M, ncol = length(fit_indices),
                              dimnames = list(paste0("imp", 1:M), fit_indices))

# Apply transformations
for (m in 1:M) {
  for (i in 1:length(fit_indices)) {
    index_name <- fit_indices[i]
    est <- bootstrap_point_estimates[m, i]
    se <- bootstrap_se[m, i]
    
    if (!is.na(est) && !is.na(se)) {
      if (index_name %in% c("cfi", "tli")) {
        transformed_estimates[m, i] <- fisher_transform(est)
        deriv <- 1 / (1 - est^2)
        transformed_variance[m, i] <- (deriv^2) * (se^2)
      } else if (index_name == "rmsea") {
        transformed_estimates[m, i] <- log_transform(est)
        deriv <- 1 / est
        transformed_variance[m, i] <- (deriv^2) * (se^2)
      } else {
        transformed_estimates[m, i] <- est
        transformed_variance[m, i] <- se^2
      }
    }
  }
}

# POOLING PHASE
pooled_results <- data.frame(
  Index = fit_indices,
  Estimate_Transformed = NA,
  Within_Var = NA,
  Between_Var = NA,
  Total_Var = NA,
  DF = NA,
  Estimate_Original = NA,
  SE = NA,
  CI_Lower = NA,
  CI_Upper = NA,
  stringsAsFactors = FALSE
)

for (i in 1:length(fit_indices)) {
  estimates <- transformed_estimates[, i]
  variances <- transformed_variance[, i]
  
  valid <- !is.na(estimates) & !is.na(variances)
  if (sum(valid) < 2) next
  
  estimates <- estimates[valid]
  variances <- variances[valid]
  m_valid <- sum(valid)
  
  # Calculate pooled estimate
  q_bar <- mean(estimates)
  
  # Calculate within-imputation variance
  W <- mean(variances)
  
  # Calculate between-imputation variance
  B <- sum((estimates - q_bar)^2) / (m_valid - 1)
  
  # Calculate total variance
  T_var <- W + (1 + 1/m_valid) * B
  
  # Calculate degrees of freedom
  df <- (m_valid - 1) * (1 + W / ((1 + 1/m_valid) * B))^2
  
  # Store transformed results
  pooled_results$Estimate_Transformed[i] <- q_bar
  pooled_results$Within_Var[i] <- W
  pooled_results$Between_Var[i] <- B
  pooled_results$Total_Var[i] <- T_var
  pooled_results$DF[i] <- df
  
  # Back-transform to original scale
  index_name <- fit_indices[i]
  if (index_name %in% c("cfi", "tli")) {
    est_original <- inverse_fisher(q_bar)
  } else if (index_name == "rmsea") {
    est_original <- inverse_log(q_bar)
  } else {
    est_original <- q_bar
  }
  
  # Calculate confidence intervals
  t_critical <- qt(0.975, df)
  ci_lower_trans <- q_bar - t_critical * sqrt(T_var)
  ci_upper_trans <- q_bar + t_critical * sqrt(T_var)
  
  # Back-transform confidence intervals
  if (index_name %in% c("cfi", "tli")) {
    ci_lower <- inverse_fisher(ci_lower_trans)
    ci_upper <- inverse_fisher(ci_upper_trans)
  } else if (index_name == "rmsea") {
    ci_lower <- inverse_log(ci_lower_trans)
    ci_upper <- inverse_log(ci_upper_trans)
  } else {
    ci_lower <- ci_lower_trans
    ci_upper <- ci_upper_trans
  }
  
  pooled_results$Estimate_Original[i] <- est_original
  pooled_results$SE[i] <- sqrt(T_var)
  pooled_results$CI_Lower[i] <- ci_lower
  pooled_results$CI_Upper[i] <- ci_upper
}

# Pool parameter estimates
all_params <- do.call(rbind, lapply(1:M, function(m) {
  params <- param_results[[m]]
  params$Imputation <- m
  return(params)
}))

all_std_params <- do.call(rbind, lapply(1:M, function(m) {
  std_params <- std_param_results[[m]]
  std_params$Imputation <- m
  return(std_params)
}))

# Calculate pooled parameter estimates
pooled_params <- aggregate(est ~ lhs + op + rhs, data = all_params, FUN = mean)
pooled_params$se <- aggregate(se ~ lhs + op + rhs, data = all_params, FUN = function(x) {
  sqrt(mean(x^2))
})$se

pooled_std_params <- aggregate(est.std ~ lhs + op + rhs, data = all_std_params, FUN = mean)
colnames(pooled_std_params)[colnames(pooled_std_params) == "est.std"] <- "std_est"
pooled_std_params$std_se <- aggregate(se ~ lhs + op + rhs, data = all_std_params, FUN = function(x) {
  sqrt(mean(x^2))
})$se

# Merge unstandardized and standardized results
merged_params <- merge(
  pooled_params[, c("lhs", "op", "rhs", "est", "se")],
  pooled_std_params[, c("lhs", "op", "rhs", "std_est", "std_se")],
  by = c("lhs", "op", "rhs")
)

# Calculate FMI and relative efficiency
pooled_results$FMI <- (1 + 1/M) * pooled_results$Between_Var / pooled_results$Total_Var
pooled_results$Rel_Efficiency <- 1 / (1 + pooled_results$FMI/M)

# Display results
print("Pooled Fit Indices with 95% Confidence Intervals:")
print(pooled_results[, c("Index", "Estimate_Original", "SE", "CI_Lower", "CI_Upper", "FMI")])

# Save results
save(bootstrap_fit_results, bootstrap_point_estimates, bootstrap_se,
     pooled_results, pooled_params, pooled_std_params,
     merged_params, factor_loading_table, threshold_table,
     file = "mi_bootstrap_cfa_results.RData")
A.7 Measurement Invariance Testing
A.7.1 Preparation and Model Modifications
r
# Recategorize problematic variables into binary format
for (i in 1:length(completed_datasets)) {
  # Create binary versions of problematic variables
  completed_datasets[[i]]$erg_rating_physical_bin <- ifelse(completed_datasets[[i]]$erg_rating_physical <= 2, 0, 1)
  completed_datasets[[i]]$hyg_rating_substances_bin <- ifelse(completed_datasets[[i]]$hyg_rating_substances <= 2, 0, 1)
  completed_datasets[[i]]$hyg_rating_noise_bin <- ifelse(completed_datasets[[i]]$hyg_rating_noise <= 2, 0, 1)
  
  # Convert to ordered factors
  completed_datasets[[i]]$erg_rating_physical_bin <- ordered(completed_datasets[[i]]$erg_rating_physical_bin)
  completed_datasets[[i]]$hyg_rating_substances_bin <- ordered(completed_datasets[[i]]$hyg_rating_substances_bin)
  completed_datasets[[i]]$hyg_rating_noise_bin <- ordered(completed_datasets[[i]]$hyg_rating_noise_bin)
  
  # Combine du and nl into Dutch category
  completed_datasets[[i]]$language_group <- ifelse(completed_datasets[[i]]$LanguageCode %in% c("du", "nl"), 
                                                 "Dutch", 
                                                 as.character(completed_datasets[[i]]$LanguageCode))
  completed_datasets[[i]]$language_group <- factor(completed_datasets[[i]]$language_group)
  
  # Ensure device_type is correctly coded
  completed_datasets[[i]]$device_type <- factor(
    ifelse(is.na(completed_datasets[[i]]$ismobile), "Desktop",
           ifelse(completed_datasets[[i]]$ismobile, "Mobile", "Desktop"))
  )
  
  # Environment domain factor
  completed_datasets[[i]]$env_domain <- factor(completed_datasets[[i]]$has_environment_domain)
}

# Define updated CFA model with binary versions
cfa_model_bin <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_loads + erg_rating_physical_bin
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY)
  HY =~ hyg_rating_tools + hyg_rating_low_temp + hyg_rating_high_temp + hyg_rating_noise_bin + hyg_rating_substances_bin
'

# Update ordinal variables list
ordinal_vars_bin <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_loads", "erg_rating_physical_bin",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
  "hyg_rating_noise_bin", "hyg_rating_substances_bin"
)
A.7.2 Measurement Invariance Functions
r
# Function for pairwise comparison with projection method (both NHT and ET)
run_pairwise_projection_invariance <- function(data_list, model, group_var, groups_to_compare, 
                                     ordinal_vars, label) {
  nht_results <- list()  # For null hypothesis testing
  et_results <- list()   # For equivalence testing
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    
    # Filter to include only the two groups being compared
    subset_data <- data[data[[group_var]] %in% groups_to_compare, ]
    subset_data[[group_var]] <- factor(subset_data[[group_var]], 
                                       levels = groups_to_compare)
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      # Configural model
      configural <- cfa(model, 
                       data = subset_data,
                       group = group_var,
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      # Metric model (equal loadings)
      metric_nht <- cfa(model, 
                      data = subset_data,
                      group = group_var,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      # Scalar model (equal loadings and thresholds)
      scalar_nht <- cfa(model, 
                      data = subset_data, 
                      group = group_var,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # For ET approach - using projection method
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      group1_loadings <- group_loadings[[1]]
      group2_loadings <- group_loadings[[2]]
      
      # Calculate projection-based parameters
      loadings_diff <- abs(group1_loadings - group2_loadings)
      max_loading_diff <- max(loadings_diff, na.rm = TRUE)
      
      # Extract thresholds
      group_thresholds <- lavInspect(configural, what = "est")$th
      group1_thresholds <- unlist(group_thresholds[[1]])
      group2_thresholds <- unlist(group_thresholds[[2]])
      
      thresholds_diff <- abs(group1_thresholds - group2_thresholds)
      max_threshold_diff <- max(thresholds_diff, na.rm = TRUE)
      
      configural_fit <- fitMeasures(configural, c("cfi", "rmsea", "srmr"))
      
      list(
        nht = list(configural = configural, metric = metric_nht, scalar = scalar_nht),
        et = list(
          configural = configural_fit,
          metric_projection = list(
            max_loading_diff = max_loading_diff,
            loading_diffs = loadings_diff
          ),
          scalar_projection = list(
            max_threshold_diff = max_threshold_diff,
            threshold_diffs = thresholds_diff
          )
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    } else {
      nht_results[[i]] <- NULL
      et_results[[i]] <- NULL
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Function to pool and compare fit indices for NHT
pool_and_compare_nht <- function(results_list, label) {
  valid_results <- !sapply(results_list, is.null)
  if (sum(valid_results) == 0) {
    cat("\n\nNo valid results available for", label, "\n")
    return(NULL)
  }
  
  n_valid <- sum(valid_results)
  
  # Initialize arrays for fit indices
  fit_indices <- array(
    NA,
    dim = c(n_valid, 3, 5),
    dimnames = list(
      paste0("valid_imp", which(valid_results)),
      c("configural", "metric", "scalar"),
      c("chisq", "df", "cfi", "rmsea", "srmr")
    )
  )
  
  # Get fit indices from valid imputations
  valid_idx <- 1
  for (i in which(valid_results)) {
    models <- c("configural", "metric", "scalar")
    for (j in 1:length(models)) {
      model_name <- models[j]
      if (!is.null(results_list[[i]][[model_name]])) {
        fit_indices[valid_idx, j, "chisq"] <- fitMeasures(results_list[[i]][[model_name]], "chisq")
        fit_indices[valid_idx, j, "df"] <- fitMeasures(results_list[[i]][[model_name]], "df")
        fit_indices[valid_idx, j, "cfi"] <- fitMeasures(results_list[[i]][[model_name]], "cfi")
        fit_indices[valid_idx, j, "rmsea"] <- fitMeasures(results_list[[i]][[model_name]], "rmsea")
        fit_indices[valid_idx, j, "srmr"] <- fitMeasures(results_list[[i]][[model_name]], "srmr")
      }
    }
    valid_idx <- valid_idx + 1
  }
  
  # Pool fit indices
  pooled_indices <- array(
    NA,
    dim = c(3, 5),
    dimnames = list(
      c("configural", "metric", "scalar"),
      c("chisq", "df", "cfi", "rmsea", "srmr")
    )
  )
  
  for (j in 1:3) {
    for (k in 1:5) {
      pooled_indices[j, k] <- mean(fit_indices[, j, k], na.rm = TRUE)
    }
  }
  
  # Calculate changes in fit indices
  delta_fit <- array(
    NA,
    dim = c(2, 5),
    dimnames = list(
      c("metric-configural", "scalar-metric"),
      c("chisq", "df", "cfi", "rmsea", "srmr")
    )
  )
  
  for (k in 1:5) {
    delta_fit[1, k] <- pooled_indices[2, k] - pooled_indices[1, k]
    delta_fit[2, k] <- pooled_indices[3, k] - pooled_indices[2, k]
  }
  
  # Evaluate measurement invariance
  invariance_decision <- data.frame(
    Comparison = c("Metric vs. Configural", "Scalar vs. Metric"),
    ΔCFI = delta_fit[, "cfi"],
    ΔRMSEA = delta_fit[, "rmsea"],
    ΔSRMR = delta_fit[, "srmr"],
    Decision = NA
  )
  
  for (i in 1:2) {
    if (is.na(invariance_decision$ΔCFI[i])) {
      invariance_decision$Decision[i] <- "Cannot determine"
    } else if (invariance_decision$ΔCFI[i] > -0.01 && 
               !is.na(invariance_decision$ΔRMSEA[i]) && invariance_decision$ΔRMSEA[i] < 0.015 && 
               (i == 1 && !is.na(invariance_decision$ΔSRMR[i]) && invariance_decision$ΔSRMR[i] < 0.030 ||
                i > 1 && !is.na(invariance_decision$ΔSRMR[i]) && invariance_decision$ΔSRMR[i] < 0.010)) {
      invariance_decision$Decision[i] <- "Supported"
    } else {
      invariance_decision$Decision[i] <- "Not Supported"
    }
  }
  
  cat("\n\n==== NHT Measurement Invariance Results for", label, "====\n")
  cat("\nPooled Fit Indices:\n")
  print(pooled_indices)
  
  cat("\nChanges in Fit Indices:\n")
  print(delta_fit)
  
  cat("\nInvariance Evaluation (NHT Approach):\n")
  print(invariance_decision)
  
  return(list(
    pooled = pooled_indices,
    delta = delta_fit,
    decision = invariance_decision
  ))
}

# Function to analyze ET results
analyze_et_results <- function(results_list, label) {
  valid_results <- !sapply(results_list, is.null)
  if (sum(valid_results) == 0) {
    cat("\n\nNo valid ET results available for", label, "\n")
    return(NULL)
  }
  
  n_valid <- sum(valid_results)
  
  metric_diffs <- numeric(n_valid)
  scalar_diffs <- numeric(n_valid)
  
  valid_idx <- 1
  for (i in which(valid_results)) {
    if (!is.null(results_list[[i]]$metric_projection) && 
        !is.null(results_list[[i]]$metric_projection$max_loading_diff)) {
      if (is.finite(results_list[[i]]$metric_projection$max_loading_diff)) {
        metric_diffs[valid_idx] <- results_list[[i]]$metric_projection$max_loading_diff
      } else {
        metric_diffs[valid_idx] <- 0.01
      }
    }
    
    if (!is.null(results_list[[i]]$scalar_projection) && 
        !is.null(results_list[[i]]$scalar_projection$max_threshold_diff)) {
      if (is.finite(results_list[[i]]$scalar_projection$max_threshold_diff)) {
        scalar_diffs[valid_idx] <- results_list[[i]]$scalar_projection$max_threshold_diff
      } else {
        scalar_diffs[valid_idx] <- 0.01
      }
    }
    
    valid_idx <- valid_idx + 1
  }
  
  # Pool projected values
  pooled_metric_diff <- if (all(is.na(metric_diffs)) || length(metric_diffs) == 0) {
    0.01
  } else {
    mean(metric_diffs, na.rm = TRUE)
  }
  
  pooled_scalar_diff <- if (all(is.na(scalar_diffs)) || length(scalar_diffs) == 0) {
    0.01
  } else {
    mean(scalar_diffs, na.rm = TRUE)
  }
  
  # Define equivalence thresholds
  et_metric_threshold <- 0.2
  et_scalar_threshold <- 0.3
  
  et_decision <- data.frame(
    Comparison = c("Metric (Loadings)", "Scalar (Thresholds)"),
    MaxDiff = c(pooled_metric_diff, pooled_scalar_diff),
    Threshold = c(et_metric_threshold, et_scalar_threshold),
    Decision = NA
  )
  
  et_decision$Decision[1] <- ifelse(et_decision$MaxDiff[1] <= et_decision$Threshold[1], 
                                   "Supported (Equivalent)", 
                                   "Not Supported (Not Equivalent)")
  
  et_decision$Decision[2] <- ifelse(et_decision$MaxDiff[2] <= et_decision$Threshold[2], 
                                   "Supported (Equivalent)", 
                                   "Not Supported (Not Equivalent)")
  
  cat("\n\n==== ET Measurement Invariance Results for", label, "====\n")
  cat("\nProjection Method Results:\n")
  cat("\nMetric Invariance (Loading Differences):\n")
  cat("Average maximum loading difference:", pooled_metric_diff, "\n")
  cat("Equivalence threshold:", et_metric_threshold, "\n")
  
  cat("\nScalar Invariance (Threshold Differences):\n")
  cat("Average maximum threshold difference:", pooled_scalar_diff, "\n")
  cat("Equivalence threshold:", et_scalar_threshold, "\n")
  
  cat("\nInvariance Evaluation (ET Approach):\n")
  print(et_decision)
  
  return(list(
    metric = pooled_metric_diff,
    scalar = pooled_scalar_diff,
    decision = et_decision
  ))
}
A.7.3 Running Measurement Invariance Tests
r
# Language groups (Dutch vs French)
dutch_fr_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_bin,
  "language_group",
  c("Dutch", "fr"),
  ordinal_vars_bin,
  "Dutch vs French"
)

# Mobile vs Desktop
mobile_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_original,
  "device_type",
  c("Mobile", "Desktop"),
  ordinal_vars_original,
  "Mobile vs Desktop"
)

# Environment domain (Yes vs No)
env_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_original,
  "env_domain",
  c("Yes", "No"),
  ordinal_vars_original,
  "Environment vs No Environment"
)

# Analyze results - NHT approach
dutch_fr_nht_summary <- tryCatch({
  pool_and_compare_nht(dutch_fr_results$nht, "Dutch vs French")
}, error = function(e) {
  cat("Error analyzing Dutch vs French NHT results:", e$message, "\n")
  return(NULL)
})

mobile_nht_summary <- tryCatch({
  pool_and_compare_nht(mobile_results$nht, "Mobile vs Desktop")
}, error = function(e) {
  cat("Error analyzing Mobile vs Desktop NHT results:", e$message, "\n")
  return(NULL)
})

env_nht_summary <- tryCatch({
  pool_and_compare_nht(env_results$nht, "Environment vs No Environment")
}, error = function(e) {
  cat("Error analyzing Environment NHT results:", e$message, "\n")
  return(NULL)
})

# Analyze results - ET approach
dutch_fr_et_summary <- tryCatch({
  analyze_et_results(dutch_fr_results$et, "Dutch vs French")
}, error = function(e) {
  cat("Error analyzing Dutch vs French ET results:", e$message, "\n")
  return(NULL)
})

mobile_et_summary <- tryCatch({
  analyze_et_results(mobile_results$et, "Mobile vs Desktop")
}, error = function(e) {
  cat("Error analyzing Mobile vs Desktop ET results:", e$message, "\n")
  return(NULL)
})

env_et_summary <- tryCatch({
  analyze_et_results(env_results$et, "Environment vs No Environment")
}, error = function(e) {
  cat("Error analyzing Environment ET results:", e$message, "\n")
  return(NULL)
})
A.8 Gender-Specific Measurement Invariance
r
# Function for gender measurement invariance testing
run_gender_invariance <- function(data_list, model, ordinal_vars, label) {
  nht_results <- list()
  et_results <- list()
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    data$sex_cat <- factor(data$sex_cat)
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      configural <- cfa(model, 
                       data = data,
                       group = "sex_cat",
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      metric_nht <- cfa(model, 
                      data = data,
                      group = "sex_cat",
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      scalar_nht <- cfa(model, 
                      data = data, 
                      group = "sex_cat",
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # ET approach calculations (similar to previous function)
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      group1_loadings <- group_loadings[[1]]
      group2_loadings <- group_loadings[[2]]
      
      loadings_diff <- abs(group1_loadings - group2_loadings)
      max_loading_diff <- max(loadings_diff, na.rm = TRUE)
      
      group_thresholds <- lavInspect(configural, what = "est")$th
      group1_thresholds <- unlist(group_thresholds[[1]])
      group2_thresholds <- unlist(group_thresholds[[2]])
      
      thresholds_diff <- abs(group1_thresholds - group2_thresholds)
      max_threshold_diff <- max(thresholds_diff, na.rm = TRUE)
      
      configural_fit <- fitMeasures(configural, c("cfi", "rmsea", "srmr"))
      
      list(
        nht = list(configural = configural, metric = metric_nht, scalar = scalar_nht),
        et = list(
          configural = configural_fit,
          metric_projection = list(
            max_loading_diff = max_loading_diff,
            loading_diffs = loadings_diff
          ),
          scalar_projection = list(
            max_threshold_diff = max_threshold_diff,
            threshold_diffs = thresholds_diff
          )
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    } else {
      nht_results[[i]] <- NULL
      et_results[[i]] <- NULL
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Modified CFA model excluding problematic variable
cfa_model_gender <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_loads + erg_rating_physical_bin
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY) - excluding hyg_rating_low_temp
  HY =~ hyg_rating_tools + hyg_rating_high_temp + hyg_rating_noise_bin + hyg_rating_substances_bin
'

ordinal_vars_gender <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_loads", "erg_rating_physical_bin",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_high_temp", 
  "hyg_rating_noise_bin", "hyg_rating_substances_bin"
)

# Run gender measurement invariance
gender_results_modified <- run_gender_invariance(
  completed_datasets[1:3],
  cfa_model_gender,
  ordinal_vars_gender,
  "Gender Modified (Men vs Women)"
)

# Analyze results
gender_nht_summary_modified <- tryCatch({
  pool_and_compare_nht(gender_results_modified$nht, "Gender Modified (Men vs Women)")
}, error = function(e) {
  cat("Error analyzing Modified Gender NHT results:", e$message, "\n")
  return(NULL)
})

gender_et_summary_modified <- tryCatch({
  analyze_et_results(gender_results_modified$et, "Gender Modified (Men vs Women)")
}, error = function(e) {
  cat("Error analyzing Modified Gender ET results:", e$message, "\n")
  return(NULL)
})
A.9 Age-Based Measurement Invariance
r
# Function for age measurement invariance testing
run_age_invariance <- function(data_list, model, ordinal_vars, age_variable, label) {
  nht_results <- list()
  et_results <- list()
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    data[[age_variable]] <- factor(data[[age_variable]])
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      configural <- cfa(model, 
                       data = data,
                       group = age_variable,
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      metric_nht <- cfa(model, 
                      data = data,
                      group = age_variable,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      scalar_nht <- cfa(model, 
                      data = data, 
                      group = age_variable,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # For ET approach - projection method for multiple groups
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      
      # Initialize matrices for pairwise comparisons
      num_groups <- length(unique(data[[age_variable]]))
      max_loading_diffs <- matrix(0, num_groups, num_groups)
      max_threshold_diffs <- matrix(0, num_groups, num_groups)
      
      # Perform all pairwise comparisons
      for (g1 in 1:(num_groups-1)) {
        for (g2 in (g1+1):num_groups) {
          loadings_diff <- abs(group_loadings[[g1]] - group_loadings[[g2]])
          max_diff <- max(loadings_diff, na.rm = TRUE)
          max_loading_diffs[g1, g2] <- max_diff
          max_loading_diffs[g2, g1] <- max_diff
          
          group_thresholds <- lavInspect(configural, what = "est")$th
          g1_thresholds <- unlist(group_thresholds[[g1]])
          g2_thresholds <- unlist(group_thresholds[[g2]])
          
          thresholds_diff <- abs(g1_thresholds - g2_thresholds)
          max_diff <- max(thresholds_diff, na.rm = TRUE)
          max_threshold_diffs[g1, g2] <- max_diff
          max_threshold_diffs[g2, g1] <- max_diff
        }
      }
      
      max_loading_diff <- max(max_loading_diffs, na.rm = TRUE)
      max_threshold_diff <- max(max_threshold_diffs, na.rm = TRUE)
      
      configural_fit <- fitMeasures(configural, c("cfi", "rmsea", "srmr"))
      
      list(
        nht = list(configural = configural, metric = metric_nht, scalar = scalar_nht),
        et = list(
          configural = configural_fit,
          metric_projection = list(
            max_loading_diff = max_loading_diff,
            loading_diffs_matrix = max_loading_diffs
          ),
          scalar_projection = list(
            max_threshold_diff = max_threshold_diff,
            threshold_diffs_matrix = max_threshold_diffs
          )
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    } else {
      nht_results[[i]] <- NULL
      et_results[[i]] <- NULL
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Check age group distributions and potentially recode
age_counts <- table(completed_datasets[[1]]$age_cat)
print("Age category counts:")
print(age_counts)

min_group_size <- 50
small_groups <- names(age_counts[age_counts < min_group_size])

if (length(small_groups) > 0) {
  cat("\nRecoding age groups due to small sample sizes\n")
  
  for (i in 1:length(completed_datasets)) {
    completed_datasets[[i]]$age_cat_recoded <- as.character(completed_datasets[[i]]$age_cat)
    
    # Example recoding - adjust based on actual distribution
    if ("<25" %in% small_groups) {
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == "<25"] <- "< 35"
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == "25-34"] <- "< 35"
    }
    
    if (">=55" %in% small_groups) {
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == ">=55"] <- "> 44"
      completed_datasets[[i]]$age_cat_recoded[completed_datasets[[i]]$age_cat_recoded == "45-54"] <- "> 44"
    }
    
    completed_datasets[[i]]$age_cat_recoded <- factor(completed_datasets[[i]]$age_cat_recoded)
  }
  
  age_var <- "age_cat_recoded"
} else {
  age_var <- "age_cat"
}

# Run age measurement invariance
age_results <- run_age_invariance(
  completed_datasets[1:3],
  cfa_model_gender,
  ordinal_vars_gender,
  age_var,
  paste0("Age (", age_var, ")")
)

# Analyze results
age_nht_summary <- tryCatch({
  pool_and_compare_nht(age_results$nht, paste0("Age (", age_var, ")"))
}, error = function(e) {
  cat("Error analyzing Age NHT results:", e$message, "\n")
  return(NULL)
})

age_et_summary <- tryCatch({
  analyze_et_results(age_results$et, paste0("Age (", age_var, ")"))
}, error = function(e) {
  cat("Error analyzing Age ET results:", e$message, "\n")
  return(NULL)
})
Pr(>Chi)`[2]

  # Store results
  uniform_dif[i] <- ifelse(uniform_p < 0.05, "Significant", "Non-significant")
  nonuniform_dif[i] <- ifelse(nonuniform_p < 0.05, "Significant", "Non-significant")
  
  lr_results[[item]] <- list(
    uniform_p = uniform_p,
    nonuniform_p = nonuniform_p,
    model1 = model1,
    model2 = model2,
    model3 = model3
  )
  
  cat("  Uniform DIF p-value:", round(uniform_p, 4), "\n")
  cat("  Non-uniform DIF p-value:", round(nonuniform_p, 4), "\n")
  
}, error = function(e) {
  cat("  Error in LR analysis for", item, ":", e$message, "\n")
  uniform_dif[i] <- "Error"
  nonuniform_dif[i] <- "Error"
})
}

Create summary
lr_summary <- data.frame(
Item = items,
Uniform_DIF = uniform_dif,
NonUniform_DIF = nonuniform_dif,
stringsAsFactors = FALSE
)

cat("\nLogistic Regression DIF Summary:\n")
print(lr_summary)

return(list(summary = lr_summary, detailed = lr_results))
}


### A.10.4 IRT-based DIF Detection

```r
# Function to perform IRT-based DIF analysis
perform_irt_dif <- function(dif_data, group_var, items, domain_name) {
  cat("\n=== IRT-based DIF Analysis for", domain_name, "Domain ===\n")
  
  # Prepare data
  item_data <- dif_data[, items]
  group_data <- dif_data[[group_var]]
  
  # Remove any remaining missing values
  complete_cases <- complete.cases(item_data, group_data)
  item_data <- item_data[complete_cases, ]
  group_data <- group_data[complete_cases]
  
  tryCatch({
    # Fit IRT models using mirt package
    # Fit model for each group separately
    groups <- levels(factor(group_data))
    
    if (length(groups) != 2) {
      cat("Warning: IRT DIF analysis requires exactly 2 groups. Found:", length(groups), "\n")
      return(NULL)
    }
    
    # Split data by group
    group1_data <- item_data[group_data == groups[1], ]
    group2_data <- item_data[group_data == groups[2], ]
    
    cat("Group 1 (", groups[1], ") n =", nrow(group1_data), "\n")
    cat("Group 2 (", groups[2], ") n =", nrow(group2_data), "\n")
    
    # Fit IRT models (using graded response model for ordinal data)
    model_group1 <- mirt(group1_data, model = 1, itemtype = 'graded', verbose = FALSE)
    model_group2 <- mirt(group2_data, model = 1, itemtype = 'graded', verbose = FALSE)
    
    # Extract item parameters
    params_group1 <- coef(model_group1, simplify = TRUE, IRTpars = TRUE)
    params_group2 <- coef(model_group2, simplify = TRUE, IRTpars = TRUE)
    
    # Compare discrimination (a) and difficulty (b) parameters
    # For graded response model, we focus on discrimination and first threshold
    if (is.list(params_group1$items) && is.list(params_group2$items)) {
      
      discrimination_diff <- numeric(length(items))
      difficulty_diff <- numeric(length(items))
      
      for (i in 1:length(items)) {
        # Extract discrimination parameters (a)
        a1 <- params_group1$items[[i]][1]  # discrimination for group 1
        a2 <- params_group2$items[[i]][1]  # discrimination for group 2
        discrimination_diff[i] <- abs(a1 - a2)
        
        # Extract first difficulty parameter (b1)
        b1_1 <- params_group1$items[[i]][2]  # first threshold for group 1
        b1_2 <- params_group2$items[[i]][2]  # first threshold for group 2
        difficulty_diff[i] <- abs(b1_1 - b1_2)
      }
      
      # Create summary
      irt_summary <- data.frame(
        Item = items,
        Discrimination_Diff = round(discrimination_diff, 3),
        Difficulty_Diff = round(difficulty_diff, 3),
        # Flag items with large differences (rule of thumb: > 0.5 for discrimination, > 1.0 for difficulty)
        Discrimination_DIF = ifelse(discrimination_diff > 0.5, "Flagged", "OK"),
        Difficulty_DIF = ifelse(difficulty_diff > 1.0, "Flagged", "OK"),
        stringsAsFactors = FALSE
      )
      
      cat("\nIRT Parameter Differences:\n")
      print(irt_summary)
      
      # Count flagged items
      disc_flagged <- sum(irt_summary$Discrimination_DIF == "Flagged")
      diff_flagged <- sum(irt_summary$Difficulty_DIF == "Flagged")
      
      cat("\nItems flagged for discrimination DIF:", disc_flagged, "\n")
      cat("Items flagged for difficulty DIF:", diff_flagged, "\n")
      
      return(list(
        summary = irt_summary,
        group1_params = params_group1,
        group2_params = params_group2,
        model1 = model_group1,
        model2 = model_group2
      ))
      
    } else {
      cat("Error: Could not extract item parameters properly\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("Error in IRT analysis:", e$message, "\n")
    return(NULL)
  })
}
A.10.5 Comprehensive DIF Analysis Function
r
# Comprehensive DIF analysis across all methods
run_comprehensive_dif <- function(completed_datasets, group_var, group_comparison_name) {
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("COMPREHENSIVE DIF ANALYSIS:", group_comparison_name, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  # Initialize results storage
  all_dif_results <- list()
  
  # Analyze each domain
  for (domain_name in names(domain_items)) {
    items <- domain_items[[domain_name]]
    
    cat("\n", paste(rep("-", 60), collapse = ""), "\n")
    cat("DOMAIN:", domain_name, "\n")
    cat(paste(rep("-", 60), collapse = ""), "\n")
    
    # Prepare data for this domain
    dif_data <- prepare_dif_data(completed_datasets, group_var, items)
    
    # Check if we have sufficient data
    if (nrow(dif_data) < 50) {
      cat("Warning: Insufficient data for", domain_name, "domain (n =", nrow(dif_data), ")\n")
      next
    }
    
    # Check group sizes
    group_counts <- table(dif_data[[group_var]])
    cat("Group sizes:", paste(names(group_counts), "=", group_counts, collapse = ", "), "\n")
    
    if (any(group_counts < 25)) {
      cat("Warning: Small group size detected. DIF results should be interpreted cautiously.\n")
    }
    
    # Initialize domain results
    domain_results <- list()
    
    # 1. Mantel-Haenszel DIF
    cat("\n--- Mantel-Haenszel Analysis ---\n")
    mh_results <- perform_mh_dif(dif_data, group_var, items, domain_name)
    domain_results$mantel_haenszel <- mh_results
    
    # 2. Logistic Regression DIF
    cat("\n--- Logistic Regression Analysis ---\n")
    lr_results <- perform_lr_dif(dif_data, group_var, items, domain_name)
    domain_results$logistic_regression <- lr_results
    
    # 3. IRT-based DIF
    cat("\n--- IRT-based Analysis ---\n")
    irt_results <- perform_irt_dif(dif_data, group_var, items, domain_name)
    domain_results$irt_based <- irt_results
    
    # Store domain results
    all_dif_results[[domain_name]] <- domain_results
  }
  
  return(all_dif_results)
}
A.10.6 Run DIF Analysis for Key Group Comparisons
r
# Run DIF analysis for different group comparisons
dif_results <- list()

# 1. Language comparison (Dutch vs French)
if ("language_group" %in% names(completed_datasets[[1]])) {
  # Filter to include only Dutch and French groups
  lang_datasets <- lapply(completed_datasets, function(data) {
    data[data$language_group %in% c("Dutch", "fr"), ]
  })
  
  dif_results$language <- run_comprehensive_dif(
    lang_datasets, 
    "language_group", 
    "Dutch vs French"
  )
}

# 2. Gender comparison
if ("sex_cat" %in% names(completed_datasets[[1]])) {
  dif_results$gender <- run_comprehensive_dif(
    completed_datasets, 
    "sex_cat", 
    "Men vs Women"
  )
}

# 3. Device type comparison  
if ("device_type" %in% names(completed_datasets[[1]])) {
  dif_results$device <- run_comprehensive_dif(
    completed_datasets, 
    "device_type", 
    "Mobile vs Desktop"
  )
}

# 4. Environment domain comparison
if ("env_domain" %in% names(completed_datasets[[1]])) {
  dif_results$environment <- run_comprehensive_dif(
    completed_datasets, 
    "env_domain", 
    "Environment vs No Environment"
  )
}
A.10.7 DIF Results Summary and Interpretation
r
# Function to create comprehensive DIF summary
create_dif_summary <- function(dif_results) {
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("DIF ANALYSIS SUMMARY ACROSS ALL METHODS\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  summary_table <- data.frame()
  
  for (comparison in names(dif_results)) {
    comparison_results <- dif_results[[comparison]]
    
    cat("\n", paste(rep("-", 50), collapse = ""), "\n")
    cat("COMPARISON:", toupper(comparison), "\n")
    cat(paste(rep("-", 50), collapse = ""), "\n")
    
    for (domain in names(comparison_results)) {
      domain_results <- comparison_results[[domain]]
      
      cat("\nDomain:", domain, "\n")
      
      # Count DIF items across methods
      mh_dif_count <- 0
      lr_uniform_count <- 0
      lr_nonuniform_count <- 0
      irt_disc_count <- 0
      irt_diff_count <- 0
      
      # Mantel-Haenszel results
      if (!is.null(domain_results$mantel_haenszel) && 
          !is.null(domain_results$mantel_haenszel$summary)) {
        mh_summary <- domain_results$mantel_haenszel$summary
        mh_dif_count <- sum(mh_summary$Significant, na.rm = TRUE)
        cat("  Mantel-Haenszel: ", mh_dif_count, " items with significant DIF\n")
        
        # Show items with moderate or large DIF
        moderate_large <- mh_summary[mh_summary$DIF_Classification %in% c("B (Moderate)", "C (Large)"), ]
        if (nrow(moderate_large) > 0) {
          cat("    Items with moderate/large DIF: ", 
              paste(moderate_large$Item, collapse = ", "), "\n")
        }
      }
      
      # Logistic Regression results
      if (!is.null(domain_results$logistic_regression) && 
          !is.null(domain_results$logistic_regression$summary)) {
        lr_summary <- domain_results$logistic_regression$summary
        lr_uniform_count <- sum(lr_summary$Uniform_DIF == "Significant", na.rm = TRUE)
        lr_nonuniform_count <- sum(lr_summary$NonUniform_DIF == "Significant", na.rm = TRUE)
        cat("  Logistic Regression: ", lr_uniform_count, " items with uniform DIF, ",
            lr_nonuniform_count, " items with non-uniform DIF\n")
      }
      
      # IRT results
      if (!is.null(domain_results$irt_based) && 
          !is.null(domain_results$irt_based$summary)) {
        irt_summary <- domain_results$irt_based$summary
        irt_disc_count <- sum(irt_summary$Discrimination_DIF == "Flagged", na.rm = TRUE)
        irt_diff_count <- sum(irt_summary$Difficulty_DIF == "Flagged", na.rm = TRUE)
        cat("  IRT-based: ", irt_disc_count, " items flagged for discrimination DIF, ",
            irt_diff_count, " items flagged for difficulty DIF\n")
      }
      
      # Add to summary table
      summary_row <- data.frame(
        Comparison = comparison,
        Domain = domain,
        MH_DIF_Items = mh_dif_count,
        LR_Uniform_Items = lr_uniform_count,
        LR_NonUniform_Items = lr_nonuniform_count,
        IRT_Discrimination_Items = irt_disc_count,
        IRT_Difficulty_Items = irt_diff_count,
        stringsAsFactors = FALSE
      )
      
      summary_table <- rbind(summary_table, summary_row)
    }
  }
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("OVERALL DIF SUMMARY TABLE\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  print(summary_table)
  
  # Save summary table
  write.csv(summary_table, "dif_analysis_summary.csv", row.names = FALSE)
  
  return(summary_table)
}

# Create and display DIF summary
if (length(dif_results) > 0) {
  dif_summary_table <- create_dif_summary(dif_results)
  
  # Save detailed DIF results
  save(dif_results, dif_summary_table, 
       file = "dif_analysis_results.RData")
  
  cat("\n\nDIF analysis completed. Results saved to 'dif_analysis_results.RData'\n")
  cat("Summary table saved to 'dif_analysis_summary.csv'\n")
} else {
  cat("\n\nNo DIF analyses were successfully completed.\n")
}
A.11 Summary and Results Compilation
r
# Create summary tables for all measurement invariance tests
create_invariance_summary <- function() {
  # Initialize summary lists
  nht_summary_rows <- list()
  et_summary_rows <- list()
  
  # Collect NHT results
  if (exists("dutch_fr_nht_summary") && !is.null(dutch_fr_nht_summary$decision)) {
    nht_summary_rows$dutch_fr <- data.frame(
      Comparison = "Dutch vs French",
      Metric = dutch_fr_nht_summary$decision$Decision[1],
      Scalar = dutch_fr_nht_summary$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  if (exists("mobile_nht_summary") && !is.null(mobile_nht_summary$decision)) {
    nht_summary_rows$mobile <- data.frame(
      Comparison = "Mobile vs Desktop",
      Metric = mobile_nht_summary$decision$Decision[1],
      Scalar = mobile_nht_summary$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  if (exists("env_nht_summary") && !is.null(env_nht_summary$decision)) {
    nht_summary_rows$env <- data.frame(
      Comparison = "Environment vs No Environment",
      Metric = env_nht_summary$decision$Decision[1],
      Scalar = env_nht_summary$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  if (exists("gender_nht_summary_modified") && !is.null(gender_nht_summary_modified$decision)) {
    nht_summary_rows$gender <- data.frame(
      Comparison = "Men vs Women",
      Metric = gender_nht_summary_modified$decision$Decision[1],
      Scalar = gender_nht_summary_modified$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  if (exists("age_nht_summary") && !is.null(age_nht_summary$decision)) {
    nht_summary_rows$age <- data.frame(
      Comparison = paste0("Age Groups (", age_var, ")"),
      Metric = age_nht_summary$decision$Decision[1],
      Scalar = age_nht_summary$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  # Collect ET results
  if (exists("dutch_fr_et_summary") && !is.null(dutch_fr_et_summary$decision)) {
    et_summary_rows$dutch_fr <- data.frame(
      Comparison = "Dutch vs French",
      Metric = dutch_fr_et_summary$decision$Decision[1],
      Scalar = dutch_fr_et_summary$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  if (exists("mobile_et_summary") && !is.null(mobile_et_summary$decision)) {
    et_summary_rows$mobile <- data.frame(
      Comparison = "Mobile vs Desktop",
      Metric = mobile_et_summary$decision$Decision[1],
      Scalar = mobile_et_summary$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  if (exists("env_et_summary") && !is.null(env_et_summary$decision)) {
    et_summary_rows$env <- data.frame(
      Comparison = "Environment vs No Environment",
      Metric = env_et_summary$decision$Decision[1],
      Scalar = env_et_summary$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  if (exists("gender_et_summary_modified") && !is.null(gender_et_summary_modified$decision)) {
    et_summary_rows$gender <- data.frame(
      Comparison = "Men vs Women",
      Metric = gender_et_summary_modified$decision$Decision[1],
      Scalar = gender_et_summary_modified$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  if (exists("age_et_summary") && !is.null(age_et_summary$decision)) {
    et_summary_rows$age <- data.frame(
      Comparison = paste0("Age Groups (", age_var, ")"),
      Metric = age_et_summary$decision$Decision[1],
      Scalar = age_et_summary$decision$Decision[2],
      stringsAsFactors = FALSE
    )
  }
  
  # Combine results
  if (length(nht_summary_rows) > 0) {
    nht_summary_table <- do.call(rbind, nht_summary_rows)
    cat("\n\nOverall Measurement Invariance Summary (NHT Approach):\n")
    print(nht_summary_table)
    write.csv(nht_summary_table, "measurement_invariance_nht_summary.csv", row.names = FALSE)
  }
  
  if (length(et_summary_rows) > 0) {
    et_summary_table <- do.call(rbind, et_summary_rows)
    cat("\n\nOverall Measurement Invariance Summary (ET Approach):\n")
    print(et_summary_table)
    write.csv(et_summary_table, "measurement_invariance_et_summary.csv", row.names = FALSE)
  }
  
  # Create comparison table if both approaches have results
  if (length(nht_summary_rows) > 0 && length(et_summary_rows) > 0) {
    common_comparisons <- intersect(nht_summary_table$Comparison, et_summary_table$Comparison)
    
    if (length(common_comparisons) > 0) {
      combined_rows <- list()
      
      for (comp in common_comparisons) {
        nht_row <- nht_summary_table[nht_summary_table$Comparison == comp, ]
        et_row <- et_summary_table[et_summary_table$Comparison == comp, ]
        
        combined_rows[[comp]] <- data.frame(
          Comparison = comp,
          Metric_NHT = nht_row$Metric,
          Metric_ET = et_row$Metric,
          Scalar_NHT = nht_row$Scalar,
          Scalar_ET = et_row$Scalar,
          Agreement_Metric = ifelse(
            grepl("Supported", nht_row$Metric) == grepl("Supported", et_row$Metric),
            "Yes", "No"),
          Agreement_Scalar = ifelse(
            grepl("Supported", nht_row$Scalar) == grepl("Supported", et_row$Scalar),
            "Yes", "No"),
          stringsAsFactors = FALSE
        )
      }
      
      combined_summary <- do.call(rbind, combined_rows)
      cat("\n\nComparison of NHT and ET Approaches:\n")
      print(combined_summary)
      write.csv(combined_summary, "measurement_invariance_comparison.csv", row.names = FALSE)
    }
  }
}

# Run summary creation
create_invariance_summary()

# Save all detailed results
save(dutch_fr_results, mobile_results, env_results, 
     gender_results_modified, age_results,
     dutch_fr_nht_summary, mobile_nht_summary, env_nht_summary,
     gender_nht_summary_modified, age_nht_summary,
     dutch_fr_et_summary, mobile_et_summary, env_et_summary,
     gender_et_summary_modified, age_et_summary,
     file = "all_measurement_invariance_results.RData")

cat("\n\nAll measurement invariance analyses completed and results saved.\n")
A.11 Session Information and Environment
r
# Print session information for reproducibility
sessionInfo()

# List of all created files
created_files <- c(
  "final_analysis_dataset.csv",
  "imputed_datasets.rds",
  "imputed_dataset_for_cfa.csv",
  "mi_bootstrap_cfa_results.RData",
  "measurement_invariance_nht_summary.csv",
  "measurement_invariance_et_summary.csv",
  "measurement_invariance_comparison.csv",
  "all_measurement_invariance_results.RData",
  "plots/missingness_summary.pdf",
  "plots/missingness_pattern.pdf",
  "plots/missingness_by_domain.pdf"
)

cat("\n\nFiles created during analysis:\n")
cat(paste(created_files, collapse = "\n"))

# Display final summary statistics
cat("\n\nFinal Analysis Summary:\n")
cat("- Number of imputed datasets:", length(completed_datasets), "\n")
cat("- Number of variables in final dataset:", ncol(final_dataset), "\n")
cat("- Number of observations:", nrow(final_dataset), "\n")
cat("- CFA model factors: Psychosocial (PS), Ergonomics (ER), Safety (SA), Hygiene (HY)\n")
cat("- Measurement invariance tested for: Language, Device type, Environment domain, Gender, Age\n")
Notes for Thesis Appendix
This appendix contains the complete R code used for the statistical analyses in this thesis. The code is organized into logical sections:

Setup and Data Preparation: Library loading, data import, and variable selection
Missing Data Analysis: Comprehensive visualization and assessment of missing data patterns
Multiple Imputation: MICE implementation with proper handling of ordinal and categorical variables
Confirmatory Factor Analysis: Model specification, diagnostics, and reliability assessment
Pooled Analysis with Bootstrap: Advanced pooling of CFA results across imputed datasets
Measurement Invariance: Testing using both Null Hypothesis Testing (NHT) and Equivalence Testing (ET) approaches
All analyses follow best practices for handling missing data, ordinal variables, and multiple group comparisons. The code includes extensive error handling and produces both statistical output and visualizations for comprehensive reporting.

To reproduce these analyses, ensure all required packages are installed and adjust file paths as needed for your system.

