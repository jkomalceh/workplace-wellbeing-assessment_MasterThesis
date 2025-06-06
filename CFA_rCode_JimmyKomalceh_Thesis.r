#Appendix: R Code for Workplace Well-being Assessment Analysis
#Table of Contents
#Setup and Data Loading
#Data Preparation and Variable Selection
#Missing Data Analysis and Visualization
#Multiple Imputation
#Confirmatory Factor Analysis (CFA)
#Reliability and Validity Assessment
#Measurement Invariance Testing
########################################################################1. Setup and Data Loading
##1.1 Load Required Libraries
r
# Load necessary libraries
library(tidyverse)
library(MVN)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, 
               knitr, psych, semPlot, semTools, wesanderson, Amelia,
               mice, corrplot, VIM, BaylorEdPsych, gridExtra, projpred)

# Setting working directory
setwd("D:/2Y_MasterThesis/R_Code")

# Load the pilot data
full_dataset <- read_delim("pilotdata_4comp.csv", delim = ";")
########################################################################2. Data Preparation and Variable Selection
##2.1 Define Model Variables
r
# Define model variables (excluding environment domain initially)
model_vars_no_env <- c(
  # Psychosocial domain
  "veerkracht_calc", "psy_rating_pace", "psy_rating_emotional", 
  "psy_rating_sphere", "psy_work_life",
  # Ergonomics domain
  "erg_capac", "erg_rating_posture", "erg_rating_repeat", 
  "erg_rating_sitting", "erg_rating_loads", "erg_rating_physical",
  # Safety domain
  "saf_satisfaction", "saf_rating_workinvolv", "saf_rating_leadengage",
  # Hygiene domain
  "hyg_satisfaction", "hyg_rating_tools", "hyg_rating_low_temp", 
  "hyg_rating_high_temp", "hyg_rating_noise", "hyg_rating_substances"
)

# Define environment domain variables
env_vars <- c("mil_rating_leadengage", "mil_satisfaction", 
              "mil_rating_contrib", "mil_rating_workinvolv")
##2.2 Select Auxiliary Variables
r
# Calculate missing rates
missing_rates <- full_dataset %>%
  summarise(across(everything(), ~sum(is.na(.))/n())) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate") %>%
  arrange(missing_rate)

# Identify auxiliary variables with <20% missing
potential_aux_vars <- missing_rates %>%
  filter(missing_rate < 0.20) %>%
  filter(!variable %in% c(model_vars_no_env, env_vars, 
                         "ismobile", "LanguageCode", "Name_Company", 
                         "Version_survey")) %>%
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

# Limit to 10 variables that exist in dataset
useful_aux_vars <- intersect(useful_aux_vars, names(full_dataset))
useful_aux_vars <- head(useful_aux_vars, 10)
##2.3 Create Final Analysis Dataset
r
# Create dataset with selected variables
final_dataset <- full_dataset %>%
  mutate(has_environment_domain = case_when(
    !is.na(mil_satisfaction) ~ "Yes",
    TRUE ~ "No"
  )) %>%
  select(all_of(c(model_vars_no_env, env_vars, useful_aux_vars,
                "ismobile", "LanguageCode", "Name_Company", "Version_survey",
                "has_environment_domain")))

# Save the processed dataset
write_csv(final_dataset, "final_analysis_dataset.csv")

# Load the analysis dataset
full_analysis_dataset <- read_delim("final_analysis_dataset.csv")
########################################################################3. Missing Data Analysis and Visualization
##3.1 Variable Labeling for Visualization
r
# Create descriptive labels for variables
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
  mil_rating_workinvolv = "Env. work involvement",
  
  # Demographics
  age_cat = "Age category",
  sex_cat = "Gender",
  
  # Frequency variables
  hyg_freq_exp_substances = "Freq. exposure to substances",
  hyg_freq_exp_tools = "Freq. exposure to tools",
  hyg_freq_exp_high_temp = "Freq. exposure to high temp",
  hyg_freq_exp_low_temp = "Freq. exposure to low temp",
  hyg_freq_exp_noise = "Freq. exposure to noise",
  erg_freq_exp_sitting = "Freq. sitting",
  erg_freq_exp_posture = "Freq. stressful postures",
  erg_freq_exp_physical = "Freq. physical strain"
)

# Function to rename variables for plotting
rename_vars <- function(var_names) {
  sapply(var_names, function(x) {
    if(x %in% names(var_labels)) var_labels[x] else x
  })
}
##3.2 Missing Data Visualizations
r
# Create directory for plots
dir.create("plots", showWarnings = FALSE)

# 1. Overall missingness summary
miss_summary <- miss_var_summary(full_analysis_dataset)
miss_summary$variable_renamed <- rename_vars(miss_summary$variable)

p1 <- ggplot(miss_summary, aes(x = reorder(variable_renamed, pct_miss), 
                               y = pct_miss)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Overall Missingness by Variable",
       subtitle = "Percentage of missing values for each variable",
       x = "", y = "Percentage Missing (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 9))

ggsave("plots/missingness_summary.pdf", p1, width = 12, height = 10)

# 2. Missingness pattern visualization
miss_plot <- vis_miss(full_analysis_dataset) +
  labs(title = "Missingness Pattern in Analysis Dataset",
       subtitle = "Black cells indicate missing values",
       x = "Variables", y = "Observations") +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/missingness_pattern.pdf", miss_plot, width = 12, height = 10)

# 3. Missingness by company
miss_by_company <- full_analysis_dataset %>%
  group_by(Name_Company) %>%
  summarise(across(everything(), ~sum(is.na(.))/n()*100)) %>%
  pivot_longer(cols = -Name_Company, 
               names_to = "variable", 
               values_to = "percent_missing")

miss_by_company$variable_renamed <- rename_vars(miss_by_company$variable)

miss_by_company_filtered <- miss_by_company %>%
  group_by(variable) %>%
  filter(any(percent_missing > 0)) %>%
  ungroup()

p3 <- ggplot(miss_by_company_filtered, 
             aes(x = variable_renamed, y = Name_Company, 
                 fill = percent_missing)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#C0C0C0", 
    mid = "yellow",
    high = "red", 
    midpoint = 50,
    limits = c(0, 100),
    name = "% Missing"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9)) +
  labs(title = "Percentage of Missing Values by Company",
       subtitle = "Darker red indicates higher percentage of missing data",
       x = "", y = "")

ggsave("plots/missingness_by_company.pdf", p3, width = 14, height = 10)
##3.3 Domain-Specific Missing Data Analysis
r
# Define domains
domains <- list(
  "Psychosocial Domain" = c("veerkracht_calc", "psy_rating_pace", 
                          "psy_rating_emotional", "psy_rating_sphere", 
                          "psy_work_life"),
  "Ergonomics Domain" = c("erg_capac", "erg_rating_posture", 
                        "erg_rating_repeat", "erg_rating_sitting", 
                        "erg_rating_loads", "erg_rating_physical"),
  "Safety Domain" = c("saf_satisfaction", "saf_rating_workinvolv", 
                    "saf_rating_leadengage"),
  "Hygiene Domain" = c("hyg_satisfaction", "hyg_rating_tools", 
                      "hyg_rating_low_temp", "hyg_rating_high_temp", 
                      "hyg_rating_noise", "hyg_rating_substances"),
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

# Plot domain-specific missingness
p6 <- ggplot(domain_missingness, 
             aes(x = reorder(variable_renamed, percent_missing), 
                 y = percent_missing, fill = domain)) +
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
##3.4 Little's MCAR Test
r
# Run Little's MCAR test if available
if(requireNamespace("BaylorEdPsych", quietly = TRUE)) {
  # Select non-environmental variables
  non_env_vars <- c(
    domains[["Psychosocial Domain"]],
    domains[["Ergonomics Domain"]],
    domains[["Safety Domain"]],
    domains[["Hygiene Domain"]]
  )
  
  non_env_vars <- non_env_vars[non_env_vars %in% names(full_analysis_dataset)]
  
  # Run Little's MCAR test
  mcar_test <- try(BaylorEdPsych::LittleMCAR(full_analysis_dataset[non_env_vars]), 
                   silent = TRUE)
  
  if(!inherits(mcar_test, "try-error")) {
    print("Little's MCAR Test Results:")
    print(mcar_test)
    capture.output(mcar_test, file = "plots/littles_mcar_test.txt")
  }
}
########################################################################4. Multiple Imputation
##4.1 Prepare Data for Imputation
r
# Create imputation dataset
imputation_dataset <- full_analysis_dataset

# Define variable types
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
  "mil_rating_leadengage", "mil_satisfaction", "mil_rating_contrib", 
  "mil_rating_workinvolv",
  # Frequency variables
  "hyg_freq_exp_substances", "hyg_freq_exp_tools", "hyg_freq_exp_high_temp",
  "hyg_freq_exp_low_temp", "hyg_freq_exp_noise", "erg_freq_exp_sitting",
  "erg_freq_exp_posture", "erg_freq_exp_physical",
  # Demographics
  "age_cat"
)

continuous_vars <- c("veerkracht_calc")
binary_vars <- c("ismobile", "sex_cat")
cat_vars <- c("LanguageCode", "Name_Company", "Version_survey", 
              "has_environment_domain")
##4.2 Convert Variables to Appropriate Types
r
# Handle age_cat as ordered factor
if("age_cat" %in% names(imputation_dataset)) {
  age_levels <- c("<25", "25-34", "35-44", "45-54", ">=55")
  imputation_dataset$age_cat <- factor(imputation_dataset$age_cat, 
                                      levels = age_levels,
                                      ordered = TRUE)
}

# Convert ordinal variables to ordered factors
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
                                             levels = levels,
                                             ordered = TRUE)
        }
      }
    }
  }
}

# Create variable to track environmental domain status
imputation_dataset$skip_env_imputation <- 
  (full_analysis_dataset$has_environment_domain == "No")
cat_vars <- c(cat_vars, "skip_env_imputation")
##4.3 Set Up MICE Imputation
r
# Initialize imputation methods
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

# Don't impute categorical variables
for(var in cat_vars) {
  if(var %in% names(imp_methods)) {
    imp_methods[var] <- ""
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
##4.4 Run Multiple Imputation
r
# Set seed for reproducibility
set.seed(12345)

# Run imputation
imputed_data <- mice(imputation_dataset, 
                    predictorMatrix = pred_matrix,
                    method = imp_methods,
                    m = 10,          # Create 10 imputed datasets
                    maxit = 20,      # Run for 20 iterations
                    seed = 12345,
                    printFlag = TRUE)
##4.5 Post-Process Imputed Data
r
# Process all imputed datasets
completed_datasets <- list()
for(i in 1:10) {
  # Get the imputed dataset
  complete_data_i <- complete(imputed_data, i)
  
  # Convert ordinal variables back to numeric
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
  
  # Store the processed dataset
  completed_datasets[[i]] <- complete_data_i
}

# Save the completed datasets
saveRDS(completed_datasets, "imputed_datasets.rds")
saveRDS(imputed_data, "mice_imputation_object.rds")

# Save individual imputed datasets
for (i in 1:length(completed_datasets)) {
  filename <- paste0("imputed_dataset_", i, ".csv")
  write.csv(completed_datasets[[i]], file = filename, row.names = FALSE)
}
#4.6 Convergence Diagnostics
r
# Plot convergence traces
plot(imputed_data)

# If chain means are available, create detailed plots
if("chainMeans" %in% names(imputed_data) || "chainMean" %in% names(imputed_data)) {
  chain_attr <- ifelse("chainMeans" %in% names(imputed_data), 
                      "chainMeans", "chainMean")
  
  imputed_vars <- names(imputed_data$method)[imputed_data$method != ""]
  
  # Create plots for key variables
  for(var in imputed_vars) {
    tryCatch({
      if(chain_attr == "chainMeans") {
        chain_data <- imputed_data$chainMeans[[var]]
      } else {
        chain_data <- imputed_data$chainMean[, var, ]
      }
      
      if(!is.null(chain_data)) {
        df <- as.data.frame(chain_data)
        colnames(df) <- paste0("Chain", 1:ncol(df))
        df$Iteration <- 1:nrow(df)
        
        var_title <- ifelse(var %in% names(var_labels), 
                           var_labels[var], var)
        
        df_long <- melt(df, id.vars = "Iteration", 
                        variable.name = "Chain", 
                        value.name = "Mean")
        
        p <- ggplot(df_long, aes(x = Iteration, y = Mean, color = Chain)) +
          geom_line() +
          geom_point(size = 1) +
          labs(title = paste("Convergence Trace for", var_title),
               x = "Iteration", y = "Mean Value") +
          theme_minimal()
        
        print(p)
      }
    }, error = function(e) {
      message("Could not create plot for ", var, ": ", e$message)
    })
  }
}
########################################################################5. Confirmatory Factor Analysis (CFA)
##5.1 Define CFA Model
r
# Define the four-factor CFA model
cfa_model <- '
  # Psychosocial work environment factor (PS)
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  
  # Ergonomics factor (ER)
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_sitting + 
        erg_rating_loads + erg_rating_physical
  
  # Safety factor (SA)
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  
  # Hygiene factor (HY)
  HY =~ hyg_rating_tools + hyg_rating_low_temp + hyg_rating_high_temp + 
        hyg_rating_noise + hyg_rating_substances
'

# Define ordinal variables for CFA
ordinal_vars_cfa <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", 
  "erg_rating_loads", "erg_rating_physical",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
  "hyg_rating_noise", "hyg_rating_substances"
)
##5.2 Fit CFA Model
r
# Fit the CFA model
fit <- cfa(cfa_model, 
          data = full_dataset,
          ordered = ordinal_vars_cfa,
          estimator = "WLSMV")

# Display fit statistics
fit_summary <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", 
                                  "rmsea", "srmr"))
print(round(fit_summary, 4))
##5.3 Model Diagnostics
r
# Select numeric variables for diagnostics
cfa_vars <- full_dataset %>% 
  select(veerkracht_calc, starts_with("psy_"), starts_with("erg_")) %>%
  select(where(is.numeric))

# Test multivariate normality
mvn_result <- mvn(data = cfa_vars, mvnTest = "mardia")
print(mvn_result$multivariateNormality)

# Correlation matrix
cor_matrix <- cor(cfa_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

# Check for multicollinearity
high_cors <- which(abs(cor_matrix) > 0.85 & abs(cor_matrix) < 1, 
                   arr.ind = TRUE)
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
print(paste("Overall KMO:", round(kmo_result$MSA, 3)))

bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(cfa_vars))
print(bartlett_result)
########################################################################6. Reliability and Validity Assessment
##6.1 Composite Reliability and AVE
r
# Function to calculate CR and AVE
calculate_CR_AVE <- function(fit) {
  # Extract standardized parameter estimates
  std_estimates <- standardizedSolution(fit)
  
  # Get factor loadings
  loadings <- subset(std_estimates, op == "=~")
  
  # Organize loadings by factor
  factors <- unique(loadings$lhs)
  
  # Initialize results
  results <- data.frame(
    Factor = factors,
    CR = NA,
    AVE = NA
  )
  
  # Calculate CR and AVE for each factor
  for(i in 1:length(factors)) {
    factor <- factors[i]
    factor_loadings <- subset(loadings, lhs == factor)$est.std
    
    # Squared factor loadings
    lambda_squared <- factor_loadings^2
    
    # Error variances
    delta <- 1 - lambda_squared
    
    # Calculate CR
    sum_lambda <- sum(factor_loadings)
    sum_delta <- sum(delta)
    CR <- sum_lambda^2 / (sum_lambda^2 + sum_delta)
    
    # Calculate AVE
    sum_lambda_squared <- sum(lambda_squared)
    AVE <- sum_lambda_squared / (sum_lambda_squared + sum_delta)
    
    # Store results
    results$CR[i] <- CR
    results$AVE[i] <- AVE
  }
  
  return(results)
}

# Calculate CR and AVE
CR_AVE_results <- calculate_CR_AVE(fit)

# Display results
cat("\nComposite Reliability (CR) and Average Variance Extracted (AVE):\n")
cat("\nFactor\t\tCR\t\tAVE\n")
cat("----------------------------------------\n")
for(i in 1:nrow(CR_AVE_results)) {
  cat(CR_AVE_results$Factor[i], "\t\t", 
      round(CR_AVE_results$CR[i], 3), "\t\t",
      round(CR_AVE_results$AVE[i], 3), "\n")
}
##6.2 Discriminant Validity (Fornell-Larcker Criterion)
r
# Get factor correlations
factor_cors <- lavInspect(fit, "cor.lv")

# Create Fornell-Larcker matrix
FL_display <- matrix(NA, nrow = length(CR_AVE_results$Factor), 
                    ncol = length(CR_AVE_results$Factor))
rownames(FL_display) <- CR_AVE_results$Factor
colnames(FL_display) <- CR_AVE_results$Factor

# Fill the matrix
for(i in 1:nrow(FL_display)) {
  for(j in 1:ncol(FL_display)) {
    if(i == j) {
      # Diagonal: square root of AVE
      FL_display[i, j] <- sqrt(CR_AVE_results$AVE[i])
    } else {
      # Off-diagonal: factor correlations
      FL_display[i, j] <- factor_cors[i, j]
    }
  }
}

# Display Fornell-Larcker matrix
cat("\nFornell-Larcker Matrix:\n")
cat("(Diagonal = sqrt(AVE), Off-diagonal = factor correlations)\n\n")
print(round(FL_display, 3))
##6.3 Internal Consistency with Multiple Imputation
r
# Define factor variables
factor_vars <- list(
  PS = c("psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", 
         "psy_work_life"),
  ER = c("erg_rating_posture", "erg_rating_repeat", "erg_rating_sitting", 
         "erg_rating_loads", "erg_rating_physical"),
  SA = c("saf_rating_workinvolv", "saf_rating_leadengage"),
  HY = c("hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
         "hyg_rating_noise", "hyg_rating_substances")
)

# Function to calculate reliability across imputed datasets
calculate_reliability_complete <- function(completed_datasets, factor_vars) {
  
  results <- data.frame()
  
  for (factor_name in names(factor_vars)) {
    vars <- factor_vars[[factor_name]]
    
    cat("Processing factor:", factor_name, "\n")
    
    # Storage for reliability estimates
    alpha_values <- numeric()
    poly_alpha_values <- numeric()
    omega_values <- numeric()
    
    # Calculate for each imputed dataset
    for (i in 1:length(completed_datasets)) {
      data_imp <- completed_datasets[[i]]
      
      # Select and prepare factor data
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
            factor_model <- paste0(factor_name, " =~ ", 
                                 paste(vars, collapse = " + "))
            fit_single <- cfa(factor_model, data = data_imp, 
                            ordered = TRUE, estimator = "WLSMV")
            
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
    
    # Pool results
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
      } else {
        return(data.frame(
          Factor = factor_name,
          Measure = measure_name,
          Estimate = NA,
          SE = NA,
          CI_Lower = NA,
          CI_Upper = NA,
          N_Valid = length(valid_values),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Combine all reliability measures
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
          # Regular Spearman-Brown
          r <- cor(factor_data[, 1], factor_data[, 2], use = "complete.obs")
          sb_values[i] <- (2 * r) / (1 + r)
          
          # Polychoric Spearman-Brown
          tryCatch({
            poly_r <- polychoric(factor_data)$rho[1, 2]
            poly_sb_values[i] <- (2 * poly_r) / (1 + poly_r)
          }, error = function(e) {
            poly_sb_values[i] <- NA
          })
        }
      }
      
      sb_result <- pool_reliability(sb_values, "Spearman_Brown")
      poly_sb_result <- pool_reliability(poly_sb_values, 
                                       "Polychoric_Spearman_Brown")
      results <- rbind(results, sb_result, poly_sb_result)
    }
  }
  
  return(results)
}

# Calculate reliability across imputed datasets
reliability_results_complete <- calculate_reliability_complete(completed_datasets, 
                                                             factor_vars)

# Display results
print(reliability_results_complete)

# Create formatted table
formatted_table_complete <- reliability_results_complete %>%
  mutate(
    CI_Range = paste0("[", CI_Lower, ", ", CI_Upper, "]"),
    Result = paste0(Estimate, " ", CI_Range)
  ) %>%
  select(Factor, Measure, Result, N_Valid) %>%
  pivot_wider(names_from = Measure, values_from = Result)

print(formatted_table_complete)
##6.4 Pooled CFA with Bootstrap
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

# Bootstrapping phase
for (m in 1:M) {
  cat("Processing imputed dataset", m, "of", M, "\n")
  current_data <- completed_datasets[[m]]
  
  # Define bootstrap function
  boot_fun <- function(data, indices) {
    boot_sample <- data[indices, ]
    
    fit <- tryCatch({
      cfa(cfa_model, 
         data = boot_sample, 
         ordered = ordinal_vars_cfa,
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
  
  # Fit model for parameter estimates
  fit_m <- cfa(cfa_model, 
              data = current_data, 
              ordered = ordinal_vars_cfa,
              estimator = "WLSMV")
  
  # Extract estimates
  params <- parameterEstimates(fit_m)
  param_results[[m]] <- params
  
  std_params <- standardizedSolution(fit_m)
  std_param_results[[m]] <- std_params
  
  intercepts <- params[params$op == "|", ]
  intercept_results[[m]] <- intercepts
}

# Calculate bootstrap estimates
bootstrap_point_estimates <- matrix(NA, nrow = M, ncol = length(fit_indices),
                                   dimnames = list(paste0("imp", 1:M), 
                                                  fit_indices))
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

# Apply transformations and pool results
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
        # Fisher transformation
        transformed_estimates[m, i] <- fisher_transform(est)
        deriv <- 1 / (1 - est^2)
        transformed_variance[m, i] <- (deriv^2) * (se^2)
      } else if (index_name == "rmsea") {
        # Log transformation
        transformed_estimates[m, i] <- log_transform(est)
        deriv <- 1 / est
        transformed_variance[m, i] <- (deriv^2) * (se^2)
      } else {
        # No transformation
        transformed_estimates[m, i] <- est
        transformed_variance[m, i] <- se^2
      }
    }
  }
}

# Pool results using Rubin's rules
pooled_results <- data.frame(
  Index = fit_indices,
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
  
  # Store results
  pooled_results$Estimate_Original[i] <- est_original
  pooled_results$SE[i] <- sqrt(T_var)
  pooled_results$CI_Lower[i] <- ci_lower
  pooled_results$CI_Upper[i] <- ci_upper
}

# Display pooled fit indices
print("Pooled Fit Indices with 95% Confidence Intervals:")
print(pooled_results[, c("Index", "Estimate_Original", "SE", 
                        "CI_Lower", "CI_Upper")])

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
pooled_params$se <- aggregate(se ~ lhs + op + rhs, data = all_params, 
                              FUN = function(x) sqrt(mean(x^2)))$se

pooled_std_params <- aggregate(est.std ~ lhs + op + rhs, data = all_std_params, 
                               FUN = mean)
pooled_std_params$std_se <- aggregate(se ~ lhs + op + rhs, data = all_std_params,
                                      FUN = function(x) sqrt(mean(x^2)))$se

# Save results
save(bootstrap_fit_results, pooled_results, pooled_params, pooled_std_params,
     file = "mi_bootstrap_cfa_results.RData")
########################################################################7. Measurement Invariance Testing
##7.1 Prepare Data for Measurement Invariance
r
# Load imputed datasets
completed_datasets <- readRDS("imputed_datasets.rds")

# Recategorize problematic variables for invariance testing
for (i in 1:length(completed_datasets)) {
  # Create binary versions of problematic variables
  completed_datasets[[i]]$erg_rating_physical_bin <- 
    ifelse(completed_datasets[[i]]$erg_rating_physical <= 2, 0, 1)
  completed_datasets[[i]]$hyg_rating_substances_bin <- 
    ifelse(completed_datasets[[i]]$hyg_rating_substances <= 2, 0, 1)
  completed_datasets[[i]]$hyg_rating_noise_bin <- 
    ifelse(completed_datasets[[i]]$hyg_rating_noise <= 2, 0, 1)
  
  # Convert to ordered factors
  completed_datasets[[i]]$erg_rating_physical_bin <- 
    ordered(completed_datasets[[i]]$erg_rating_physical_bin)
  completed_datasets[[i]]$hyg_rating_substances_bin <- 
    ordered(completed_datasets[[i]]$hyg_rating_substances_bin)
  completed_datasets[[i]]$hyg_rating_noise_bin <- 
    ordered(completed_datasets[[i]]$hyg_rating_noise_bin)
  
  # Combine Dutch language variants
  completed_datasets[[i]]$language_group <- 
    ifelse(completed_datasets[[i]]$LanguageCode %in% c("du", "nl"), 
           "Dutch", as.character(completed_datasets[[i]]$LanguageCode))
  completed_datasets[[i]]$language_group <- 
    factor(completed_datasets[[i]]$language_group)
  
  # Ensure device type is correctly coded
  completed_datasets[[i]]$device_type <- factor(
    ifelse(is.na(completed_datasets[[i]]$ismobile), "Desktop",
           ifelse(completed_datasets[[i]]$ismobile, "Mobile", "Desktop"))
  )
  
  # Code environment domain as factor
  completed_datasets[[i]]$env_domain <- 
    factor(completed_datasets[[i]]$has_environment_domain)
}
##7.2 Define Modified CFA Models for Invariance Testing
r
# Model with binary versions of problematic variables
cfa_model_bin <- '
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_loads + 
        erg_rating_physical_bin
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  HY =~ hyg_rating_tools + hyg_rating_low_temp + hyg_rating_high_temp + 
        hyg_rating_noise_bin + hyg_rating_substances_bin
'

# Original model for comparison
cfa_model_original <- '
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_loads + 
        erg_rating_physical
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  HY =~ hyg_rating_tools + hyg_rating_low_temp + hyg_rating_high_temp + 
        hyg_rating_noise + hyg_rating_substances
'

# Update ordinal variables lists
ordinal_vars_bin <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_loads", 
  "erg_rating_physical_bin",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
  "hyg_rating_noise_bin", "hyg_rating_substances_bin"
)

ordinal_vars_original <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_loads", 
  "erg_rating_physical",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_low_temp", "hyg_rating_high_temp", 
  "hyg_rating_noise", "hyg_rating_substances"
)
##7.3 Measurement Invariance Testing Functions
r
# Function for pairwise comparison with both NHT and ET approaches
run_pairwise_projection_invariance <- function(data_list, model, group_var, 
                                             groups_to_compare, ordinal_vars, 
                                             label) {
  nht_results <- list()  # Null hypothesis testing
  et_results <- list()   # Equivalence testing
  
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
      
      # NHT approach - conventional models with constraints
      metric_nht <- cfa(model, 
                      data = subset_data,
                      group = group_var,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      scalar_nht <- cfa(model, 
                      data = subset_data, 
                      group = group_var,
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # ET approach - projection method
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
        nht = list(configural = configural, metric = metric_nht, 
                  scalar = scalar_nht),
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
  
  # Extract fit indices
  valid_idx <- 1
  for (i in which(valid_results)) {
    models <- c("configural", "metric", "scalar")
    for (j in 1:length(models)) {
      model_name <- models[j]
      if (!is.null(results_list[[i]][[model_name]])) {
        fit_indices[valid_idx, j, "chisq"] <- 
          fitMeasures(results_list[[i]][[model_name]], "chisq")
        fit_indices[valid_idx, j, "df"] <- 
          fitMeasures(results_list[[i]][[model_name]], "df")
        fit_indices[valid_idx, j, "cfi"] <- 
          fitMeasures(results_list[[i]][[model_name]], "cfi")
        fit_indices[valid_idx, j, "rmsea"] <- 
          fitMeasures(results_list[[i]][[model_name]], "rmsea")
        fit_indices[valid_idx, j, "srmr"] <- 
          fitMeasures(results_list[[i]][[model_name]], "srmr")
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
               !is.na(invariance_decision$ΔRMSEA[i]) && 
               invariance_decision$ΔRMSEA[i] < 0.015) {
      if (i == 1 && !is.na(invariance_decision$ΔSRMR[i]) && 
          invariance_decision$ΔSRMR[i] < 0.030) {
        invariance_decision$Decision[i] <- "Supported"
      } else if (i == 2 && !is.na(invariance_decision$ΔSRMR[i]) && 
                 invariance_decision$ΔSRMR[i] < 0.010) {
        invariance_decision$Decision[i] <- "Supported"
      } else {
        invariance_decision$Decision[i] <- "Not Supported"
      }
    } else {
      invariance_decision$Decision[i] <- "Not Supported"
    }
  }
  
  # Print results
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
  
  # Initialize arrays for projected values
  metric_diffs <- numeric(n_valid)
  scalar_diffs <- numeric(n_valid)
  
  # Get projected values from valid imputations
  valid_idx <- 1
  for (i in which(valid_results)) {
    if (!is.null(results_list[[i]]$metric_projection) && 
        !is.null(results_list[[i]]$metric_projection$max_loading_diff)) {
      if (is.finite(results_list[[i]]$metric_projection$max_loading_diff)) {
        metric_diffs[valid_idx] <- results_list[[i]]$metric_projection$max_loading_diff
      } else {
        metric_diffs[valid_idx] <- 0.01  # Default value for non-finite
      }
    }
    
    if (!is.null(results_list[[i]]$scalar_projection) && 
        !is.null(results_list[[i]]$scalar_projection$max_threshold_diff)) {
      if (is.finite(results_list[[i]]$scalar_projection$max_threshold_diff)) {
        scalar_diffs[valid_idx] <- results_list[[i]]$scalar_projection$max_threshold_diff
      } else {
        scalar_diffs[valid_idx] <- 0.01  # Default value for non-finite
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
  et_metric_threshold <- 0.2  # Maximum allowable difference in loadings
  et_scalar_threshold <- 0.3  # Maximum allowable difference in thresholds
  
  # Evaluate measurement invariance using ET approach
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
  
  # Print results
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
##7.4 Run Measurement Invariance Tests
r
# 7.4.1 Language groups (Dutch vs French)
dutch_fr_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],  # Use first 3 imputed datasets initially
  cfa_model_bin,
  "language_group",
  c("Dutch", "fr"),
  ordinal_vars_bin,
  "Dutch vs French"
)

# Analyze results - NHT approach
dutch_fr_nht_summary <- tryCatch({
  pool_and_compare_nht(dutch_fr_results$nht, "Dutch vs French")
}, error = function(e) {
  cat("Error analyzing Dutch vs French NHT results:", e$message, "\n")
  return(NULL)
})

# Analyze results - ET approach
dutch_fr_et_summary <- tryCatch({
  analyze_et_results(dutch_fr_results$et, "Dutch vs French")
}, error = function(e) {
  cat("Error analyzing Dutch vs French ET results:", e$message, "\n")
  return(NULL)
})

# 7.4.2 Mobile vs Desktop
mobile_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_original,
  "device_type",
  c("Mobile", "Desktop"),
  ordinal_vars_original,
  "Mobile vs Desktop"
)

mobile_nht_summary <- tryCatch({
  pool_and_compare_nht(mobile_results$nht, "Mobile vs Desktop")
}, error = function(e) {
  cat("Error analyzing Mobile vs Desktop NHT results:", e$message, "\n")
  return(NULL)
})

mobile_et_summary <- tryCatch({
  analyze_et_results(mobile_results$et, "Mobile vs Desktop")
}, error = function(e) {
  cat("Error analyzing Mobile vs Desktop ET results:", e$message, "\n")
  return(NULL)
})

# 7.4.3 Environment domain (Yes vs No)
env_results <- run_pairwise_projection_invariance(
  completed_datasets[1:3],
  cfa_model_original,
  "env_domain",
  c("Yes", "No"),
  ordinal_vars_original,
  "Environment vs No Environment"
)

env_nht_summary <- tryCatch({
  pool_and_compare_nht(env_results$nht, "Environment vs No Environment")
}, error = function(e) {
  cat("Error analyzing Environment NHT results:", e$message, "\n")
  return(NULL)
})

env_et_summary <- tryCatch({
  analyze_et_results(env_results$et, "Environment vs No Environment")
}, error = function(e) {
  cat("Error analyzing Environment ET results:", e$message, "\n")
  return(NULL)
})
##7.5 Gender Measurement Invariance
r
# Modified model excluding problematic variable for gender
cfa_model_gender <- '
  PS =~ psy_rating_pace + psy_rating_emotional + psy_rating_sphere + psy_work_life
  ER =~ erg_rating_posture + erg_rating_repeat + erg_rating_loads + 
        erg_rating_physical_bin
  SA =~ saf_rating_workinvolv + saf_rating_leadengage
  HY =~ hyg_rating_tools + hyg_rating_high_temp + hyg_rating_noise_bin + 
        hyg_rating_substances_bin
'

ordinal_vars_gender <- c(
  "psy_rating_pace", "psy_rating_emotional", "psy_rating_sphere", "psy_work_life",
  "erg_rating_posture", "erg_rating_repeat", "erg_rating_loads", 
  "erg_rating_physical_bin",
  "saf_rating_workinvolv", "saf_rating_leadengage",
  "hyg_rating_tools", "hyg_rating_high_temp", 
  "hyg_rating_noise_bin", "hyg_rating_substances_bin"
)

# Function for gender invariance (modified from pairwise function)
run_gender_invariance <- function(data_list, model, ordinal_vars, label) {
  nht_results <- list()
  et_results <- list()
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    
    # Filter to include only M and F
    subset_data <- data[data$sex_cat %in% c("M", "F"), ]
    subset_data$sex_cat <- factor(subset_data$sex_cat, levels = c("M", "F"))
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      # Run models
      configural <- cfa(model, 
                       data = subset_data,
                       group = "sex_cat",
                       ordered = ordinal_vars,
                       estimator = "WLSMV")
      
      metric_nht <- cfa(model, 
                      data = subset_data,
                      group = "sex_cat",
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = "loadings")
      
      scalar_nht <- cfa(model, 
                      data = subset_data, 
                      group = "sex_cat",
                      ordered = ordinal_vars,
                      estimator = "WLSMV",
                      group.equal = c("loadings", "thresholds"))
      
      # ET calculations
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      loadings_diff <- abs(group_loadings[[1]] - group_loadings[[2]])
      max_loading_diff <- max(loadings_diff, na.rm = TRUE)
      
      group_thresholds <- lavInspect(configural, what = "est")$th
      thresholds_diff <- abs(unlist(group_thresholds[[1]]) - 
                           unlist(group_thresholds[[2]]))
      max_threshold_diff <- max(thresholds_diff, na.rm = TRUE)
      
      list(
        nht = list(configural = configural, metric = metric_nht, 
                  scalar = scalar_nht),
        et = list(
          metric_projection = list(max_loading_diff = max_loading_diff),
          scalar_projection = list(max_threshold_diff = max_threshold_diff)
        )
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(inv_result)) {
      nht_results[[i]] <- inv_result$nht
      et_results[[i]] <- inv_result$et
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Run gender measurement invariance
gender_results_modified <- run_gender_invariance(
  completed_datasets[1:3],
  cfa_model_gender,
  ordinal_vars_gender,
  "Gender Modified (Men vs Women)"
)

# Analyze results
gender_nht_summary_modified <- tryCatch({
  pool_and_compare_nht(gender_results_modified$nht, 
                      "Gender Modified (Men vs Women)")
}, error = function(e) {
  cat("Error analyzing Modified Gender NHT results:", e$message, "\n")
  return(NULL)
})

gender_et_summary_modified <- tryCatch({
  analyze_et_results(gender_results_modified$et, 
                    "Gender Modified (Men vs Women)")
}, error = function(e) {
  cat("Error analyzing Modified Gender ET results:", e$message, "\n")
  return(NULL)
})
##7.6 Age Category Measurement Invariance
r
# Check age distribution
age_counts <- table(completed_datasets[[1]]$age_cat)
print("Age category counts:")
print(age_counts)

# Function for age invariance (multi-group)
run_age_invariance <- function(data_list, model, ordinal_vars, age_variable, label) {
  nht_results <- list()
  et_results <- list()
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    data[[age_variable]] <- factor(data[[age_variable]])
    
    cat(paste0("\nRunning ", label, " comparison for imputation ", i, "\n"))
    
    inv_result <- tryCatch({
      # Run models
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
      
      # ET calculations for multi-group
      group_loadings <- lavInspect(configural, what = "std.lv")$lambda
      num_groups <- length(unique(data[[age_variable]]))
      
      # Pairwise comparisons
      max_loading_diffs <- matrix(0, num_groups, num_groups)
      max_threshold_diffs <- matrix(0, num_groups, num_groups)
      
      for (g1 in 1:(num_groups-1)) {
        for (g2 in (g1+1):num_groups) {
          loadings_diff <- abs(group_loadings[[g1]] - group_loadings[[g2]])
          max_diff <- max(loadings_diff, na.rm = TRUE)
          max_loading_diffs[g1, g2] <- max_diff
          max_loading_diffs[g2, g1] <- max_diff
          
          group_thresholds <- lavInspect(configural, what = "est")$th
          thresholds_diff <- abs(unlist(group_thresholds[[g1]]) - 
                               unlist(group_thresholds[[g2]]))
          max_diff <- max(thresholds_diff, na.rm = TRUE)
          max_threshold_diffs[g1, g2] <- max_diff
          max_threshold_diffs[g2, g1] <- max_diff
        }
      }
      
      max_loading_diff <- max(max_loading_diffs, na.rm = TRUE)
      max_threshold_diff <- max(max_threshold_diffs, na.rm = TRUE)
      
      list(
        nht = list(configural = configural, metric = metric_nht, 
                  scalar = scalar_nht),
        et = list(
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
    }
  }
  
  return(list(nht = nht_results, et = et_results))
}

# Run age measurement invariance
age_results <- run_age_invariance(
  completed_datasets[1:3],
  cfa_model_gender,  # Use modified model
  ordinal_vars_gender,
  "age_cat",
  "Age (age_cat)"
)

# Analyze results
age_nht_summary <- tryCatch({
  pool_and_compare_nht(age_results$nht, "Age (age_cat)")
}, error = function(e) {
  cat("Error analyzing Age NHT results:", e$message, "\n")
  return(NULL)
})

age_et_summary <- tryCatch({
  analyze_et_results(age_results$et, "Age (age_cat)")
}, error = function(e) {
  cat("Error analyzing Age ET results:", e$message, "\n")
  return(NULL)
})
##7.7 Create Summary Tables
r
# Create summary tables for all invariance tests
# Combine NHT results
nht_summary_rows <- list()

if (!is.null(dutch_fr_nht_summary) && !is.null(dutch_fr_nht_summary$decision)) {
  nht_summary_rows$dutch_fr <- data.frame(
    Comparison = "Dutch vs French",
    Metric = dutch_fr_nht_summary$decision$Decision[1],
    Scalar = dutch_fr_nht_summary$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

if (!is.null(mobile_nht_summary) && !is.null(mobile_nht_summary$decision)) {
  nht_summary_rows$mobile <- data.frame(
    Comparison = "Mobile vs Desktop",
    Metric = mobile_nht_summary$decision$Decision[1],
    Scalar = mobile_nht_summary$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

if (!is.null(env_nht_summary) && !is.null(env_nht_summary$decision)) {
  nht_summary_rows$env <- data.frame(
    Comparison = "Environment vs No Environment",
    Metric = env_nht_summary$decision$Decision[1],
    Scalar = env_nht_summary$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

if (!is.null(gender_nht_summary_modified) && 
    !is.null(gender_nht_summary_modified$decision)) {
  nht_summary_rows$gender <- data.frame(
    Comparison = "Gender (Men vs Women)",
    Metric = gender_nht_summary_modified$decision$Decision[1],
    Scalar = gender_nht_summary_modified$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

if (!is.null(age_nht_summary) && !is.null(age_nht_summary$decision)) {
  nht_summary_rows$age <- data.frame(
    Comparison = "Age Categories",
    Metric = age_nht_summary$decision$Decision[1],
    Scalar = age_nht_summary$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

# Combine available NHT rows
if (length(nht_summary_rows) > 0) {
  nht_summary_table <- do.call(rbind, nht_summary_rows)
  
  cat("\n\nOverall Measurement Invariance Summary (NHT Approach):\n")
  print(nht_summary_table)
  
  write.csv(nht_summary_table, "measurement_invariance_nht_summary.csv", 
            row.names = FALSE)
}

# Combine ET results
et_summary_rows <- list()

if (!is.null(dutch_fr_et_summary) && !is.null(dutch_fr_et_summary$decision)) {
  et_summary_rows$dutch_fr <- data.frame(
    Comparison = "Dutch vs French",
    Metric = dutch_fr_et_summary$decision$Decision[1],
    Scalar = dutch_fr_et_summary$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

if (!is.null(mobile_et_summary) && !is.null(mobile_et_summary$decision)) {
  et_summary_rows$mobile <- data.frame(
    Comparison = "Mobile vs Desktop",
    Metric = mobile_et_summary$decision$Decision[1],
    Scalar = mobile_et_summary$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

if (!is.null(env_et_summary) && !is.null(env_et_summary$decision)) {
  et_summary_rows$env <- data.frame(
    Comparison = "Environment vs No Environment",
    Metric = env_et_summary$decision$Decision[1],
    Scalar = env_et_summary$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

if (!is.null(gender_et_summary_modified) && 
    !is.null(gender_et_summary_modified$decision)) {
  et_summary_rows$gender <- data.frame(
    Comparison = "Gender (Men vs Women)",
    Metric = gender_et_summary_modified$decision$Decision[1],
    Scalar = gender_et_summary_modified$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

if (!is.null(age_et_summary) && !is.null(age_et_summary$decision)) {
  et_summary_rows$age <- data.frame(
    Comparison = "Age Categories",
    Metric = age_et_summary$decision$Decision[1],
    Scalar = age_et_summary$decision$Decision[2],
    stringsAsFactors = FALSE
  )
}

# Combine available ET rows
if (length(et_summary_rows) > 0) {
  et_summary_table <- do.call(rbind, et_summary_rows)
  
  cat("\n\nOverall Measurement Invariance Summary (ET Approach):\n")
  print(et_summary_table)
  
  write.csv(et_summary_table, "measurement_invariance_et_summary.csv", 
            row.names = FALSE)
}

# Create comparison table between NHT and ET approaches
if (length(nht_summary_rows) > 0 && length(et_summary_rows) > 0) {
  common_comparisons <- intersect(
    nht_summary_table$Comparison,
    et_summary_table$Comparison
  )
  
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
  
  write.csv(combined_summary, "measurement_invariance_comparison.csv", 
            row.names = FALSE)
}

# Save all results
save(list = ls(pattern = ".*_summary.*|.*_results.*"), 
     file = "measurement_invariance_all_results.RData")

cat("\n\nAll measurement invariance analyses completed successfully!\n")
#Summary
#This appendix provides the complete R code used for the workplace well-being assessment analysis in this thesis. The code covers:

#Data Preparation: Loading and selecting relevant variables, including model variables and auxiliary variables for imputation
#Missing Data Analysis: Comprehensive visualization and assessment of missing data patterns across variables, companies, and domains
#Multiple Imputation: Using the MICE algorithm with appropriate methods for different variable types
#Confirmatory Factor Analysis: Fitting a four-factor model with proper handling of ordinal variables
#Reliability and Validity: Calculating composite reliability, AVE, discriminant validity, and internal consistency measures
#Measurement Invariance: Testing invariance across language groups, device types, environment domain presence, gender, and age categories using both null hypothesis testing (NHT) and equivalence testing (ET) approaches
#The code demonstrates best practices for handling complex survey data with missing values and ordinal variables in the context of workplace assessment instruments.

