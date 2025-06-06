# workplace-wellbeing-assessment_MasterThesis
R code for Master's thesis: Workplace Well-being validation and  Assessment - CFA
Workplace Well-being validation and Assessment: Master's Thesis Analysis Code
This repository contains the R code used for the analysis in my Master's thesis on workplace well-being assessment using confirmatory factor analysis and measurement invariance testing.

Overview
This project analyzes workplace well-being data across four domains:

Psychosocial (PS): Work pace, emotional demands, atmosphere, work-life balance
Ergonomics (ER): Physical postures, repetitive work, manual handling
Safety (SA): Safety satisfaction, work involvement, leadership engagement
Hygiene (HY): Exposure to noise, temperature, vibrations, hazardous substances
Enivronmential (EN): Additional domain for company 4
Key Analyses
Missing Data Analysis: Comprehensive assessment of missing data patterns
Multiple Imputation: Using MICE with appropriate methods for ordinal data
Confirmatory Factor Analysis: Four-factor model with WLSMV estimation
Reliability & Validity: Composite reliability, AVE, discriminant validity
Measurement Invariance: Testing across language, device type, gender, age using both NHT and ET approaches
Requirements
R version 4.0.0 or higher
RStudio (recommended)
Required R Packages
r
# Core packages
install.packages(c("tidyverse", "lavaan", "semTools", "mice"))

# Additional packages
install.packages(c("psych", "MVN", "corrplot", "VIM", "boot"))

# For visualization
install.packages(c("ggplot2", "ggthemes", "wesanderson", "gridExtra"))

# Set working directory to the project root
setwd("path/to/workplace-wellbeing-assessment-MasterThesis")

# Run scripts in order
source("R/01_data_preparation.R")
source("R/02_missing_data_analysis.R")
source("R/03_multiple_imputation.R")
source("R/04_cfa_analysis.R")
source("R/05_reliability_validity.R")
source("R/06_measurement_invariance.R")
source("R/07_pooled_analysis.R")
Project Structure
├── R/                      # Main analysis scripts
│   ├── 01_data_preparation.R
│   ├── 02_missing_data_analysis.R
│   ├── 03_multiple_imputation.R
│   ├── 04_cfa_analysis.R
│   ├── 05_reliability_validity.R
│   ├── 06_measurement_invariance.R
│   ├── 07_pooled_analysis.R
│   └── utils/             # Helper functions
├── data/                  # Data directory (not tracked)
│   ├── raw/              # Original data files
│   └── processed/        # Processed data files
├── output/               # Analysis outputs
│   ├── figures/         # Generated plots
│   ├── tables/          # Result tables
│   └── results/         # Model objects and summaries
└── docs/                # Documentation
Key Functions
Composite Reliability & AVE Calculation
r
calculate_CR_AVE(fit)  # Returns CR and AVE for each factor
Measurement Invariance Testing
r
test_invariance(data, model, group_var, ordinal_vars)  # Tests configural, metric, and scalar invariance
Pooled Reliability Analysis
r
calculate_pooled_reliability(datasets, factor_vars)  # Pools reliability across imputed datasets
Results
The analysis outputs include:

Missing data visualizations
Model fit statistics
Factor loadings (standardized and unstandardized)
Reliability measures (Cronbach's α, McDonald's ω, CR)
Validity measures (AVE, discriminant validity)
Measurement invariance test results
Data Privacy
The raw data is not included in this repository due to privacy considerations. The code is designed to work with data in the following format:

CSV file with semicolon delimiter
Ordinal variables coded 1-5
Missing values coded as NA
Environmental domain variables coded as -999 when not applicable
Citation
If you use this code in your research, please cite:

[Your Name]. (2024). Workplace Well-being Assessment: Analysis Code for Master's Thesis. 
GitHub repository: https://github.com/yourusername/workplace-wellbeing-assessment-MasterThesis
License
This project is licensed under the MIT License - see the LICENSE file for details.

Author
[Jimmy Komalceh]
Master's Student in [Statistics and Data Science: Biostatistics]
[Hasselt University]
Email: [jkomalceh@gmail.com]

Acknowledgments
Thesis supervisor: [Prof. dr. Steven Abrams]
Data collection: [Mensura/R&D Team]
External superviso: [Lieve Van Dyck]
This code was developed as part of a Master's thesis at [Hasselt University], [2025]

