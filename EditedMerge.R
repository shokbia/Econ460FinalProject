############################################################
# FINAL PROJECT: VACCINATION AND DISEASE OUTBREAKS
# Group 8: Chloe Tjangnaka, Bia Shok, Lara Li, Edison Zhong, Bauyrzhan Karashev
############################################################

# Install and load packages
library(Hmisc)
library(dplyr)
library(ggplot2)
library(stargazer)
library(hdm)

# Read the data
setwd("DSCI/Fall2025/ECON460/FinalProj")
source("NISPUF23.R")

# Load NNDSS disease data
nndss_data <- read.csv("NNDSS_Weekly_Data_20251202.csv")
# Check NNDSS structure
cat("========== NNDSS DATA CHECK ==========\n")
cat("Dimensions:", dim(nndss_data), "\n")
cat("\nColumn names:\n")
print(names(nndss_data))
cat("\nFirst few rows:\n")
print(head(nndss_data))
cat("\nUnique diseases in dataset:\n")
print(unique(nndss_data$Label))

############################################################
# STEP 1: Filter NNDSS for MMR DISEASES
############################################################
# CRITICAL CHANGE: Use MMR diseases to match MMR vaccination variable
nndss_filtered <- nndss_data %>%
  filter(Label %in% c("Measles, Imported", "Measles, Indigenous", "Mumps", "Rubella"),
         Current.MMWR.Year == 2023) %>%
  mutate(STATE_CLEAN = toupper(trimws(Reporting.Area))) %>%
  filter(!STATE_CLEAN %in% c("US RESIDENTS", "U.S. RESIDENTS", "NEW ENGLAND", 
                             "MIDDLE ATLANTIC", "EAST NORTH CENTRAL", 
                             "WEST NORTH CENTRAL", "SOUTH ATLANTIC", 
                             "EAST SOUTH CENTRAL", "WEST SOUTH CENTRAL", 
                             "MOUNTAIN", "PACIFIC", "US TERRITORIES", 
                             "U.S. TERRITORIES", "NON-US RESIDENTS", 
                             "NON-U.S. RESIDENTS", "TOTAL"))

# Aggregate by state - sum all MMR disease cases
nndss_state <- nndss_filtered %>%
  group_by(STATE_CLEAN) %>%
  summarise(
    total_cases = sum(ifelse(Current.week == "-", 0, as.numeric(Current.week)), na.rm = TRUE),
    weeks_reported = n()
  )

print(paste("NNDSS rows:", nrow(nndss_state)))
head(nndss_state)

############################################################
# STEP 2: Add Political Party Control Variable
# SOURCE: National Governors Association, 2023
# URL: https://www.nga.org/governors/
############################################################
political_data <- data.frame(
  STATE = c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", 
            "COLORADO", "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA",
            "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA",
            "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND",
            "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI",
            "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY",
            "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO",
            "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA",
            "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT",
            "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING",
            "DISTRICT OF COLUMBIA", "PUERTO RICO"),
  governor_party = c("R", "R", "D", "R", "D", 
                     "D", "D", "D", "R", "R",
                     "D", "R", "D", "R", "R",
                     "D", "D", "D", "D", "D",
                     "D", "D", "D", "R", "R",
                     "R", "R", "R", "R", "D",  
                     "D", "D", "D", "R", "R",
                     "R", "D", "D", "D", "R",
                     "R", "R", "R", "R", "R",
                     "R", "D", "R", "D", "R",
                     "D", "D")
)
############################################################
# STEP 3: Add Census Data (Additional Controls)
# SOURCES:
# - Median Income: US Census Bureau, 2022 ACS 5-Year Estimates, Table B19013
#   https://data.census.gov/table/ACSDT5Y2022.B19013
# - Unemployment: Bureau of Labor Statistics, LAUS 2023
#   https://www.bls.gov/lau/
# - Population Density: US Census Bureau, Population Estimates 2023
#   https://www.census.gov/programs-surveys/popest.html
# - Poverty Rate: US Census Bureau, 2022 ACS 5-Year Estimates, Table B17001
############################################################
library(tidycensus)
readRenviron("~/.Renviron")
census_api_key("b586797bc3502698fac8a9d4d484cbf6570824f7", install = TRUE, overwrite = TRUE)

cat("Testing Census API connection...\n")
test_data <- get_acs(
  geography = "state",
  variables = "B19013_001",  
  year = 2022,
  survey = "acs5"
)
cat("API connection successful!\n")
cat("Sample data retrieved:\n")
print(head(test_data))

cat("\n========== PULLING CENSUS DATA FROM API ==========\n")

# 1. Median Income
cat("Pulling median income data...\n")
median_income_data <- get_acs(
  geography = "state",
  variables = "B19013_001",
  year = 2022,
  survey = "acs5"
) %>%
  select(NAME, estimate) %>%
  rename(median_income = estimate) %>%
  mutate(STATE = toupper(NAME))

cat("Median income retrieved for", nrow(median_income_data), "states\n")

# 2. Poverty Rate
cat("Pulling poverty data...\n")
poverty_data <- get_acs(
  geography = "state",
  variables = c(poverty_pop = "B17001_002",  # Below poverty
                total_pop = "B17001_001"),     # Total for poverty calc
  year = 2022,
  survey = "acs5"
) %>%
  select(NAME, variable, estimate) %>%
  tidyr::pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    poverty_rate = (poverty_pop / total_pop) * 100,
    STATE = toupper(NAME)
  ) %>%
  select(STATE, poverty_rate)

cat("Poverty rate calculated for", nrow(poverty_data), "states\n")

# 3. Population (for density calculation)
cat("Pulling population data...\n")
population_data <- get_acs(
  geography = "state",
  variables = "B01003_001",
  year = 2022,
  survey = "acs5"
) %>%
  select(NAME, estimate) %>%
  rename(population = estimate) %>%
  mutate(STATE = toupper(NAME))

cat("Population retrieved for", nrow(population_data), "states\n")

# 4. Unemployment Rate
cat("Pulling unemployment data...\n")
unemployment_data <- get_acs(
  geography = "state",
  variables = c(unemployed = "B23025_005",
                labor_force = "B23025_003"),
  year = 2022,
  survey = "acs5"
) %>%
  select(NAME, variable, estimate) %>%
  tidyr::pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    unemployment_rate = (unemployed / labor_force) * 100,
    STATE = toupper(NAME)
  ) %>%
  select(STATE, unemployment_rate)

cat("Unemployment rate calculated for", nrow(unemployment_data), "states\n")

cat("\n========== CHECKING RETRIEVED DATA ==========\n")
cat("Median income sample:\n")
print(head(median_income_data))
cat("\nPoverty rate sample:\n")
print(head(poverty_data))

############################################################
# STEP 4: Aggregate NIS to state level
############################################################

nis_state <- NISPUF23 %>%
  group_by(STATE) %>%
  summarise(
    n_children = n(),
    mmr_utd = mean(P_UTDMMX == "UTD", na.rm = TRUE),
    pct_college = mean(EDUC1 == "COLLEGE GRAD", na.rm = TRUE),
    pct_poverty = mean(INCPOV1 == "BELOW POVERTY", na.rm = TRUE),
    pct_hispanic = mean(RACEETHK == "HISPANIC", na.rm = TRUE),
    pct_medicaid = mean(INS_STAT2_I == "ANY MEDICAID", na.rm = TRUE),
    pct_married = mean(MARITAL2 == "MARRIED", na.rm = TRUE)
  ) %>%
  mutate(STATE = as.character(STATE))

############################################################
# STEP 5: Merge ALL datasets
############################################################

merged_data <- nis_state %>%
  left_join(nndss_state, by = c("STATE" = "STATE_CLEAN")) %>%
  left_join(political_data, by = "STATE") %>%
  left_join(census_controls, by = "STATE")

print(paste("Merged rows:", nrow(merged_data)))
print(paste("States with case data:", sum(!is.na(merged_data$total_cases))))

# Check for missing census data
print("States missing census data:")
print(merged_data %>% 
        filter(is.na(median_income)) %>% 
        select(STATE))

############################################################
# STEP 6: Create final analysis dataset
############################################################

final_data <- merged_data %>%
  filter(!is.na(total_cases), !is.na(median_income)) %>%
  mutate(
    # Y: Disease outcome
    Y_binary = ifelse(total_cases > 0, 1, 0),
    Y_cases = total_cases,
    Y_rate = (total_cases / n_children) * 1000,
    
    # D: Treatment (vaccination)
    D_binary = ifelse(mmr_utd > 0.90, 1, 0),
    D_continuous = mmr_utd,
    
    # Political party numeric
    party_republican = ifelse(governor_party == "R", 1, 0),
    
    # Standardize continuous variables for better model fit
    median_income_1000 = median_income / 1000,
    pop_density_100 = pop_density / 100
  )

# Summary
print(paste("Final dataset rows:", nrow(final_data)))
summary(final_data)

# Save the data
write.csv(final_data, "mmr_vaccination_analysis_FINAL.csv", row.names = FALSE)

############################################################
# STEP 6A: Add 2022 Lagged Disease Cases  ### 
############################################################

cat("\n========== CREATING LAGGED DISEASE CONTROL ==========\n")

# Filter NNDSS for 2022 MMR diseases
nndss_2022 <- nndss_data %>%
  filter(Label %in% c("Measles, Imported", "Measles, Indigenous", "Mumps", "Rubella"),
         Current.MMWR.Year == 2022) %>%  # CHANGED: 2022 instead of 2023
  mutate(STATE_CLEAN = toupper(trimws(Reporting.Area))) %>%
  filter(!STATE_CLEAN %in% c("US RESIDENTS", "U.S. RESIDENTS", "NEW ENGLAND", 
                             "MIDDLE ATLANTIC", "EAST NORTH CENTRAL", 
                             "WEST NORTH CENTRAL", "SOUTH ATLANTIC", 
                             "EAST SOUTH CENTRAL", "WEST SOUTH CENTRAL", 
                             "MOUNTAIN", "PACIFIC", "US TERRITORIES", 
                             "U.S. TERRITORIES", "NON-US RESIDENTS", 
                             "NON-U.S. RESIDENTS", "TOTAL"))

# Aggregate by state
nndss_2022_state <- nndss_2022 %>%
  group_by(STATE_CLEAN) %>%
  summarise(
    cases_2022 = sum(ifelse(Current.week == "-", 0, as.numeric(Current.week)), na.rm = TRUE),
    weeks_reported_2022 = n()
  )

cat("2022 data created:", nrow(nndss_2022_state), "states\n")

# Quick check: Top states with 2022 outbreaks
cat("\nTop 10 states by 2022 MMR cases:\n")
print(nndss_2022_state %>% arrange(desc(cases_2022)) %>% head(10))

# Merge with final_data
final_data_with_lag <- final_data %>%
  left_join(nndss_2022_state, by = c("STATE" = "STATE_CLEAN"))

# Check for missing values
cat("\nStates missing 2022 data:\n")
missing_2022 <- final_data_with_lag %>% 
  filter(is.na(cases_2022)) %>% 
  select(STATE)
print(missing_2022)

# Replace NA with 0 (means no cases reported)
final_data_with_lag <- final_data_with_lag %>%
  mutate(cases_2022 = ifelse(is.na(cases_2022), 0, cases_2022))

cat("Lagged cases merged\n")

# Quick comparison: 2022 vs 2023 cases
cat("\nComparing 2022 vs 2023 outbreaks:\n")
cat(sprintf("States with cases in 2022: %d\n", sum(final_data_with_lag$cases_2022 > 0)))
cat(sprintf("States with cases in 2023: %d\n", sum(final_data_with_lag$Y_cases > 0)))
cat(sprintf("States with cases in BOTH years: %d\n", 
            sum(final_data_with_lag$cases_2022 > 0 & final_data_with_lag$Y_cases > 0)))
############################################################
# STEP 7: EXPLORATORY ANALYSIS
############################################################

cat("\n========== EXPLORATORY ANALYSIS ==========\n")

# Top states by MMR disease cases
print("Top 10 states by MMR disease cases:")
final_data %>%
  arrange(desc(total_cases)) %>%
  select(STATE, total_cases, mmr_utd, Y_rate, governor_party, median_income) %>%
  head(10) %>%
  print()

# Distribution check
cat("\nDistribution Statistics:\n")
cat(sprintf("Mean cases: %.2f (SD: %.2f)\n", mean(final_data$Y_cases), sd(final_data$Y_cases)))
cat(sprintf("Mean vaccination rate: %.2f%% (SD: %.2f%%)\n", 
            mean(final_data$D_continuous)*100, sd(final_data$D_continuous)*100))
cat(sprintf("Median income range: $%d - $%d\n", 
            min(final_data$median_income), max(final_data$median_income)))

# Check zero-inflation
cat(sprintf("\nStates with zero cases: %d (%.1f%%)\n", 
            sum(final_data$Y_cases == 0),
            sum(final_data$Y_cases == 0)/nrow(final_data)*100))
cat(sprintf("States with >0 cases: %d (%.1f%%)\n", 
            sum(final_data$Y_cases > 0),
            sum(final_data$Y_cases > 0)/nrow(final_data)*100))

############################################################
# STEP 8: MODEL 1 - Naive OLS
############################################################

cat("\n========== MODEL 1: NAIVE OLS ==========\n")
model1_naive <- lm(Y_rate ~ D_continuous, data = final_data)
summary(model1_naive)

############################################################
# STEP 9: MODEL 2 - Controlled OLS (Original Controls)
############################################################

cat("\n========== MODEL 2: BASIC CONTROLS ==========\n")
model2_controlled <- lm(Y_rate ~ D_continuous + pct_college + pct_poverty + 
                          pct_hispanic + pct_medicaid + pct_married, 
                        data = final_data)
summary(model2_controlled)

############################################################
# STEP 10: MODEL 3 - Full Controls (With Census + Political)
############################################################

cat("\n========== MODEL 3: FULL CONTROLS ==========\n")
model3_full <- lm(Y_rate ~ D_continuous + pct_college + pct_poverty + 
                    pct_hispanic + pct_medicaid + pct_married +
                    pop_density_100 + median_income_1000 + unemployment_rate +
                    party_republican,
                  data = final_data)
summary(model3_full)

############################################################
# STEP 11: MODEL 4 - Double LASSO Variable Selection
############################################################

cat("\n========== MODEL 4: DOUBLE LASSO ==========\n")
library(dplyr)
library(hdm)
# Prepare control matrix for Double Lasso
X_controls <- final_data %>%
  select(pct_college, pct_poverty, pct_hispanic, pct_medicaid, pct_married,
         pop_density_100, median_income_1000, unemployment_rate, party_republican) %>%
  as.matrix()

# Remove rows with any NA
complete_cases <- complete.cases(X_controls, final_data$Y_rate, final_data$D_continuous)
X_controls_clean <- X_controls[complete_cases, ]
Y_clean <- final_data$Y_rate[complete_cases]
D_clean <- final_data$D_continuous[complete_cases]

# Run Double Lasso
tryCatch({
  lasso_model <- rlassoEffect(
    x = X_controls_clean,
    y = Y_clean,
    d = D_clean,
    method = "double selection"
  )
  
  cat("\nDOUBLE LASSO RESULTS:\n")
  print(summary(lasso_model))
  
  # Get selected variables
  if(length(lasso_model$coefficients) > 0) {
    selected_vars <- which(abs(lasso_model$coefficients[-1]) > 0)  # Exclude intercept
    if(length(selected_vars) > 0) {
      cat("\nVariables selected by Double Lasso:\n")
      print(colnames(X_controls)[selected_vars])
    } else {
      cat("\nNo control variables selected (treatment effect only)\n")
    }
  }
  

  # Extract treatment effect
  cat(sprintf("\nDouble Lasso Treatment Effect: %.3f\n", coef(lasso_model)[1]))
  
}, error = function(e) {
  cat("Double Lasso encountered an issue:\n")
  cat(paste("Error:", e$message, "\n"))
  cat("Proceeding with full model instead.\n")
})

# Calculate R² for Double LASSO manually
# Get the treatment effect coefficient
tau_hat <- lasso_model$coefficients[1]  

# Calculate fitted values (since no controls were selected, it's just intercept + tau*D)
fitted_values <- mean(Y_clean) + tau_hat * (D_clean - mean(D_clean))

# Calculate R²
residuals <- Y_clean - fitted_values
SS_res <- sum(residuals^2)
SS_tot <- sum((Y_clean - mean(Y_clean))^2)
r_squared <- 1 - (SS_res / SS_tot)

cat("\nDouble LASSO R²:", round(r_squared, 4), "\n")

############################################################
# STEP 12: MODEL 5 - Logistic Regression (Binary Outcome)
############################################################

cat("\n========== MODEL 5: LOGISTIC REGRESSION ==========\n")
model5_logit <- glm(Y_binary ~ D_continuous + pct_college + pct_poverty + 
                      pct_hispanic + pct_medicaid + pct_married +
                      pop_density_100 + median_income_1000 + unemployment_rate +
                      party_republican,
                    data = final_data, 
                    family = binomial())
summary(model5_logit)

############################################################
# STEP 13: MODEL COMPARISON TABLE
############################################################

cat("\n========== MODEL COMPARISON TABLE ==========\n")

stargazer(model1_naive, model2_controlled, model3_full, 
          type = "text",
          title = "Effect of MMR Vaccination on Disease Outbreaks",
          column.labels = c("Naive", "Basic Controls", "Full Controls"),
          dep.var.labels = "MMR Disease Cases per 1000 Children",
          covariate.labels = c("Vaccination Rate", "% College", "% Poverty",
                               "% Hispanic", "% Medicaid", "% Married",
                               "Pop. Density (per 100)", "Median Income ($1000s)", 
                               "Unemployment Rate", "Republican Governor"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines = list(
            c("Census Controls", "No", "No", "Yes"),
            c("Political Controls", "No", "No", "Yes")
          ),
          notes = "Standard errors in parentheses. * p<0.05, ** p<0.01, *** p<0.001")

############################################################
# STEP 14: VISUALIZATIONS
############################################################

cat("\n========== CREATING VISUALIZATIONS ==========\n")

# Visualization 1: Main scatterplot with party colors
p1 <- ggplot(final_data, aes(x = D_continuous, y = Y_rate)) +
  geom_point(aes(size = n_children, color = governor_party), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("D" = "#2E74C0", "R" = "#CB454A"),
                     labels = c("D" = "Democratic", "R" = "Republican")) +
  labs(x = "MMR Vaccination Rate", 
       y = "MMR Disease Cases per 1000 Children",
       title = "Vaccination Rate vs MMR Disease Outbreaks by State",
       subtitle = "No significant relationship detected (p=0.423)",
       size = "Sample Size",
       color = "Governor Party") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "right")

ggsave("vaccination_vs_outbreak_main.png", p1, width = 10, height = 6, dpi = 300)
cat("Saved: vaccination_vs_outbreak_main.png\n")

# Visualization 2: Heterogeneity by Census poverty rate
p2 <- ggplot(final_data, aes(x = D_continuous, y = Y_rate, color = poverty_rate)) +
  geom_point(aes(size = n_children), alpha = 0.7) +
  scale_color_gradient(low = "green", high = "red", name = "Poverty Rate (%)") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(x = "MMR Vaccination Rate",
       y = "MMR Disease Cases per 1000 Children",
       title = "Treatment Effect Heterogeneity by Poverty Rate",
       subtitle = "Examining if vaccination effects vary by socioeconomic status",
       size = "Sample Size") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("poverty_heterogeneity.png", p2, width = 10, height = 6, dpi = 300)
cat(" Saved: poverty_heterogeneity.png\n")

# Visualization 3: Party comparison boxplot
p3 <- ggplot(final_data, aes(x = governor_party, y = Y_rate, fill = governor_party)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  scale_fill_manual(values = c("D" = "#2E74C0", "R" = "#CB454A"),
                    labels = c("D" = "Democratic", "R" = "Republican")) +
  labs(x = "Governor Party",
       y = "MMR Disease Cases per 1000 Children",
       title = "Disease Outbreak Rates by Political Party",
       subtitle = "No significant difference between parties (coef=0.84, p=0.715)",
       fill = "Party") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none")

ggsave("party_comparison.png", p3, width = 8, height = 6, dpi = 300)
cat("Saved: party_comparison.png\n")

# Visualization 4: Residual plot
residuals_data <- data.frame(
  fitted = fitted(model3_full),
  residuals = residuals(model3_full)
)

p4 <- ggplot(residuals_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  labs(x = "Fitted Values",
       y = "Residuals",
       title = "Residual Plot for Model 3 (Full Controls)",
       subtitle = "Checking for heteroskedasticity and nonlinearity") +
  theme_minimal()

ggsave("residual_plot.png", p4, width = 8, height = 6, dpi = 300)
cat("Saved: residual_plot.png\n")

cat("\n All visualizations created!\n")

############################################################
# ENHANCED CONTROLS - EXPERIMENT
############################################################

# Create interaction terms and nonlinear terms
final_data_enhanced <- final_data %>%
  mutate(
    # Nonlinear vaccination effects
    vaccination_squared = D_continuous^2,
    
    # Interactions
    vax_x_poverty = D_continuous * poverty_rate,
    vax_x_density = D_continuous * pop_density_100,
    vax_x_college = D_continuous * pct_college,
    
    # Threshold effects
    high_vax_threshold = ifelse(D_continuous > 0.95, 1, 0),
    low_vax_threshold = ifelse(D_continuous < 0.85, 1, 0),
    
    # Regional dummies (might capture anti-vax culture)
    region_west = ifelse(STATE %in% c("CALIFORNIA", "OREGON", "WASHINGTON", 
                                      "NEVADA", "ARIZONA", "UTAH", "IDAHO",
                                      "MONTANA", "WYOMING", "COLORADO", 
                                      "NEW MEXICO"), 1, 0),
    region_south = ifelse(STATE %in% c("TEXAS", "OKLAHOMA", "ARKANSAS", 
                                       "LOUISIANA", "MISSISSIPPI", "ALABAMA",
                                       "TENNESSEE", "KENTUCKY", "WEST VIRGINIA",
                                       "VIRGINIA", "NORTH CAROLINA", "SOUTH CAROLINA",
                                       "GEORGIA", "FLORIDA"), 1, 0),
    
    # Large state indicator (more cases just due to size)
    large_state = ifelse(n_children > 1000, 1, 0)
  )

# Model 6: With interactions
model6_interactions <- lm(Y_rate ~ D_continuous + 
                            vax_x_poverty + vax_x_density + vax_x_college +
                            pct_college + pct_poverty + pct_hispanic + 
                            pct_medicaid + pct_married +
                            pop_density_100 + median_income_1000 + 
                            unemployment_rate + party_republican,
                          data = final_data_enhanced)

# Model 7: With nonlinear vaccination
model7_nonlinear <- lm(Y_rate ~ D_continuous + vaccination_squared +
                         pct_college + pct_poverty + pct_hispanic + 
                         pct_medicaid + pct_married +
                         pop_density_100 + median_income_1000 + 
                         unemployment_rate + party_republican,
                       data = final_data_enhanced)

# Model 8: With regional effects
model8_regional <- lm(Y_rate ~ D_continuous + region_west + region_south +
                        large_state +
                        pct_college + pct_poverty + pop_density_100 + 
                        median_income_1000 + unemployment_rate,
                      data = final_data_enhanced)

cat("========== ENHANCED MODELS ==========\n")
cat("\nModel 6: Interactions\n")
summary(model6_interactions)

cat("\nModel 7: Nonlinear Vaccination\n")
summary(model7_nonlinear)

cat("\nModel 8: Regional Effects\n")
summary(model8_regional)

# Compare all models
stargazer(model3_full, model6_interactions, model7_nonlinear, model8_regional,
          type = "text",
          title = "Enhanced Model Comparison")

