############################################################
# FINAL PROJECT: VACCINATION AND DISEASE OUTBREAKS
# Updated with: MMR diseases, Census controls, Double Lasso
# All data sources documented and cited
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

############################################################
# STEP 1: Filter NNDSS for MMR DISEASES (NOT Pertussis!)
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
                     "R", "R", "D", "R", "D",
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

census_controls <- read.csv(text = "
STATE,median_income,unemployment_rate,pop_density,poverty_rate
ALABAMA,59609,2.6,96.9,15.7
ALASKA,84177,4.6,1.3,10.1
ARIZONA,72581,3.4,64.9,12.8
ARKANSAS,56335,2.8,58.4,16.2
CALIFORNIA,84907,4.1,256.4,11.8
COLORADO,80184,3.1,56.4,9.3
CONNECTICUT,90213,3.4,740.7,9.4
DELAWARE,79325,3.9,504.3,11.3
FLORIDA,67521,2.5,405.4,12.1
GEORGIA,69021,3.0,186.6,13.5
HAWAII,88005,3.2,222.0,9.3
IDAHO,63987,2.9,22.8,10.7
ILLINOIS,72205,4.1,228.5,11.5
INDIANA,67173,3.0,189.0,11.9
IOWA,67126,2.7,56.9,10.2
KANSAS,69747,2.8,35.9,11.2
KENTUCKY,60293,4.0,113.9,16.0
LOUISIANA,57852,3.4,107.5,17.8
MAINE,68251,2.7,43.6,10.4
MARYLAND,98461,1.8,636.0,9.0
MASSACHUSETTS,96505,2.9,894.4,9.4
MICHIGAN,64488,3.6,177.6,12.7
MINNESOTA,84313,2.5,71.5,8.9
MISSISSIPPI,52985,3.3,63.7,18.7
MISSOURI,65920,2.8,89.5,12.7
MONTANA,66341,2.5,7.5,11.7
NEBRASKA,71722,2.2,25.4,9.9
NEVADA,71646,5.4,28.5,12.5
NEW HAMPSHIRE,90845,2.4,153.2,7.2
NEW JERSEY,97126,3.4,1263.0,9.0
NEW MEXICO,57731,3.9,17.5,16.8
NEW YORK,81386,4.1,419.0,13.6
NORTH CAROLINA,64730,3.1,218.0,12.3
NORTH DAKOTA,73959,2.1,11.0,10.6
OHIO,66990,3.5,287.5,12.9
OKLAHOMA,61364,3.0,57.7,14.3
OREGON,76362,3.5,44.8,11.1
PENNSYLVANIA,73170,3.3,286.5,11.8
RHODE ISLAND,81370,2.8,1061.4,11.3
SOUTH CAROLINA,63623,2.8,173.3,13.0
SOUTH DAKOTA,69457,2.0,11.9,10.9
TENNESSEE,64035,3.0,167.0,13.2
TEXAS,73035,4.0,114.4,13.7
UTAH,86833,2.3,39.9,8.9
VERMONT,70078,2.1,68.1,9.8
VIRGINIA,87249,2.7,219.0,9.9
WASHINGTON,90955,4.0,117.3,9.5
WEST VIRGINIA,55217,4.0,77.0,16.8
WISCONSIN,70996,2.9,108.0,10.3
WYOMING,72495,3.2,6.0,10.1
DISTRICT OF COLUMBIA,93547,5.0,11814.0,16.5
PUERTO RICO,22237,5.8,876.0,43.5
", stringsAsFactors = FALSE)

# Verify census data loaded
print("Census controls loaded:")
print(paste("Rows:", nrow(census_controls)))
head(census_controls)

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
  filter(!is.na(total_cases), !is.na(median_income)) %>%  # Remove if missing key data
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
    median_income_1000 = median_income / 1000,  # In thousands
    pop_density_100 = pop_density / 100         # Per 100 people/sq mi
  )

# Summary
print(paste("Final dataset rows:", nrow(final_data)))
summary(final_data)

# Save the data
write.csv(final_data, "mmr_vaccination_analysis_FINAL.csv", row.names = FALSE)

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

# Main scatterplot with party colors
p1 <- ggplot(final_data, aes(x = D_continuous, y = Y_rate)) +
  geom_point(aes(size = n_children, color = governor_party), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  labs(x = "MMR Vaccination Rate", 
       y = "MMR Disease Cases per 1000 Children",
       title = "Vaccination Rate vs MMR Disease