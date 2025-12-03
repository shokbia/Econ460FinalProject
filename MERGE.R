install.packages("Hmisc")
library(Hmisc)

#Read the data
setwd("DSCI/Fall2025/ECON460/FinalProj")
source("NISPUF23.R")
ls()

############################################################
# STEP 1: Filter NNDSS for Pertussis
############################################################

# Option A: Use Pertussis (simpler - one disease)
nndss_filtered <- nndss_data %>%
  filter(Label == "Pertussis",
         Current.MMWR.Year == 2023) %>%
  mutate(STATE_CLEAN = toupper(trimws(Reporting.Area))) %>%
  filter(!STATE_CLEAN %in% c("US RESIDENTS", "U.S. RESIDENTS", "NEW ENGLAND", 
                             "MIDDLE ATLANTIC", "EAST NORTH CENTRAL", 
                             "WEST NORTH CENTRAL", "SOUTH ATLANTIC", 
                             "EAST SOUTH CENTRAL", "WEST SOUTH CENTRAL", 
                             "MOUNTAIN", "PACIFIC", "US TERRITORIES", 
                             "U.S. TERRITORIES", "NON-US RESIDENTS", 
                             "NON-U.S. RESIDENTS", "TOTAL"))

# Aggregate by state
nndss_state <- nndss_filtered %>%
  group_by(STATE_CLEAN) %>%
  summarise(
    total_cases = sum(ifelse(Current.week == "-", 0, as.numeric(Current.week)), na.rm = TRUE),
    weeks_reported = n()
  )

# CHECK: This should now have data
print(paste("NNDSS rows:", nrow(nndss_state)))
head(nndss_state)

############################################################
# STEP 2: Aggregate NIS to state level
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
  )

# Convert STATE to character for merging
nis_state <- nis_state %>%
  mutate(STATE = as.character(STATE))

############################################################
# STEP 3: Merge the datasets
############################################################

merged_data <- nis_state %>%
  left_join(nndss_state, by = c("STATE" = "STATE_CLEAN"))

# CHECK: Should have matching data now
print(paste("Merged rows:", nrow(merged_data)))
print(paste("States with case data:", sum(!is.na(merged_data$total_cases))))
head(merged_data, 10)

############################################################
# STEP 4: Create final analysis dataset
############################################################

final_data <- merged_data %>%
  filter(!is.na(total_cases)) %>%
  mutate(
    # Y: Disease outcome
    Y_binary = ifelse(total_cases > 0, 1, 0),
    Y_cases = total_cases,
    Y_rate = (total_cases / n_children) * 1000,
    
    # D: Treatment (vaccination)
    D_binary = ifelse(mmr_utd > 0.90, 1, 0),
    D_continuous = mmr_utd
  )

# Summary
print(paste("Final dataset rows:", nrow(final_data)))
summary(final_data)

# Save the data
write.csv(final_data, "pertussis_vaccination_analysis.csv", row.names = FALSE)

# Show top states by cases
final_data %>%
  arrange(desc(total_cases)) %>%
  select(STATE, total_cases, mmr_utd, n_children, Y_rate) %>%
  head(10)
# 1. OLS Regression (Question 8)
model_ols <- lm(Y_cases ~ D_continuous + pct_college + pct_poverty + 
                  pct_hispanic + pct_medicaid + pct_married, 
                data = final_data)
summary(model_ols)

# 2. Logistic Regression for binary outcome
model_logit <- glm(Y_binary ~ D_continuous + pct_college + pct_poverty + 
                     pct_hispanic + pct_medicaid + pct_married, 
                   data = final_data, family = binomial())
summary(model_logit)

# 3. Quick visualization
library(ggplot2)
ggplot(final_data, aes(x = D_continuous, y = Y_rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "MMR Vaccination Rate", 
       y = "Pertussis Cases per 1000 Children",
       title = "Vaccination Rate vs Pertussis Cases by State (2023)")


