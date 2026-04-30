# =========================================================
# 03_ANALYSIS.R
# Purpose: Generate simple clinical summaries from ADaM datasets
# Inputs: ADSL, ADAE, ADLB
# Outputs: summary tables for reporting + Shiny
# =========================================================

# ---------------------------
# 1. PACKAGES
# ---------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(tidyverse)

# ---------------------------
# 2. LOAD ADAM DATA
# ---------------------------
adsl <- read.csv("data/processed/adsl.csv")
adae <- read.csv("data/processed/adae.csv")
adlb <- read.csv("data/processed/adlb.csv")

# =========================================================
# 3. POPULATION SUMMARY (ADSL)
# =========================================================

population_summary <- adsl %>%
  group_by(ARM) %>%
  summarise(
    subjects = n(),
    avg_age = mean(AGE, na.rm = TRUE)
  )

population_summary

# =========================================================
# 4. SAFETY ANALYSIS (ADAE)
# =========================================================

# AE count per treatment
ae_by_arm <- adae %>%
  group_by(ARM) %>%
  summarise(
    total_events = n(),
    patients_with_ae = n_distinct(USUBJID)
  )

ae_by_arm

# Serious events
ae_serious <- adae %>%
  group_by(ARM, SERIOUS_FLAG) %>%
  summarise(count = n(), .groups = "drop")

ae_serious

# Most common AE terms
top_ae_terms <- adae %>%
  count(AETERM, sort = TRUE) %>%
  head(10)

top_ae_terms

# =========================================================
# 5. LAB ANALYSIS (ADLB)
# =========================================================

# Mean change from baseline per treatment
lab_change <- adlb %>%
  group_by(ARM, LBTEST) %>%
  summarise(
    mean_change = mean(CHG, na.rm = TRUE),
    sd_change = sd(CHG, na.rm = TRUE),
    .groups = "drop"
  )

lab_change

# Extreme lab values
lab_outliers <- adlb %>%
  filter(
    CHG > quantile(CHG, 0.99, na.rm = TRUE) |
      CHG < quantile(CHG, 0.01, na.rm = TRUE)
  )

lab_outliers

# =========================================================
# 6. OUTPUTS FOR SHINY
# =========================================================

if (!dir.exists("data/outputs")) {
  dir.create("data/outputs", recursive = TRUE)
}

write.csv(population_summary, "data/outputs/population_summary.csv", row.names = FALSE)
write.csv(ae_by_arm, "data/outputs/ae_by_arm.csv", row.names = FALSE)
write.csv(ae_serious, "data/outputs/ae_serious.csv", row.names = FALSE)
write.csv(top_ae_terms, "data/outputs/top_ae_terms.csv", row.names = FALSE)
write.csv(lab_change, "data/outputs/lab_change.csv", row.names = FALSE)

# ---------------------------
# FINAL MESSAGE
# ---------------------------
cat("\nAnalysis completed: outputs created for Shiny dashboard\n")