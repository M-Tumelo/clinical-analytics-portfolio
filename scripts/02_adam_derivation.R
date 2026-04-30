# ---------------------------
# PACKAGES
# ---------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}

library(tidyverse)
library(haven)

# ---------------------------
# SET WORKING DIRECTORY
# ---------------------------
setwd("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio")

# ---------------------------
# LOAD SDTM DATA
# ---------------------------
dm <- read_xpt("data/raw/dm.xpt")
ae <- read_xpt("data/raw/ae.xpt")
lb <- read_xpt("data/raw/lb.xpt")

# =========================================================
# ADSL - SUBJECT LEVEL DATASET
# =========================================================

adsl <- dm %>%
  select(STUDYID, USUBJID, AGE, SEX, ARM)

# AE flag
ae_flag <- ae %>%
  distinct(USUBJID) %>%
  mutate(AEFL = "Y")

# AE count
ae_count <- ae %>%
  group_by(USUBJID) %>%
  summarise(AE_COUNT = n(), .groups = "drop")

# LB flag
lb_flag <- lb %>%
  distinct(USUBJID) %>%
  mutate(LBFL = "Y")

# Merge into ADSL
adsl <- adsl %>%
  left_join(ae_flag, by = "USUBJID") %>%
  left_join(ae_count, by = "USUBJID") %>%
  left_join(lb_flag, by = "USUBJID") %>%
  mutate(
    AEFL = ifelse(is.na(AEFL), "N", AEFL),
    LBFL = ifelse(is.na(LBFL), "N", LBFL),
    AE_COUNT = ifelse(is.na(AE_COUNT), 0, AE_COUNT),
    SAFFL = ifelse(AEFL == "Y", "Y", "N")
  )

# =========================================================
# ADAE - ADVERSE EVENTS ANALYSIS DATASET
# =========================================================

adae <- ae %>%
  left_join(adsl %>% select(USUBJID, ARM, SAFFL), by = "USUBJID") %>%
  mutate(
    AESER = as.character(AESER),
    AETOXGR = as.numeric(AETOXGR),
    AE_SEV = case_when(
      AETOXGR == 1 ~ "MILD",
      AETOXGR == 2 ~ "MODERATE",
      AETOXGR == 3 ~ "SEVERE",
      TRUE ~ "UNKNOWN"
    ),
    SERIOUS_FLAG = ifelse(AESER == "Y", "Y", "N")
  )

# =========================================================
# ADLB - LAB ANALYSIS DATASET
# =========================================================

# Baseline = first available value per subject per test
baseline_lb <- lb %>%
  group_by(USUBJID, LBTEST) %>%
  arrange(LBDY) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, LBTEST, BASE = LBSTRESN)

# Merge baseline back
adlb <- lb %>%
  left_join(baseline_lb, by = c("USUBJID", "LBTEST")) %>%
  left_join(adsl %>% select(USUBJID, ARM), by = "USUBJID") %>%
  mutate(
    CHG = LBSTRESN - BASE
  )

# =========================================================
# SAVE OUTPUTS
# =========================================================

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

write.csv(adsl, "data/processed/adsl.csv", row.names = FALSE)
write.csv(adae, "data/processed/adae.csv", row.names = FALSE)
write.csv(adlb, "data/processed/adlb.csv", row.names = FALSE)

# ---------------------------
# FINAL MESSAGE
# ---------------------------
cat("\nADaM datasets created:\n")
cat("- ADSL (subject-level)\n")
cat("- ADAE (adverse events)\n")
cat("- ADLB (lab analysis)\n")