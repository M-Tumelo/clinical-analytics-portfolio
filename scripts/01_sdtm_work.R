if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}

library(tidyverse)
library(haven)

setwd("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio")

dm <- read_xpt("data/raw/dm.xpt")
ae <- read_xpt("data/raw/ae.xpt")
lb <- read_xpt("data/raw/lb.xpt")

# ---------------------------
# SUBJECT OVERVIEW
# ---------------------------
dm_n <- n_distinct(dm$USUBJID)
ae_n <- n_distinct(ae$USUBJID)
lb_n <- n_distinct(lb$USUBJID)

subject_summary <- tibble(
  dataset = c("DM", "AE", "LB"),
  subjects = c(dm_n, ae_n, lb_n)
)

subject_summary

# ---------------------------
# CROSS-DOMAIN INTEGRITY
# ---------------------------
ae_not_in_dm <- setdiff(ae$USUBJID, dm$USUBJID)
lb_not_in_dm <- setdiff(lb$USUBJID, dm$USUBJID)

cross_domain_summary <- tibble(
  check = c("AE not in DM", "LB not in DM"),
  count = c(length(ae_not_in_dm), length(lb_not_in_dm)),
  status = case_when(
    length(ae_not_in_dm) == 0 & length(lb_not_in_dm) == 0 ~ "PASS",
    length(ae_not_in_dm) < 10 & length(lb_not_in_dm) < 10 ~ "REVIEW",
    TRUE ~ "FAIL"
  )
)

cross_domain_summary

# ---------------------------
# MISSING DATA CHECK
# ---------------------------
missing_summary <- tibble(
  dataset = c("DM", "AE", "LB"),
  missing_cells = c(sum(is.na(dm)), sum(is.na(ae)), sum(is.na(lb)))
)

missing_summary

# ---------------------------
# DUPLICATE CHECK
# ---------------------------
duplicate_summary <- tibble(
  dataset = c("DM", "AE", "LB"),
  duplicates = c(
    sum(duplicated(dm$USUBJID)),
    NA,  # AE expected to have repeats
    NA   # LB expected to have repeats
  )
)

duplicate_summary

# ---------------------------
# AE SAFETY PROFILE
# ---------------------------
ae_severity <- table(ae$AETOXGR, useNA = "ifany")
ae_serious <- table(ae$AESER, useNA = "ifany")

ae_summary <- list(
  severity = ae_severity,
  serious = ae_serious
)

ae_summary

# ---------------------------
# LAB DISTRIBUTION CHECK
# ---------------------------
lb_stats <- summary(lb$LBSTRESN)

lb_outliers <- lb %>%
  filter(
    LBSTRESN > quantile(LBSTRESN, 0.99, na.rm = TRUE) |
      LBSTRESN < quantile(LBSTRESN, 0.01, na.rm = TRUE)
  )

lab_summary <- list(
  stats = lb_stats,
  outliers = nrow(lb_outliers)
)

lab_summary

# ---------------------------
# TREATMENT ARM DISTRIBUTION
# ---------------------------
arm_summary <- table(dm$ARM, useNA = "ifany")
arm_summary

# ---------------------------
# FINAL QC STATUS
# ---------------------------
qc_status <- case_when(
  length(ae_not_in_dm) == 0 & length(lb_not_in_dm) == 0 ~ "PASS",
  length(ae_not_in_dm) < 10 & length(lb_not_in_dm) < 10 ~ "REVIEW",
  TRUE ~ "FAIL"
)

cat("\nSDTM QC STATUS:", qc_status, "\n")
