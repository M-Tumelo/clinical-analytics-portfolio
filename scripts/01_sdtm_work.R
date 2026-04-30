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
# LOAD DATA
# ---------------------------
dm <- read_xpt("data/raw/dm.xpt")
ae <- read_xpt("data/raw/ae.xpt")
lb <- read_xpt("data/raw/lb.xpt")

# ---------------------------
# SUBJECT OVERVIEW
# ---------------------------
dm_n <- n_distinct(dm$USUBJID)
ae_n <- n_distinct(ae$USUBJID)
lb_n <- n_distinct(lb$USUBJID)

# ---------------------------
# CROSS-DOMAIN CHECK
# ---------------------------
ae_not_in_dm <- setdiff(unique(ae$USUBJID), unique(dm$USUBJID))
lb_not_in_dm <- setdiff(unique(lb$USUBJID), unique(dm$USUBJID))

ae_not_in_dm_n <- length(ae_not_in_dm)
lb_not_in_dm_n <- length(lb_not_in_dm)

# ---------------------------
# MISSING DATA
# ---------------------------
missing_dm <- sum(is.na(dm))
missing_ae <- sum(is.na(ae))
missing_lb <- sum(is.na(lb))

# ---------------------------
# DUPLICATES (DM ONLY)
# ---------------------------
dup_dm <- sum(duplicated(dm$USUBJID))

# ---------------------------
# AE SUMMARY
# ---------------------------
ae_severity <- as.data.frame(table(ae$AETOXGR, useNA = "ifany"))
colnames(ae_severity) <- c("AETOXGR", "count")

ae_serious <- as.data.frame(table(ae$AESER, useNA = "ifany"))
colnames(ae_serious) <- c("AESER", "count")

# ---------------------------
# LAB SUMMARY
# ---------------------------
lb_stats <- summary(lb$LBSTRESN)

lb_outliers <- lb %>%
  filter(
    LBSTRESN > quantile(LBSTRESN, 0.99, na.rm = TRUE) |
      LBSTRESN < quantile(LBSTRESN, 0.01, na.rm = TRUE)
  )

lb_outlier_n <- nrow(lb_outliers)

# ---------------------------
# ARM DISTRIBUTION
# ---------------------------
arm_summary <- as.data.frame(table(dm$ARM, useNA = "ifany"))
colnames(arm_summary) <- c("ARM", "count")

# ---------------------------
# QC STATUS
# ---------------------------
qc_status <- case_when(
  ae_not_in_dm_n == 0 & lb_not_in_dm_n == 0 ~ "PASS",
  ae_not_in_dm_n < 10 & lb_not_in_dm_n < 10 ~ "REVIEW",
  TRUE ~ "REVIEW REQUIRED"
)

# ---------------------------
# CREATE OUTPUT DIRECTORY
# ---------------------------
if (!dir.exists("data/outputs")) {
  dir.create("data/outputs", recursive = TRUE)
}

# ---------------------------
# FINAL QC SUMMARY TABLE
# ---------------------------
qc_summary <- tibble(
  dm_subjects = dm_n,
  ae_subjects = ae_n,
  lb_subjects = lb_n,
  ae_not_in_dm = ae_not_in_dm_n,
  lb_not_in_dm = lb_not_in_dm_n,
  missing_dm = missing_dm,
  missing_ae = missing_ae,
  missing_lb = missing_lb,
  duplicate_dm = dup_dm,
  lab_outliers = lb_outlier_n,
  qc_status = qc_status
)

# ---------------------------
# QC ISSUES TABLE
# ---------------------------
ae_issue_tbl <- tibble(
  issue = "AE not in DM",
  USUBJID = ae_not_in_dm
)

lb_issue_tbl <- tibble(
  issue = "LB not in DM",
  USUBJID = lb_not_in_dm
)

qc_issues <- bind_rows(ae_issue_tbl, lb_issue_tbl)

# ---------------------------
# SAVE OUTPUTS
# ---------------------------
write.csv(qc_summary, "data/outputs/sdtm_qc_summary.csv", row.names = FALSE)
write.csv(qc_issues, "data/outputs/sdtm_qc_issues.csv", row.names = FALSE)

write.csv(ae_severity, "data/outputs/ae_severity.csv", row.names = FALSE)
write.csv(ae_serious, "data/outputs/ae_serious.csv", row.names = FALSE)
write.csv(arm_summary, "data/outputs/arm_summary.csv", row.names = FALSE)
write.csv(lb_outliers, "data/outputs/lb_outliers.csv", row.names = FALSE)

# ---------------------------
# PRINT FINAL STATUS
# ---------------------------
cat("\nSDTM QC STATUS:", qc_status, "\n")