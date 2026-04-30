# =========================================================
# SDTM REVIEW SCRIPT
# Purpose: Load, inspect, and validate SDTM datasets
# Domains: DM, AE, LB
# =========================================================

# ---------------------------
# 1. PACKAGES
# ---------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}

# Load required packages
library(tidyverse)
library(haven)

# Set project root directory
setwd("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio")

# ---------------------------
# Load SDTM datasets (XPT format)
# ---------------------------
dm <- read_xpt("data/raw/dm.xpt")   # Demographics
ae <- read_xpt("data/raw/ae.xpt")   # Adverse Events
lb <- read_xpt("data/raw/lb.xpt")   # Laboratory results

# ---------------------------
# Subject-level identifiers
# ---------------------------
dm_ids <- unique(dm$USUBJID)
ae_ids <- unique(ae$USUBJID)
lb_ids <- unique(lb$USUBJID)

length(dm_ids)  # number of subjects in DM
length(ae_ids)  # subjects with adverse events
length(lb_ids)  # subjects with lab data

# Check for subjects appearing in AE/LB but not in DM
setdiff(ae_ids, dm_ids)
setdiff(lb_ids, dm_ids)

# ---------------------------
# Missing data overview
# ---------------------------
colSums(is.na(dm))
colSums(is.na(ae))
colSums(is.na(lb))

# ---------------------------
# Duplicate subject checks
# ---------------------------
anyDuplicated(dm$USUBJID)
anyDuplicated(ae$USUBJID)
anyDuplicated(lb$USUBJID)

# ---------------------------
# Safety signal overview (AE)
# ---------------------------
table(ae$AESER, useNA = "ifany")   # serious vs non-serious events
table(ae$AETOXGR, useNA = "ifany") # severity grading

# ---------------------------
# Lab data distribution check
# ---------------------------
summary(lb$LBSTRESN)

# Flag extreme lab values (basic outlier detection)
lb %>%
  filter(
    LBSTRESN > quantile(LBSTRESN, 0.99, na.rm = TRUE) |
      LBSTRESN < quantile(LBSTRESN, 0.01, na.rm = TRUE)
  )

# ---------------------------
# Treatment arm overview
# ---------------------------
table(dm$ARM, useNA = "ifany")

# End of SDTM review
cat("SDTM review completed\n")