# =========================================================
# RBQM SIGNAL ENGINE
# Purpose: Detect safety + lab signals for MDR
# =========================================================

# ---------------------------
# 1. PACKAGES
# ---------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

library(tidyverse)

# ---------------------------
# 2. LOAD DATA
# ---------------------------
setwd("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio")

adsl <- read.csv("data/processed/adsl.csv")
adae <- read.csv("data/processed/adae.csv")
adlb <- read.csv("data/processed/adlb.csv")

# ---------------------------
# HELPER FUNCTION
# ---------------------------
flag_signal <- function(value, threshold_low, threshold_high) {
  case_when(
    value >= threshold_high ~ "HIGH",
    value >= threshold_low ~ "MEDIUM",
    TRUE ~ "LOW"
  )
}

# =========================================================
# SAFETY SIGNALS (ADAE)
# =========================================================

# ---------------------------
# 1. AE RATE PER ARM
# ---------------------------
ae_rate <- adae %>%
  group_by(ARM) %>%
  summarise(
    total_ae = n(),
    subjects = n_distinct(USUBJID),
    ae_rate = total_ae / subjects
  )

max_rate <- max(ae_rate$ae_rate)
min_rate <- min(ae_rate$ae_rate)

ae_imbalance_ratio <- max_rate / min_rate

ae_imbalance_signal <- tibble(
  signal = "AE Imbalance",
  value = round(ae_imbalance_ratio, 2),
  threshold = "1.5",
  severity = flag_signal(ae_imbalance_ratio, 1.2, 1.5)
)

# ---------------------------
# 2. SERIOUS AE RATE
# ---------------------------
serious_rate <- adae %>%
  filter(AESER == "Y") %>%
  summarise(rate = n() / n_distinct(USUBJID)) %>%
  pull(rate)

serious_signal <- tibble(
  signal = "Serious AE Rate",
  value = round(serious_rate, 3),
  threshold = "0.10",
  severity = flag_signal(serious_rate, 0.05, 0.10)
)

# ---------------------------
# 3. HIGH TOXICITY (GRADE 3+)
# ---------------------------
tox_rate <- adae %>%
  filter(AETOXGR >= 3) %>%
  summarise(rate = n() / n_distinct(USUBJID)) %>%
  pull(rate)

tox_signal <- tibble(
  signal = "High Toxicity Rate",
  value = round(tox_rate, 3),
  threshold = "0.05",
  severity = flag_signal(tox_rate, 0.03, 0.05)
)

# ---------------------------
# 4. TOP AE TERM IMBALANCE
# ---------------------------
top_terms <- adae %>%
  count(AEDECOD, ARM) %>%
  group_by(AEDECOD) %>%
  mutate(ratio = max(n) / min(n + 1)) %>%
  summarise(max_ratio = max(ratio)) %>%
  arrange(desc(max_ratio))

term_signal <- tibble(
  signal = "AE Term Imbalance",
  value = round(top_terms$max_ratio[1], 2),
  threshold = "2.0",
  severity = flag_signal(top_terms$max_ratio[1], 1.5, 2.0)
)

# =========================================================
# LAB SIGNALS (ADLB)
# =========================================================

# ---------------------------
# 5. MEAN CHANGE BY ARM
# ---------------------------
lab_mean <- adlb %>%
  group_by(ARM) %>%
  summarise(mean_change = mean(CHG, na.rm = TRUE))

lab_range <- max(lab_mean$mean_change) - min(lab_mean$mean_change)

lab_mean_signal <- tibble(
  signal = "Lab Mean Difference",
  value = round(lab_range, 2),
  threshold = "20%",
  severity = flag_signal(abs(lab_range), 10, 20)
)

# ---------------------------
# 6. OUTLIER RATE
# ---------------------------
upper <- quantile(adlb$CHG, 0.99, na.rm = TRUE)
lower <- quantile(adlb$CHG, 0.01, na.rm = TRUE)

outliers <- adlb %>%
  filter(CHG > upper | CHG < lower)

outlier_rate <- nrow(outliers) / nrow(adlb)

lab_outlier_signal <- tibble(
  signal = "Lab Outliers",
  value = round(outlier_rate, 3),
  threshold = "0.05",
  severity = flag_signal(outlier_rate, 0.03, 0.05)
)

# ---------------------------
# 7. SHIFT ANALYSIS (simple proxy)
# ---------------------------
shift_rate <- adlb %>%
  filter(CHG > mean(CHG, na.rm = TRUE) + 2 * sd(CHG, na.rm = TRUE)) %>%
  summarise(rate = n() / nrow(adlb)) %>%
  pull(rate)

shift_signal <- tibble(
  signal = "Lab Shift",
  value = round(shift_rate, 3),
  threshold = "0.05",
  severity = flag_signal(shift_rate, 0.03, 0.05)
)

# =========================================================
# COMBINE SIGNALS
# =========================================================
signal_summary <- bind_rows(
  ae_imbalance_signal,
  serious_signal,
  tox_signal,
  term_signal,
  lab_mean_signal,
  lab_outlier_signal,
  shift_signal
)

# ---------------------------
# ADD STATUS FLAG
# ---------------------------
signal_summary <- signal_summary %>%
  mutate(
    status = case_when(
      severity == "HIGH" ~ "🔴",
      severity == "MEDIUM" ~ "🟡",
      TRUE ~ "🟢"
    )
  )

# ---------------------------
# SAVE OUTPUT
# ---------------------------
write.csv(signal_summary, "data/outputs/signal_summary.csv", row.names = FALSE)

cat("Signal engine completed\n")