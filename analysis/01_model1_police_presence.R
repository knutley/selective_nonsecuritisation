# ============================================================================
# MODEL 1: POLICE PRESENCE (LOGIT) — rethresholded v3
# Author: Katie Nutley
# Date: 23-09-2026
# ============================================================================
#
# Script 1 of 2. Script 2 (02_heckman_severity.R) estimates the severity
# models and shares this specification, plus protestor violence and the
# exclusion restriction.
#
# SPECIFICATION (final)
#   DV        : police presence at a 0.9 classifier threshold (see THRESHOLD
#               below; 4,213 positives of 106,446 events).
#   Partisan  : left / right / unknown / counter-protest, CENTRE as reference.
#   Controls  : country FE, incumbent partisanship, logged distance to the
#               nearest government building and major road, Covid period.
#   Excluded  : crowd size (ACLED recoded no-size events into a default band
#               mid-period, so the measure is not comparable across years);
#               urban/rural typology and police-station proximity (mediators
#               of partisanship — where a protest happens is a partisan
#               choice). All three are reported as robustness specifications
#               below.
#   Weekend   : RETAINED. Right-coded protests are far more weekend-
#               concentrated than left-coded ones (36.7 vs 25.7 per cent), and
#               weekend predicts arrest in the severity models (p < .001), so
#               it is kept as Earl's timing control even though its own
#               coefficient in the presence model is marginal.
#
# THRESHOLD NOTE — read before running.
#   This script uses the 0.9 threshold, which is what the current draft of the
#   paper reports. An earlier version of this script used police_presence_08.
#   Set THRESHOLD <- 0.8 below to reproduce that; every number downstream
#   changes, including everything in Sections 7 and 8 of the manuscript.
#   Do not mix the two.
#
# ============================================================================

library(tidyverse)
library(sandwich)
library(lmtest)
library(car)

THRESHOLD <- 0.9

# ============================================================================
# DATA
# ============================================================================

acled_data <- read_csv(
  "~/Documents/GitHub/Police_Response/2020-2024/acled_merged_controls_rethresholded_v3.csv",
  show_col_types = FALSE
)

cat("Rows loaded:", nrow(acled_data), "\n")
cat("\nevent_partisan_type_final values present — check before trusting the dummies:\n")
print(table(acled_data$event_partisan_type_final, useNA = "always"))

acled_data <- acled_data %>%
  mutate(
    police_presence = as.integer(police_presence_prob >= THRESHOLD),
    counter_protest = as.integer(
      event_partisan_type_final %in% c("left_and_right", "centre_and_left", "centre_and_right")
    ),
    left_pure    = as.integer(event_partisan_type_final == "left"),
    right_pure   = as.integer(event_partisan_type_final == "right"),
    unknown_pure = as.integer(event_partisan_type_final == "unknown"),
    # centre omitted as the reference category
    covid        = as.integer(year %in% c(2020, 2021)),
    log_dist_govt_building  = log1p(dist_govt_building_m),
    log_dist_major_road     = log1p(dist_major_road_m),
    log_dist_police_station = log1p(dist_police_station_m)
  )

# A label mismatch would silently zero out counter_protest — fail loudly instead
stopifnot(sum(acled_data$counter_protest) > 0)

cat("\nThreshold:", THRESHOLD, "| police-present events:", sum(acled_data$police_presence), "\n")
cat("Missing — weekend:", sum(is.na(acled_data$is_weekend)), "| govt distance:", sum(is.na(acled_data$log_dist_govt_building)),
    "| road distance:", sum(is.na(acled_data$log_dist_major_road)),
    "| admin2:", sum(is.na(acled_data$admin2)), "\n")

partisan_vars <- c("left_pure", "right_pure", "unknown_pure", "counter_protest")

# ============================================================================
# DESCRIPTIVE: POLICE PRESENCE RATE BY PARTISAN TYPE
# ============================================================================

presence_desc <- acled_data %>%
  group_by(event_partisan_type_final) %>%
  summarise(n_events = n(),
            n_police = sum(police_presence),
            pct_police = round(mean(police_presence) * 100, 2),
            .groups = "drop") %>%
  arrange(desc(pct_police))
cat("\n=== PRESENCE RATE BY PARTISAN TYPE ===\n")
print(presence_desc)

# ============================================================================
# BASELINE MODEL
# ============================================================================

model1 <- glm(
  police_presence ~
    left_pure + right_pure + unknown_pure + counter_protest +
    incumbent_left + incumbent_right +
    country +
    log_dist_govt_building + log_dist_major_road +
    covid + is_weekend,
  data = acled_data, family = binomial(link = "logit")
)

vcov_hc3 <- vcovHC(model1, type = "HC3")

cat("\n=== MODEL 1 (BASELINE), HC3 ===\n")
print(coeftest(model1, vcov = vcov_hc3))
cat("\nObservations used:", nobs(model1),
    "| dropped (listwise):", nrow(acled_data) - nobs(model1), "\n")

# ============================================================================
# TABLE 8 SOURCE: ODDS RATIOS AND CIs
# ============================================================================

ct <- coeftest(model1, vcov = vcov_hc3)[partisan_vars, ]
results_or <- data.frame(
  Variable    = c("Left", "Right", "Unknown", "Counter-Protest"),
  Coefficient = round(ct[, "Estimate"], 4),
  Std_Error   = round(ct[, "Std. Error"], 4),
  Odds_Ratio  = round(exp(ct[, "Estimate"]), 3),
  CI_Lower    = round(exp(ct[, "Estimate"] - 1.96 * ct[, "Std. Error"]), 3),
  CI_Upper    = round(exp(ct[, "Estimate"] + 1.96 * ct[, "Std. Error"]), 3),
  P_Value     = round(ct[, "Pr(>|z|)"], 4),
  row.names   = NULL
)
cat("\n=== TABLE 8 SOURCE: ODDS RATIOS ===\n")
print(results_or, row.names = FALSE)

# ============================================================================
# HYPOTHESIS TESTS
# ============================================================================

cat("\n=== H2: LEFT vs RIGHT (direct contrast, HC3) ===\n")
print(linearHypothesis(model1, "right_pure - left_pure = 0", vcov. = vcov_hc3))

diff_lr <- coef(model1)["right_pure"] - coef(model1)["left_pure"]
se_lr <- sqrt(vcov_hc3["right_pure", "right_pure"] + vcov_hc3["left_pure", "left_pure"] -
                2 * vcov_hc3["right_pure", "left_pure"])
cat("Right - left:", round(diff_lr, 4), "| SE:", round(se_lr, 4),
    "| OR:", round(exp(diff_lr), 3),
    "| one-tailed p (right < left):", round(pnorm(diff_lr / se_lr), 5), "\n")

cat("\n=== H2: each pole against the centrist reference (one-tailed) ===\n")
for (v in c("left_pure", "right_pure")) {
  z <- coef(model1)[v] / sqrt(vcov_hc3[v, v])
  cat(sprintf("  %-11s z = %6.3f | one-tailed p (greater than centre) = %.4f\n",
              v, z, pnorm(z, lower.tail = FALSE)))
}

cat("\n=== H4: counter-protest vs single-partisan events ===\n")
print(linearHypothesis(model1,
  c("counter_protest - left_pure = 0", "counter_protest - right_pure = 0"),
  vcov. = vcov_hc3))

# ============================================================================
# CLASSIFIED-ONLY SUBSAMPLE
# ============================================================================
# Unknown-ideology events are the residue of the classification pipeline
# rather than a substantive category. This drops them entirely.

classified <- filter(acled_data, event_partisan_type_final != "unknown")
model1_classified <- update(model1, . ~ . - unknown_pure, data = classified)
v_cls <- vcovHC(model1_classified, type = "HC3")

cat("\n=== CLASSIFIED-ONLY SUBSAMPLE (unknown dropped) ===\n")
cat("N:", nobs(model1_classified), "\n")
print(coeftest(model1_classified, vcov = v_cls)[c("left_pure", "right_pure", "counter_protest"), ])
print(linearHypothesis(model1_classified, "right_pure - left_pure = 0", vcov. = v_cls))

# ============================================================================
# ADMIN2-CLUSTERED STANDARD ERRORS (Table 13)
# ============================================================================
# vcovCL cannot handle NAs in the cluster variable, so the clustered model is
# fitted on admin2-complete rows. Coefficients are unaffected.

acled_admin2 <- filter(acled_data, !is.na(admin2))
model1_cl <- update(model1, data = acled_admin2)
vcov_cl   <- vcovCL(model1_cl, cluster = ~admin2, type = "HC1")

cat("\n=== TABLE 13: HC3 vs ADMIN2-CLUSTERED ===\n")
cat("Clusters:", n_distinct(acled_admin2$admin2),
    "| N:", nobs(model1_cl),
    "| dropped for missing admin2:", nrow(acled_data) - nrow(acled_admin2), "\n")

ct_cl <- coeftest(model1_cl, vcov = vcov_cl)[partisan_vars, ]
table13 <- data.frame(
  Variable  = c("Left", "Right", "Unknown", "Counter-Protest"),
  Est_HC3   = round(ct[, "Estimate"], 4),
  SE_HC3    = round(ct[, "Std. Error"], 4),
  p_HC3     = round(ct[, "Pr(>|z|)"], 4),
  Est_Clust = round(ct_cl[, "Estimate"], 4),
  SE_Clust  = round(ct_cl[, "Std. Error"], 4),
  p_Clust   = round(ct_cl[, "Pr(>|z|)"], 4),
  row.names = NULL
)
print(table13, row.names = FALSE)

cat("\nLeft vs right, clustered:\n")
print(linearHypothesis(model1_cl, "right_pure - left_pure = 0", vcov. = vcov_cl))
cat("\nH4, clustered:\n")
print(linearHypothesis(model1_cl,
  c("counter_protest - left_pure = 0", "counter_protest - right_pure = 0"), vcov. = vcov_cl))

# ============================================================================
# ROBUSTNESS: PARTISAN COEFFICIENTS ACROSS SPECIFICATIONS
# ============================================================================
# Each specification adds one excluded variable back to the baseline.

extract_partisan <- function(model, vcov, label) {
  coefs <- coef(model)[partisan_vars]
  ses   <- sqrt(diag(vcov))[partisan_vars]
  data.frame(Model = label, Variable = partisan_vars,
             Estimate = round(coefs, 4), SE = round(ses, 4),
             p_value = round(2 * pnorm(-abs(coefs / ses)), 4),
             N = nobs(model), row.names = NULL)
}

specs <- list(Baseline = model1)
add_spec <- function(label, term) {
  if (!term %in% names(acled_data) || all(is.na(acled_data[[term]]))) {
    warning(term, " missing or entirely NA — skipping '", label, "'.")
    return(invisible(NULL))
  }
  specs[[label]] <<- update(model1, as.formula(paste(". ~ . +", term)))
}
add_spec("+ Protestor violence",   "protestor_violence")
add_spec("+ Police station dist.", "log_dist_police_station")
add_spec("+ Urban/rural",          "urb_rur")
add_spec("+ Logged crowd size",    "log_crowd_size")

comparison <- bind_rows(lapply(names(specs), function(l)
  extract_partisan(specs[[l]], vcovHC(specs[[l]], type = "HC3"), l)))

cat("\n=== PARTISAN COEFFICIENTS ACROSS SPECIFICATIONS ===\n")
print(comparison, row.names = FALSE)

cat("\nRight-left contrast under each specification:\n")
for (l in names(specs)) {
  m <- specs[[l]]; v <- vcovHC(m, type = "HC3")
  d <- coef(m)["right_pure"] - coef(m)["left_pure"]
  s <- sqrt(v["right_pure","right_pure"] + v["left_pure","left_pure"] - 2*v["right_pure","left_pure"])
  cat(sprintf("  %-24s diff = %+.4f  p = %.5f\n", l, d, 2 * pnorm(-abs(d / s))))
}

# ============================================================================
# DIAGNOSTICS
# ============================================================================

cat("\n=== FIT ===\n")
cat("Null deviance:    ", round(model1$null.deviance, 1), "\n")
cat("Residual deviance:", round(model1$deviance, 1), "\n")
cat("AIC:              ", round(AIC(model1), 1), "\n")

# ============================================================================
# PREDICTED PROBABILITIES BY PARTISAN TYPE
# ============================================================================
# Held at the modal country, centre-aligned incumbency, non-Covid period and
# median distances.

modal_country <- names(sort(table(acled_data$country), decreasing = TRUE))[1]

pred_data <- data.frame(
  left_pure       = c(1, 0, 0, 0, 0),
  right_pure      = c(0, 1, 0, 0, 0),
  unknown_pure    = c(0, 0, 1, 0, 0),
  counter_protest = c(0, 0, 0, 1, 0),
  incumbent_left  = 0L,
  incumbent_right = 0L,
  country         = modal_country,
  log_dist_govt_building = median(acled_data$log_dist_govt_building, na.rm = TRUE),
  log_dist_major_road    = median(acled_data$log_dist_major_road,    na.rm = TRUE),
  covid           = 0L,
  is_weekend      = as.integer(names(sort(table(acled_data$is_weekend), decreasing = TRUE))[1])
)

pred_summary <- data.frame(
  Partisan_Type  = c("Left", "Right", "Unknown", "Counter-Protest", "Centre (reference)"),
  Predicted_Pct  = round(predict(model1, newdata = pred_data, type = "response") * 100, 2),
  N_Events = c(sum(acled_data$left_pure), sum(acled_data$right_pure),
               sum(acled_data$unknown_pure), sum(acled_data$counter_protest),
               sum(acled_data$event_partisan_type_final == "centre"))
)
cat("\n=== PREDICTED PROBABILITIES (modal country:", modal_country, ") ===\n")
print(pred_summary, row.names = FALSE)

# ============================================================================
# SAVE
# ============================================================================

write.csv(presence_desc, "results/model1_presence_by_partisan_type.csv", row.names = FALSE)
write.csv(results_or,    "results/table8_odds_ratios.csv",               row.names = FALSE)
write.csv(comparison,    "results/model1_partisan_coefs_across_specs.csv", row.names = FALSE)
write.csv(table13,       "results/table13_admin2_clustered.csv",         row.names = FALSE)
write.csv(pred_summary,  "results/model1_predicted_probabilities.csv",   row.names = FALSE)
saveRDS(model1,            "models/model1_baseline.rds")
saveRDS(model1_classified, "models/model1_classified_only.rds")
saveRDS(model1_cl,         "models/model1_admin2_clean.rds")

cat("\n=== Model 1 complete ===\n")
