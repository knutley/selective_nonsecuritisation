# ============================================================================
# APPENDIX ROBUSTNESS CHECKS — rethresholded v3, REGION + YEAR FE
# Author: Katie Nutley
# Date: 23-09-2026; re-specified 07-10-2026 (region + year FE, self-contained)
# ============================================================================
#
# Self-contained: repeats the 01/02 data preparation, so it no longer needs
# objects from an earlier R session. Run from the repository root.
#
# Produces:
#   - Table 3   : police presence by country (descriptive, all 106,446 events)
#   - Table 14  : uncorrected logits on police-present events, all four
#                 partisan categories (the Heckman column comes from 02)
#   - Table 18  : within-county estimates (admin2 + year FE, admin2-clustered),
#                 with the main region + year model as the pooled column
#   - Table 19  : probit vs logit selection equation (region + year FE)
#
# Moved elsewhere: alternative instruments (02), counter-protest contrasts (01).
# ============================================================================

library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)

THRESHOLD <- 0.9
FE        <- "region + year_f"
dir.create("analysis/results", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# DATA (identical to 01 / 02)
# ============================================================================

raw <- read_csv("data/combined/acled_merged_controls_rethresholded_v3.csv",
                show_col_types = FALSE) %>%
  mutate(
    police_presence = as.integer(police_presence_prob >= THRESHOLD),
    counter_protest = as.integer(
      event_partisan_type_final %in% c("left_and_right", "centre_and_left", "centre_and_right")
    ),
    left_pure    = as.integer(event_partisan_type_final == "left"),
    right_pure   = as.integer(event_partisan_type_final == "right"),
    unknown_pure = as.integer(event_partisan_type_final == "unknown"),
    log_dist_govt_building = log1p(dist_govt_building_m),
    log_dist_major_road    = log1p(dist_major_road_m),
    region = paste(country, admin1, sep = " | "),
    year_f = factor(year)
  ) %>%
  group_by(country, event_date) %>%
  mutate(protest_load = n() - 1L) %>%
  ungroup() %>%
  mutate(log_protest_load = log1p(protest_load))

# ---- Table 3: presence by country (all events, before any restriction) -----
table3 <- raw %>%
  group_by(country) %>%
  summarise(total = n(), present = sum(police_presence),
            pct = round(100 * mean(police_presence), 2), .groups = "drop")
cat("\n=== TABLE 3: POLICE PRESENCE BY COUNTRY ===\n")
print(table3, n = Inf)

# ---- analysis sample: drop regions with no police-present event ------------
acled_data <- raw %>%
  group_by(region) %>%
  filter(sum(police_presence) > 0) %>%
  ungroup()
cat("\nAnalysis sample:", nrow(acled_data), "events (dropped",
    nrow(raw) - nrow(acled_data), ")\n")

partisan_vars <- c("left_pure", "right_pure", "unknown_pure", "counter_protest")
m1_rhs  <- c(partisan_vars, "incumbent_left", "incumbent_right",
             "log_dist_govt_building", "log_dist_major_road", "is_weekend")
sel_rhs <- c(m1_rhs, "protestor_violence", "log_protest_load")
out_rhs <- c(partisan_vars, "log_dist_govt_building", "log_dist_major_road",
             "is_weekend", "protestor_violence")

mk <- function(y, rhs, fe = FE) {
  as.formula(paste(y, "~", paste(rhs, collapse = " + "), if (!is.null(fe)) paste("|", fe)))
}

rl <- function(m, vc = NULL) {
  b <- coef(m); V <- if (is.null(vc)) vcov(m) else vc
  d  <- unname(b["right_pure"] - b["left_pure"])
  se <- sqrt(V["right_pure","right_pure"] + V["left_pure","left_pure"] -
               2 * V["right_pure","left_pure"])
  c(diff = d, se = se, OR = exp(d), chisq = (d / se)^2,
    p = pchisq((d / se)^2, 1, lower.tail = FALSE))
}

# ============================================================================
# TABLE 14: UNCORRECTED LOGITS ON POLICE-PRESENT EVENTS
# ============================================================================

pp <- filter(acled_data, police_presence == 1)

table14 <- bind_rows(lapply(c("arrest", "brutality"), function(dv) {
  m  <- feglm(mk(dv, out_rhs), data = pp, family = binomial("logit"), vcov = "hetero")
  ct <- coeftable(m)[partisan_vars, ]
  data.frame(Outcome = dv, Variable = c("Left", "Right", "Unknown", "Counter-Protest"),
             Logit_Est = round(ct[, "Estimate"], 3), Logit_p = round(ct[, "Pr(>|z|)"], 3),
             N = nobs(m), row.names = NULL)
}))
cat("\n=== TABLE 14: UNCORRECTED LOGIT (region + year FE, HC SEs) ===\n")
print(table14, row.names = FALSE)

# ============================================================================
# TABLE 18: POOLED (MAIN) vs WITHIN-COUNTY
# ============================================================================

pooled <- glm(mk("police_presence", m1_rhs, NULL) |> update(. ~ . + factor(region) + year_f),
              data = acled_data, family = binomial("logit"))
v_pooled <- vcovHC(pooled, type = "HC3")

ad <- filter(acled_data, !is.na(admin2))
within_year <- feglm(mk("police_presence", m1_rhs, "admin2 + year_f"), data = ad,
                     family = binomial("logit"), cluster = ~admin2)
# previous within-county specification (Covid dummy instead of year FE), for reference
ad$covid <- as.integer(ad$year %in% c(2020, 2021))
within_covid <- feglm(mk("police_presence", c(m1_rhs, "covid"), "admin2"), data = ad,
                      family = binomial("logit"), cluster = ~admin2)

coef_se <- function(b, V) data.frame(Variable = c("Left", "Right", "Unknown", "Counter-Protest"),
                                     Est = round(b[partisan_vars], 3),
                                     SE  = round(sqrt(diag(V))[partisan_vars], 3), row.names = NULL)
cat("\n=== TABLE 18: POOLED (region + year FE, HC3) ===\n")
print(coef_se(coef(pooled), v_pooled), row.names = FALSE)
cat("\n=== TABLE 18: WITHIN-COUNTY (admin2 + year FE, admin2-clustered) ===\n")
print(coef_se(coef(within_year), vcov(within_year)), row.names = FALSE)

kept_counties <- length(unique(ad$admin2[obs(within_year)]))
table18_contrast <- rbind(
  pooled            = rl(pooled, v_pooled),
  within_year       = rl(within_year),
  within_covid_prev = rl(within_covid))
cat("\nRight - left contrast:\n"); print(round(table18_contrast, 4))
cat("Pooled N:", nobs(pooled),
    "| within-county N:", nobs(within_year), "in", kept_counties, "counties",
    "| dropped:", nrow(ad) - nobs(within_year), "events in",
    length(unique(ad$admin2)) - kept_counties, "counties\n")

# ============================================================================
# TABLE 19: SELECTION EQUATION, PROBIT vs LOGIT (region + year FE)
# ============================================================================

p_sel <- feglm(mk("police_presence", sel_rhs), data = acled_data,
               family = binomial("probit"), vcov = "hetero")
l_sel <- feglm(mk("police_presence", sel_rhs), data = acled_data,
               family = binomial("logit"),  vcov = "hetero")

v19 <- c(partisan_vars, "protestor_violence", "log_protest_load")
table19 <- data.frame(
  Variable = v19,
  Logit    = round(coef(l_sel)[v19], 3),
  Probit   = round(coef(p_sel)[v19], 3),
  Ratio    = round(coef(l_sel)[v19] / coef(p_sel)[v19], 2),
  P_probit = round(coeftable(p_sel)[v19, "Pr(>|z|)"], 3),
  row.names = NULL)
cat("\n=== TABLE 19: PROBIT vs LOGIT SELECTION EQUATION ===\n")
print(table19, row.names = FALSE)
rl19 <- rbind(logit = rl(l_sel), probit = rl(p_sel))
cat("\nRight - left:\n"); print(round(rl19, 4))
cat("Logit/probit ratio of right - left:", round(rl19["logit", "diff"] / rl19["probit", "diff"], 2),
    "| N:", nobs(p_sel), "\n")

# ============================================================================
# SAVE
# ============================================================================

write.csv(table3,  "analysis/results/table3_presence_by_country.csv", row.names = FALSE)
write.csv(table14, "analysis/results/table14_uncorrected_logit.csv",  row.names = FALSE)
write.csv(table18_contrast, "analysis/results/table18_within_county_contrast.csv")
write.csv(table19, "analysis/results/table19_probit_vs_logit.csv",    row.names = FALSE)

cat("\n=== Appendix robustness complete ===\n")
