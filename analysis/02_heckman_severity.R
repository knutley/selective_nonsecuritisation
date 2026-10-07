# ============================================================================
# MODEL 2: RESPONSE SEVERITY (HECKMAN TWO-STEP) — rethresholded v3
# Author: Katie Nutley
# Date: 23-09-2026
# ============================================================================
#
# Script 2 of 2. Run 01_model1_police_presence.R first; this script repeats
# the data preparation so it can also be run on its own.
#
# STRUCTURE
#   Selection equation : police presence across all 106,446 events
#                        (Model 1 specification + protestor violence + the
#                        exclusion restriction).
#   Outcome equations  : arrest and brutality, observed only where police were
#                        present. Estimated separately because the two are not
#                        mutually exclusive. Incumbent partisanship is in the
#                        selection equation ONLY: it addresses a confound in
#                        deployment, and the framework (H1/H3) gives no reason
#                        to expect national incumbency to shape officer
#                        conduct conditional on presence.
#
# EXCLUSION RESTRICTION: same-day protest load — the logged count of other
#   protest events in the same country on the same date. Competing demand for
#   finite public-order resources shapes whether a unit can be spared, but not
#   how the officers who do attend behave. Presence falls from 5.9 per cent of
#   events on the quietest quartile of days to 2.6 per cent on the busiest.
#   It replaces n_police_stations_5km, which failed the sensitivity check for
#   arrest (b = +0.002, p = 0.003) — station density also measures custody
#   capacity, which reaches arrest directly rather than only through
#   deployment. Both alternatives are re-estimated in the robustness block.
#
# BRUTALITY CAVEAT (carried forward from the classifier notes): brutality is
#   the least well-calibrated of the three labels. Roughly 45 per cent of its
#   0.5-threshold positives sit below 0.8, driven by a wrong-actor confound
#   (violence attributed to police that was committed by managers, civilians
#   or other protesters), and the classifier saw only 44 hand-coded positives
#   in training. Treat the brutality models with corresponding caution.
#
# ============================================================================

library(tidyverse)
library(sandwich)
library(lmtest)
library(sampleSelection)
library(boot)
library(car)

THRESHOLD <- 0.9   # must match 01_model1_police_presence.R
R_BOOT    <- 500   # 200 for a quick check, 500 for final results

# ============================================================================
# DATA
# ============================================================================

acled_data <- read_csv(
  "~/Documents/GitHub/Police_Response/2020-2024/acled_merged_controls_rethresholded_v3.csv",
  show_col_types = FALSE
)

required <- c("police_presence_prob", "event_partisan_type_final", "arrest", "brutality",
              "protestor_violence", "n_police_stations_5km", "dist_police_station_m",
              "event_date", "country", "is_weekend", "incumbent_left", "incumbent_right",
              "dist_govt_building_m", "dist_major_road_m", "year")
missing_vars <- setdiff(required, names(acled_data))
if (length(missing_vars) > 0) stop("Missing from v3 file: ", paste(missing_vars, collapse = ", "))

acled_data <- acled_data %>%
  mutate(
    police_presence = as.integer(police_presence_prob >= THRESHOLD),
    counter_protest = as.integer(
      event_partisan_type_final %in% c("left_and_right", "centre_and_left", "centre_and_right")
    ),
    left_pure    = as.integer(event_partisan_type_final == "left"),
    right_pure   = as.integer(event_partisan_type_final == "right"),
    unknown_pure = as.integer(event_partisan_type_final == "unknown"),
    covid        = as.integer(year %in% c(2020, 2021)),
    log_dist_govt_building  = log1p(dist_govt_building_m),
    log_dist_major_road     = log1p(dist_major_road_m),
    log_dist_police_station = log1p(dist_police_station_m)
  ) %>%
  # ---- exclusion restriction: same-day protest load -------------------------
  group_by(country, event_date) %>%
  mutate(protest_load = n() - 1L) %>%
  ungroup() %>%
  mutate(log_protest_load = log1p(protest_load))

stopifnot(sum(acled_data$counter_protest) > 0)

partisan_vars <- c("left_pure", "right_pure", "unknown_pure", "counter_protest")

cat("Events:", nrow(acled_data), "| police-present:", sum(acled_data$police_presence), "\n")
cat("Arrests among police-present:", sum(acled_data$arrest[acled_data$police_presence == 1], na.rm = TRUE),
    "| brutality:", sum(acled_data$brutality[acled_data$police_presence == 1], na.rm = TRUE), "\n")
cat("Protest load — mean:", round(mean(acled_data$protest_load), 1),
    "| median:", median(acled_data$protest_load), "| max:", max(acled_data$protest_load), "\n")

cat("\nPresence rate by quartile of same-day protest load:\n")
acled_data %>%
  mutate(load_q = ntile(protest_load, 4)) %>%
  group_by(load_q) %>%
  summarise(events = n(), presence_rate = round(mean(police_presence) * 100, 2), .groups = "drop") %>%
  print()

# ============================================================================
# DESCRIPTIVE: SEVERITY AMONG POLICE-PRESENT EVENTS (Tables 9 and 10)
# ============================================================================

severity_desc <- acled_data %>%
  filter(police_presence == 1) %>%
  mutate(partisan_group = case_when(
    event_partisan_type_final == "centre" ~ "Centre",
    left_pure == 1 ~ "Left", right_pure == 1 ~ "Right",
    unknown_pure == 1 ~ "Unknown", counter_protest == 1 ~ "Counter-Protest")) %>%
  group_by(partisan_group) %>%
  summarise(N = n(),
            Arrests = sum(arrest, na.rm = TRUE),
            Arrest_Rate = round(mean(arrest, na.rm = TRUE) * 100, 2),
            Brutal = sum(brutality, na.rm = TRUE),
            Brutal_Rate = round(mean(brutality, na.rm = TRUE) * 100, 2),
            .groups = "drop")
cat("\n=== SEVERITY DESCRIPTIVES ===\n")
print(severity_desc)

# ============================================================================
# FORMULAS — single source of truth
# ============================================================================

sel_formula <- police_presence ~
  left_pure + right_pure + unknown_pure + counter_protest +
  incumbent_left + incumbent_right +
  country + log_dist_govt_building + log_dist_major_road + covid + is_weekend +
  protestor_violence +
  log_protest_load

arr_formula <- arrest ~
  left_pure + right_pure + unknown_pure + counter_protest +
  country + log_dist_govt_building + log_dist_major_road + covid + is_weekend +
  protestor_violence

brut_formula <- brutality ~
  left_pure + right_pure + unknown_pure + counter_protest +
  country + log_dist_govt_building + log_dist_major_road + covid + is_weekend +
  protestor_violence

cat("\nListwise-deleted observations:",
    nrow(acled_data) - nrow(na.omit(acled_data[, all.vars(sel_formula)])), "\n")

# ============================================================================
# INSTRUMENT STRENGTH
# ============================================================================

first_stage <- glm(sel_formula, data = acled_data, family = binomial(link = "probit"))

cat("\n=== INSTRUMENT STRENGTH: same-day protest load ===\n")
print(linearHypothesis(first_stage, "log_protest_load = 0"))
print(summary(first_stage)$coefficients["log_protest_load", ])
cat("LR chi-sq:", round(2 * (logLik(first_stage) -
      logLik(update(first_stage, . ~ . - log_protest_load))), 2), "on 1 df\n")

# ============================================================================
# EXCLUSION-RESTRICTION SENSITIVITY CHECKS
# ============================================================================
# A valid instrument should return null when entered directly into the outcome
# equations on the police-present subsample.

present_data <- filter(acled_data, police_presence == 1)

exclusion_check <- function(candidate) {
  bind_rows(lapply(c("arrest", "brutality"), function(dv) {
    f <- as.formula(paste(dv, "~ left_pure + right_pure + unknown_pure + counter_protest +",
                          "country + log_dist_govt_building + log_dist_major_road + covid +",
                          "is_weekend + protestor_violence +", candidate))
    m  <- lm(f, data = present_data)
    ct <- coeftest(m, vcov = vcovHC(m, type = "HC3"))
    data.frame(Instrument = candidate, Outcome = dv,
               Estimate = round(ct[candidate, "Estimate"], 5),
               p_value  = round(ct[candidate, "Pr(>|t|)"], 4), row.names = NULL)
  }))
}

exclusion_results <- bind_rows(lapply(
  c("log_protest_load", "n_police_stations_5km", "log_dist_police_station"), exclusion_check))
cat("\n=== EXCLUSION SENSITIVITY (null = passes) ===\n")
print(exclusion_results, row.names = FALSE)

# ============================================================================
# HECKMAN MODELS
# ============================================================================

cat("\n=== HECKMAN: ARREST ===\n")
heck_arrest <- heckit(selection = sel_formula, outcome = arr_formula,
                      data = acled_data, method = "2step")
print(summary(heck_arrest))

cat("\n=== HECKMAN: BRUTALITY ===\n")
heck_brutality <- heckit(selection = sel_formula, outcome = brut_formula,
                         data = acled_data, method = "2step")
print(summary(heck_brutality))

cat("\n=== IMR (model-based; bootstrapped p below) ===\n")
cat("Arrest    — rho:", round(heck_arrest$rho, 4), "| IMR p:",
    round(summary(heck_arrest)$estimate["invMillsRatio", "Pr(>|t|)"], 4), "\n")
cat("Brutality — rho:", round(heck_brutality$rho, 4), "| IMR p:",
    round(summary(heck_brutality)$estimate["invMillsRatio", "Pr(>|t|)"], 4), "\n")

# ============================================================================
# BOOTSTRAPPED STANDARD ERRORS
# ============================================================================
# heckit's model-based vcov is not compatible with heteroskedasticity-
# consistent correction, so SEs are bootstrapped. The draw matrix is kept
# because the right-left contrast needs the covariance, not just the SEs.

boot_heck <- function(data, indices, selection_formula, outcome_formula, n_coef) {
  d <- data[indices, ]
  fit <- tryCatch(heckit(selection = selection_formula, outcome = outcome_formula,
                         data = d, method = "2step"), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) != n_coef) return(rep(NA_real_, n_coef))
  coef(fit)
}

run_boot <- function(heck_model, outcome_formula, R = R_BOOT) {
  set.seed(42)
  b <- boot(data = acled_data, statistic = boot_heck, R = R,
            selection_formula = sel_formula, outcome_formula = outcome_formula,
            n_coef = length(coef(heck_model)))
  cat("Failed bootstrap draws:", sum(!complete.cases(b$t)), "of", R, "\n")
  se <- apply(b$t, 2, sd, na.rm = TRUE); names(se) <- names(coef(heck_model))
  list(se = se, draws = b$t)
}

cat("\nBootstrapping arrest model (R =", R_BOOT, ")...\n")
ba <- run_boot(heck_arrest, arr_formula)
coefs_arrest <- coef(heck_arrest); boot_se_arrest <- ba$se
p_arrest <- 2 * pnorm(-abs(coefs_arrest / boot_se_arrest))

cat("Bootstrapping brutality model (R =", R_BOOT, ")...\n")
bb <- run_boot(heck_brutality, brut_formula)
coefs_brutality <- coef(heck_brutality); boot_se_brutality <- bb$se
p_brutality <- 2 * pnorm(-abs(coefs_brutality / boot_se_brutality))

arrest_results <- data.frame(
  Term = names(coefs_arrest), Estimate = round(coefs_arrest, 4),
  Std_Error = round(boot_se_arrest, 4),
  z_value = round(coefs_arrest / boot_se_arrest, 4),
  p_value = round(p_arrest, 4), row.names = NULL)
brutality_results <- data.frame(
  Term = names(coefs_brutality), Estimate = round(coefs_brutality, 4),
  Std_Error = round(boot_se_brutality, 4),
  z_value = round(coefs_brutality / boot_se_brutality, 4),
  p_value = round(p_brutality, 4), row.names = NULL)

cat("\n=== ARREST (bootstrap SEs) ===\n");    print(arrest_results, row.names = FALSE)
cat("\n=== BRUTALITY (bootstrap SEs) ===\n"); print(brutality_results, row.names = FALSE)

# heckit lists every predictor twice: selection block, then outcome block. The
# SECOND occurrence of left_pure starts the outcome block. Plain name indexing
# silently returns the selection-equation coefficient — never use it here.
outcome_idx <- which(names(coefs_arrest) == "left_pure")[2] + 0:3
stopifnot(identical(names(coefs_arrest)[outcome_idx], partisan_vars))
stopifnot(identical(names(coefs_brutality)[outcome_idx], partisan_vars))

imr_a <- which(names(coefs_arrest) == "invMillsRatio")
imr_b <- which(names(coefs_brutality) == "invMillsRatio")
cat("\nIMR, bootstrap SEs — arrest: b =", round(coefs_arrest[imr_a], 4),
    "p =", round(p_arrest[imr_a], 4),
    "| brutality: b =", round(coefs_brutality[imr_b], 4),
    "p =", round(p_brutality[imr_b], 4), "\n")

heck_comparison <- bind_rows(
  data.frame(Variable = c("Left", "Right", "Unknown", "Counter-Protest"), Outcome = "Arrest",
             Estimate = round(coefs_arrest[outcome_idx], 4),
             SE = round(boot_se_arrest[outcome_idx], 4),
             p_value = round(p_arrest[outcome_idx], 4), row.names = NULL),
  data.frame(Variable = c("Left", "Right", "Unknown", "Counter-Protest"), Outcome = "Brutality",
             Estimate = round(coefs_brutality[outcome_idx], 4),
             SE = round(boot_se_brutality[outcome_idx], 4),
             p_value = round(p_brutality[outcome_idx], 4), row.names = NULL))
cat("\n=== PARTISAN COEFFICIENTS, OUTCOME EQUATIONS (ref = centre) ===\n")
print(heck_comparison, row.names = FALSE)

# ============================================================================
# TOST: EQUIVALENCE TESTING (H3)
# ============================================================================

tost_z <- function(estimate, se, delta) {
  p_lower <- pnorm((estimate + delta) / se, lower.tail = FALSE)
  p_upper <- pnorm((estimate - delta) / se, lower.tail = TRUE)
  p_tost  <- max(p_lower, p_upper)
  data.frame(estimate = round(estimate, 4), se = round(se, 4), delta = delta,
             p_tost = round(p_tost, 4),
             ci90_lower = round(estimate - 1.645 * se, 4),
             ci90_upper = round(estimate + 1.645 * se, 4),
             equivalent = p_tost < 0.05, row.names = NULL)
}

achieved_delta <- function(se) round(se * 2.49, 4)
achieved_bounds <- c(
  arrest_left     = achieved_delta(boot_se_arrest[outcome_idx[1]]),
  arrest_right    = achieved_delta(boot_se_arrest[outcome_idx[2]]),
  brutality_left  = achieved_delta(boot_se_brutality[outcome_idx[1]]),
  brutality_right = achieved_delta(boot_se_brutality[outcome_idx[2]]))
print(achieved_bounds)
delta_primary <- max(achieved_bounds)
cat("\ndelta_primary:", delta_primary, "-> driven by:", names(which.max(achieved_bounds)), "\n")

run_tost <- function(coefs, ses) bind_rows(
  cbind(Variable = "Left",  tost_z(coefs[outcome_idx[1]], ses[outcome_idx[1]], delta_primary)),
  cbind(Variable = "Right", tost_z(coefs[outcome_idx[2]], ses[outcome_idx[2]], delta_primary)))

cat("\n=== TOST vs CENTRE: ARREST ===\n")
tost_arrest <- run_tost(coefs_arrest, boot_se_arrest); print(tost_arrest, row.names = FALSE)
cat("\n=== TOST vs CENTRE: BRUTALITY ===\n")
tost_brutality <- run_tost(coefs_brutality, boot_se_brutality); print(tost_brutality, row.names = FALSE)

# --- right - left: the contrast H3 is actually about -------------------------
rl_diff <- function(coefs, draws) {
  est <- unname(coefs[outcome_idx[2]] - coefs[outcome_idx[1]])
  se  <- sd(draws[, outcome_idx[2]] - draws[, outcome_idx[1]], na.rm = TRUE)
  list(est = est, se = se)
}
rl_a <- rl_diff(coefs_arrest, ba$draws)
rl_b <- rl_diff(coefs_brutality, bb$draws)

rl_results <- bind_rows(
  cbind(Outcome = "Arrest", Test = "right - left", tost_z(rl_a$est, rl_a$se, delta_primary),
        z = round(rl_a$est / rl_a$se, 3), p_diff = round(2 * pnorm(-abs(rl_a$est / rl_a$se)), 4)),
  cbind(Outcome = "Brutality", Test = "right - left", tost_z(rl_b$est, rl_b$se, delta_primary),
        z = round(rl_b$est / rl_b$se, 3), p_diff = round(2 * pnorm(-abs(rl_b$est / rl_b$se)), 4)))
cat("\n=== RIGHT - LEFT: difference and equivalence ===\n")
print(rl_results, row.names = FALSE)

# ============================================================================
# ROBUSTNESS 1: NO HECKMAN CORRECTION
# ============================================================================

logit_arrest    <- glm(arr_formula,  data = present_data, family = binomial(link = "logit"))
logit_brutality <- glm(brut_formula, data = present_data, family = binomial(link = "logit"))

compare_coefs <- function(heck_coefs, heck_p, logit_model, label) {
  lc <- summary(logit_model)$coefficients
  data.frame(Variable = c("Left", "Right"), Outcome = label,
             Heck_Est = round(heck_coefs[1:2], 4), Heck_p = round(heck_p[1:2], 4),
             Logit_Est = round(lc[c("left_pure", "right_pure"), "Estimate"], 4),
             Logit_p = round(lc[c("left_pure", "right_pure"), "Pr(>|z|)"], 4), row.names = NULL)
}
comparison_table <- bind_rows(
  compare_coefs(coefs_arrest[outcome_idx],    p_arrest[outcome_idx],    logit_arrest,    "Arrest"),
  compare_coefs(coefs_brutality[outcome_idx], p_brutality[outcome_idx], logit_brutality, "Brutality"))
cat("\n=== ROBUSTNESS: NO-CORRECTION COMPARISON ===\n")
cat("Heckman estimates are linear probability; logit estimates are log-odds.\n")
cat("Compare sign and significance, not magnitude.\n")
print(comparison_table, row.names = FALSE)

# ============================================================================
# ROBUSTNESS 2: ALTERNATIVE INSTRUMENTS
# ============================================================================

alt_results <- bind_rows(lapply(
  c("log_protest_load", "n_police_stations_5km", "log_dist_police_station"), function(instr) {
    sf <- update(sel_formula, as.formula(paste(". ~ . - log_protest_load +", instr)))
    bind_rows(lapply(c("arrest", "brutality"), function(dv) {
      of <- if (dv == "arrest") arr_formula else brut_formula
      m   <- heckit(selection = sf, outcome = of, data = acled_data, method = "2step")
      idx <- which(names(coef(m)) == "left_pure")[2] + 0:3
      est <- summary(m)$estimate
      data.frame(Instrument = instr, Outcome = dv,
                 Variable = c("Left", "Right", "Unknown", "Counter-Protest"),
                 Estimate = round(coef(m)[idx], 4),
                 p_model  = round(est[idx, "Pr(>|t|)"], 4),
                 IMR_p    = round(est["invMillsRatio", "Pr(>|t|)"], 4), row.names = NULL)
    }))
  }))
cat("\n=== ROBUSTNESS: ALTERNATIVE INSTRUMENTS (model-based p) ===\n")
print(alt_results, row.names = FALSE)

# ============================================================================
# ROBUSTNESS 3: LOGGED CROWD SIZE ADDED BACK
# ============================================================================

if ("log_crowd_size" %in% names(acled_data)) {
  sf <- update(sel_formula,  . ~ . + log_crowd_size)
  af <- update(arr_formula,  . ~ . + log_crowd_size)
  bf <- update(brut_formula, . ~ . + log_crowd_size)
  hac <- heckit(selection = sf, outcome = af, data = acled_data, method = "2step")
  hbc <- heckit(selection = sf, outcome = bf, data = acled_data, method = "2step")
  idx_c <- which(names(coef(hac)) == "left_pure")[2] + 0:3
  stopifnot(identical(names(coef(hac))[idx_c], partisan_vars))
  crowd_robustness <- data.frame(
    Variable = c("Left", "Right", "Unknown", "Counter-Protest"),
    Arrest_Est = round(coef(hac)[idx_c], 4),
    Arrest_p   = round(summary(hac)$estimate[idx_c, "Pr(>|t|)"], 4),
    Brutality_Est = round(coef(hbc)[idx_c], 4),
    Brutality_p   = round(summary(hbc)$estimate[idx_c, "Pr(>|t|)"], 4), row.names = NULL)
  cat("\n=== ROBUSTNESS: LOGGED CROWD SIZE ADDED ===\n")
  print(crowd_robustness, row.names = FALSE)
  write.csv(crowd_robustness, "results/heckman_crowd_size_robustness.csv", row.names = FALSE)
} else {
  cat("\nlog_crowd_size not found — skipping crowd-size robustness.\n")
}

# ============================================================================
# SAVE
# ============================================================================

write.csv(severity_desc,     "results/table9_table10_descriptive_columns.csv", row.names = FALSE)
write.csv(exclusion_results, "results/exclusion_restriction_checks.csv",       row.names = FALSE)
write.csv(heck_comparison,   "results/heckman_partisan_coefs.csv",             row.names = FALSE)
write.csv(arrest_results,    "results/heckman_arrest_full.csv",                row.names = FALSE)
write.csv(brutality_results, "results/heckman_brutality_full.csv",             row.names = FALSE)
write.csv(comparison_table,  "results/heckman_vs_nocorrection.csv",            row.names = FALSE)
write.csv(alt_results,       "results/alternative_instruments.csv",            row.names = FALSE)
write.csv(bind_rows(cbind(Outcome = "Arrest",    Test = "vs centre", tost_arrest),
                    cbind(Outcome = "Brutality", Test = "vs centre", tost_brutality)),
          "results/tost_equivalence.csv", row.names = FALSE)
write.csv(rl_results,        "results/tost_right_minus_left.csv",              row.names = FALSE)
saveRDS(heck_arrest,    "models/heckman_arrest.rds")
saveRDS(heck_brutality, "models/heckman_brutality.rds")

cat("\n=== Model 2 complete ===\n")
