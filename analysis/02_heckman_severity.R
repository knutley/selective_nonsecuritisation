# ============================================================================
# MODEL 2: RESPONSE SEVERITY (HECKMAN TWO-STEP) — rethresholded v3,
#          REGION + YEAR FIXED EFFECTS
# Author: Katie Nutley
# Date: 23-09-2026; re-specified 07-10-2026 (region + year FE)
# ============================================================================
#
# Script 2 of 2. Run 01_model1_police_presence.R first; this script repeats
# the data preparation so it can also be run on its own. Run from the
# repository root.
#
# SPECIFICATION CHANGE (07-10-2026)
#   Country FE + Covid dummy replaced by region (admin1) + year fixed effects,
#   the BIC-preferred specification in the FE grid (05_fe_specification_grid.R;
#   Appendix "Fixed-Effects Specification and Model Selection"). The Covid
#   dummy is absorbed by the year FE. Country FE are nested in region FE.
#   One region with no police-present event (Melilla, 81 events) is dropped,
#   as its events are perfectly predicted.
#
# ESTIMATION CHANGE
#   sampleSelection::heckit cannot carry 72 region dummies through the
#   bootstrap: nine regions have fewer than five police-present events, so a
#   majority of resamples leave at least one region dummy empty in the outcome
#   equation and the fit fails. The two steps are therefore estimated directly
#   with fixest: (1) probit selection equation with region + year FE ->
#   inverse Mills ratio; (2) linear outcome equation on police-present events
#   with the IMR and the same FE. Point estimates are identical to heckit's
#   two-step estimator; rho is computed with heckit's formula. Standard errors
#   are bootstrapped (both steps re-estimated in every draw), as before.
#
# STRUCTURE
#   Selection equation : police presence across all events (Model 1
#                        specification + protestor violence + the exclusion
#                        restriction).
#   Outcome equations  : arrest and brutality, observed only where police were
#                        present. Estimated separately because the two are not
#                        mutually exclusive. Incumbent partisanship is in the
#                        selection equation ONLY.
#
# EXCLUSION RESTRICTION: same-day protest load — the logged count of other
#   protest events in the same country on the same date (computed on all
#   events, before any sample restriction). Competing demand for finite
#   public-order resources shapes whether a unit can be spared, but not how
#   the officers who do attend behave. Alternatives (station density, distance
#   to station) are re-estimated in the robustness block.
#
# BRUTALITY CAVEAT: brutality is the least well-calibrated of the three
#   classifier labels (wrong-actor confound; 44 hand-coded positives in
#   training). Treat the brutality models with corresponding caution.
#
# ============================================================================

library(tidyverse)
library(fixest)
library(boot)

THRESHOLD <- 0.9   # must match 01_model1_police_presence.R
R_BOOT    <- 500   # 200 for a quick check, 500 for final results
FE        <- "region + year_f"
dir.create("analysis/results", showWarnings = FALSE, recursive = TRUE)
dir.create("analysis/models",  showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# DATA
# ============================================================================

acled_data <- read_csv("data/combined/acled_merged_controls_rethresholded_v3.csv",
                       show_col_types = FALSE)

required <- c("police_presence_prob", "event_partisan_type_final", "arrest", "brutality",
              "protestor_violence", "n_police_stations_5km", "dist_police_station_m",
              "event_date", "country", "admin1", "is_weekend", "incumbent_left",
              "incumbent_right", "dist_govt_building_m", "dist_major_road_m", "year")
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
    log_dist_govt_building  = log1p(dist_govt_building_m),
    log_dist_major_road     = log1p(dist_major_road_m),
    log_dist_police_station = log1p(dist_police_station_m),
    region = paste(country, admin1, sep = " | "),
    year_f = factor(year)
  ) %>%
  # ---- exclusion restriction: same-day protest load (all events) ------------
  group_by(country, event_date) %>%
  mutate(protest_load = n() - 1L) %>%
  ungroup() %>%
  mutate(log_protest_load = log1p(protest_load))

n_all <- nrow(acled_data)
acled_data <- acled_data %>%
  group_by(region) %>%
  filter(sum(police_presence) > 0) %>%     # drops regions with no police-present event
  ungroup()
cat("Dropped", n_all - nrow(acled_data), "events in regions with no police-present event\n")

stopifnot(sum(acled_data$counter_protest) > 0)

partisan_vars <- c("left_pure", "right_pure", "unknown_pure", "counter_protest")

cat("Events:", nrow(acled_data), "| police-present:", sum(acled_data$police_presence),
    "| regions:", n_distinct(acled_data$region), "\n")
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
# SPECIFICATION — single source of truth
# ============================================================================

sel_rhs <- c(partisan_vars, "incumbent_left", "incumbent_right",
             "log_dist_govt_building", "log_dist_major_road", "is_weekend",
             "protestor_violence", "log_protest_load")
out_rhs <- c(partisan_vars, "log_dist_govt_building", "log_dist_major_road",
             "is_weekend", "protestor_violence")

mk <- function(y, rhs, fe = FE) {
  as.formula(paste(y, "~", paste(rhs, collapse = " + "), if (!is.null(fe)) paste("|", fe)))
}

# Two-step Heckman with fixed effects in both equations
heck2 <- function(data, y, s_rhs = sel_rhs, o_rhs = out_rhs) {
  sel <- feglm(mk("police_presence", s_rhs), data = data,
               family = binomial(link = "probit"), notes = FALSE, warn = FALSE)
  xb  <- predict(sel, newdata = data, type = "link")
  data$imr <- dnorm(xb) / pnorm(xb)
  keep <- data$police_presence == 1 & !is.na(data$imr)
  pres <- data[keep, ]
  out  <- feols(mk(y, c(o_rhs, "imr")), data = pres, notes = FALSE, warn = FALSE)
  # rho as in sampleSelection::heckit (two-step)
  used  <- obs(out)
  lam   <- pres$imr[used]; xbp <- xb[keep][used]
  b_imr <- unname(coef(out)["imr"])
  sigma <- sqrt(mean(resid(out)^2) + b_imr^2 * mean(lam * (lam + xbp)))
  list(selection = sel, outcome = out, rho = b_imr / sigma, sigma = sigma,
       n_sel = nobs(sel), n_out = nobs(out))
}

cat("\nListwise-deleted observations (selection):",
    nrow(acled_data) - nrow(na.omit(acled_data[, c("police_presence", sel_rhs, "region", "year_f")])), "\n")

# ============================================================================
# INSTRUMENT STRENGTH
# ============================================================================

first_stage <- feglm(mk("police_presence", sel_rhs), data = acled_data,
                     family = binomial(link = "probit"), vcov = "hetero")
first_stage_no_iv <- feglm(mk("police_presence", setdiff(sel_rhs, "log_protest_load")),
                           data = acled_data, family = binomial(link = "probit"))

cat("\n=== INSTRUMENT STRENGTH: same-day protest load ===\n")
print(coeftable(first_stage)["log_protest_load", ])
w_iv <- wald(first_stage, keep = "^log_protest_load$", print = FALSE)
cat("Wald chi-sq:", round(w_iv$stat * w_iv$df1, 2), "on", w_iv$df1, "df\n")
cat("LR chi-sq:", round(2 * (as.numeric(logLik(first_stage)) - as.numeric(logLik(first_stage_no_iv))), 2),
    "on 1 df\n")

# ============================================================================
# EXCLUSION-RESTRICTION SENSITIVITY CHECKS
# ============================================================================
# A valid instrument should return null when entered directly into the outcome
# equations on the police-present subsample.

present_data <- filter(acled_data, police_presence == 1)

exclusion_check <- function(candidate) {
  bind_rows(lapply(c("arrest", "brutality"), function(dv) {
    m  <- feols(mk(dv, c(out_rhs, candidate)), data = present_data, vcov = "hetero")
    ct <- coeftable(m)
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

heck_arrest    <- heck2(acled_data, "arrest")
heck_brutality <- heck2(acled_data, "brutality")

cat("\n=== SELECTION EQUATION (probit, region + year FE, HC SEs) ===\n")
selection_results <- as.data.frame(coeftable(heck_arrest$selection, vcov = "hetero"))
print(round(selection_results, 4))

cat("\n=== OUTCOME: ARREST (model-based SEs; bootstrapped below) ===\n")
print(coeftable(heck_arrest$outcome, vcov = "iid"))
cat("\n=== OUTCOME: BRUTALITY (model-based SEs; bootstrapped below) ===\n")
print(coeftable(heck_brutality$outcome, vcov = "iid"))

cat("\nN selection:", heck_arrest$n_sel, "| N outcome:", heck_arrest$n_out, "\n")
cat("Arrest    — rho:", round(heck_arrest$rho, 4), "\n")
cat("Brutality — rho:", round(heck_brutality$rho, 4), "\n")

# ============================================================================
# BOOTSTRAPPED STANDARD ERRORS
# ============================================================================
# Both steps are re-estimated in every draw. The draw matrix is kept because
# the right-left contrast needs the covariance, not just the SEs. A region
# that happens to have no police-present event in a draw simply drops out of
# that draw's outcome FE, so draws do not fail on sparse regions.

boot_heck <- function(data, indices, y, ref_names) {
  fit <- tryCatch(heck2(data[indices, ], y), error = function(e) NULL)
  if (is.null(fit)) return(rep(NA_real_, length(ref_names)))
  b <- coef(fit$outcome)[ref_names]
  if (any(is.na(b))) return(rep(NA_real_, length(ref_names)))
  unname(b)
}

run_boot <- function(heck_model, y, R = R_BOOT) {
  ref <- names(coef(heck_model$outcome))
  set.seed(42)
  b <- boot(data = acled_data, statistic = boot_heck, R = R, y = y, ref_names = ref)
  colnames(b$t) <- ref
  cat("Failed bootstrap draws:", sum(!complete.cases(b$t)), "of", R, "\n")
  se <- apply(b$t, 2, sd, na.rm = TRUE); names(se) <- ref
  list(se = se, draws = b$t)
}

cat("\nBootstrapping arrest model (R =", R_BOOT, ")...\n")
ba <- run_boot(heck_arrest, "arrest")
coefs_arrest <- coef(heck_arrest$outcome); boot_se_arrest <- ba$se[names(coefs_arrest)]
p_arrest <- 2 * pnorm(-abs(coefs_arrest / boot_se_arrest))

cat("Bootstrapping brutality model (R =", R_BOOT, ")...\n")
bb <- run_boot(heck_brutality, "brutality")
coefs_brutality <- coef(heck_brutality$outcome); boot_se_brutality <- bb$se[names(coefs_brutality)]
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

cat("\n=== ARREST (bootstrap SEs; region + year FE absorbed) ===\n");    print(arrest_results, row.names = FALSE)
cat("\n=== BRUTALITY (bootstrap SEs; region + year FE absorbed) ===\n"); print(brutality_results, row.names = FALSE)

cat("\nIMR, bootstrap SEs — arrest: b =", round(coefs_arrest["imr"], 4),
    "p =", round(p_arrest["imr"], 4),
    "| brutality: b =", round(coefs_brutality["imr"], 4),
    "p =", round(p_brutality["imr"], 4), "\n")

heck_comparison <- bind_rows(
  data.frame(Variable = c("Left", "Right", "Unknown", "Counter-Protest"), Outcome = "Arrest",
             Estimate = round(coefs_arrest[partisan_vars], 4),
             SE = round(boot_se_arrest[partisan_vars], 4),
             p_value = round(p_arrest[partisan_vars], 4), row.names = NULL),
  data.frame(Variable = c("Left", "Right", "Unknown", "Counter-Protest"), Outcome = "Brutality",
             Estimate = round(coefs_brutality[partisan_vars], 4),
             SE = round(boot_se_brutality[partisan_vars], 4),
             p_value = round(p_brutality[partisan_vars], 4), row.names = NULL))
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
  arrest_left     = achieved_delta(boot_se_arrest[["left_pure"]]),
  arrest_right    = achieved_delta(boot_se_arrest[["right_pure"]]),
  brutality_left  = achieved_delta(boot_se_brutality[["left_pure"]]),
  brutality_right = achieved_delta(boot_se_brutality[["right_pure"]]))
print(achieved_bounds)
delta_primary <- max(achieved_bounds)
cat("\ndelta_primary:", delta_primary, "-> driven by:", names(which.max(achieved_bounds)), "\n")

run_tost <- function(coefs, ses) bind_rows(
  cbind(Variable = "Left",  tost_z(coefs[["left_pure"]],  ses[["left_pure"]],  delta_primary)),
  cbind(Variable = "Right", tost_z(coefs[["right_pure"]], ses[["right_pure"]], delta_primary)))

cat("\n=== TOST vs CENTRE: ARREST ===\n")
tost_arrest <- run_tost(coefs_arrest, boot_se_arrest); print(tost_arrest, row.names = FALSE)
cat("\n=== TOST vs CENTRE: BRUTALITY ===\n")
tost_brutality <- run_tost(coefs_brutality, boot_se_brutality); print(tost_brutality, row.names = FALSE)

# --- right - left: the contrast H3 is actually about -------------------------
rl_diff <- function(coefs, draws) {
  est <- unname(coefs[["right_pure"]] - coefs[["left_pure"]])
  se  <- sd(draws[, "right_pure"] - draws[, "left_pure"], na.rm = TRUE)
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
# Logit with region + year FE on police-present events. Groups with no arrest
# (or no brutality) are perfectly predicted and dropped by feglm.

logit_arrest    <- feglm(mk("arrest",    out_rhs), data = present_data,
                         family = binomial(link = "logit"), vcov = "hetero")
logit_brutality <- feglm(mk("brutality", out_rhs), data = present_data,
                         family = binomial(link = "logit"), vcov = "hetero")

compare_coefs <- function(heck_coefs, heck_p, logit_model, label) {
  lc <- coeftable(logit_model)
  data.frame(Variable = c("Left", "Right"), Outcome = label,
             Heck_Est = round(heck_coefs[c("left_pure", "right_pure")], 4),
             Heck_p   = round(heck_p[c("left_pure", "right_pure")], 4),
             Logit_Est = round(lc[c("left_pure", "right_pure"), "Estimate"], 4),
             Logit_p   = round(lc[c("left_pure", "right_pure"), "Pr(>|z|)"], 4),
             Logit_N   = nobs(logit_model), row.names = NULL)
}
comparison_table <- bind_rows(
  compare_coefs(coefs_arrest,    p_arrest,    logit_arrest,    "Arrest"),
  compare_coefs(coefs_brutality, p_brutality, logit_brutality, "Brutality"))
cat("\n=== ROBUSTNESS: NO-CORRECTION COMPARISON ===\n")
cat("Heckman estimates are linear probability; logit estimates are log-odds.\n")
cat("Compare sign and significance, not magnitude.\n")
print(comparison_table, row.names = FALSE)

# ============================================================================
# ROBUSTNESS 2: ALTERNATIVE INSTRUMENTS
# ============================================================================
# p-values here are heteroskedasticity-robust from the outcome equation and do
# not account for the generated regressor (no bootstrap).

alt_results <- bind_rows(lapply(
  c("log_protest_load", "n_police_stations_5km", "log_dist_police_station"), function(instr) {
    s_rhs <- c(setdiff(sel_rhs, "log_protest_load"), instr)
    bind_rows(lapply(c("arrest", "brutality"), function(dv) {
      h  <- heck2(acled_data, dv, s_rhs = s_rhs)
      ct <- coeftable(h$outcome, vcov = "hetero")
      data.frame(Instrument = instr, Outcome = dv,
                 Variable = c("Left", "Right", "Unknown", "Counter-Protest"),
                 Estimate = round(ct[partisan_vars, "Estimate"], 4),
                 p_hetero = round(ct[partisan_vars, "Pr(>|t|)"], 4),
                 IMR_p    = round(ct["imr", "Pr(>|t|)"], 4), row.names = NULL)
    }))
  }))
cat("\n=== ROBUSTNESS: ALTERNATIVE INSTRUMENTS (HC p-values) ===\n")
print(alt_results, row.names = FALSE)

# ============================================================================
# ROBUSTNESS 3: LOGGED CROWD SIZE ADDED BACK
# ============================================================================

if ("log_crowd_size" %in% names(acled_data)) {
  hac <- heck2(acled_data, "arrest",    s_rhs = c(sel_rhs, "log_crowd_size"),
               o_rhs = c(out_rhs, "log_crowd_size"))
  hbc <- heck2(acled_data, "brutality", s_rhs = c(sel_rhs, "log_crowd_size"),
               o_rhs = c(out_rhs, "log_crowd_size"))
  ca <- coeftable(hac$outcome, vcov = "hetero"); cb <- coeftable(hbc$outcome, vcov = "hetero")
  crowd_robustness <- data.frame(
    Variable = c("Left", "Right", "Unknown", "Counter-Protest"),
    Arrest_Est    = round(ca[partisan_vars, "Estimate"], 4),
    Arrest_p      = round(ca[partisan_vars, "Pr(>|t|)"], 4),
    Brutality_Est = round(cb[partisan_vars, "Estimate"], 4),
    Brutality_p   = round(cb[partisan_vars, "Pr(>|t|)"], 4), row.names = NULL)
  cat("\n=== ROBUSTNESS: LOGGED CROWD SIZE ADDED (HC p-values) ===\n")
  print(crowd_robustness, row.names = FALSE)
  write.csv(crowd_robustness, "analysis/results/heckman_crowd_size_robustness.csv", row.names = FALSE)
} else {
  cat("\nlog_crowd_size not found — skipping crowd-size robustness.\n")
}

# ============================================================================
# SAVE
# ============================================================================

write.csv(severity_desc,     "analysis/results/table9_table10_descriptive_columns.csv", row.names = FALSE)
write.csv(exclusion_results, "analysis/results/exclusion_restriction_checks.csv",       row.names = FALSE)
write.csv(selection_results, "analysis/results/heckman_selection_equation.csv")
write.csv(heck_comparison,   "analysis/results/heckman_partisan_coefs.csv",             row.names = FALSE)
write.csv(arrest_results,    "analysis/results/heckman_arrest_full.csv",                row.names = FALSE)
write.csv(brutality_results, "analysis/results/heckman_brutality_full.csv",             row.names = FALSE)
write.csv(comparison_table,  "analysis/results/heckman_vs_nocorrection.csv",            row.names = FALSE)
write.csv(alt_results,       "analysis/results/alternative_instruments.csv",            row.names = FALSE)
write.csv(bind_rows(cbind(Outcome = "Arrest",    Test = "vs centre", tost_arrest),
                    cbind(Outcome = "Brutality", Test = "vs centre", tost_brutality)),
          "analysis/results/tost_equivalence.csv", row.names = FALSE)
write.csv(rl_results,        "analysis/results/tost_right_minus_left.csv",              row.names = FALSE)
saveRDS(heck_arrest,    "analysis/models/heckman_arrest.rds")
saveRDS(heck_brutality, "analysis/models/heckman_brutality.rds")

cat("\n=== Model 2 complete ===\n")
