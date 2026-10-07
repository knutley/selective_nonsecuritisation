# ============================================================================
# FIXED VS. RANDOM EFFECTS: HAUSMAN AND MUNDLAK TESTS — rethresholded v3
# Author: Katie Nutley
# Date: 07-10-2026
# ============================================================================
#
# Script 3. Self-contained: repeats the data preparation from
# 01_model1_police_presence.R and 02_heckman_severity.R (0.9 presence
# threshold, same dummies, same controls, same protest-load instrument), so
# it can be run on its own. Run from the repository root.
#
# TESTS
#   1. Police presence (Model 1 logit)
#      a. Country level  (5 groups)   classical Hausman: country FE vs RE
#      b. County level   (admin2)     classical Hausman: admin2 FE vs RE
#      c. County level   (admin2)     Mundlak / CRE test, admin2-clustered
#   2. Arrest and brutality (Heckman outcome equation, LPM with IMR,
#      police-present events) — same three tests.
#
# READING THE RESULTS
#   - The classical Hausman test needs the efficient (non-robust) vcov
#     under H0, so it is computed with iid SEs. The paper's HC3 / clustered
#     SEs violate that assumption, so the Mundlak test (cluster-robust
#     equivalent; Wooldridge 2010, 10.7.3 and 15.8.2) is the one to report.
#   - With 5 countries the between-country variance is barely identified;
#     the country-level test is reported for completeness only. Country FE
#     are justified on design grounds (purposively selected cases).
#   - admin2 names are unique across the five countries (779 units), so
#     admin2 is used directly as the county identifier.
#   - Severity-model SEs ignore that the IMR is a generated regressor.
# ============================================================================

library(readr)
library(dplyr)
library(fixest)   # feglm / feols with high-dimensional FE; wald()
library(lme4)     # glmer / lmer random-effects models

DATA_PATH <- "data/combined/acled_merged_controls_rethresholded_v3.csv"
OUT_DIR   <- "analysis/results"
THRESHOLD <- 0.9
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# DATA (identical to 01 / 02)
# ============================================================================

acled_data <- read_csv(DATA_PATH, show_col_types = FALSE) %>%
  mutate(
    police_presence = as.integer(police_presence_prob >= THRESHOLD),
    counter_protest = as.integer(
      event_partisan_type_final %in% c("left_and_right", "centre_and_left", "centre_and_right")
    ),
    left_pure    = as.integer(event_partisan_type_final == "left"),
    right_pure   = as.integer(event_partisan_type_final == "right"),
    unknown_pure = as.integer(event_partisan_type_final == "unknown"),
    covid        = as.integer(year %in% c(2020, 2021)),
    log_dist_govt_building = log1p(dist_govt_building_m),
    log_dist_major_road    = log1p(dist_major_road_m),
    country = factor(country)
  ) %>%
  group_by(country, event_date) %>%
  mutate(protest_load = n() - 1L) %>%          # computed on all events, as in 02
  ungroup() %>%
  mutate(log_protest_load = log1p(protest_load))

stopifnot(sum(acled_data$counter_protest) > 0)

partisan  <- c("left_pure", "right_pure", "unknown_pure", "counter_protest")  # centre = ref
incumbent <- c("incumbent_left", "incumbent_right")                           # centre = ref
controls  <- c("log_dist_govt_building", "log_dist_major_road", "covid", "is_weekend")

rhs1    <- c(partisan, incumbent, controls)                        # Model 1 (+ country)
sel_rhs <- c(rhs1, "protestor_violence", "log_protest_load")       # selection eq. (+ country)
rhs2    <- c(partisan, controls, "protestor_violence", "imr")      # outcome eq. (+ country)

cat("Events:", nrow(acled_data),
    "| police-present:", sum(acled_data$police_presence),
    "| missing admin2:", sum(is.na(acled_data$admin2)), "\n")

# ============================================================================
# HELPERS
# ============================================================================

# Classical Hausman: H = d' [V_FE - V_RE]^- d on the coefficients both
# models estimate. Generalised inverse + rank df guard against a variance
# difference that is not positive definite (flagged in V_diff_not_PD).
hausman <- function(b_fe, V_fe, b_re, V_re, keep) {
  k  <- intersect(intersect(names(b_fe), names(b_re)), keep)
  dv <- b_fe[k] - b_re[k]
  Vd <- as.matrix(V_fe[k, k]) - as.matrix(V_re[k, k])
  ev <- eigen(Vd, symmetric = TRUE, only.values = TRUE)$values
  df <- sum(ev > 1e-8 * max(abs(ev)))
  H  <- as.numeric(t(dv) %*% MASS::ginv(Vd) %*% dv)
  data.frame(chisq = round(H, 2), df = df,
             p = signif(pchisq(H, df, lower.tail = FALSE), 3),
             V_diff_not_PD = any(ev < 0))
}

# Mundlak: add admin2 means of every regressor that varies within admin2
add_means <- function(df, vars, g = "admin2") {
  for (v in vars) df[[paste0("m_", v)]] <- ave(df[[v]], df[[g]], FUN = mean)
  df
}

mundlak_row <- function(model, outcome) {
  w <- wald(model, keep = "^m_", print = FALSE)
  data.frame(outcome = outcome, level = "admin2", test = "Mundlak (admin2-clustered)",
             chisq = round(w$stat * w$df1, 2), df = w$df1,
             p = signif(w$p, 3), V_diff_not_PD = NA)
}

fml <- function(y, rhs, extra = NULL) as.formula(paste(y, "~", paste(c(rhs, extra), collapse = " + ")))

# ============================================================================
# 1. POLICE PRESENCE (Model 1 logit)
# ============================================================================

d <- acled_data[complete.cases(acled_data[, c("police_presence", rhs1, "country", "admin2")]), ]
d$admin2 <- factor(d$admin2)
cat("\nPresence sample (complete cases with admin2):", nrow(d),
    "| counties:", nlevels(d$admin2), "\n")

## 1a. Country level ---------------------------------------------------------
fe_c <- glm(fml("police_presence", rhs1, "country"), data = d, family = binomial)
re_c <- glmer(fml("police_presence", rhs1, "(1 | country)"), data = d,
              family = binomial, nAGQ = 10,
              control = glmerControl(optimizer = "bobyqa"))
H_pres_country <- hausman(coef(fe_c), vcov(fe_c), fixef(re_c), vcov(re_c), keep = rhs1)

## 1b. County level ----------------------------------------------------------
# admin2 FE nest country FE. Counties with no police-present events carry no
# within information and are dropped by feglm; RE is fitted on the same rows.
fe_a <- feglm(as.formula(paste("police_presence ~", paste(rhs1, collapse = " + "), "| admin2")),
              data = d, family = binomial, vcov = "iid")
d_a  <- d[obs(fe_a), ]
cat("County-FE logit sample:", nrow(d_a), "events in",
    length(unique(d_a$admin2)), "counties with variation in presence\n")

re_a <- glmer(fml("police_presence", rhs1, c("country", "(1 | admin2)")),
              data = d_a, family = binomial, nAGQ = 1,
              control = glmerControl(optimizer = "bobyqa"))
H_pres_county <- hausman(coef(fe_a), vcov(fe_a, vcov = "iid"),
                         fixef(re_a), vcov(re_a), keep = rhs1)

## 1c. Mundlak, admin2-clustered ----------------------------------------------
d_m  <- add_means(d, rhs1)
mund <- feglm(as.formula(paste("police_presence ~",
                               paste(c(rhs1, paste0("m_", rhs1)), collapse = " + "),
                               "| country")),
              data = d_m, family = binomial, cluster = ~admin2)

presence_results <- rbind(
  cbind(outcome = "presence", level = "country (5)", test = "Hausman (iid)", H_pres_country),
  cbind(outcome = "presence", level = "admin2",      test = "Hausman (iid)", H_pres_county),
  mundlak_row(mund, "presence"))

# ============================================================================
# 2. ARREST AND BRUTALITY (Heckman outcome equation, police-present events)
# ============================================================================
# First stage = the probit selection equation from 02 (heckit step 1),
# estimated on all events; IMR then attached to the police-present subset.

sc  <- acled_data[complete.cases(acled_data[, c("police_presence", sel_rhs, "country")]), ]
sel <- glm(fml("police_presence", sel_rhs, "country"), data = sc,
           family = binomial(link = "probit"))
xb  <- predict(sel, type = "link")
sc$imr <- dnorm(xb) / pnorm(xb)

dp <- sc %>% filter(police_presence == 1, !is.na(admin2))
dp$admin2 <- droplevels(factor(dp$admin2))
cat("\nSeverity sample (police-present, with admin2):", nrow(dp),
    "| counties:", nlevels(dp$admin2), "\n")

severity_results <- list()
for (y in c("arrest", "brutality")) {

  # 2a. Country level
  fe_c2 <- lm(fml(y, rhs2, "country"), data = dp)
  re_c2 <- lmer(fml(y, rhs2, "(1 | country)"), data = dp, REML = FALSE)
  h_c   <- hausman(coef(fe_c2), vcov(fe_c2), fixef(re_c2), vcov(re_c2), keep = rhs2)

  # 2b. County level (RE on the same rows the within estimator uses)
  fe_a2 <- feols(as.formula(paste(y, "~", paste(rhs2, collapse = " + "), "| admin2")),
                 data = dp, vcov = "iid")
  dp_a  <- dp[obs(fe_a2), ]
  re_a2 <- lmer(fml(y, rhs2, c("country", "(1 | admin2)")), data = dp_a, REML = FALSE)
  h_a   <- hausman(coef(fe_a2), vcov(fe_a2, vcov = "iid"),
                   fixef(re_a2), vcov(re_a2), keep = rhs2)

  # 2c. Mundlak, admin2-clustered
  dpm <- add_means(dp, rhs2)
  mu2 <- feols(as.formula(paste(y, "~",
                                paste(c(rhs2, paste0("m_", rhs2)), collapse = " + "),
                                "| country")),
               data = dpm, cluster = ~admin2)

  severity_results[[y]] <- rbind(
    cbind(outcome = y, level = "country (5)", test = "Hausman (iid)", h_c),
    cbind(outcome = y, level = "admin2",      test = "Hausman (iid)", h_a),
    mundlak_row(mu2, y))
}

# ============================================================================
# 3. RESULTS
# ============================================================================

results <- rbind(presence_results, do.call(rbind, severity_results))
cat("\n=== FE vs RE: HAUSMAN AND MUNDLAK TESTS ===\n")
print(results, row.names = FALSE)

# Coefficient comparison for the appendix (presence, county level)
k   <- intersect(names(coef(fe_a)), rhs1)
cmp <- data.frame(term = k,
                  FE_admin2 = round(coef(fe_a)[k], 3),
                  RE_admin2 = round(fixef(re_a)[k], 3),
                  row.names = NULL)
cat("\n=== PRESENCE: admin2 FE vs RE coefficients ===\n")
print(cmp, row.names = FALSE)

# Left-right contrast under each estimator (the paper's key quantity)
lr <- function(b, V) {
  d <- b["right_pure"] - b["left_pure"]
  se <- sqrt(V["right_pure", "right_pure"] + V["left_pure", "left_pure"] -
               2 * V["right_pure", "left_pure"])
  c(diff = unname(round(d, 3)), se = unname(round(se, 3)),
    p = unname(signif(2 * pnorm(-abs(d / se)), 3)))
}
cat("\nRight - left contrast (presence):\n")
print(rbind(FE_admin2 = lr(coef(fe_a), vcov(fe_a, vcov = "iid")),
            RE_admin2 = lr(fixef(re_a), as.matrix(vcov(re_a)))))

write.csv(results, file.path(OUT_DIR, "hausman_results.csv"), row.names = FALSE)
write.csv(cmp,     file.path(OUT_DIR, "hausman_presence_fe_vs_re_coefs.csv"), row.names = FALSE)

cat("\n=== Hausman / Mundlak tests complete ===\n")
