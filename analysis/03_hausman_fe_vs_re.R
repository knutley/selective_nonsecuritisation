# =====================================================================
# Fixed vs. random effects: Hausman and Mundlak (robust Hausman) tests
# Protestor Partisanship and Police Response
#
# Run after your main data prep. Rename the variables in the CONFIG
# block to match your dataset; everything else should run as-is.
# =====================================================================

library(fixest)    # feglm / feols with high-dimensional FE
library(lme4)      # glmer / lmer random-effects models
library(plm)       # phtest for the linear severity models
library(MASS)      # ginv

# ---- CONFIG ---------------------------------------------------------
d <- readRDS("analysis_data.rds")          # <- your event-level data

y_presence  <- "police_present"
y_severity  <- c("arrest", "brutality")
partisan    <- c("left", "right", "counter_protest", "unknown")   # centre = ref
incumbent   <- c("inc_left", "inc_right")                         # centre = ref
controls    <- c("is_weekend", "log_dist_road", "log_dist_gov", "covid")
violence    <- "protestor_violence"
instrument  <- "protest_load"              # exclusion restriction (Sec. 6.2.3)
country     <- "country"
county      <- "admin2"
# ---------------------------------------------------------------------

d <- d[!is.na(d[[county]]), ]              # 51 events lack admin2
d[[country]] <- factor(d[[country]])
d[[county]]  <- factor(d[[county]])

rhs1 <- c(partisan, incumbent, controls)               # Model 1
rhs2 <- c(partisan, controls, violence, "imr")         # Model 2 outcome eq.

# ---- Classic Hausman (model-based, non-robust vcov) -----------------
# H = (b_FE - b_RE)' [V_FE - V_RE]^- (b_FE - b_RE), compared only on
# coefficients both models estimate. Generalised inverse + rank df guard
# against the variance difference not being positive definite.
hausman <- function(b_fe, V_fe, b_re, V_re, keep) {
  k  <- intersect(intersect(names(b_fe), names(b_re)), keep)
  dv <- b_fe[k] - b_re[k]
  Vd <- as.matrix(V_fe[k, k]) - as.matrix(V_re[k, k])
  ev <- eigen(Vd, symmetric = TRUE, only.values = TRUE)$values
  df <- sum(ev > 1e-8 * max(abs(ev)))
  H  <- as.numeric(t(dv) %*% ginv(Vd) %*% dv)
  data.frame(chisq = round(H, 2), df = df,
             p = signif(pchisq(H, df, lower.tail = FALSE), 3),
             V_diff_not_PD = any(ev < 0))
}

# Mundlak / correlated-random-effects test: add group means of every
# within-varying regressor; joint Wald test that they are all zero.
# With clustered SEs this is the heteroskedasticity/cluster-robust
# equivalent of the Hausman test (Wooldridge 2010, 10.7.3; 15.8.2).
add_means <- function(df, vars, g) {
  for (v in vars) df[[paste0("m_", v)]] <- ave(df[[v]], df[[g]], FUN = mean)
  df
}

# =====================================================================
# 1. POLICE PRESENCE (logit)
# =====================================================================
f1 <- reformulate(rhs1, y_presence)

## 1a. Country level (as in the paper: 5 countries) --------------------
fe_c <- glm(update(f1, paste(". ~ . +", country)), data = d,
            family = binomial)
re_c <- glmer(update(f1, paste(". ~ . + (1|", country, ")")), data = d,
              family = binomial, nAGQ = 10)
H_pres_country <- hausman(coef(fe_c), vcov(fe_c),
                          fixef(re_c), vcov(re_c), keep = rhs1)

## 1b. County level (779 admin2 units) ---------------------------------
# County FE nest country FE. Counties with no police events carry no
# within information and are dropped by feglm, so RE is fitted on the
# same estimation sample for a like-for-like comparison.
fe_a <- feglm(as.formula(paste(deparse(f1), "|", county)),
              data = d, family = binomial, vcov = "iid")
d_a  <- d[obs(fe_a), ]
re_a <- glmer(update(f1, paste(". ~ . +", country, "+ (1|", county, ")")),
              data = d_a, family = binomial, nAGQ = 1,
              control = glmerControl(optimizer = "bobyqa"))
H_pres_county <- hausman(coef(fe_a), vcov(fe_a, vcov = "iid"),
                         fixef(re_a), vcov(re_a), keep = rhs1)

## 1c. Mundlak, cluster-robust (county) ---------------------------------
d_m  <- add_means(d, rhs1, county)
mund <- feglm(as.formula(paste(y_presence, "~",
                paste(c(rhs1, paste0("m_", rhs1)), collapse = " + "),
                "|", country)),
              data = d_m, family = binomial, cluster = county)
M_pres <- wald(mund, keep = "^m_", print = FALSE)

# =====================================================================
# 2. SEVERITY (Heckman outcome equation, LPM, police-present events)
# =====================================================================
# First-stage probit -> IMR (same as heckit's step 1)
sel <- glm(reformulate(c(rhs1, violence, instrument, country), y_presence),
           data = d, family = binomial(link = "probit"))
xb  <- predict(sel, type = "link")
d$imr <- dnorm(xb) / pnorm(xb)
dp  <- d[d[[y_presence]] == 1, ]
dp[[county]] <- droplevels(dp[[county]])

sev_out <- list()
for (y in y_severity) {
  f2 <- reformulate(rhs2, y)

  # 2a. Country level
  fe_c2 <- lm(update(f2, paste(". ~ . +", country)), data = dp)
  re_c2 <- lmer(update(f2, paste(". ~ . + (1|", country, ")")), data = dp,
                REML = FALSE)
  h_c <- hausman(coef(fe_c2), vcov(fe_c2), fixef(re_c2), vcov(re_c2),
                 keep = rhs2)

  # 2b. County level: plm within vs. random (Swamy-Arora) + phtest
  pd   <- pdata.frame(dp, index = county)
  fe_p <- plm(f2, data = pd, model = "within")
  re_p <- plm(update(f2, paste(". ~ . +", country)), data = pd,
              model = "random")
  h_a  <- phtest(fe_p, re_p)

  # 2c. Mundlak, cluster-robust (county)
  dpm <- add_means(dp, rhs2, county)
  mu2 <- feols(as.formula(paste(y, "~",
                 paste(c(rhs2, paste0("m_", rhs2)), collapse = " + "),
                 "|", country)), data = dpm, cluster = county)
  m2  <- wald(mu2, keep = "^m_", print = FALSE)

  sev_out[[y]] <- rbind(
    cbind(outcome = y, level = "country (5)",  test = "Hausman", h_c),
    data.frame(outcome = y, level = "admin2", test = "Hausman (phtest)",
               chisq = round(unname(h_a$statistic), 2),
               df = unname(h_a$parameter), p = signif(h_a$p.value, 3),
               V_diff_not_PD = NA),
    data.frame(outcome = y, level = "admin2", test = "Mundlak (clustered)",
               chisq = round(m2$stat * m2$df1, 2), df = m2$df1,
               p = signif(m2$p, 3), V_diff_not_PD = NA))
}

# =====================================================================
# 3. Summary table
# =====================================================================
results <- rbind(
  cbind(outcome = "presence", level = "country (5)", test = "Hausman",
        H_pres_country),
  cbind(outcome = "presence", level = "admin2", test = "Hausman",
        H_pres_county),
  data.frame(outcome = "presence", level = "admin2",
             test = "Mundlak (clustered)",
             chisq = round(M_pres$stat * M_pres$df1, 2), df = M_pres$df1,
             p = signif(M_pres$p, 3), V_diff_not_PD = NA),
  do.call(rbind, sev_out))
print(results, row.names = FALSE)
write.csv(results, "hausman_results.csv", row.names = FALSE)

# Coefficient comparison for the appendix (presence, county level)
cmp <- data.frame(
  term = intersect(names(coef(fe_a)), rhs1),
  FE   = coef(fe_a)[intersect(names(coef(fe_a)), rhs1)],
  RE   = fixef(re_a)[intersect(names(coef(fe_a)), rhs1)])
print(round(cmp[, -1], 3))

# NOTES
# - Hausman requires the efficient (non-robust) vcov under H0, hence
#   vcov = "iid" above. Your HC3 / clustered SEs violate that, which is
#   why the Mundlak test is the one to report as primary.
# - Severity-model SEs here ignore that the IMR is a generated
#   regressor; for the write-up, bootstrap the Mundlak Wald stat
#   (resample counties, re-estimate both stages) to match your R = 500.
# - 4,213 present events over ~779 counties leaves many singleton
#   counties in 2b; they contribute nothing to the within estimator.
