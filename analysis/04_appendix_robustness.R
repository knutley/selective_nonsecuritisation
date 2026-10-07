# ============================================================================
# APPENDIX ROBUSTNESS CHECKS — rethresholded v3
# Author: Katie Nutley
# Date: 23-09-2026 (recovered from unsaved RStudio buffer "Untitled10", 07-10-2026)
# ============================================================================
#
# Run AFTER 01_model1_police_presence.R and 02_heckman_severity.R, in the same
# R session. Uses objects those scripts create:
#   - model1      (baseline logit, from 01)
#   - acled_data  (prepared data incl. log_protest_load, from 02)
#
# Produces:
#   - Uncorrected logits on the police-present subsample (Table 14)
#   - Alternative exclusion restrictions, with/without incumbent controls (Table 15)
#   - Police presence by country
#   - Counter-protest vs. left / right contrasts (reported in text)
#   - Within-county (admin2) conditional logit (Table 18)
#   - Logit vs. probit selection-equation comparison
# ============================================================================

library(dplyr)
library(sandwich)
library(lmtest)
library(car)
library(sampleSelection)

stopifnot(exists("model1"), exists("acled_data"),
          "log_protest_load" %in% names(acled_data))


# ---- Appendix 9, Table A: uncorrected logit on the police-present subsample ----
pp <- dplyr::filter(acled_data, police_presence == 1)

rhs <- "left_pure + right_pure + unknown_pure + counter_protest +
        country + log_dist_govt_building + log_dist_major_road +
        covid + is_weekend + protestor_violence"

for (dv in c("arrest", "brutality")) {
  m <- glm(as.formula(paste(dv, "~", rhs)), data = pp, family = binomial())
  ct <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC3"))
  cat("\n---", dv, "(uncorrected logit) ---\n")
  print(round(ct[c("left_pure", "right_pure",
                   "unknown_pure", "counter_protest"), c(1, 4)], 3))
}

# ---- Appendix 9, Table B: alternative exclusion restrictions ----
instruments <- c("log_protest_load", "n_police_stations_5km", "log_dist_police_station")

for (iv in instruments) {
  sel <- as.formula(paste("police_presence ~", rhs, "+", iv))
  for (dv in c("arrest", "brutality")) {
    out <- as.formula(paste(dv, "~ left_pure + right_pure + unknown_pure +
                             counter_protest + country + log_dist_govt_building +
                             log_dist_major_road + covid + is_weekend +
                             protestor_violence"))
    h <- sampleSelection::heckit(sel, out, data = acled_data, method = "2step")
    co <- summary(h)$estimate
    idx <- which(rownames(co) == "left_pure")[2] + 0:1   # outcome-equation rows
    cat("\n---", iv, "/", dv, "---\n")
    print(round(co[idx, c(1, 4)], 3))
  }
}

rhs_sel <- paste(rhs, "+ incumbent_left + incumbent_right")

for (iv in instruments) {
  sel <- as.formula(paste("police_presence ~", rhs_sel, "+", iv))
  for (dv in c("arrest", "brutality")) {
    out <- as.formula(paste(dv, "~", rhs))
    h <- sampleSelection::heckit(sel, out, data = acled_data, method = "2step")
    co <- summary(h)$estimate
    idx <- which(rownames(co) == "left_pure")[2] + 0:1
    cat("\n---", iv, "/", dv, "---\n"); print(round(co[idx, c(1, 4)], 3))
  }
}

acled_data %>%
  group_by(country) %>%
  summarise(total   = n(),
            present = sum(police_presence),
            pct     = round(100 * mean(police_presence), 2),
            .groups = "drop") %>%
  print(n = Inf)

car::linearHypothesis(model1, "counter_protest - left_pure = 0", vcov = vcovHC(model1, "HC3"))
car::linearHypothesis(model1, "counter_protest - right_pure = 0", vcov = vcovHC(model1, "HC3"))

library(survival)

acled_admin2 <- dplyr::filter(acled_data, !is.na(admin2))

m_within <- clogit(
  police_presence ~ left_pure + right_pure + unknown_pure + counter_protest +
    incumbent_left + incumbent_right +
    log_dist_govt_building + log_dist_major_road +
    covid + is_weekend + strata(admin2),
  data = acled_admin2)

print(summary(m_within))

# right - left contrast
b  <- coef(m_within); V <- vcov(m_within)
d  <- b["right_pure"] - b["left_pure"]
se <- sqrt(V["right_pure","right_pure"] + V["left_pure","left_pure"] -
             2 * V["right_pure","left_pure"])
cat("\ndiff =", round(d, 4), " OR =", round(exp(d), 3),
    " chisq =", round((d/se)^2, 3),
    " p =", signif(pchisq((d/se)^2, 1, lower.tail = FALSE), 3), "\n")
cat("events used =", m_within$nevent, " n =", m_within$n, "\n")

install.packages("fixest")
library(fixest)

m_within <- feglm(
  police_presence ~ left_pure + right_pure + unknown_pure + counter_protest +
    incumbent_left + incumbent_right +
    log_dist_govt_building + log_dist_major_road +
    covid + is_weekend | admin2,
  data = acled_admin2, family = binomial("logit"), cluster = ~admin2)

summary(m_within)

b  <- coef(m_within); V <- vcov(m_within)
d  <- b["right_pure"] - b["left_pure"]
se <- sqrt(V["right_pure","right_pure"] + V["left_pure","left_pure"] -
             2 * V["right_pure","left_pure"])
cat("\ndiff =", round(d, 4), " OR =", round(exp(d), 3),
    " chisq =", round((d/se)^2, 3),
    " p =", signif(pchisq((d/se)^2, 1, lower.tail = FALSE), 3), "\n")
cat("obs used =", nobs(m_within), "\n")

sel_rhs <- police_presence ~ left_pure + right_pure + unknown_pure + counter_protest +
  incumbent_left + incumbent_right + country +
  log_dist_govt_building + log_dist_major_road + covid + is_weekend +
  protestor_violence + log_protest_load

p <- glm(sel_rhs, data = acled_data, family = binomial("probit"))
l <- glm(sel_rhs, data = acled_data, family = binomial("logit"))

v <- c("left_pure", "right_pure", "unknown_pure", "counter_protest",
       "protestor_violence", "log_protest_load")

print(data.frame(
  Variable = v,
  Logit    = round(coef(l)[v], 4),
  Probit   = round(coef(p)[v], 4),
  Ratio    = round(coef(l)[v] / coef(p)[v], 2),
  P_probit = round(summary(p)$coefficients[v, 4], 4),
  row.names = NULL), row.names = FALSE)

for (nm in c("logit", "probit")) {
  m <- if (nm == "logit") l else p
  V <- vcov(m); d <- coef(m)["right_pure"] - coef(m)["left_pure"]
  se <- sqrt(V["right_pure","right_pure"] + V["left_pure","left_pure"] -
               2 * V["right_pure","left_pure"])
  cat(sprintf("%-7s  right - left = %+.4f  SE = %.4f  p = %.5f\n",
              nm, d, se, 2 * pnorm(-abs(d / se))))
}
