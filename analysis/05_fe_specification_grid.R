# ============================================================================
# FIXED-EFFECTS SPECIFICATION GRID: FE vs RE, AIC/BIC, SE CLUSTERING
# Selective Non-Securitisation — rethresholded v3
# Author: Katie Nutley
# Date: 07-10-2026
# ============================================================================
#
# Script 5. Self-contained (repeats the 01/02 data preparation). Run from the
# repository root. Produces one appendix's worth of tables.
#
# GRID
#   Spatial FE   : none | country (5) | region/admin1 (72) | NUTS-3 (~840)
#                  | county/admin2 (779) | city/location (~10,600)
#   Temporal FE  : none | year (5) | year-month (60) | ISO year-week (262)
#   -> 6 x 4 = 24 specifications (none x none = pooled logit).
#   The Covid dummy is dropped whenever a temporal FE is included (absorbed).
#
# FOR EACH SPECIFICATION
#   Model 1 (presence logit):
#     - AIC / BIC on a common sample (see note below)
#     - Mundlak (correlated RE) test, admin2-clustered  [robust Hausman]
#     - Classical Hausman: FE vs RE with the same grouping as random
#       intercepts (glmer)                            [optional, slow]
#     - Right - left contrast under 7 variance estimators:
#       HC (robust), clustered by country / region / NUTS-3 / county / city,
#       and Conley spatial HAC (CONLEY_KM cutoff)
#   Heckman outcome equations (arrest, brutality; LPM on police-present
#   events, IMR from a probit selection equation with the SAME fixed effects):
#     - Right - left contrast under the same variance estimators
#     - Mundlak test, admin2-clustered
#
# NOTES
#   - Common sample: events with non-missing admin2, NUTS-3 and all
#     regressors, so AIC/BIC are comparable across specifications.
#   - FE logits drop groups with no police-present events (perfectly
#     predicted). Those observations contribute 0 to the log-likelihood, so
#     log-likelihoods remain comparable; AIC/BIC below count every FE level
#     in the common sample as a parameter and use the full common-sample N.
#   - Clustering on 5 countries is reported for completeness only; with so
#     few clusters those SEs are unreliable.
#   - Severity SEs ignore that the IMR is a generated regressor.
#   - Results are cached per specification (CACHE_FILE); delete the cache to
#     force a full re-run.
# ============================================================================

library(readr)
library(dplyr)
library(fixest)
library(lme4)

DATA_PATH           <- "data/combined/acled_merged_controls_rethresholded_v3.csv"
OUT_DIR             <- "analysis/results"
CACHE_FILE          <- file.path(OUT_DIR, "fe_grid_cache.rds")
THRESHOLD           <- 0.9
CONLEY_KM           <- 50
RUN_CLASSIC_HAUSMAN <- TRUE     # glmer fits; set FALSE for a fast run
RUN_SEVERITY        <- TRUE
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# DATA (identical to 01 / 02, plus spatial and temporal identifiers)
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
    event_date = as.Date(event_date)
  ) %>%
  group_by(country, event_date) %>%
  mutate(protest_load = n() - 1L) %>%          # all events, as in 02
  ungroup() %>%
  mutate(
    log_protest_load = log1p(protest_load),
    # identifiers (prefixed so names never collide across countries)
    region = paste(country, admin1, sep = " | "),
    nuts3  = NUTS_ID,
    city   = paste(country, admin1, location, sep = " | "),
    year_f = factor(year),
    ym     = format(event_date, "%Y-%m"),
    yw     = format(event_date, "%G-W%V")
  )

partisan  <- c("left_pure", "right_pure", "unknown_pure", "counter_protest")
incumbent <- c("incumbent_left", "incumbent_right")
base_ctrl <- c("log_dist_govt_building", "log_dist_major_road", "is_weekend")

id_vars <- c("country", "region", "nuts3", "admin2", "city", "year_f", "ym", "yw",
             "latitude", "longitude")
needed  <- c("police_presence", "arrest", "brutality", partisan, incumbent, base_ctrl,
             "covid", "protestor_violence", "log_protest_load", id_vars)

d <- acled_data[complete.cases(acled_data[, needed]), ]
cat("Common sample:", nrow(d), "of", nrow(acled_data), "events",
    "| police-present:", sum(d$police_presence), "\n")

# ============================================================================
# GRID AND VARIANCE ESTIMATORS
# ============================================================================

spatial_fe  <- c(none = NA, country = "country", region = "region",
                 nuts3 = "nuts3", county = "admin2", city = "city")
temporal_fe <- c(none = NA, year = "year_f", month = "ym", week = "yw")

grid <- expand.grid(spatial = names(spatial_fe), temporal = names(temporal_fe),
                    stringsAsFactors = FALSE)
grid$id <- paste(grid$spatial, grid$temporal, sep = "__")

vcov_list <- list(
  HC      = "hetero",
  country = ~country,
  region  = ~region,
  nuts3   = ~nuts3,
  county  = ~admin2,
  city    = ~city,
  conley  = vcov_conley(lat = "latitude", lon = "longitude", cutoff = CONLEY_KM)
)

# ============================================================================
# HELPERS
# ============================================================================

mk_formula <- function(y, rhs, fe = character(0)) {
  f <- paste(y, "~", paste(rhs, collapse = " + "))
  if (length(fe)) f <- paste(f, "|", paste(fe, collapse = " + "))
  as.formula(f)
}

contrast <- function(b, V) {
  if (!all(c("right_pure", "left_pure") %in% names(b))) return(c(diff = NA, se = NA, p = NA))
  dlt <- unname(b["right_pure"] - b["left_pure"])
  se  <- sqrt(V["right_pure", "right_pure"] + V["left_pure", "left_pure"] -
                2 * V["right_pure", "left_pure"])
  c(diff = dlt, se = se, p = 2 * pnorm(-abs(dlt / se)))
}

contrasts_all_vcov <- function(m) {
  out <- lapply(names(vcov_list), function(v) {
    r <- tryCatch(contrast(coef(m), vcov(m, vcov = vcov_list[[v]])),
                  error = function(e) c(diff = NA, se = NA, p = NA))
    data.frame(vcov = v, diff = r["diff"], se = r["se"], p = r["p"], row.names = NULL)
  })
  do.call(rbind, out)
}

hausman <- function(b_fe, V_fe, b_re, V_re, keep) {
  k  <- intersect(intersect(names(b_fe), names(b_re)), keep)
  dv <- b_fe[k] - b_re[k]
  Vd <- as.matrix(V_fe[k, k]) - as.matrix(V_re[k, k])
  ev <- eigen(Vd, symmetric = TRUE, only.values = TRUE)$values
  df <- sum(ev > 1e-8 * max(abs(ev)))
  H  <- as.numeric(t(dv) %*% MASS::ginv(Vd) %*% dv)
  c(chisq = H, df = df, p = pchisq(H, df, lower.tail = FALSE), not_pd = any(ev < 0))
}

# Mundlak: replace each FE dimension with group means of the regressors;
# joint Wald test (admin2-clustered) that all means are zero.
mundlak <- function(y, rhs, fe, data, family = NULL) {
  if (!length(fe)) return(c(chisq = NA, df = NA, p = NA))
  mvars <- character(0)
  for (g in fe) for (v in rhs) {
    nm <- paste0("m_", g, "_", v)
    data[[nm]] <- ave(data[[v]], data[[g]], FUN = mean)
    mvars <- c(mvars, nm)
  }
  f <- mk_formula(y, c(rhs, mvars))
  m <- if (is.null(family)) feols(f, data = data, cluster = ~admin2)
       else feglm(f, data = data, family = family, cluster = ~admin2)
  w <- wald(m, keep = "^m_", print = FALSE)
  c(chisq = w$stat * w$df1, df = w$df1, p = w$p)
}

# AIC/BIC counting every FE level in the common sample
info_crit <- function(m, fe, data) {
  ll <- as.numeric(logLik(m))
  k  <- length(coef(m))
  if (length(fe)) {
    k <- k + sum(sapply(fe, function(g) length(unique(data[[g]])))) - (length(fe) - 1)
  }
  n <- nrow(data)
  c(logLik = ll, k = k, AIC = -2 * ll + 2 * k, BIC = -2 * ll + log(n) * k)
}

# ============================================================================
# RUN THE GRID
# ============================================================================

cache <- if (file.exists(CACHE_FILE)) readRDS(CACHE_FILE) else list()

for (i in seq_len(nrow(grid))) {
  id <- grid$id[i]
  if (!is.null(cache[[id]])) { cat("[cached]", id, "\n"); next }
  t0 <- Sys.time()

  fe   <- na.omit(c(spatial_fe[grid$spatial[i]], temporal_fe[grid$temporal[i]]))
  fe   <- unname(as.character(fe))
  ctrl <- c(base_ctrl, if (grid$temporal[i] == "none") "covid")
  rhs1 <- c(partisan, incumbent, ctrl)
  cat("\n>>", id, "| FE:", if (length(fe)) paste(fe, collapse = " + ") else "none", "\n")

  res <- list(spatial = grid$spatial[i], temporal = grid$temporal[i])

  # ---- Model 1: presence logit ----------------------------------------------
  m1 <- feglm(mk_formula("police_presence", rhs1, fe), data = d,
              family = binomial("logit"))
  res$n_used    <- nobs(m1)
  res$ic        <- info_crit(m1, fe, d)
  res$coefs     <- coef(m1)[intersect(partisan, names(coef(m1)))]
  res$presence  <- contrasts_all_vcov(m1)
  res$mundlak   <- mundlak("police_presence", rhs1, fe, d, family = binomial("logit"))

  res$hausman <- c(chisq = NA, df = NA, p = NA, not_pd = NA)
  if (RUN_CLASSIC_HAUSMAN && length(fe)) {
    d_fe <- d[obs(m1), ]
    re_f <- as.formula(paste("police_presence ~", paste(rhs1, collapse = " + "), "+",
                             paste0("(1 | ", fe, ")", collapse = " + ")))
    re <- tryCatch(
      glmer(re_f, data = d_fe, family = binomial, nAGQ = 0,
            control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)),
      error = function(e) { message("  glmer failed: ", conditionMessage(e)); NULL })
    if (!is.null(re)) {
      res$hausman <- hausman(coef(m1), vcov(m1, vcov = "iid"),
                             fixef(re), as.matrix(vcov(re)), keep = rhs1)
    }
  }

  # ---- Heckman outcome equations with the same FE ---------------------------
  if (RUN_SEVERITY) {
    sel <- feglm(mk_formula("police_presence",
                            c(rhs1, "protestor_violence", "log_protest_load"), fe),
                 data = d, family = binomial("probit"))
    xb <- predict(sel, newdata = d, type = "link")
    d$imr <- dnorm(xb) / pnorm(xb)
    dp <- d[d$police_presence == 1 & !is.na(d$imr), ]
    rhs2 <- c(partisan, ctrl, "protestor_violence", "imr")

    for (y in c("arrest", "brutality")) {
      m2 <- feols(mk_formula(y, rhs2, fe), data = dp)
      res[[y]] <- contrasts_all_vcov(m2)
      res[[paste0(y, "_mundlak")]] <- tryCatch(mundlak(y, rhs2, fe, dp),
                                               error = function(e) c(chisq = NA, df = NA, p = NA))
    }
    res$n_present <- nrow(dp)
  }

  cache[[id]] <- res
  saveRDS(cache, CACHE_FILE)
  cat("   done in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
}

# ============================================================================
# ASSEMBLE RESULTS
# ============================================================================

lab_sp <- c(none = "None", country = "Country", region = "Region (admin1)",
            nuts3 = "NUTS-3", county = "County (admin2)", city = "City")
lab_tm <- c(none = "None", year = "Year", month = "Year-month", week = "Year-week")

pick <- function(df, v) df[df$vcov == v, ]

summary_tab <- do.call(rbind, lapply(grid$id, function(id) {
  r  <- cache[[id]]
  pc <- pick(r$presence, "county")
  data.frame(
    id = id, spatial = lab_sp[r$spatial], temporal = lab_tm[r$temporal],
    n_used = r$n_used, k = r$ic["k"], AIC = r$ic["AIC"], BIC = r$ic["BIC"],
    hausman_chisq = r$hausman["chisq"], hausman_df = r$hausman["df"],
    hausman_p = r$hausman["p"], hausman_not_pd = r$hausman["not_pd"],
    mundlak_chisq = r$mundlak["chisq"], mundlak_df = r$mundlak["df"], mundlak_p = r$mundlak["p"],
    left = r$coefs["left_pure"], right = r$coefs["right_pure"],
    rl_diff = pc$diff, rl_se_county = pc$se, rl_p_county = pc$p,
    row.names = NULL)
}))
summary_tab$best_AIC <- summary_tab$AIC == min(summary_tab$AIC)
summary_tab$best_BIC <- summary_tab$BIC == min(summary_tab$BIC)

se_tab <- do.call(rbind, lapply(grid$id, function(id) {
  r <- cache[[id]]
  cbind(id = id, outcome = "presence", r$presence)
}))
if (RUN_SEVERITY) {
  se_tab <- rbind(se_tab, do.call(rbind, lapply(grid$id, function(id) {
    r <- cache[[id]]
    rbind(cbind(id = id, outcome = "arrest",    r$arrest),
          cbind(id = id, outcome = "brutality", r$brutality))
  })))
}

sev_tab <- NULL
if (RUN_SEVERITY) {
  sev_tab <- do.call(rbind, lapply(grid$id, function(id) {
    r <- cache[[id]]
    a <- pick(r$arrest, "county"); b <- pick(r$brutality, "county")
    data.frame(id = id, spatial = lab_sp[r$spatial], temporal = lab_tm[r$temporal],
               n_present = r$n_present,
               arrest_diff = a$diff, arrest_se = a$se, arrest_p = a$p,
               arrest_mundlak_p = r$arrest_mundlak["p"],
               brut_diff = b$diff, brut_se = b$se, brut_p = b$p,
               brut_mundlak_p = r$brutality_mundlak["p"], row.names = NULL)
  }))
}

write.csv(summary_tab, file.path(OUT_DIR, "fe_grid_presence_summary.csv"), row.names = FALSE)
write.csv(se_tab,      file.path(OUT_DIR, "fe_grid_contrasts_by_vcov.csv"), row.names = FALSE)
if (!is.null(sev_tab))
  write.csv(sev_tab, file.path(OUT_DIR, "fe_grid_severity_summary.csv"), row.names = FALSE)

cat("\n=== PRESENCE: SPECIFICATION GRID ===\n")
print(summary_tab[, c("spatial", "temporal", "n_used", "AIC", "BIC", "hausman_p",
                      "mundlak_p", "rl_diff", "rl_se_county", "rl_p_county",
                      "best_AIC", "best_BIC")], row.names = FALSE, digits = 4)
cat("\nLowest AIC:", summary_tab$id[summary_tab$best_AIC],
    "| Lowest BIC:", summary_tab$id[summary_tab$best_BIC], "\n")

# ============================================================================
# LATEX TABLES
# ============================================================================

fmt  <- function(x, d = 3) ifelse(is.na(x), "--", formatC(x, format = "f", digits = d))
fmtp <- function(p) ifelse(is.na(p), "--", ifelse(p < 0.001, "$<$0.001", formatC(p, format = "f", digits = 3)))
stars <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))
row_label <- function(s, t, best, paper) {
  lab <- paste0(s, " & ", t)
  if (paper) lab <- paste0(s, "$^\\dagger$ & ", t)
  lab
}

write_tex <- function(lines, file) writeLines(lines, file.path(OUT_DIR, file))

# Table A: presence specification grid
tA <- c("\\begin{table}[htbp]\\centering\\footnotesize",
        "\\caption{Police Presence: Fixed-Effects Specification Grid}\\label{tab:fe_grid}",
        "\\begin{tabular}{llrrrccc}\\toprule",
        "Spatial FE & Temporal FE & $N$ used & AIC & BIC & Hausman $p$ & Mundlak $p$ & Right $-$ left (SE) \\\\ \\midrule")
for (j in seq_len(nrow(summary_tab))) {
  r <- summary_tab[j, ]
  paper <- r$id == "country__none"
  aic <- fmt(r$AIC, 1); bic <- fmt(r$BIC, 1)
  if (r$best_AIC) aic <- paste0("\\textbf{", aic, "}")
  if (r$best_BIC) bic <- paste0("\\textbf{", bic, "}")
  tA <- c(tA, paste0(row_label(r$spatial, r$temporal, FALSE, paper), " & ",
                     format(r$n_used, big.mark = ","), " & ", aic, " & ", bic, " & ",
                     fmtp(r$hausman_p), " & ", fmtp(r$mundlak_p), " & ",
                     fmt(r$rl_diff), stars(r$rl_p_county), " (", fmt(r$rl_se_county), ") \\\\"))
}
tA <- c(tA, "\\bottomrule\\end{tabular}",
        "\\begin{minipage}{0.95\\linewidth}\\footnotesize",
        paste0("\\textit{Note:} Logit of police presence (classifier probability $\\geq$ 0.9) on the Model 1 ",
               "specification; the Covid indicator is omitted when a temporal fixed effect is included. ",
               "All models are estimated on a common sample; $N$ used excludes groups with no police-present ",
               "events, which are perfectly predicted. AIC and BIC count every fixed-effect level as a parameter; ",
               "lowest values in bold. Hausman: fixed vs.\\ random intercepts for the same groupings (model-based ",
               "variance). Mundlak: joint Wald test on group means, admin2-clustered. Right $-$ left contrast with ",
               "admin2-clustered SEs. $^\\dagger$Specification reported in the main text. ",
               "***$p<0.001$, **$p<0.01$, *$p<0.05$."),
        "\\end{minipage}\\end{table}")
write_tex(tA, "tableA_fe_grid_presence.tex")

# Table B: right - left p-values across variance estimators (presence)
vlab <- c(HC = "HC", country = "Country", region = "Region", nuts3 = "NUTS-3",
          county = "County", city = "City", conley = paste0("Conley ", CONLEY_KM, "km"))
tB <- c("\\begin{table}[htbp]\\centering\\footnotesize",
        "\\caption{Police Presence: Right $-$ Left Contrast under Alternative Standard Errors}\\label{tab:fe_grid_se}",
        paste0("\\begin{tabular}{ll", strrep("c", length(vlab)), "}\\toprule"),
        paste0("Spatial FE & Temporal FE & ", paste(vlab, collapse = " & "), " \\\\ \\midrule"))
for (id in grid$id) {
  r <- cache[[id]]; pr <- r$presence
  cells <- sapply(names(vlab), function(v) {
    x <- pick(pr, v); paste0(fmt(x$se), stars(x$p))
  })
  tB <- c(tB, paste0(row_label(lab_sp[r$spatial], lab_tm[r$temporal], FALSE, id == "country__none"),
                     " & ", paste(cells, collapse = " & "), " \\\\"))
}
tB <- c(tB, "\\bottomrule\\end{tabular}",
        "\\begin{minipage}{0.95\\linewidth}\\footnotesize",
        paste0("\\textit{Note:} Cells report the standard error of the right $-$ left coefficient contrast ",
               "(point estimates in Table~\\ref{tab:fe_grid}) under each variance estimator, with significance ",
               "stars. HC: heteroskedasticity-robust. Country clustering uses only five clusters and is shown for ",
               "completeness. Conley: spatial HAC with a ", CONLEY_KM, "km cutoff. $^\\dagger$Main-text specification."),
        "\\end{minipage}\\end{table}")
write_tex(tB, "tableB_fe_grid_se.tex")

# Table C: severity
if (!is.null(sev_tab)) {
  tC <- c("\\begin{table}[htbp]\\centering\\footnotesize",
          "\\caption{Arrest and Brutality: Fixed-Effects Specification Grid (Heckman Outcome Equations)}\\label{tab:fe_grid_sev}",
          "\\begin{tabular}{llrcccc}\\toprule",
          " & & & \\multicolumn{2}{c}{Arrest} & \\multicolumn{2}{c}{Brutality} \\\\ \\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
          "Spatial FE & Temporal FE & $N$ & R $-$ L (SE) & Mundlak $p$ & R $-$ L (SE) & Mundlak $p$ \\\\ \\midrule")
  for (j in seq_len(nrow(sev_tab))) {
    r <- sev_tab[j, ]
    tC <- c(tC, paste0(row_label(r$spatial, r$temporal, FALSE, r$id == "country__none"), " & ",
                       format(r$n_present, big.mark = ","), " & ",
                       fmt(r$arrest_diff), stars(r$arrest_p), " (", fmt(r$arrest_se), ") & ",
                       fmtp(r$arrest_mundlak_p), " & ",
                       fmt(r$brut_diff), stars(r$brut_p), " (", fmt(r$brut_se), ") & ",
                       fmtp(r$brut_mundlak_p), " \\\\"))
  }
  tC <- c(tC, "\\bottomrule\\end{tabular}",
          "\\begin{minipage}{0.95\\linewidth}\\footnotesize",
          paste0("\\textit{Note:} Linear probability outcome equations on police-present events, with the inverse ",
                 "Mills ratio from a probit selection equation that includes the same fixed effects and the ",
                 "same-day protest-load exclusion restriction. Right $-$ left contrast with admin2-clustered SEs, ",
                 "which do not account for the generated regressor. $^\\dagger$Main-text specification ",
                 "(country fixed effects; main-text SEs are bootstrapped)."),
          "\\end{minipage}\\end{table}")
  write_tex(tC, "tableC_fe_grid_severity.tex")
}

cat("\nWrote:", paste(list.files(OUT_DIR, pattern = "^(fe_grid|table[ABC]_fe_grid)"), collapse = ", "), "\n")
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "fe_grid_sessionInfo.txt"))
cat("\n=== FE specification grid complete ===\n")
