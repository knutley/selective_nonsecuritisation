# Title: France Partisanship Classification - Addendum (NLP Bootstrap)
# Author: Katelyn Nutley
# Date: 23-02-2026 (cleaned up, keeping final classification round + rule-
#       correction pass only; earlier bootstrap1/bootstrap2 rounds and the
#       superseded bootstrap4 correction pass have been removed)
# Description: NLP-based bootstrap to classify unknown actors in FRANCE ACLED
#              data using zero-shot classification on the notes field.
#              Requires: france_acled_partisan_classification_2020_2024.csv
#              from the main classification script.

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(reticulate)
library(writexl)

reticulate::py_install("transformers")
reticulate::py_install("torch")

# Load output from main classification script
france_acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/france_acled_partisan_classification_2020_2024.csv")

# ------------------------------------------------------------------------
# NOTE: has_classified / has_unknown / mixed were computed in the original
# script (Step 1) but never referenced by the actual classification logic
# below, which filters purely on event_partisan_type == "unknown" and notes
# length. Left out here as dead code -- reinstate if you're planning to use
# the "mixed event" flag for something downstream that isn't shown here.
# ------------------------------------------------------------------------

# ============================================================================
# SETUP: ZERO-SHOT CLASSIFIER
# ============================================================================

transformers <- import("transformers")
torch <- import("torch")

classifier <- transformers$pipeline(
  "zero-shot-classification",
  model = "facebook/bart-large-mnli"
)

classify_text_multi <- function(text, classifier, labels, threshold, short_names) {
  tryCatch({
    result <- classifier(text, labels, multi_label = TRUE)
    scores <- setNames(unlist(result$scores), unlist(result$labels))
    
    active <- character(0)
    for (i in seq_along(labels)) {
      if (scores[labels[i]] >= threshold) {
        active <- c(active, short_names[i])
      }
    }
    
    partisan_type <- if (length(active) == 0) {
      "unknown"
    } else {
      paste(sort(active), collapse = "_and_")
    }
    
    data.frame(
      predicted_partisan_type = partisan_type,
      confidence = max(unlist(result$scores)),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(predicted_partisan_type = "unknown", confidence = 0,
               stringsAsFactors = FALSE)
  })
}

# ============================================================================
# CLASSIFICATION ROUND (final candidate labels, matches paper Appendix 3)
# ============================================================================

candidate_labels <- c(
  "a left-wing protest about pensions, trade unions, pay, public
   services, healthcare workers, teachers, LGBTQI+ rights, feminism,
   anti-racism, Palestine, the environment, or anti-austerity",
  
  "a right-wing protest about immigration, the passe sanitaire,
   vaccine mandates, nationalism, anti-Islam, hunting rights,
   law and order, or EU farming regulations",
  
  "a local community protest about a specific planning or
   infrastructure decision with no broader political ideology"
)

label_short <- c("left", "right", "centre")
score_threshold <- 0.45

unknown_events <- france_acled_df %>%
  filter(event_partisan_type == "unknown") %>%
  filter(!is.na(notes) & str_length(notes) > 20)

cat("Events to classify:", nrow(unknown_events), "\n")

text_classifications <- unknown_events %>%
  mutate(
    classification_result = map(
      notes,
      ~classify_text_multi(.x, classifier, candidate_labels, score_threshold, label_short)
    ),
    predicted_partisan_type = map_chr(classification_result, "predicted_partisan_type"),
    confidence              = map_dbl(classification_result, "confidence"),
    propagation_source = if_else(
      predicted_partisan_type != "unknown",
      "text_classification",
      "unknown"
    )
  ) %>%
  select(event_id_cnty, predicted_partisan_type, confidence, propagation_source)

france_acled_df <- france_acled_df %>%
  left_join(
    text_classifications %>% select(event_id_cnty, predicted_partisan_type, confidence, propagation_source),
    by = "event_id_cnty"
  ) %>%
  mutate(
    event_partisan_type_final = case_when(
      event_partisan_type != "unknown" ~ event_partisan_type,
      !is.na(predicted_partisan_type) & predicted_partisan_type != "unknown" ~ predicted_partisan_type,
      TRUE ~ "unknown"
    ),
    classification_source = case_when(
      event_partisan_type != "unknown" ~ "original",
      propagation_source == "text_classification" ~ "text_classification",
      TRUE ~ "unknown"
    )
  ) %>%
  select(-propagation_source)

# Normalise compound category naming (e.g., left_only -> left; alphabetise combos)
normalize_partisan_type <- function(x) {
  x <- str_remove(x, "_only$")
  parts <- str_split(x, "_and_")
  sapply(parts, function(p) paste(sort(p), collapse = "_and_"))
}

france_acled_df <- france_acled_df %>%
  mutate(event_partisan_type_final_norm = normalize_partisan_type(event_partisan_type_final))

cat("\nFinal classification breakdown:\n")
france_acled_df %>%
  count(event_partisan_type_final_norm, classification_source) %>%
  arrange(desc(n)) %>%
  print()

cat("\nRemaining unknowns:", sum(france_acled_df$event_partisan_type_final == "unknown"), "\n")

write_csv(france_acled_df, "~/Downloads/france_acled_partisan_classification_bootstrapped3_2020_2024.csv")

# --- Validation sample 3 ---
text_classified_obs <- france_acled_df %>%
  filter(classification_source == "text_classification")
random_sample <- text_classified_obs %>% slice_sample(n = 100)
write_xlsx(random_sample, "~/Documents/france_text_class_random_sample3_2020_2024.xlsx")

# ============================================================================
# RULE-CORRECTION PASS (final version, with discrepancy check -> bootstrap5)
# ============================================================================
# FIX: was reading "bootstrapped3.csv" (no "_2020_2024" suffix), which does
# not match the file just written above. Corrected to read the file that was
# actually saved, so this pass runs on the current 2020-2024 data rather than
# risking a silent read of a stale file from an earlier run.

france_acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/France/france_acled_partisan_classification_bootstrapped3_2020_2024.csv")

france_acled_df <- france_acled_df %>%
  mutate(
    matched_rule_left = (
      str_detect(notes, regex(
        "xenophob|racis|anti-immigration bill|passe sanitaire|loi immigration",
        ignore_case = TRUE
      )) &
        str_detect(notes, regex(
          "against|protest|oppos|counter|anti|condemn|reject|denounc|call on|deemed",
          ignore_case = TRUE
        )) &
        event_partisan_type_final == "right" &
        classification_source != "original"
    ),
    matched_rule_centre = (
      str_detect(notes, regex(
        "lawyer|avocat|barrister|ambulance|court clerk|greffier|notaire|notary",
        ignore_case = TRUE
      )) &
        str_detect(notes, regex(
          "salary|wage|pay|conditions|status|resources|regime|pension",
          ignore_case = TRUE
        )) &
        event_partisan_type_final == "left" &
        classification_source != "original"
    ),
    event_partisan_type_final = case_when(
      matched_rule_left   ~ "left",
      matched_rule_centre ~ "centre",
      TRUE ~ event_partisan_type_final
    ),
    classification_source = case_when(
      matched_rule_left   ~ "rule_correction",
      matched_rule_centre ~ "rule_correction",
      TRUE ~ classification_source
    )
  ) %>%
  select(-matched_rule_left, -matched_rule_centre)

# Sanity check: no rows should have a real classification_source but a
# still-"unknown" final label
discrepancy <- france_acled_df %>%
  filter(classification_source != "unknown" & event_partisan_type_final == "unknown")
cat("Discrepancy rows remaining:", nrow(discrepancy), "\n")

table(france_acled_df$classification_source)
table(france_acled_df$event_partisan_type_final)

write_csv(france_acled_df, "~/Downloads/france_acled_partisan_classification_bootstrapped5_2020_2024.csv")

# --- Validation sample 5 ---
# FIX: `== c("text_classification", "rule_correction")` recycles the vector
# instead of checking membership, silently dropping ~half the intended rows.
# Corrected to %in%.
random_sample <- france_acled_df %>%
  filter(classification_source %in% c("text_classification", "rule_correction")) %>%
  slice_sample(n = 100)

write_xlsx(random_sample, "~/Documents/france_text_class_random_sample5_2020_2024.xlsx")