# Title: Spain Partisanship Classification - Classification Pipeline + Corrections
# Description: NLP-based bootstrap to classify unknown actors in SPAIN ACLED
#              data, followed by two rounds of regex-based rule correction.
#              Round 1 (vox/occupational) was already validated; Round 2 below
#              adds five new corrections identified through manual validation
#              of the round-3 sample.

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(reticulate)
library(writexl)

# One-time setup - comment out after first run
# reticulate::py_install("transformers")
# reticulate::py_install("torch")

# =============================================================================
# STEP 0: LOAD BASE DATA
# =============================================================================

spain_acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/spain_acled_partisan_classification_2020_2024.csv")

# =============================================================================
# STEP 1: IDENTIFY MIXED EVENTS
# =============================================================================

spain_acled_df <- spain_acled_df %>%
  mutate(
    has_classified = as.integer(
      assoc_actor1_left == 1 | assoc_actor1_right == 1 | assoc_actor1_centre == 1 |
        assoc_actor2_left == 1 | assoc_actor2_right == 1 | assoc_actor2_centre == 1
    ),
    has_unknown = as.integer(
      assoc_actor1_unknown == 1 | assoc_actor2_unknown == 1
    ),
    mixed = as.integer(has_classified == 1 & has_unknown == 1)
  )

cat("Mixed events for NLP classification:", sum(spain_acled_df$mixed == 1), "\n")

# =============================================================================
# STEP 2: ZERO-SHOT TEXT CLASSIFICATION
# =============================================================================

transformers <- import("transformers")
torch <- import("torch")
torch$set_num_threads(as.integer(parallel::detectCores() %/% 2))

classifier <- transformers$pipeline(
  "zero-shot-classification",
  model = "facebook/bart-large-mnli"
)

candidate_labels <- c(
  "a left-wing protest about workers rights, trade union organising,
   anti-racism, feminism, housing rights, Palestine solidarity, regional
   autonomy, environmental activism, antifascist action, or anti-austerity",
  "a right-wing protest about immigration, Vox politics, Spanish nationalism,
   anti-feminism, coronavirus restrictions, vaccine mandates, traditional
   Catholic values, anti-Islam positions, or far-right and identitarian
   movements",
  "a local community protest about a specific planning or infrastructure
   decision with no broader political ideology"
)
label_short <- c("left", "right", "centre")
score_threshold <- 0.45

classify_text_multi <- function(text, classifier, labels, threshold, short_names) {
  tryCatch({
    result <- classifier(text, labels, multi_label = TRUE)
    scores <- setNames(unlist(result$scores), unlist(result$labels))
    
    active <- character(0)
    for (i in seq_along(labels)) {
      if (scores[labels[i]] >= threshold) active <- c(active, short_names[i])
    }
    
    partisan_type <- if (length(active) == 0) "unknown" else paste(sort(active), collapse = "_and_")
    data.frame(predicted_partisan_type = partisan_type, confidence = max(unlist(result$scores)),
               stringsAsFactors = FALSE)
  }, error = function(e) {
    data.frame(predicted_partisan_type = "unknown", confidence = 0, stringsAsFactors = FALSE)
  })
}

unknown_events <- spain_acled_df %>%
  filter(event_partisan_type == "unknown") %>%
  filter(!is.na(notes) & str_length(notes) > 20)

cat("Events to classify:", nrow(unknown_events), "\n")

text_classifications <- unknown_events %>%
  mutate(
    classification_result   = map(notes, ~classify_text_multi(.x, classifier, candidate_labels, score_threshold, label_short)),
    predicted_partisan_type = map_chr(classification_result, "predicted_partisan_type"),
    confidence               = map_dbl(classification_result, "confidence"),
    propagation_source       = if_else(predicted_partisan_type != "unknown", "text_classification", "unknown")
  ) %>%
  select(event_id_cnty, predicted_partisan_type, confidence, propagation_source)

# =============================================================================
# STEP 3: MERGE AND PRODUCE FINAL CLASSIFICATION
# =============================================================================

spain_acled_df <- spain_acled_df %>%
  left_join(text_classifications %>% select(event_id_cnty, predicted_partisan_type, confidence, propagation_source),
            by = "event_id_cnty") %>%
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

cat("\nFinal classification breakdown:\n")
spain_acled_df %>%
  count(event_partisan_type_final, classification_source) %>%
  arrange(desc(n)) %>%
  print()

cat("\nRemaining unknowns:", sum(spain_acled_df$event_partisan_type_final == "unknown"), "\n")

write_csv(spain_acled_df, "~/Downloads/spain_acled_partisan_classification_bootstrapped3.csv")

set.seed(42)
random_sample <- spain_acled_df %>%
  filter(classification_source == "text_classification") %>%
  slice_sample(n = 100)
write_xlsx(random_sample, "~/Documents/spain_text_class_random_sample3.xlsx")

# =============================================================================
# STEP 4: RULE CORRECTION - ROUND 1 (vox / occupational)
# =============================================================================

spain_acled_df <- spain_acled_df %>%
  mutate(
    matched_vox_to_left = (
      str_detect(notes, regex(
        paste0(
          "vox|catalan alliance|aliança catalana|orriols|stop mare mortum|",
          "fascis|immigration forum|expulsion order|melilla"
        ),
        ignore_case = TRUE
      )) &
        str_detect(notes, regex(
          "against|protest|oppos|counter|anti|condemn|jeer|complain|reject|denounc",
          ignore_case = TRUE
        )) &
        event_partisan_type_final == "right"
    ),
    matched_occupational_to_centre = (
      str_detect(notes, regex(
        paste0(
          "police officer|ertzaintza|transport worker|camionero|trucker|",
          "market worker|vendedor ambulante|hospitality|hostelería"
        ),
        ignore_case = TRUE
      )) &
        str_detect(notes, regex(
          "salary|wage|working conditions|pay|labor agreement|convenio|economic situation",
          ignore_case = TRUE
        )) &
        event_partisan_type_final == "left"
    ),
    event_partisan_type_final = case_when(
      matched_vox_to_left            ~ "left",
      matched_occupational_to_centre ~ "centre",
      TRUE ~ event_partisan_type_final
    ),
    classification_source = case_when(
      matched_vox_to_left            ~ "rule_correction",
      matched_occupational_to_centre ~ "rule_correction",
      TRUE ~ classification_source
    )
  ) %>%
  select(-matched_vox_to_left, -matched_occupational_to_centre)

# =============================================================================
# STEP 5: RULE CORRECTION - ROUND 2 (from round-3 sample manual validation)
# =============================================================================

# R1: self-employed "turno de oficio"-style lawyers miscoded as left -> centre.
# Excludes "administration of justice" / LAJ, who ARE civil servants and
# correctly stay left (confirmed separately: LAJ = funcionarios publicos,
# not independent liberal professionals like turno de oficio lawyers).
#
# R2: business/professional-sector tax-or-fee relief demands (VAT reduction,
# fuel-price relief, "decrease taxation") miscoded as left -> centre. Same
# structure as the French farmer/liberal-profession corrections: a sector-wide
# economic relief ask, not an employer-vs-employee labor dispute.
#
# R3: named non-partisan regional-equity infrastructure platforms (Milana
# Bonita, No al Muro) miscoded as right -> centre. Both are explicitly
# self-described non-partisan civic platforms (confirmed via search for
# Milana Bonita); their claims are territorial-parity, not ideological.
#
# R4: protests against the PSOE-Junts/ERC amnesty deal miscoded as left ->
# right. This is a defining right/unionist mobilization in Spanish politics
# (opposition to Catalan independence-linked concessions).
#
# R5: protests celebrating/defending that same amnesty for Catalan
# independence activists miscoded as right -> left. Civil-liberties /
# anti-repression framing, the opposite side of the same conflict as R4.
#
# R6: anti-racism / pro-migrant-rights protests miscoded as right -> left.

spain_acled_df <- spain_acled_df %>%
  mutate(
    matched_lawyers_to_centre = (
      str_detect(notes, regex("lawyer", ignore_case = TRUE)) &
        !str_detect(notes, regex("administration of justice|\\bLAJ\\b", ignore_case = TRUE)) &
        str_detect(notes, regex(
          "salary increase|maternity|paternity|dignified (job|work)|duty shift|pension|working conditions",
          ignore_case = TRUE
        )) &
        event_partisan_type_final == "left"
    ),
    matched_sector_relief_to_centre = (
      str_detect(notes, regex(
        "decrease taxation|reduce the vat|vat rate|financial aid|high costs? of fuel|high fuel prices",
        ignore_case = TRUE
      )) &
        event_partisan_type_final == "left"
    ),
    matched_named_platform_to_centre = (
      str_detect(notes, regex("milana bonita|no al muro", ignore_case = TRUE)) &
        event_partisan_type_final == "right"
    ),
    matched_amnesty_opposition_to_right = (
      str_detect(notes, regex("amnesty", ignore_case = TRUE)) &
        str_detect(notes, regex("against the deal|junts|erc", ignore_case = TRUE)) &
        event_partisan_type_final == "left"
    ),
    matched_amnesty_solidarity_to_left = (
      str_detect(notes, regex("amnesty", ignore_case = TRUE)) &
        str_detect(notes, regex("celebrat|catalan nationalist|granollers", ignore_case = TRUE)) &
        event_partisan_type_final == "right"
    ),
    matched_antiracism_to_left = (
      str_detect(notes, regex("anti-racis|against racism|racism", ignore_case = TRUE)) &
        str_detect(notes, regex("migrant|refugee|immigra", ignore_case = TRUE)) &
        event_partisan_type_final == "right"
    ),
    
    event_partisan_type_final = case_when(
      matched_lawyers_to_centre           ~ "centre",
      matched_sector_relief_to_centre     ~ "centre",
      matched_named_platform_to_centre    ~ "centre",
      matched_amnesty_opposition_to_right ~ "right",
      matched_amnesty_solidarity_to_left  ~ "left",
      matched_antiracism_to_left          ~ "left",
      TRUE ~ event_partisan_type_final
    ),
    classification_source = case_when(
      matched_lawyers_to_centre           ~ "rule_correction",
      matched_sector_relief_to_centre     ~ "rule_correction",
      matched_named_platform_to_centre    ~ "rule_correction",
      matched_amnesty_opposition_to_right ~ "rule_correction",
      matched_amnesty_solidarity_to_left  ~ "rule_correction",
      matched_antiracism_to_left          ~ "rule_correction",
      TRUE ~ classification_source
    )
  ) %>%
  select(-matched_lawyers_to_centre, -matched_sector_relief_to_centre,
         -matched_named_platform_to_centre, -matched_amnesty_opposition_to_right,
         -matched_amnesty_solidarity_to_left, -matched_antiracism_to_left)

# NOT auto-corrected: the "Ence" jobs-vs-environmental-regulation cases.
# Two near-identical Ence protests (Pontevedra Feb 2021, Madrid Chamberi
# March 2021) received DIFFERENT correct answers in manual validation (left
# vs left_and_right) - the underlying claim genuinely straddles the
# left/right line (job protection + anti-green-regulation), and a blanket
# regex rule would be wrong roughly half the time. Flagging these for manual
# review instead of forcing an automated call:
ence_review <- spain_acled_df %>%
  filter(str_detect(notes, regex("\\bence\\b", ignore_case = TRUE)))
cat("\nEnce-related rows flagged for manual review (not auto-corrected):", nrow(ence_review), "\n")

# =============================================================================
# DIAGNOSTICS
# =============================================================================

cat("\nClassification source breakdown:\n")
print(table(spain_acled_df$classification_source))

cat("\nFinal partisan type breakdown:\n")
print(table(spain_acled_df$event_partisan_type_final))

discrepancy <- spain_acled_df %>%
  filter(classification_source == "rule_correction" & event_partisan_type_final == "unknown")
cat("\nRule-corrected rows still showing 'unknown' (should be 0):", nrow(discrepancy), "\n")

# =============================================================================
# SAVE & VALIDATION SAMPLE
# =============================================================================

write_csv(spain_acled_df, "~/Downloads/spain_acled_partisan_classification_bootstrapped7.csv")

random_sample <- spain_acled_df %>%
  filter(classification_source %in% c("text_classification", "rule_correction")) %>%
  slice_sample(n = 100)

cat("\nSample composition:\n")
print(table(random_sample$classification_source))

write_xlsx(random_sample, "~/Documents/spain_text_class_random_sample7.xlsx")
