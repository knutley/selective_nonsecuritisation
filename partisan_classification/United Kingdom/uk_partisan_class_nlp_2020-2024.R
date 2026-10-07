# Title: UK Partisanship Classification - NLP Addendum + Rule Correction
# Description: NLP-based bootstrap to classify unknown actors in UK ACLED
#              data (picks up from uk_acled_partisan_classification_2020_2024.csv,
#              the output of the main actor-based classification script),
#              followed by rule corrections built from manual validation of
#              the round-3 sample.

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
# STEP 0: LOAD BASE DATA (output of the main actor-classification script)
# =============================================================================

uk_acled_df <- read_csv("~/Downloads/uk_acled_partisan_classification_2020_2024.csv")

# =============================================================================
# STEP 1: IDENTIFY MIXED EVENTS
# =============================================================================

uk_acled_df <- uk_acled_df %>%
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

cat("Mixed events for NLP classification:", sum(uk_acled_df$mixed == 1), "\n")

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
  "a left-wing protest about workers rights, trade union organising
   including NHS pay disputes, housing rights, anti-Brexit or pro-EU
   campaigning, anti-racism, anti-fascism, environmental activism,
   animal rights, Palestine solidarity, or migrant and refugee rights",
  "a right-wing protest about immigration or asylum seekers, pro-Brexit
   or nationalist sovereignty, coronavirus lockdown or vaccine mandate
   opposition, anti-green regulation such as Clean Air Zones, law and
   order, or far-right and identitarian movements",
  "a local community protest about a specific planning, infrastructure,
   or amenity decision with no broader political ideology"
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

unknown_events <- uk_acled_df %>%
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

uk_acled_df <- uk_acled_df %>%
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
uk_acled_df %>%
  count(event_partisan_type_final, classification_source) %>%
  arrange(desc(n)) %>%
  print()

cat("\nRemaining unknowns:", sum(uk_acled_df$event_partisan_type_final == "unknown"), "\n")

write_csv(uk_acled_df, "~/Downloads/uk_acled_partisan_classification_bootstrapped3_2020_2024.csv")

random_sample <- uk_acled_df %>%
  filter(classification_source == "text_classification") %>%
  slice_sample(n = 100)
write_xlsx(random_sample, "~/Documents/uk_text_class_random_sample3_2020_2024.xlsx")

# =============================================================================
# STEP 4: RULE CORRECTION - built from manual validation of the round-3 sample
# =============================================================================

# R1: protests opposing local asylum-seeker housing/hotel placements miscoded
# as centre -> right. Distinguished from R3 (below) by requiring the CURRENT
# value to be centre, not right - avoids any collision with pro-migrant rows.
#
# R2: anti-Brexit protests miscoded as right -> left. Very consistent pattern
# across this dataset - includes named campaign groups (Led By Donkeys,
# "Missing EU Already").
#
# R3: pro-refugee / anti-hostile-environment / anti-deportation-bill protests
# miscoded as right -> left. Distinguished from anti-immigration protests by
# requiring solidarity/rights language, not opposition-to-presence language.
#
# R4: protests explicitly against a far-right figure/event (chanting
# "fascists out", condemning a racist post) miscoded as right -> left.
#
# R5: far-right protest WITH an explicit counter-protest present -> left_and_right,
# same counter-mobilization logic used for Germany's Querdenken corrections.
#
# R6: environmental-activism protests (explicit ecological/habitat framing,
# not generic local NIMBY language) miscoded as centre -> left.
#
# R7: Scottish civic-nationalist protests (independence calls, "England get
# out of Scotland" banner cases) miscoded as right -> left. Deliberately
# EXCLUDES "Action for Scotland" / tourist-exclusion framing, which reads
# closer to exclusionary ethno-territorial framing than civic nationalism -
# left for manual review rather than auto-corrected.
#
# R8: opposition to a Clean Air Zone / low-emission vehicle charging scheme
# miscoded as pure centre -> centre_and_right, matching the established
# anti-green-regulation-as-right pattern from the Germany farmer corrections.
#
# R9: anti-lockdown / anti-vaccine-passport convoy protest miscoded as left -> right.
#
# R10: Criminal Bar Association barrister strikes miscoded as left -> centre.
# UK criminal barristers doing legal aid work are self-employed, paid per-case
# via government-set fee schedules, not salaried employees of Chambers -
# structurally identical to the Spanish "turno de oficio" lawyers case.

uk_acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/UK/uk_acled_partisan_classification_bootstrapped3_2020_2024.csv")

uk_acled_df <- uk_acled_df %>%
  mutate(
    matched_asylum_housing_to_right = (
      str_detect(notes, regex("asylum seeker|asylum-seeker", ignore_case = TRUE)) &
        str_detect(notes, regex(
          "oppos|against the housing|impact on local resources|ahead of the arrival",
          ignore_case = TRUE
        )) &
        event_partisan_type_final == "centre"
    ),
    matched_antibrexit_to_left = (
      str_detect(notes, regex("brexit", ignore_case = TRUE)) &
        str_detect(notes, regex(
          "against|anti-brexit|oppose|protest against|handling of the uk|delays at the port|led by donkeys|missing eu already",
          ignore_case = TRUE
        )) &
        event_partisan_type_final == "right"
    ),
    matched_prorefugee_to_left = (
      str_detect(notes, regex("asylum|refugee|migrant", ignore_case = TRUE)) &
        str_detect(notes, regex(
          "hostile environment|criminaliz|permanent changes|racist and brutal|right to remain|these walls must fall",
          ignore_case = TRUE
        )) &
        event_partisan_type_final == "right"
    ),
    matched_antifarright_to_left = (
      str_detect(notes, regex("fascists out|racist comments|condemned.*post", ignore_case = TRUE)) &
        event_partisan_type_final == "right"
    ),
    matched_farright_counter_mixed = (
      str_detect(notes, regex("far-right|far right", ignore_case = TRUE)) &
        str_detect(notes, regex("counter-protest|counter-demonstration|anti-racism.*gathered|anti-fascist.*gathered", ignore_case = TRUE)) &
        event_partisan_type_final == "right"
    ),
    matched_environmental_to_left = (
      str_detect(notes, regex(
        "environmental activist|ecologist|wildlife habitat|green belt|mature trees|ancient woodland|damaging wildlife",
        ignore_case = TRUE
      )) &
        event_partisan_type_final == "centre"
    ),
    matched_scottish_civic_to_left = (
      str_detect(notes, regex("scottish independence|england get out of scotland", ignore_case = TRUE)) &
        !str_detect(notes, regex("action for scotland|tourist", ignore_case = TRUE)) &
        event_partisan_type_final == "right"
    ),
    matched_cleanairzone_mixed = (
      str_detect(notes, regex("clean air zone", ignore_case = TRUE)) &
        event_partisan_type_final == "centre"
    ),
    matched_antivaxconvoy_to_right = (
      str_detect(notes, regex("anti-lockdown", ignore_case = TRUE)) &
        str_detect(notes, regex("vaccine passport|convoy", ignore_case = TRUE)) &
        event_partisan_type_final == "left"
    ),
    matched_barristers_to_centre = (
      str_detect(notes, regex("criminal bar association|barrister", ignore_case = TRUE)) &
        str_detect(notes, regex("legal aid", ignore_case = TRUE)) &
        event_partisan_type_final == "left"
    ),
    
    event_partisan_type_final = case_when(
      matched_asylum_housing_to_right ~ "right",
      matched_antibrexit_to_left      ~ "left",
      matched_prorefugee_to_left      ~ "left",
      matched_antifarright_to_left    ~ "left",
      matched_farright_counter_mixed  ~ "left_and_right",
      matched_environmental_to_left   ~ "left",
      matched_scottish_civic_to_left  ~ "left",
      matched_cleanairzone_mixed      ~ "centre_and_right",
      matched_antivaxconvoy_to_right  ~ "right",
      matched_barristers_to_centre    ~ "centre",
      TRUE ~ event_partisan_type_final
    ),
    classification_source = case_when(
      matched_asylum_housing_to_right | matched_antibrexit_to_left | matched_prorefugee_to_left |
        matched_antifarright_to_left | matched_farright_counter_mixed | matched_environmental_to_left |
        matched_scottish_civic_to_left | matched_cleanairzone_mixed | matched_antivaxconvoy_to_right |
        matched_barristers_to_centre ~ "rule_correction",
      TRUE ~ classification_source
    )
  ) %>%
  select(-matched_asylum_housing_to_right, -matched_antibrexit_to_left, -matched_prorefugee_to_left,
         -matched_antifarright_to_left, -matched_farright_counter_mixed, -matched_environmental_to_left,
         -matched_scottish_civic_to_left, -matched_cleanairzone_mixed, -matched_antivaxconvoy_to_right,
         -matched_barristers_to_centre)

# Flagged for manual review, not auto-corrected: "Action for Scotland" /
# tourist-exclusion protests. See note above R7 - the exclusionary framing
# (demanding a national-origin group not enter the territory) mirrors the
# anti-immigration pattern coded right elsewhere, unlike the civic-nationalist
# framing of the other Scottish independence cases.
action_for_scotland_review <- uk_acled_df %>%
  filter(str_detect(notes, regex("action for scotland", ignore_case = TRUE)))
cat("\n'Action for Scotland' rows flagged for manual review (not auto-corrected):",
    nrow(action_for_scotland_review), "\n")

# =============================================================================
# DIAGNOSTICS
# =============================================================================

cat("\nClassification source breakdown:\n")
print(table(uk_acled_df$classification_source))

cat("\nFinal partisan type breakdown:\n")
print(table(uk_acled_df$event_partisan_type_final))

discrepancy <- uk_acled_df %>%
  filter(classification_source == "rule_correction" & event_partisan_type_final == "unknown")
cat("\nRule-corrected rows still showing 'unknown' (should be 0):", nrow(discrepancy), "\n")

# =============================================================================
# SAVE & VALIDATION SAMPLE
# =============================================================================

write_csv(uk_acled_df, "~/Downloads/uk_acled_partisan_classification_bootstrapped4.csv")

random_sample <- uk_acled_df %>%
  filter(classification_source %in% c("text_classification", "rule_correction")) %>%
  slice_sample(n = 100)

cat("\nSample composition:\n")
print(table(random_sample$classification_source))

write_xlsx(random_sample, "~/Documents/uk_text_class_random_sample4_2020_2024.xlsx")
