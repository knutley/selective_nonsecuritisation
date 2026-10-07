# Title: Partisanship Classification - Addendum (NLP Bootstrap) v2
# Author: Katelyn Nutley (revised)
# Date: 24-02-2026
# Description: NLP-based bootstrap to classify unknown actors in ITALY ACLED data.
#              For mixed events (classified / unknown assoc_actors co-occurring),
#              uses the notes field alongside the classified actor as a partisan
#              anchor to contextualise unknown actors via zero-shot classification.
#              Requires: italy_acled_partisan_classification.csv from main script.
#
# Changes from v1:
#   - Candidate labels rewritten for Italian protest context
#   - multi_label = TRUE to recover right_and_left / right_and_centre etc.
#   - Combined-label logic replaces forced single-label output
#   - Post-hoc correction block updated to reflect revised label set
#   - Bug fix: random_sample3 now draws from correct object (text_classified_obs2)

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
italy_acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/italy_acled_partisan_classification_2020_2024.csv")

################################################################################
# STEP 1: IDENTIFY MIXED EVENTS
################################################################################

# Logic: if an event has at least one classified actor and at least one unknown
# actor, use NLP on the notes field to contextualise political behaviour.

italy_acled_df <- italy_acled_df %>%
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

cat("Mixed events for NLP classification:", sum(italy_acled_df$mixed == 1), "\n")

################################################################################
# STEP 2: ZERO-SHOT TEXT CLASSIFICATION
################################################################################

# Point to your Python environment - update path as needed
# use_condaenv("your_env_name")
# use_virtualenv("path/to/venv")

transformers <- import("transformers")
torch <- import("torch")
torch$set_num_threads(as.integer(parallel::detectCores() %/% 2))

classifier <- transformers$pipeline(
  "zero-shot-classification",
  model = "facebook/bart-large-mnli"
)

candidate_labels <- c(
  "a left-wing protest about labor unions, workers rights, housing,
   student occupations, precarious work, public services, migrant
   workers, anti-racism, or anti-fascism",
  
  "a right-wing protest about the green pass, vaccine mandates,
   coronavirus restrictions, immigration, national sovereignty,
   law and order, or EU farming regulations",
  
  "a local community protest about a specific planning or
   infrastructure decision with no broader political ideology"
)

label_short <- c("left", "right", "centre")
score_threshold <- 0.45

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
    
    if (length(active) == 0) {
      partisan_type <- "unknown"
    } else {
      active_sorted <- sort(active)  # alphabetical: centre, left, right
      partisan_type <- paste(active_sorted, collapse = "_and_")
    }
    
    best_score <- max(unlist(result$scores))
    
    data.frame(
      predicted_partisan_type = partisan_type,
      confidence = best_score,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(predicted_partisan_type = "unknown", confidence = 0,
               stringsAsFactors = FALSE)
  })
}

unknown_events <- italy_acled_df %>%
  filter(event_partisan_type == "unknown") %>%
  filter(!is.na(notes) & str_length(notes) > 20)

cat("Events to classify:", nrow(unknown_events), "\n") # 10713 to class 

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

################################################################################
# STEP 3: MERGE AND PRODUCE FINAL CLASSIFICATION
################################################################################

italy_acled_df <- italy_acled_df %>%
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

cat("\nFinal classification breakdown:\n")
italy_acled_df %>%
  count(event_partisan_type_final, classification_source) %>%
  arrange(desc(n)) %>%
  print()

cat("\nRemaining unknowns:", sum(italy_acled_df$event_partisan_type_final == "unknown"), "\n")

write_csv(italy_acled_df, "~/Downloads/italy_acled_partisan_classification_bootstrapped3_2020_2024.csv")

text_classified_obs <- italy_acled_df %>%
  filter(classification_source == "text_classification")
random_sample <- text_classified_obs %>% 
  slice_sample(n=100)
write_xlsx(random_sample, "~/Documents/italy_text_class_random_sample3_2020_2024.xlsx")

################################################################################
# STEP 4: RULE CORRECTION
################################################################################

# Targets three confirmed error patterns identified through manual
# validation of the round-3 sample:
#
#   R1: green pass / anti-vaccine-mandate opposition miscoded as left/centre -> right
#   R2: "Tricolore"/"Pro Patria" nationalist May Day counter-events miscoded as left -> right
#   R3: anti-fascist/anti-racist protests with neo-Nazi counter-presence -> left_and_right
#
# Each match is computed once as a boolean flag and reused for both the
# value update and the source-label update, so they can't drift apart.

italy_acled_df <- italy_acled_df %>%
  mutate(
    # R1: green pass / anti-vaccine-mandate opposition miscoded as left or
    # centre. Catches cases the classifier's own right-wing label only picks
    # up when far-right buzzwords are present (e.g. "No Vax", "3V Movement").
    matched_greenpass_to_right = (
      classification_source == "text_classification" &
        str_detect(notes, regex(
          paste0(
            "green pass|no vax|no green pass|vaccine mandate|",
            "compulsory vaccin|coronavirus restriction"
          ),
          ignore_case = TRUE
        )) &
        str_detect(notes, regex(
          "against|oppose|protest|denounc|reject|mandatory",
          ignore_case = TRUE
        )) &
        event_partisan_type_final %in% c("left", "centre")
    ),
    
    # R2: nationalist "Tricolore" / "Pro Patria" May Day counter-events
    # miscoded as left. Documented right-wing counter-tradition to the
    # mainstream CGIL/CISL/UIL-led May Day march, not labor activism - the
    # classifier picks up surface cues ("workers", "1 May") without
    # registering the nationalist framing.
    matched_tricolore_to_right = (
      classification_source == "text_classification" &
        str_detect(notes, regex("tricolore|pro patria", ignore_case = TRUE)) &
        event_partisan_type_final == "left"
    ),
    
    # R3: anti-fascist/anti-racist protests where neo-Nazi or far-right
    # actors were physically present (even as disruptors/counter-presence)
    # -> left_and_right, matching the counter-mobilization logic used for
    # Germany's Querdenken corrections.
    matched_antifa_mixed = (
      classification_source == "text_classification" &
        str_detect(notes, regex("anti-racist|anti-fascist|antifascist", ignore_case = TRUE)) &
        str_detect(notes, regex("neo-nazi|neo-fascist|far-right|extreme right|far right", ignore_case = TRUE)) &
        event_partisan_type_final == "left"
    ),
    
    event_partisan_type_final = case_when(
      matched_greenpass_to_right ~ "right",
      matched_tricolore_to_right ~ "right",
      matched_antifa_mixed       ~ "left_and_right",
      TRUE ~ event_partisan_type_final
    ),
    classification_source = case_when(
      matched_greenpass_to_right ~ "rule_correction",
      matched_tricolore_to_right ~ "rule_correction",
      matched_antifa_mixed       ~ "rule_correction",
      TRUE ~ classification_source
    )
  ) %>%
  select(-matched_greenpass_to_right, -matched_tricolore_to_right, -matched_antifa_mixed)

cat("\nClassification source breakdown after rule correction:\n")
print(table(italy_acled_df$classification_source))

cat("\nFinal partisan type breakdown after rule correction:\n")
print(table(italy_acled_df$event_partisan_type_final))

discrepancy <- italy_acled_df %>%
  filter(classification_source == "rule_correction" & event_partisan_type_final == "unknown")
cat("\nRule-corrected rows still showing 'unknown' (should be 0):", nrow(discrepancy), "\n")

write_csv(italy_acled_df, "~/Downloads/italy_acled_partisan_classification_bootstrapped4.csv")

set.seed(42)
random_sample <- italy_acled_df %>%
  slice_sample(n = 100)

cat("\nSample composition:\n")
print(table(random_sample$classification_source))

write_xlsx(random_sample, "~/Documents/italy_text_class_random_sample4.xlsx")