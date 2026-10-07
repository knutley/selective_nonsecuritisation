# Title: Germany Partisanship Classification - Clean Pipeline
# Author: Katelyn Nutley
# Description: NLP-based bootstrap to classify unknown actors in GERMANY ACLED
#              data. For mixed events (classified + unknown assoc_actors
#              co-occurring), uses the notes field alongside the classified
#              actor as a partisan anchor to contextualise unknown actors via
#              zero-shot classification.
#
# This is the condensed pipeline: one classification pass (the label set that
# validated well and produced "bootstrapped3"), followed by seven regex-based
# corrections (R1-R7), where R7 replaces the earlier blunt "querdenken -> right"
# rule with an actor-vs-target + counter-protest-aware version.

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

DATA_DIR <- "~/Documents/GitHub/Police_Response/2020-2024/Germany/"

# =============================================================================
# STEP 0: LOAD BASE DATA
# =============================================================================

germany_acled_df <- read_csv(paste0(DATA_DIR, "germany_acled_partisan_classification_2020_2024.csv"))

# Identify mixed events (at least one classified + one unknown assoc_actor),
# kept for methodological documentation of which events are eligible for
# text-based contextualisation.
germany_acled_df <- germany_acled_df %>%
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

# =============================================================================
# STEP 1: ZERO-SHOT TEXT CLASSIFICATION
# =============================================================================

transformers <- import("transformers")
torch <- import("torch")
torch$set_num_threads(as.integer(parallel::detectCores() %/% 2))  # leave headroom if running other jobs in parallel

classifier <- transformers$pipeline(
  "zero-shot-classification",
  model = "facebook/bart-large-mnli"
)

candidate_labels <- c(
  "a left-wing protest about workers rights, trade union organising,
   anti-racism, anti-AfD or pro-democracy movements, Palestine solidarity,
   environmental activism, antifascist action, or anti-austerity",
  "a right-wing protest about immigration, AfD politics, nationalism,
   PEGIDA, anti-Islam positions, coronavirus restrictions, vaccine mandates,
   pro-Russia or anti-NATO causes, or far-right and identitarian movements",
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
      if (scores[labels[i]] >= threshold) active <- c(active, short_names[i])
    }
    
    partisan_type <- if (length(active) == 0) "unknown" else paste(sort(active), collapse = "_and_")
    data.frame(predicted_partisan_type = partisan_type, confidence = max(unlist(result$scores)),
               stringsAsFactors = FALSE)
  }, error = function(e) {
    data.frame(predicted_partisan_type = "unknown", confidence = 0, stringsAsFactors = FALSE)
  })
}

unknown_events <- germany_acled_df %>%
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

germany_acled_df <- germany_acled_df %>%
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

cat("\nRemaining unknowns after classification:",
    sum(germany_acled_df$event_partisan_type_final == "unknown"), "\n")

write_csv(germany_acled_df, paste0(DATA_DIR, "germany_acled_partisan_classification_bootstrapped3_2020_2024.csv"))

# =============================================================================
# STEP 2: REGEX-BASED CORRECTIONS (R1-R7)
# =============================================================================

germany_acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/Germany/germany_acled_partisan_classification_bootstrapped3_2020_2024.csv")

table(germany_acled_df$event_partisan_type_final)

germany_acled_df <- germany_acled_df %>%
  mutate(
    # R1: anti-coronavirus measures miscoded as left -> right
    matched_corona_to_right = (
      classification_source == "text_classification" &
        str_detect(notes, regex(paste0(
          "coronavirus protection measures|coronavirus lockdown|corona-massnahmen|",
          "impfpflicht|covid-massnahmen|lockdown measures|contact ban|health pass|",
          "querdenken|coronavirus-related measures|compulsory vacc|",
          "corona schutzmasnahmen|against the coronavirus|covid protection|",
          "corona measures|impfung|walk.*corona|corona.*walk"
        ), ignore_case = TRUE)) &
        event_partisan_type_final == "left"
    ),
    # R2: farmer protests miscoded as left -> centre
    matched_farmers_to_centre = (
      classification_source == "text_classification" &
        str_detect(notes, regex(paste0(
          "agrardiesel|agricultural subsid|agrarian polic|farmers.*protest|",
          "bauern|agricultural polic|agrarian package|farmers.*tractor|",
          "tractor.*protest|farmers.*blockade|farmers.*demonstration|",
          "vehicle tax exemption|farmers and craftsmen|farmers.*nationwide|",
          "nationwide farmers"
        ), ignore_case = TRUE)) &
        event_partisan_type_final == "left"
    ),
    # R3: motorbike/vehicle protests miscoded as left -> centre
    matched_motorbike_to_centre = (
      classification_source == "text_classification" &
        str_detect(notes, regex(paste0(
          "motorbike.*protest|motorcycle.*protest|biker.*protest|ffmc|",
          "bikers.*gathered|motorbike.*ban|weekend.*driving ban|",
          "technical control.*motorbike|motorbike.*technical|",
          "ride free|motor biker"
        ), ignore_case = TRUE)) &
        event_partisan_type_final == "left"
    ),
    # R4: anti-refugee housing miscoded as left_and_right -> right
    matched_anti_refugee = (
      str_detect(notes, regex("refugee|asylbewerber|flüchtling|asylum seeker", ignore_case = TRUE)) &
        str_detect(notes, regex("against|stop|oppose|verhindern|ablehnung", ignore_case = TRUE)) &
        event_partisan_type_final == "left_and_right" &
        classification_source != "original"
    ),
    # R5: no-borders / pro-asylum / anti-deportation miscoded as left_and_right -> left
    matched_no_borders = (
      str_detect(notes, regex("no borders|no one is illegal|deportation|abschiebung", ignore_case = TRUE)) &
        event_partisan_type_final == "left_and_right" &
        classification_source != "original"
    ),
    # R6: anti-AfD / pro-democracy miscoded as right or left_and_right -> left
    matched_anti_afd = (
      str_detect(notes, regex(paste0(
        "against.*afd|gegen.*afd|protest.*afd|afd.*protest|",
        "against the rise of far-right|against right-wing extremism|",
        "sea of lights|lights against the right|",
        "we are the firewall|firewall.*against.*right|",
        "remigration|against.*sellner|",
        "against.*alternative for germany|alternative for germany.*protest|",
        "response to.*afd|response to a campaign event held by the afd|",
        "against an alternative for germany|against the alternative for germany"
      ), ignore_case = TRUE)) &
        event_partisan_type_final %in% c("right", "left_and_right") &
        classification_source != "original"
    )
  ) %>%
  mutate(
    event_partisan_type_final = case_when(
      matched_corona_to_right     ~ "right",
      matched_farmers_to_centre   ~ "centre",
      matched_motorbike_to_centre ~ "centre",
      matched_anti_refugee        ~ "right",
      matched_no_borders          ~ "left",
      matched_anti_afd            ~ "left",
      TRUE ~ event_partisan_type_final
    ),
    classification_source = case_when(
      matched_corona_to_right | matched_farmers_to_centre | matched_motorbike_to_centre |
        matched_anti_refugee | matched_no_borders | matched_anti_afd ~ "rule_correction",
      TRUE ~ classification_source
    )
  ) %>%
  select(-starts_with("matched_"))

# R7 (v2): querdenken actor-vs-target + counter-protest detection.
# Replaces the earlier blunt "any mention of querdenken -> right" rule, which
# ignored whether Querdenken was the actor or the target of the protest, and
# whether a counter-protest co-occurred at the same event.

querdenken_as_actor <- paste0(
  "querdenken (followers|supporters|members|activists|movement)s?\\s*",
  "(demonstrated|protested|staged|gathered)|",
  "protest(ed)?\\s*(was\\s*)?organi[sz]ed by (a local (offshoot|initiative) of )?querdenken|",
  "at the call of (the |several groups including )?(the )?querdenken|",
  "people,?\\s*including querdenken members,?\\s*(demonstrated|protested)|",
  "activists (of|from) (the )?initiative querdenken|",
  "people (of|from) the initiative querdenken protested"
)
querdenken_as_target <- "against.{0,70}querdenken"
counter_mobilization <- paste0(
  "counter-?protest|counter-?demonstration|gathered for a counter|",
  "temporarily protested against the demonstration|",
  "protested against the demonstration|",
  "querdenken members (left|were escorted|were removed).{0,40}police"
)

germany_acled_df <- germany_acled_df %>%
  mutate(
    has_querdenken_actor  = str_detect(notes, regex(querdenken_as_actor, ignore_case = TRUE)),
    has_querdenken_target = str_detect(notes, regex(querdenken_as_target, ignore_case = TRUE)),
    has_counter_mob        = str_detect(notes, regex(counter_mobilization, ignore_case = TRUE)),
    has_querdenken_any     = str_detect(notes, regex("querdenken|lateral thinking", ignore_case = TRUE)),
    
    matched_querdenken_mixed = (
      classification_source == "text_classification" &
        has_querdenken_any & has_querdenken_actor & has_counter_mob
    ),
    matched_querdenken_target_only = (
      classification_source == "text_classification" &
        has_querdenken_any & has_querdenken_target & !has_querdenken_actor
    ),
    matched_querdenken_right = (
      classification_source == "text_classification" &
        has_querdenken_any & has_querdenken_actor & !has_counter_mob
    ),
    
    event_partisan_type_final = case_when(
      matched_querdenken_mixed       ~ "left_and_right",
      matched_querdenken_target_only ~ "left",
      matched_querdenken_right       ~ "right",
      TRUE ~ event_partisan_type_final
    ),
    classification_source = case_when(
      matched_querdenken_mixed | matched_querdenken_target_only | matched_querdenken_right ~ "rule_correction",
      TRUE ~ classification_source
    )
  ) %>%
  select(-has_querdenken_actor, -has_querdenken_target, -has_counter_mob, -has_querdenken_any,
         -matched_querdenken_mixed, -matched_querdenken_target_only, -matched_querdenken_right)

# =============================================================================
# STEP 3: DIAGNOSTICS
# =============================================================================

cat("\nClassification source breakdown:\n")
print(table(germany_acled_df$classification_source))

cat("\nFinal partisan type breakdown:\n")
print(table(germany_acled_df$event_partisan_type_final))

discrepancy <- germany_acled_df %>%
  filter(classification_source != "unknown" & event_partisan_type_final == "unknown")
cat("\nDiscrepancy count (should be 0):", nrow(discrepancy), "\n")

# =============================================================================
# STEP 4: SAVE & VALIDATION SAMPLE
# =============================================================================

write_csv(germany_acled_df, paste0(DATA_DIR, "germany_acled_partisan_classification_bootstrapped9_2020_2024.csv"))

set.seed(42)
random_sample <- germany_acled_df %>%
  slice_sample(n = 100)

cat("\nSample composition (should show a mix of sources):\n")
print(table(random_sample$classification_source))

write_xlsx(random_sample, paste0(DATA_DIR, "germany_text_class_random_sample9_2020_2024.xlsx"))