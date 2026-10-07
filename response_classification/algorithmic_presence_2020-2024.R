################################################################################
# ROBUST POLICE PRESENCE CLASSIFICATION FOR ACLED DATA
# Author: Katelyn Nutley
# Date: 17-12-2025
################################################################################

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)

################################################################################
# STEP 0: MERGE COUNTRY BOOTSTRAPPED FILES INTO THE COMBINED DATASET
################################################################################

# Replaces the previous manual assembly of acled_all_countries_combined_classed.csv
# with a direct merge of each country's final, validated partisan-classification
# bootstrap: UK v4, Spain v7, Germany v9, Italy v4, France v5.

BASE_DIR <- "~/Documents/GitHub/Police_Response/2020-2024/"

country_files <- list(
  France  = list(folder = "France",  file = "france_acled_partisan_classification_bootstrapped5_2020_2024.csv"),
  UK      = list(folder = "UK",      file = "uk_acled_partisan_classification_bootstrapped4_2020_2024.csv"),
  Germany = list(folder = "Germany", file = "germany_acled_partisan_classification_bootstrapped9_2020_2024.csv"),
  Italy   = list(folder = "Italy",   file = "italy_acled_partisan_classification_bootstrapped4_2020_2024.csv"),
  Spain   = list(folder = "Spain",   file = "spain_acled_partisan_classification_bootstrapped7_2020_2024.csv")
)

read_country_file <- function(spec, country_name) {
  path <- paste0(BASE_DIR, spec$folder, "/", spec$file)
  cat("Reading", country_name, "from:", path, "\n")
  df <- read_csv(path, show_col_types = FALSE)
  cat("  ->", nrow(df), "rows,", ncol(df), "columns\n")
  df
}

country_dfs <- imap(country_files, read_country_file)

# Integrity check: verify each country's total unknown count against the
# last confirmed-correct figure, so a stale file on disk (same filename,
# different underlying data - this has already happened once for Germany
# due to bootstrapped9 being written by three different script versions
# over the course of this project) fails loudly here instead of silently
# propagating into the combined dataset.
expected_unknown_counts <- list(
  Germany = 10176
  # Add other countries' confirmed unknown counts here once validated,
  # e.g. France = 13210
)

for (nm in names(expected_unknown_counts)) {
  actual_unknown <- sum(country_dfs[[nm]]$event_partisan_type_final == "unknown", na.rm = TRUE)
  expected_unknown <- expected_unknown_counts[[nm]]
  if (actual_unknown != expected_unknown) {
    stop(
      nm, ": unknown count on disk (", actual_unknown, ") does not match ",
      "the last confirmed-correct figure (", expected_unknown, "). ",
      "This means the file at the path above is a stale/different version ",
      "than the one you validated - regenerate it before re-running this merge."
    )
  } else {
    cat(nm, ": unknown count verified (", actual_unknown, ") - matches confirmed data.\n", sep = "")
  }
}

# Normalize event_partisan_type_final across ALL countries before binding.
# ACLED's own event_partisan_type uses "_only" suffixes and non-alphabetized
# combo labels (e.g. "left_and_centre"); the NLP classifier's output uses
# bare labels with alphabetized combos (e.g. "centre_and_left"). Left
# unnormalized, the same substantive category can appear as two different
# strings.
#
# France already has event_partisan_type_final_norm - the column actually
# used for validation/accuracy checking earlier - so that's the source of
# truth for France, used as-is rather than recomputed (it may reflect manual
# corrections beyond what a pure normalization function would produce). For
# countries without a pre-existing _norm column, compute it the same way.

normalize_partisan_type <- function(x) {
  x <- str_remove(x, "_only$")                # left_only -> left
  parts <- str_split(x, "_and_")                # split combo categories
  sapply(parts, function(p) paste(sort(p), collapse = "_and_"))  # alphabetize
}

country_dfs <- map(country_dfs, function(df) {
  if ("event_partisan_type_final_norm" %in% colnames(df)) {
    df <- df %>%
      mutate(event_partisan_type_final = event_partisan_type_final_norm) %>%
      select(-event_partisan_type_final_norm)
  } else {
    df <- df %>%
      mutate(event_partisan_type_final = normalize_partisan_type(event_partisan_type_final))
  }
  df
})

# Check for column mismatches before binding - bind_rows() will fill
# missing columns with NA, but it's worth knowing about them rather than
# discovering it downstream.
all_cols <- map(country_dfs, colnames)
common_cols <- Reduce(intersect, all_cols)
for (nm in names(all_cols)) {
  extra <- setdiff(all_cols[[nm]], common_cols)
  if (length(extra) > 0) {
    cat("\n", nm, "has columns not shared by all countries:\n", sep = "")
    print(extra)
  }
}

acled_data <- bind_rows(country_dfs, .id = "country_source_file")

cat("\nCombined dataset:", nrow(acled_data), "rows from", length(country_dfs), "countries\n")
print(table(acled_data$country))

write_csv(acled_data, "~/Documents/GitHub/Police_Response/2020-2024/acled_all_countries_combined_classed_2020_2024.csv")

################################################################################
# HELPER FUNCTION: Context-Aware Police Presence Detection
################################################################################

# This has been re-worked repeatedly to improve it; make it more sympathetic to ACLED's data 

detect_police_presence <- function(notes, interaction = NA, actor1 = NA) {
  
  # Return FALSE if no notes
  if (is.na(notes) || nchar(trimws(notes)) < 10) {
    return(FALSE)
  }
  
  notes_lower <- tolower(notes)
  
  # STEP 1: EXCLUDE - Police as the protesters themselves
  police_as_protesters <- c(
    "police officers?.*(gathered|demonstrated|protested|rallied|marched|dropped their)",
    "officers?.*(gathered|demonstrated|protested|rallied).*(in front of|at|outside)",
    "police (union|forces?).*(protested|demonstrated|gathered)",
    "police.*dropped.*(their )?handcuffs",
    "JUSAPOL"
  )
  
  if (any(sapply(police_as_protesters, function(p) grepl(p, notes_lower, perl = TRUE)))) {
    return(FALSE)
  }
  
  # Check actor field for police as protesters
  if (!is.na(actor1) && grepl("Police Forces", actor1, ignore.case = TRUE)) {
    if (!is.na(interaction) && interaction == "Protesters only") {
      return(FALSE)
    }
  }
  
  # STEP 2: EXCLUDE - Protesting AGAINST police (foreign/past events)
  against_police <- c(
    "protest.*against police (brutality|violence|reform)",
    "demonstrat.*against police",
    "denounced.*police brutality",
    # Foreign events
    "in (solidarity with|response to|view of).*(protest|death).*in (the )?(United States|U\\.S\\.|US|Serbia|Iran|Belarus)",
    "after.*death.*by.*police officer in (the )?(United States|America|US)",
    "black lives matter.*(in the United States|after.*killing.*United States)",
    "police brutality.*(in|after).*(United States|America)"
  )
  
  if (any(sapply(against_police, function(p) grepl(p, notes_lower, perl = TRUE)))) {
    return(FALSE)
  }
  
  # STEP 3: EXCLUDE - Legislative/administrative mentions of police custody
  custody_legislative <- c(
    "prohibit.*publication.*police custody",
    "amendment.*police custody",
    "law.*would prohibit.*custody",
    "against.*law.*custody orders"
  )
  
  if (any(sapply(custody_legislative, function(p) grepl(p, notes_lower, perl = TRUE)))) {
    return(FALSE)
  }
  
  # STEP 4: EXCLUDE - Explicit statements of NO police intervention
  no_intervention <- c(
    "no report of.*police intervention",
    "no police intervention",
    "without.*police intervention",
    "police did not intervene"
  )
  
  if (any(sapply(no_intervention, function(p) grepl(p, notes_lower, perl = TRUE)))) {
    return(FALSE)
  }
  
  # STEP 5: DETECT - Strong evidence of police intervention
  strong_intervention <- c(
    "police (intervened|intervention|dispersed|dispersing)",
    "police.*removed.*demonstrators",
    "police.*used (tear gas|water cannon|pepper spray)",
    "officers? (intervened|dispersed|removed|deployed)",
    "(tear gas|water cannon|pepper spray) (was|were) used",
    "police.*physically.*(remove|removed)",
    "police.*blocked.*(access|entrance|road|demonstrators)",
    "police.*arrested.*(demonstrators|protesters)",
    "police.*detained.*(protesters|demonstrators)",
    "\\d+.*arrested (by police|on site)",
    "scuffles?.*(with|between).*police",
    "clashed with police",
    "police charged.*protesters",
    "baton charge",
    "police cordon",
    "kettled|kettling",
    "police formed.*cordon"
  )
  
  if (any(sapply(strong_intervention, function(p) grepl(p, notes_lower, perl = TRUE)))) {
    return(TRUE)
  }
  
  # STEP 6: DETECT - Interaction field indicates state forces
  if (!is.na(interaction) && grepl("State forces", interaction, ignore.case = TRUE)) {
    return(TRUE)
  }
  
  # STEP 7: DETECT - Weaker evidence (only if no exclusions triggered)
  weak_presence <- c(
    "police presence",
    "police (were|was) present",
    "escorted by police",
    "police escort",
    "police monitored",
    "police observed",
    "riot police.*deployed"
  )
  
  if (any(sapply(weak_presence, function(p) grepl(p, notes_lower, perl = TRUE)))) {
    return(TRUE)
  }
  
  # Default: no police presence
  return(FALSE)
}

################################################################################
# CLASSIFY ALL EVENTS
################################################################################

acled_data <- acled_data %>%
  rowwise() %>%
  mutate(
    police_presence = detect_police_presence(notes, interaction, actor1)
  ) %>%
  ungroup()

table(acled_data$police_presence)

################################################################################
# SUMMARY STATISTICS
################################################################################

# By country
country_summary <- acled_data %>%
  group_by(country) %>%
  summarise(
    total = n(),
    with_police = sum(police_presence),
    pct = round(with_police / total * 100, 2)
  ) %>%
  arrange(desc(pct))

print(country_summary)

################################################################################
# VALIDATION SAMPLE
################################################################################

set.seed(123)
validation_sample <- acled_data %>%
  filter(police_presence == TRUE) %>%
  sample_n(20) %>%
  select(country, event_date, interaction, notes)

# Hand coded everything; very pleased with this! 

################################################################################
# EXPORT POLICE PRESENCE SUBSET
################################################################################

police_response_subset <- acled_data %>%
  filter(police_presence == TRUE)

write_csv(police_response_subset, "~/Documents/GitHub/Police_Response/2020-2024/acled_police_response_subset_2020_2024.csv")
write_csv(acled_data, "~/Documents/GitHub/Police_Response/2020-2024/acled_classified_police_presence_2020_2024.csv")

################################################################################
# DETAILED BREAKDOWN FOR PAPER
################################################################################

# Event types with police presence
event_type_summary <- acled_data %>%
  filter(police_presence == TRUE) %>%
  count(event_type, sub_event_type, sort = TRUE) %>%
  head(10)

print(event_type_summary)

# Temporal trends
temporal_summary <- acled_data %>%
  filter(police_presence == TRUE) %>%
  group_by(year) %>%
  summarise(
    events_with_police = n(),
    total_events_that_year = sum(acled_data$year == year[1])
  ) %>%
  mutate(
    pct = round(events_with_police / total_events_that_year * 100, 2)
  ) %>%
  arrange(year)

print(temporal_summary)

# Cross-tabulation: interaction type vs police presence
interaction_table <- acled_data %>%
  count(interaction, police_presence) %>%
  tidyr::pivot_wider(names_from = police_presence, values_from = n, values_fill = 0)

print(interaction_table)

###############

# making a sample to handcode 

set.seed(42)
police_manual_sample <- police_response_subset %>%
  slice_sample(prop = 0.1)
write_xlsx(police_manual_sample, "police_manual_sample.xlsx")


set.seed(42)
police_manual_sample <- police_response_subset %>%
  slice_sample(prop = 0.1)
write_xlsx(police_manual_sample, "police_manual_sample1.xlsx")