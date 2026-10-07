library(readr); library(dplyr)
read_csv("data/combined/acled_classified_severity_v3_2020_2024.csv") |>
  mutate(across(c(police_presence, arrest, brutality),
                ~ as.integer(get(paste0(cur_column(), "_prob")) >= 0.8),
                .names = "{.col}_08")) |>
  write_csv("data/combined/acled_classified_severity_v3_rethresholded.csv")