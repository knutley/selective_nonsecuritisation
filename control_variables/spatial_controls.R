# Control Variable Construction - rethresholded v3
# Base dataset: acled_classified_severity_v3_rethresholded_2020_2024.csv
# (police_presence/arrest/brutality now available at both the original 0.5
# threshold and a stricter 0.8 threshold - see 22-09-2026 sniff-test findings:
# police_presence and arrest are reliable at 0.8; brutality remains thin even
# at 0.8, ~45% of its positives sit below that cutoff, driven by a "wrong
# actor" confound - treat brutality results with real caution regardless of
# threshold).
#
# Spatial controls (government building / major road / police station
# proximity) are REUSED from the existing acled_merged_controls.csv
# checkpoint via join on event_id_cnty, rather than re-querying OSM/
# HuggingFace - those pulls are expensive and the underlying event geometry
# (lat/lon) hasn't changed, only the severity classification has. Unmatched
# rows are backfilled from cached (already-downloaded, already-classified)
# OSM objects - no re-downloading, no re-classification.
#
# Author: Katelyn Nutley
# Date: 22-09-2026

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(sf)
library(giscoR)

# =============================================================================
# 0. PATHS
# =============================================================================

new_base_path       <- "~/Documents/GitHub/Police_Response/2020-2024/acled_classified_severity_v3_rethresholded.csv"
old_checkpoint_path <- "~/Documents/GitHub/Police_Response/data/combined/acled_merged_controls.csv"
checkpoint_path      <- "~/Documents/GitHub/Police_Response/2020-2024/acled_merged_controls_rethresholded_v3.csv"
cache_dir            <- path.expand("~/Documents/GitHub/Police_Response/data/cache")

# =============================================================================
# 1. LOAD NEW BASE + REUSE SPATIAL CONTROLS FROM OLD CHECKPOINT
# =============================================================================

acled_merged <- read_csv(new_base_path)
n_start <- nrow(acled_merged)
message("New rethresholded base: ", n_start, " rows")

old_checkpoint <- read_csv(old_checkpoint_path)
message("Old checkpoint: ", nrow(old_checkpoint), " rows")
message("Row count difference: ", nrow(old_checkpoint) - n_start,
        " - expected given the two files come from different pipeline runs; ",
        "the join diagnostics below show how many v3 rows this actually affects.")

spatial_cols_to_reuse <- c(
  "event_id_cnty",
  "dist_govt_building_m", "near_govt_building", "govt_proximity_cat",
  "dist_major_road_m", "near_major_road", "road_proximity_cat",
  "dist_police_station_m", "log_dist_police_station",
  "near_police_station", "police_proximity_cat", "n_police_stations_5km"
)

missing_from_old <- setdiff(spatial_cols_to_reuse, names(old_checkpoint))
if (length(missing_from_old) > 0) {
  stop("Old checkpoint is missing expected columns: ",
       paste(missing_from_old, collapse = ", "),
       ". Cannot safely reuse spatial controls - check old_checkpoint_path ",
       "points at the right file before proceeding.")
}

old_spatial <- old_checkpoint %>% select(all_of(spatial_cols_to_reuse))

acled_merged <- acled_merged %>%
  left_join(old_spatial, by = "event_id_cnty")

n_matched   <- sum(!is.na(acled_merged$dist_govt_building_m))
n_unmatched <- sum(is.na(acled_merged$dist_govt_building_m))
message("\nSpatial control join diagnostics:")
message("  Matched (reused from old checkpoint): ", n_matched)
message("  Unmatched (NA): ", n_unmatched)

if (n_unmatched > 0) {
  message("\n  Unmatched rows by country:")
  acled_merged %>%
    filter(is.na(dist_govt_building_m)) %>%
    count(country, sort = TRUE) %>%
    print(n = Inf)
}

stopifnot(nrow(acled_merged) == n_start)

# =============================================================================
# 2. BACKFILL UNMATCHED ROWS (continues from the SAME in-memory acled_merged
#    above - no re-reading from disk, no risk of discarding the join above)
# =============================================================================

unmatched <- acled_merged %>%
  filter(is.na(dist_govt_building_m), !is.na(latitude), !is.na(longitude))
message("\nBackfilling ", nrow(unmatched), " unmatched rows")

if (nrow(unmatched) == 0) {
  
  message("Nothing to backfill - all rows already matched.")
  
} else {
  
  govt_buildings_non_france <- readRDS(file.path(cache_dir, "govt_buildings_no_police_non_france.rds"))
  govt_buildings_france     <- readRDS(file.path(cache_dir, "govt_buildings_no_police_france.rds"))
  govt_buildings_all <- bind_rows(govt_buildings_non_france, govt_buildings_france) %>%
    st_transform(3035)
  
  countries <- c("Germany", "Italy", "Spain", "United Kingdom", "France")
  
  major_roads_list <- lapply(countries, function(place) {
    cache_file <- file.path(cache_dir, paste0("major_roads_", gsub(" ", "_", place), ".rds"))
    if (!file.exists(cache_file)) {
      message("  No cached major_roads for ", place, " - skipping")
      return(NULL)
    }
    readRDS(cache_file)
  })
  major_roads_all <- bind_rows(major_roads_list[!sapply(major_roads_list, is.null)]) %>%
    st_transform(3035)
  
  gov_points_list <- lapply(countries, function(place) {
    cache_file <- file.path(cache_dir, paste0("gov_points_", gsub(" ", "_", place), ".rds"))
    if (!file.exists(cache_file)) return(NULL)
    readRDS(cache_file)
  })
  gov_points_all <- bind_rows(gov_points_list[!sapply(gov_points_list, is.null)])
  police_stations_all <- gov_points_all %>% filter(amenity == "police") %>% st_transform(3035)
  
  unmatched_sf <- unmatched %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(3035)
  
  unmatched_sf$dist_govt_building_m <- st_distance(
    unmatched_sf, govt_buildings_all[st_nearest_feature(unmatched_sf, govt_buildings_all), ],
    by_element = TRUE
  ) |> as.numeric()
  
  unmatched_sf$dist_major_road_m <- st_distance(
    unmatched_sf, major_roads_all[st_nearest_feature(unmatched_sf, major_roads_all), ],
    by_element = TRUE
  ) |> as.numeric()
  
  unmatched_sf$dist_police_station_m <- st_distance(
    unmatched_sf, police_stations_all[st_nearest_feature(unmatched_sf, police_stations_all), ],
    by_element = TRUE
  ) |> as.numeric()
  
  police_counts <- st_is_within_distance(unmatched_sf, police_stations_all, dist = 5000)
  unmatched_sf$n_police_stations_5km <- lengths(police_counts)
  
  unmatched_sf <- unmatched_sf %>%
    mutate(
      near_govt_building = as.integer(dist_govt_building_m <= 100),
      govt_proximity_cat = case_when(
        dist_govt_building_m <= 50  ~ "On/adjacent (<50m)",
        dist_govt_building_m <= 100 ~ "Very close (50-100m)",
        dist_govt_building_m <= 250 ~ "Close (100-250m)",
        dist_govt_building_m <= 500 ~ "Nearby (250-500m)",
        TRUE                        ~ "Distant (500m+)"
      ),
      near_major_road = as.integer(dist_major_road_m <= 100),
      road_proximity_cat = case_when(
        dist_major_road_m <= 50   ~ "On/adjacent (<50m)",
        dist_major_road_m <= 100  ~ "Very close (50-100m)",
        dist_major_road_m <= 250  ~ "Close (100-250m)",
        dist_major_road_m <= 500  ~ "Nearby (250-500m)",
        TRUE                      ~ "Distant (500m+)"
      ),
      log_dist_police_station = log1p(dist_police_station_m),
      near_police_station = as.integer(dist_police_station_m <= 500),
      police_proximity_cat = case_when(
        dist_police_station_m <= 250  ~ "Very close (<250m)",
        dist_police_station_m <= 500  ~ "Close (250-500m)",
        dist_police_station_m <= 1000 ~ "Nearby (500m-1km)",
        dist_police_station_m <= 2000 ~ "Moderate (1-2km)",
        TRUE                          ~ "Distant (2km+)"
      )
    )
  
  backfill_values <- unmatched_sf %>%
    st_drop_geometry() %>%
    select(event_id_cnty, dist_govt_building_m, near_govt_building, govt_proximity_cat,
           dist_major_road_m, near_major_road, road_proximity_cat,
           dist_police_station_m, log_dist_police_station, near_police_station,
           police_proximity_cat, n_police_stations_5km)
  
  acled_merged <- acled_merged %>%
    rows_update(backfill_values, by = "event_id_cnty", unmatched = "ignore")
  
  message("Backfilled ", nrow(backfill_values), " rows")
  message("Still NA after backfill: ", sum(is.na(acled_merged$dist_govt_building_m)))
}

stopifnot(nrow(acled_merged) == n_start)

# =============================================================================
# 3. PROTEST SIZE
# =============================================================================
# ACLED's cleanup left a handful of compound tags (e.g. "Repression; crowd
# size=large") where the crowd-size portion needs isolating before the
# direct match below.

table(acled_merged$tags)

acled_merged <- acled_merged %>%
  mutate(tags = case_match(tags,
                           "Repression; crowd size=large" ~ "crowd size=large",
                           "local administrators; crowd size=very small" ~ "crowd size=very small",
                           "Repression; crowd size=massive" ~ "crowd size=massive",
                           "Repression; crowd size=medium" ~ "crowd size=medium",
                           "Repression; crowd size=small" ~ "crowd size=small",
                           "Repression; crowd size=very small" ~ "crowd size=very small",
                           "sexual violence; crowd size=medium" ~ "crowd size=medium",
                           "sexual violence; Repression; crowd size=small" ~ "crowd size=small",
                           "women targeted: protesters; crowd size=very small" ~ "crowd size=very small",
                           .default = tags))

acled_merged <- acled_merged %>%
  mutate(
    crowd_size_cat = case_when(
      str_detect(tags, "crowd size=very small") ~ "Very small (<20)",
      str_detect(tags, "crowd size=small")       ~ "Small (20-99)",
      str_detect(tags, "crowd size=medium")      ~ "Medium (100-999)",
      str_detect(tags, "crowd size=large")       ~ "Large (1,000-9,999)",
      str_detect(tags, "crowd size=massive")     ~ "Massive (10,000+)",
      TRUE ~ NA_character_
    ),
    crowd_size_midpoint = case_when(
      crowd_size_cat == "Very small (<20)"    ~ 10,
      crowd_size_cat == "Small (20-99)"        ~ 60,
      crowd_size_cat == "Medium (100-999)"     ~ 550,
      crowd_size_cat == "Large (1,000-9,999)"  ~ 5500,
      crowd_size_cat == "Massive (10,000+)"    ~ 15000,
      TRUE ~ NA_real_
    ),
    log_crowd_size = log1p(crowd_size_midpoint)
  )

table(acled_merged$crowd_size_cat, useNA = "always")
summary(acled_merged$crowd_size_midpoint)

# =============================================================================
# 4. PROTESTOR VIOLENCE
# =============================================================================

acled_merged <- acled_merged %>%
  mutate(
    protestor_violence = case_when(
      str_detect(tolower(notes),
                 "threw (stones?|rocks?|bottles?|fireworks?|firecrackers?|molotov|projectiles?|paint)|
        |hurled (stones?|rocks?|bottles?|fireworks?|firecrackers?|molotov|projectiles?)|
        |threw .{0,20} at (police|officers|gendarm)|
        |hurled .{0,20} at (police|officers|gendarm)|
        |clashed with police|clashed with officers?|clashed with gendarm|
        |attacked (police|officers|gendarm)|
        |assaulted (police|officers|gendarm)|
        |molotov|broke through (police|a police|the police)|
        |stormed (the parliament|the building|the prefecture|the courthouse)|
        |set fire to (police|vehicles|cars|a police)|
        |vandali[sz]ed") ~ 1,
      TRUE ~ 0
    )
  )

table(acled_merged$protestor_violence)
table(acled_merged$protestor_violence, acled_merged$sub_event_type)

# =============================================================================
# 5. URBAN/RURAL TYPOLOGY
# =============================================================================

nuts3_2021 <- gisco_get_nuts(year = "2021", epsg = "4326", resolution = "03", nuts_level = "3")

acled_merged <- acled_merged %>%
  select(-any_of(c("NUTS_ID", "NUTS_NAME", "CNTR_CODE", "urb_rur")))

acled_sf <- acled_merged %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

acled_nuts3 <- st_join(acled_sf, nuts3_2021 %>% select(NUTS_ID, NUTS_NAME, CNTR_CODE, URBN_TYPE))

acled_merged <- acled_merged %>%
  left_join(
    acled_nuts3 %>% st_drop_geometry() %>%
      select(event_id_cnty, NUTS_ID, NUTS_NAME, CNTR_CODE, URBN_TYPE),
    by = "event_id_cnty"
  ) %>%
  mutate(
    urb_rur = factor(URBN_TYPE, levels = c(1, 2, 3),
                     labels = c("Predominantly urban", "Intermediate", "Predominantly rural")),
    urb_rur = relevel(urb_rur, ref = "Predominantly urban")
  ) %>%
  select(-URBN_TYPE)

table(is.na(acled_merged$NUTS_ID))
acled_merged %>% filter(is.na(NUTS_ID)) %>% count(country, sort = TRUE)
table(acled_merged$urb_rur, useNA = "always")

# =============================================================================
# 6. TEMPORAL CONTROLS
# =============================================================================

acled_merged <- acled_merged %>%
  mutate(
    event_date  = as.Date(event_date),
    day_of_week = weekdays(event_date),
    is_weekend  = as.integer(day_of_week %in% c("Saturday", "Sunday")),
    year        = lubridate::year(event_date)
  )

table(acled_merged$is_weekend)
table(acled_merged$year)

# =============================================================================
# 7. NATIONAL INCUMBENT PARTISANSHIP
# =============================================================================
# France's "right" classification confirmed against CPDS; the country x
# incumbent crosstab has been checked and confirmed correct.

acled_merged <- acled_merged %>%
  mutate(
    incumbent_partisan_type = case_when(
      country == "France"                                              ~ "right",
      country == "Germany" & event_date <  as.Date("2021-12-08")        ~ "centre",
      country == "Germany" & event_date >= as.Date("2021-12-08")        ~ "left",
      country == "Italy"   & event_date <  as.Date("2022-10-22")        ~ "centre",
      country == "Italy"   & event_date >= as.Date("2022-10-22")        ~ "right",
      country == "Spain"                                                ~ "left",
      country == "United Kingdom" & event_date <  as.Date("2024-07-05") ~ "right",
      country == "United Kingdom" & event_date >= as.Date("2024-07-05") ~ "left",
      TRUE ~ NA_character_
    ),
    incumbent_left  = as.integer(incumbent_partisan_type == "left"),
    incumbent_right = as.integer(incumbent_partisan_type == "right")
  )

table(acled_merged$country, acled_merged$incumbent_partisan_type, useNA = "always")
table(acled_merged$event_partisan_type_final, acled_merged$incumbent_partisan_type)

# =============================================================================
# SAVE
# =============================================================================

stopifnot(nrow(acled_merged) == n_start)

message("\nFinal DV totals - both thresholds:")
message("  police_presence (0.5): ", sum(acled_merged$police_presence, na.rm = TRUE),
        " | (0.8): ", sum(acled_merged$police_presence_08, na.rm = TRUE))
message("  arrest (0.5): ", sum(acled_merged$arrest, na.rm = TRUE),
        " | (0.8): ", sum(acled_merged$arrest_08, na.rm = TRUE))
message("  brutality (0.5): ", sum(acled_merged$brutality, na.rm = TRUE),
        " | (0.8): ", sum(acled_merged$brutality_08, na.rm = TRUE),
        " - CAUTION: ~45% of brutality's 0.5-threshold positives fall below ",
        "0.8; treat brutality results conservatively regardless of threshold.")

write.csv(acled_merged, checkpoint_path, row.names = FALSE)

cat("\n=== Control variable construction (rethresholded v3) complete ===\n")
cat("Saved to:", checkpoint_path, "\n")
cat("Rows:", nrow(acled_merged), "\n")
cat("Spatial controls reused for", n_matched, "rows; backfilled for the rest.\n")