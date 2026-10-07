# Trying to access ACLED's API 
# 09-10-2025

# Required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(R6)

# ============================================================================
# ACLED Client Class
# ============================================================================

ACLEDClient <- R6Class("ACLEDClient",
                       public = list(
                         email = NULL,
                         password = NULL,
                         token_url = "https://acleddata.com/oauth/token",
                         base_url = "https://acleddata.com/api/acled/read",
                         access_token = NULL,
                         refresh_token = NULL,
                         
                         # Initialize the client
                         initialize = function(email, password) {
                           self$email <- email
                           self$password <- password
                         },
                         
                         # Authenticate and get access token
                         authenticate = function() {
                           response <- POST(
                             url = self$token_url,
                             body = list(
                               username = self$email,
                               password = self$password,
                               grant_type = "password",
                               client_id = "acled",
                               scope = "authenticated" 
                             ),
                             encode = "form"
                           )
                           
                           if (status_code(response) == 200) {
                             token_data <- content(response, "parsed")
                             self$access_token <- token_data$access_token
                             self$refresh_token <- token_data$refresh_token
                             message("✓ Authentication successful")
                             return(TRUE)
                           } else {
                             stop(paste("Authentication failed:", status_code(response), 
                                        content(response, "text")))
                           }
                         },
                         
                         # Get data from ACLED API
                         get_data = function(country = NULL, year = NULL, 
                                             event_date_start = NULL, event_date_end = NULL,
                                             limit = 5000, format = "json", ...) {
                           # Authenticate if needed
                           if (is.null(self$access_token)) {
                             self$authenticate()
                           }
                           
                           # Build query parameters
                           params <- list()
                           
                           if (format != "json") {
                             params$`_format` <- format
                           }
                           
                           if (!is.null(country)) {
                             params$country <- country
                           }
                           
                           if (!is.null(year)) {
                             params$year <- year
                           }
                           
                           if (!is.null(event_date_start) && !is.null(event_date_end)) {
                             params$event_date <- paste(event_date_start, event_date_end, sep = "|")
                             params$event_date_where <- "BETWEEN"
                           }
                           
                           if (!is.null(limit)) {
                             params$limit <- limit
                           }
                           
                           # Add additional parameters
                           additional_params <- list(...)
                           if (length(additional_params) > 0) {
                             params <- c(params, additional_params)
                           }
                           
                           # Make API request
                           response <- GET(
                             url = self$base_url,
                             query = params,
                             add_headers(
                               Authorization = paste("Bearer", self$access_token),
                               `Content-Type` = "application/json"
                             )
                           )
                           
                           # Handle response
                           if (status_code(response) == 200) {
                             if (format == "json") {
                               data <- content(response, "parsed")
                               if (data$status == 200) {
                                 df <- bind_rows(data$data)
                                 return(df)
                               } else {
                                 stop(paste("API returned error:", toJSON(data, auto_unbox = TRUE)))
                               }
                             } else {
                               return(content(response, "text"))
                             }
                           } else {
                             stop(paste("Request failed:", status_code(response), 
                                        content(response, "text")))
                           }
                         }
                       )
)

# ============================================================================
# Helper Functions
# ============================================================================

# Function to get all data with pagination
get_all_data_paginated <- function(client, country, event_type, year, page_size = 5000) {
  all_data <- list()
  offset <- 0
  page <- 1
  
  repeat {
    cat(sprintf("    Page %d (offset: %d)...\n", page, offset))
    
    df_page <- tryCatch({
      client$get_data(
        country = country,
        event_type = event_type,
        year = year,
        limit = page_size,
        offset = offset
      )
    }, error = function(e) {
      cat(sprintf("    Error on page %d: %s\n", page, e$message))
      return(NULL)
    })
    
    # If no data returned, we're done
    if (is.null(df_page) || nrow(df_page) == 0) {
      break
    }
    
    all_data[[page]] <- df_page
    
    # If we got less than page_size records, we're done
    if (nrow(df_page) < page_size) {
      break
    }
    
    offset <- offset + page_size
    page <- page + 1
    
    # Small delay between pages
    Sys.sleep(0.3)
  }
  
  if (length(all_data) > 0) {
    return(bind_rows(all_data))
  } else {
    return(NULL)
  }
}

# Function to collect protest data for a country across multiple years
collect_country_data <- function(client, country, years, event_type = "Protests", 
                                 output_dir = "~/Downloads") {
  
  cat(sprintf("\n" , strrep("=", 70), "\n"))
  cat(sprintf("COLLECTING DATA FOR: %s\n", toupper(country)))
  cat(sprintf("%s\n\n", strrep("=", 70)))
  
  country_data_list <- list()
  
  for (year in years) {
    cat(sprintf("  Year %d:\n", year))
    
    tryCatch({
      df_temp <- get_all_data_paginated(
        client = client,
        country = country,
        event_type = event_type,
        year = year,
        page_size = 10000
      )
      
      if (!is.null(df_temp) && nrow(df_temp) > 0) {
        country_data_list[[as.character(year)]] <- df_temp
        cat(sprintf("  ✓ %d records retrieved\n\n", nrow(df_temp)))
      } else {
        cat(sprintf("  ✓ 0 records (no data available)\n\n"))
      }
      
    }, error = function(e) {
      cat(sprintf("  ✗ Error: %s\n\n", e$message))
    })
    
    # Delay between years
    Sys.sleep(0.5)
  }
  
  # Combine all years
  if (length(country_data_list) > 0) {
    df_all <- bind_rows(country_data_list, .id = "year_source")
    
    cat(sprintf("SUMMARY FOR %s:\n", toupper(country)))
    cat(sprintf("Total records: %d\n\n", nrow(df_all)))
    
    # Summary by year
    yearly_summary <- df_all %>%
      group_by(year) %>%
      summarise(
        events = n(),
        total_fatalities = sum(fatalities, na.rm = TRUE),
        unique_locations = n_distinct(location),
        .groups = "drop"
      ) %>%
      arrange(year)
    
    print(yearly_summary)
    
    # Save to CSV
    filename <- sprintf("%s/acled_%s_%s.csv", 
                        output_dir,
                        tolower(gsub(" ", "_", country)),
                        event_type)
    write_csv(df_all, filename)
    cat(sprintf("\n✓ Data saved to: %s\n\n", filename))
    
    return(df_all)
  } else {
    cat(sprintf("No data retrieved for %s.\n\n", country))
    return(NULL)
  }
}

# ============================================================================
# Main Execution
# ============================================================================

# Initialize client
client <- ACLEDClient$new(
  email = "insert email",
  password = "insert password"
)

# Authenticate
client$authenticate()

# Define countries and years to collect
countries <- c(
  "United Kingdom",
  "France",
  "Germany",
  "Spain",
  "Italy"
  # Add more countries as needed
)

years <- 2020:2024  # Adjust range as needed

# Collect data for all countries
all_country_data <- list()

for (country in countries) {
  country_data <- collect_country_data(
    client = client,
    country = country,
    years = years,
    event_type = "Protests",
    output_dir = "~/Downloads"
  )
  
  if (!is.null(country_data)) {
    all_country_data[[country]] <- country_data
  }
  
  # Delay between countries
  Sys.sleep(1)
}

# Optional: Combine all countries into one dataset
if (length(all_country_data) > 0) {
  df_all_countries <- bind_rows(all_country_data, .id = "country_source")
  
  cat(sprintf("\n%s\n", strrep("=", 70)))
  cat("OVERALL SUMMARY ACROSS ALL COUNTRIES\n")
  cat(sprintf("%s\n\n", strrep("=", 70)))
  cat(sprintf("Total records: %d\n", nrow(df_all_countries)))
  cat(sprintf("Countries: %d\n", length(unique(df_all_countries$country))))
  cat(sprintf("Years: %s\n\n", paste(range(df_all_countries$year), collapse = " - ")))
  
  # Summary by country and year
  country_year_summary <- df_all_countries %>%
    group_by(country, year) %>%
    summarise(
      events = n(),
      total_fatalities = sum(fatalities, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(country, year)
  
  print(country_year_summary)
  
  # Save combined dataset
  write_csv(df_all_countries, "~/Downloads/acled_all_countries_combined_2020_2024.csv")
  cat("\n✓ Combined data saved to: ~/Downloads/acled_all_countries_combined_2020_2024.csv\n")
}

cat("\n✓ All data collection complete!\n")
