# Title: Classifying Partisanship in ACLED data (UK)
# Author: Katelyn Nutley 
# Date: 22-10-2025

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)

acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/acled_all_countries_combined_2020_2024.csv")

# Subset to UK to start with (proof of concept here):

uk_acled_df <- acled_df %>% 
  filter(country == "United Kingdom") # Right, this looks good. 

# Now to parse and isolate the actors: 

# Parsing function: 
parse_actor <- function(actor) {
  actor %>%
    # Remove brackets only
    str_remove_all("\\[[^]]+\\]") %>%
    # Remove common prefixes
    str_remove_all("^members of |^supporters of |^protesters from ") %>%
    # Remove extra whitespace
    str_squish() %>%
    # Trim
    str_trim()
}

# Get all actors and split them
all_actors_raw <- c(
  uk_acled_df$actor1,
  uk_acled_df$actor2,
  uk_acled_df$assoc_actor_1,
  uk_acled_df$assoc_actor_2
)

# Create dataframe, split by semicolon, and deduplicate
unique_uk_actors_df <- data.frame(
  actor_raw = all_actors_raw
) %>%
  filter(!is.na(actor_raw)) %>%
  # Parse first
  mutate(actor_parsed = parse_actor(actor_raw)) %>%
  # Split by semicolon into separate rows
  separate_rows(actor_parsed, sep = ";") %>%
  # Clean whitespace
  mutate(
    actor_clean = str_trim(actor_parsed),
    actor_clean_lower = str_to_lower(actor_clean)
  ) %>%
  # Remove empty
  filter(actor_clean != "") %>%
  # Now collapse to unique actors only (this is the key step)
  distinct(actor_clean, .keep_all = FALSE) %>%
  # Add back a clean lowercase version for matching
  mutate(actor_clean_lower = str_to_lower(actor_clean))

View(unique_uk_actors_df) # Okay, now that we have the unique actors! 

# Step 1: Classify based on Government! 

cpds_data <- read_xlsx("~/Documents/GitHub/Police_Response/external datasets/cpds-1960-2023-update-2025.xlsx")
cpds_uk <- cpds_data %>%
  filter(country == "United Kingdom") # This serves as a country specific reference point. 
# The CPDS has a weighted (that's not the right word) number that is meant to designate
# how left, right, or centrist a government in power is. For 2011-2024, the government
# is described as 0.000 left, 0.000 centre, and 1.000 right - so I just adapted that
# and I've classified the governments accordingly with the gov't from 2024- as left ("1").

# Create separate left/right/centre/unknown columns and classify according to gov: 

unique_uk_actors_df <- unique_uk_actors_df %>%
  mutate(
    # Initialize all as 0
    left = 0L,
    right = 0L,
    centre = 0L,
    unknown = 0L,
    
    # Classify governments
    left = if_else(actor_clean == "Government of the United Kingdom (2024-)", 1L, left),
    left = if_else(actor_clean == "Government of the United Kingdom (2024-) Northern Ireland Executive", 1L, left),
    right = if_else(actor_clean == "Government of the United Kingdom (2010-2024)", 1L, right),
    right = if_else(actor_clean == "Government of the United Kingdom (2010-2024) Northern Ireland Executive", 1L, right),
    right = if_else(actor_clean == "Former Government of the United Kingdom (2010-2024)", 1L, right),
    right = if_else(actor_clean == "Former Government of the United Kingdom (2010-2024) Northern Ireland Executive", 1L, right),
    # Mark as unknown if nothing is classified yet
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, unknown)
  )

unique_uk_actors_df %>%
  filter(str_detect(actor_clean_lower, "government of the united kingdom")) %>%
  select(actor_clean, left, right, centre, unknown) # love-lah 
table(unique_uk_actors_df$unknown) # Right, so 6 have been classified! 

# Step 2: Classify based on Mainstream Political Parties  

ppdb_data <- read_csv("~/Documents/GitHub/Police_Response/external datasets/PPDB_Round2_v4.csv")
ppdb_uk <- ppdb_data %>%
  filter(COUNTRY == "United Kingdom") # So, the difference here is that the PPDB has 
# a categorical variable in the PARTYFAMNEW column: 
# 1. Communists/Left Socialists - Left 
## explanation: anti-capitalist, socialist economic policies, strong welfare state
# 2. Social Democrats - Left
## explanation: centre-left, support welfare states, regulated markets, workers' rights
# 3. Greens - Left 
## explanation: environmental focus with progressive social policies 
# 4. Christian Democrats - Right
## explanation: centre-right, conservative social values, market economy with social protections
# 5. Conservative - Right 
## explanation: traditional values, free market economics, limited state intervention
# 6. Liberals - Centre 
## explanation: in Euro contect, typically pro-market, socially progressive
# 7. Agraian/ Farmer's Party - Centre
## explanation: rural interests, often pragmatic coalition partners, not left or right 
# 8. Right-wing (populists) - Right 
## explanation: nationalist, anti-immigration, euroskeptic 
# 9. Far right - Right 
# explanation: extreme nationalism, authoritarian tendencies 
# 10. Regionalist - Unknown
## explanation: varies widely, can't say one way or the other; context dependent 
# 11. Ethnic - Unknown
## explanation: varies 
# 12. Confessional, other than Christian Democratic (i.e. Islamic, Buddhist, Hindu, etc.) - Unknown
## explanation: varies 
# 13. Other - provide label in followign text variable - Unknown
## explanation - varies 

# The Mainstream Political Parties Classification 

# This is done in direct reference to what was in the PPDB data; 
unique_uk_actors_df <- unique_uk_actors_df %>%
  mutate(
    # Match Labour Party (Social Democrats - Left)
    left = if_else(str_detect(actor_clean_lower, "labour|labor") & 
                     !str_detect(actor_clean_lower, "liberal"), 1L, left),  # <-- CHANGED 0L to left
    
    # Match Conservative Party (Conservative - Right)
    right = if_else(str_detect(actor_clean_lower, "conservative|tory|tories"), 1L, right),  # <-- CHANGED 0L to right
    
    # Match Liberal Democrats (Liberals - Centre)
    centre = if_else(str_detect(actor_clean_lower, "liberal democrat|lib dem"), 1L, centre),  # <-- CHANGED 0L to centre
    
    # Match Green Party (Greens - Left)
    left = if_else(str_detect(actor_clean_lower, "green party|greens"), 1L, left),
    
    # Match UKIP (Right-wing populist - Right)
    right = if_else(str_detect(actor_clean_lower, "ukip|uk independence"), 1L, right),
    
    # Match SNP (Socialist - Left)
    left = if_else(str_detect(actor_clean_lower, "scottish socialist|socialist"), 1L, left),
    
    # Match Plaid Cymru (Democrats?)
    left = if_else(str_detect(actor_clean_lower, "plaid cymru"), 1L, left),
    
    # Update unknown (only reset to 1 if nothing is classified)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )

unique_uk_actors_df %>%
  filter(left == 1 | right == 1 | centre == 1) %>%
  filter(str_detect(actor_clean_lower, "party|labour|conservative|liberal|green|snp|plaid|ukip|brexit|reform")) %>%
  select(actor_clean, left, right, centre, unknown) %>%
  View()

table(unique_uk_actors_df$unknown) # Right, this looks good so far! 27 classified!

# Step 3: Indetify Fringe Political Parties Using PPDB Classification System

unique_uk_actors_df <- unique_uk_actors_df %>%
  mutate(
    # LEFT FRINGE PARTIES
    # Communists/Left Socialists
    left = if_else(str_detect(actor_clean_lower, "socialist party|scottish socialist|communist|saoradh|people before profit|scottish national party|progressive unionist party|breakthrough party"), 1L, left),
    
    # Social Democrats (Sinn Féin - left-wing republicans)
    left = if_else(str_detect(actor_clean_lower, "sinn fein|sinn féin|fianna fail|new irish republican army|republican militia"), 1L, left),
    
    # Minority Rights Parties 
    left = if_else(str_detect(actor_clean_lower, "women's equality party|an dream dearg"), 1L, left),
    
    # CENTRE FRINGE PARTIES
    # Liberals (Northern Ireland)
    centre = if_else(str_detect(actor_clean_lower, "alliance party|alliance party of northern ireland"), 1L, centre),
    
    # RIGHT FRINGE PARTIES
    # Conservatives (Northern Ireland Unionists)
    right = if_else(str_detect(actor_clean_lower, "democratic unionist party|dup|traditional unionist voice|tuv|ulster unionist party|uup"), 1L, right),
    
    # Right-wing populists
    right = if_else(str_detect(actor_clean_lower, "brexit party|reform uk"), 1L, right),
    
    # Far right
    right = if_else(str_detect(actor_clean_lower, "patriotic alternative|scottish family party|national front|britain first|alba"), 1L, right),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )

# View classified parties
unique_uk_actors_df %>%
  filter(left == 1 | right == 1 | centre == 1) %>%
  filter(str_detect(actor_clean_lower, "party|labour|conservative|liberal|green|snp|plaid|ukip|brexit|reform|socialist|sinn|unionist|alliance")) %>%
  select(actor_clean, left, right, centre, unknown) %>%
  View() # this is just a rough look, so you're aware 

table(unique_uk_actors_df$unknown) # Right, this looks good so far! 50 classed!!! 

# Step 4: Classify Based on Stated Objectives! 

unique_uk_actors_df <- unique_uk_actors_df %>%
  mutate(
    # LEFTIST GROUPS
    # Anti-Racism Groups
    left = if_else(str_detect(actor_clean_lower, "stand up to racism|racism|black lives matter|united against racism"), 1L, left),
    
    # Pro-Migrant Groups
    left = if_else(str_detect(actor_clean_lower, "care4calais|migrants organise|united voices of the world|safe passage|calais action|unis resist border controls|lesbians and gays support the migrants"), 1L, left),
    
    # Human Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "amnesty international|oxfam|save our rights uk|save our children"), 1L, left),
    
    # Anti-War
    left = if_else(str_detect(actor_clean_lower, "stop the war|women in black|campaign for nuclear disarmament|pax christi"), 1L, left),
    
    # Palestine Solidarity Groups 
    left = if_else(str_detect(actor_clean_lower, "ireland palestine solidarity|palestine action|palestinian group|palestine solidarity campaign|sisters uncut|palestine national"), 1L, left),
    
    # Iranian Resistance Groups 
    left = if_else(str_detect(actor_clean_lower, "people's mujahedin organisation of iran|national council of resistance of iran"), 1L, left),
    
    # Anti-Austerity/Tenants Rights Groups
    left = if_else(str_detect(actor_clean_lower, "people's assembly against austerity|living rent|insulate britain|acorn union|this is rigged"), 1L, left),
    
    # Social Goods Groups 
    left = if_else(str_detect(actor_clean_lower, "keep our nhs public|defend our nhs|north wales save outdoor education|royal college of nursing|british medical association"), 1L, left),
    
    # Antifascist Groups
    left = if_else(str_detect(actor_clean_lower, "unite against fascism|antifa"), 1L, left),
    
    # Environmental Groups
    left = if_else(str_detect(actor_clean_lower, "extinction rebellion|friends of the earth|fridays for future|climate network|climate action|critical mass|cop26 coalition|just stop oil|fossil free|surfers against sewage|world wildlife fund|ocean rebellion|greenpeace|stop cambo|stay grounded|scientist rebellion|mothers rebellion"), 1L, left),
    
    # Animal Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "animal equality|people for the ethical treatment of animals|animal rising|direct action everywhere|anonymous for the voiceless|animal rebellion"), 1L, left),
    
    # Womens Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "reclaim the night|women against state pension inequality|reclaim the agenda|rosa: for reproductive rights, against oppression, sexism & austerity|pregnant then screwed|party of women"), 1L, left),
    
    # CENTRE GROUPS
    centre = if_else(str_detect(actor_clean_lower, "yellow vests|border communities against brexit|stop 5g international|save british farming|make votes matter|all under one banner|stop hs2|action for scotland|world wide rally for freedom|irish farmers association"), 1L, centre),
    
    # RIGHT GROUPS
    # Anti-Migrant Groups
    right = if_else(str_detect(actor_clean_lower, "for britain movement"), 1L, right),
    
    # Nationalist Groups
    right = if_else(str_detect(actor_clean_lower, "uk freedom movement|orange order|ulster defenders of the realm|pakistan muslim league|english constitution party|hizb ut-tahrir|apprentice boys of derry|loyalist militia|loyalist group|qanon"), 1L, right),
    
    # Anti-Abortion Groups
    right = if_else(str_detect(actor_clean_lower, "center for bio-ethical reform|40 days for life"), 1L, right),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )

# View classified advocacy/activist groups
unique_uk_actors_df %>%
  filter(left == 1 | right == 1 | centre == 1) %>%
  filter(str_detect(actor_clean_lower, 
                    "racism|black lives matter|migrants|calais|amnesty|oxfam|stop the war|palestine|austerity|living rent|insulate britain|nhs|fascism|antifa|extinction rebellion|friends of the earth|fridays for future|climate|cop26|just stop oil|fossil free|surfers against sewage|animal equality|ethical treatment|animal rising|for britain|uk freedom")) %>%
  select(actor_clean, left, right, centre, unknown) %>%
  View()

table(unique_uk_actors_df$unknown) # 140!!!! 

# Step 5: Classification of Labor Unions Based on Previous Party Alignment

unique_uk_actors_df <- unique_uk_actors_df %>%
  mutate(
    # LEFT UNIONS
    left = if_else(str_detect(actor_clean_lower, "northern ireland public service alliance|national union of rail, maritime and transport workers|fire brigades union|general, municipal and boilermakers union|unison the public service union|unite the union|educational institute of scotland|union of shop, distributive and allied workers|public and commercial services union|nurses united uk|independent workers union of great britain"), 1L, left),
    
    # Left Unions (cont'd)
    left = if_else(str_detect(actor_clean_lower, "university and college union|national education union|mandate|services industrial professional and technical union|national association of schoolmasters union|app drivers and couriers union|associated society of locomotive engineers|communication workers union|ulster teachers' union|irish congress of trade unions"), 1L, left),
    
    # CENTRE UNIONS
    #centre = if_else(str_detect(actor_clean_lower, ""), 1L, centre),
    
    # RIGHT UNIONS
    #right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Update unknown (only reset to 1 if nothing is classified)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )
table(unique_uk_actors_df$unknown) #165 and done!!! 

# Step 6: Map partisanship back to original ACLED data

# Create mapping function to handle multiple actors per field
map_partisanship <- function(actor_string, lookup_df) {
  if (is.na(actor_string)) {
    return(list(left = 0L, right = 0L, centre = 0L, unknown = 1L))
  }
  
  # Parse and split actors (same logic as before)
  actors <- actor_string %>%
    str_remove_all("\\[[^]]+\\]") %>%
    str_remove_all("^members of |^supporters of |^protesters from ") %>%
    str_squish() %>%
    str_split(";") %>%
    unlist() %>%
    str_trim()
  
  # Look up each actor and aggregate
  results <- actors %>%
    map_dfr(~{
      match <- lookup_df %>% filter(actor_clean == .x)
      if (nrow(match) > 0) {
        match[1, c("left", "right", "centre", "unknown")]
      } else {
        data.frame(left = 0L, right = 0L, centre = 0L, unknown = 1L)
      }
    })
  
  # Aggregate: if ANY actor is left/right/centre, code as 1
  list(
    left = as.integer(any(results$left == 1)),
    right = as.integer(any(results$right == 1)),
    centre = as.integer(any(results$centre == 1)),
    unknown = as.integer(all(results$left == 0 & results$right == 0 & results$centre == 0))
  )
}

# Apply to all actor columns
uk_acled_df <- uk_acled_df %>%
  mutate(
    # Actor 1
    actor1_partisan = map(actor1, ~map_partisanship(.x, unique_uk_actors_df)),
    actor1_left = map_int(actor1_partisan, "left"),
    actor1_right = map_int(actor1_partisan, "right"),
    actor1_centre = map_int(actor1_partisan, "centre"),
    actor1_unknown = map_int(actor1_partisan, "unknown"),
    
    # Actor 2
    actor2_partisan = map(actor2, ~map_partisanship(.x, unique_uk_actors_df)),
    actor2_left = map_int(actor2_partisan, "left"),
    actor2_right = map_int(actor2_partisan, "right"),
    actor2_centre = map_int(actor2_partisan, "centre"),
    actor2_unknown = map_int(actor2_partisan, "unknown"),
    
    # Associated Actor 1
    assoc_actor1_partisan = map(assoc_actor_1, ~map_partisanship(.x, unique_uk_actors_df)),
    assoc_actor1_left = map_int(assoc_actor1_partisan, "left"),
    assoc_actor1_right = map_int(assoc_actor1_partisan, "right"),
    assoc_actor1_centre = map_int(assoc_actor1_partisan, "centre"),
    assoc_actor1_unknown = map_int(assoc_actor1_partisan, "unknown"),
    
    # Associated Actor 2
    assoc_actor2_partisan = map(assoc_actor_2, ~map_partisanship(.x, unique_uk_actors_df)),
    assoc_actor2_left = map_int(assoc_actor2_partisan, "left"),
    assoc_actor2_right = map_int(assoc_actor2_partisan, "right"),
    assoc_actor2_centre = map_int(assoc_actor2_partisan, "centre"),
    assoc_actor2_unknown = map_int(assoc_actor2_partisan, "unknown")
  ) %>%
  select(-ends_with("_partisan"))  # Remove intermediate list columns

# Step 7: Create event-level partisanship indicators

uk_acled_df <- uk_acled_df %>%
  mutate(
    # Replace any remaining NAs with 0 (shouldn't be needed but just in case)
    across(c(actor1_left, actor1_right, actor1_centre, actor1_unknown,
             actor2_left, actor2_right, actor2_centre, actor2_unknown,
             assoc_actor1_left, assoc_actor1_right, assoc_actor1_centre, assoc_actor1_unknown,
             assoc_actor2_left, assoc_actor2_right, assoc_actor2_centre, assoc_actor2_unknown),
           ~replace_na(.x, 0L)),
    
    # Calculate event-level indicators (any actor is left/right/centre)
    event_left = as.integer(actor1_left == 1 | actor2_left == 1 | 
                              assoc_actor1_left == 1 | assoc_actor2_left == 1),
    
    event_right = as.integer(actor1_right == 1 | actor2_right == 1 | 
                               assoc_actor1_right == 1 | assoc_actor2_right == 1),
    
    event_centre = as.integer(actor1_centre == 1 | actor2_centre == 1 | 
                                assoc_actor1_centre == 1 | assoc_actor2_centre == 1),
    
    # Create conflict type variable
    event_partisan_type = case_when(
      event_left == 1 & event_right == 1 ~ "left_and_right",
      event_left == 1 & event_centre == 1 ~ "left_and_centre",
      event_right == 1 & event_centre == 1 ~ "right_and_centre",
      event_left == 1 & event_right == 0 & event_centre == 0 ~ "left_only",
      event_right == 1 & event_left == 0 & event_centre == 0 ~ "right_only",
      event_centre == 1 & event_left == 0 & event_right == 0 ~ "centre_only",
      TRUE ~ "unknown"
    )
  )

# Check the results
table(uk_acled_df$event_partisan_type)

# Summary statistics
uk_acled_df %>%
  count(event_partisan_type) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))

# View some examples of left vs right conflicts
uk_acled_df %>%
  filter(event_partisan_type == "left_vs_right") %>%
  select(event_date, actor1, actor2, event_type, sub_event_type, event_partisan_type) %>%
  head(20) %>%
  View()

# Check what the most common unclassified actors are
unclassified_actors <- uk_acled_df %>%
  filter(event_partisan_type == "unknown") %>%
  select(actor1, actor2, assoc_actor_1, assoc_actor_2) %>%
  pivot_longer(everything(), names_to = "actor_field", values_to = "actor") %>%
  filter(!is.na(actor)) %>%
  count(actor, sort = TRUE) %>%
  head(50)

print(unclassified_actors, n = 50)

View(uk_acled_df) 

#################################################################################
# SAVING DOCUMENTS 
#################################################################################

write_csv(uk_acled_df, "~/Downloads/uk_acled_partisan_classification_2020_2024.csv")
write_csv(unique_uk_actors_df, "~/Downloads/uk_acled_classified_actors_2020_2024.csv")

