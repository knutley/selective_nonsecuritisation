# Title: Classifying Partisanship in ACLED data (France)
# Author: Katelyn Nutley 
# Date: 28-10-2025

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)

acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/acled_all_countries_combined_2020_2024.csv")

# Subset to France

france_acled_df <- acled_df %>% 
  filter(country == "France") 

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
  france_acled_df$actor1,
  france_acled_df$actor2,
  france_acled_df$assoc_actor_1,
  france_acled_df$assoc_actor_2
)

# Create dataframe, split by semicolon, and deduplicate
unique_france_actors_df <- data.frame(
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

View(unique_france_actors_df)

# Step 1: Classify based on Government! 

cpds_data <- read_xlsx("~/Documents/GitHub/Police_Response/external datasets/cpds-1960-2023-update-2025.xlsx")
cpds_france <- cpds_data %>%
  filter(country == "France") 
View(cpds_france)

# Create separate left/right/centre/unknown columns and classify according to gov: 

unique_france_actors_df <- unique_france_actors_df %>%
  mutate(
    # Initialize all as 0
    left = 0L,
    right = 0L,
    centre = 0L,
    unknown = 0L,
    
    # Classify governments
    left = if_else(actor_clean == "Former Government of France (2017-)", 1L, left), # Hollande
    left = if_else(actor_clean == "Former Government of Spain (2020-)", 1L, left), # second Sanchez
    right = if_else(actor_clean == "Government of France (2017-)", 1L, right), # Macron
    # Mark as unknown if nothing is classified yet
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, unknown)
  ) # I can throw spain in there because it's classified in CPDS; might remove it later 

unique_france_actors_df %>%
  filter(str_detect(actor_clean_lower, "government of france")) %>%
  select(actor_clean, left, right, centre, unknown) # love-lah 
table(unique_france_actors_df$unknown) # Right, so 3 have been classified! 

# Step 2: Classify based on Mainstream Political Parties  

ppdb_data <- read_csv("~/Documents/GitHub/Police_Response/external datasets/PPDB_Round2_v4.csv")
ppdb_france <- ppdb_data %>%
  filter(COUNTRY == "France") # So, the difference here is that the PPDB has 
# a categorical variable in the PARTYFAMNEW column: 
# 1. Communists/Left Socialists - Left 
## explanation: anti-capitalist, socialist economic policies, strong welfare state
# 2. Social Democrats - Left (Louise says 'centre' in France)
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
unique_france_actors_df <- unique_france_actors_df %>%
  mutate(
    # Match The Republicans (right according to PPDB)
    right = if_else(str_detect(actor_clean_lower, "the republicans|right for the republic"), 1L, right),
    
    # Match the National Rally (Nationalist - far right)
    right = if_else(str_detect(actor_clean_lower, "national rally|generation identitaire|reconquest party|patriots collective|national council of the new resistance|the patriots|zouaves paris|france arise|party of france|arise picardy|french action|independentist youth"), 1L, right),
    
    # Match broad, right leaning coalitions 
    right = if_else(str_detect(actor_clean_lower, "miscellaneous right|league of the south|to the right"), 1L, right),
    
    # Match Union of Democrats and Independents (usually classed as centre or centre right)
    centre = if_else(str_detect(actor_clean_lower, "democrats and independents|miscellaneous center|democratic workers party|democratic movement|miscellaneous center"), 1L, centre),
    
    # Match The Republic on the Move (usually classed as centre or centre-right; classic Liberals - changed their name) 
    centre = if_else(str_detect(actor_clean_lower, "republic on the move|together for the republic|renaissance|horizons|let's be free|region and people in solidarity"), 1L, centre),
    
    # Match Socialist/Communist Parties (Left)
    left = if_else(str_detect(actor_clean_lower, "socialist party|ecosocialist left|communist|independent workers party|workers party of belgium|workers party|french revolutionary communist party|workers' struggle|independent workers' party|workers' party (france)|workers' front|pt: workers' party (france)"), 1L, left),
    
    # Match the Social Democrats (inclusive of the NPF, left)
    left = if_else(str_detect(actor_clean_lower, "social democrat|new popular|democratic left|republican left|generation.s|territories of progress|mrc: citizen and republican movement|france rebellious|breton party|public square"), 1L, left),
    
    # Match the Greens (Left)
    left = if_else(str_detect(actor_clean_lower, "the greens|greens/efa|ecology generation|the ecologists|ecologist pole"), 1L, left),
    
    # Broader Left Coalitions
    left = if_else(str_detect(actor_clean_lower, "left party|the left|radical party of the left|the left in the european parliament|miscellaneous left|federation of the republican left|together left|radical party"), 1L, left),
    
    # Anticapitalist
    left = if_else(str_detect(actor_clean_lower, "new anticapitalist party|permanent revolution"), 1L, left),
    
    # Left-Nationalists (Corsican/Basque/Caledonia liberation movement)
    left = if_else(str_detect(actor_clean_lower, "free corsica|occitan party|paolina youth|euskal herria bildu|caledonian union|national liberation fron of corsica|committees for the defence of the republic"), 1L, left),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )
# Ask Lou about this - perhaps there's a remaining issue with Democratic Workers' Party (I read a lot suggesting that it trended centre).
# Also ask about the Basque Nationalist Party (PNV). 
table(unique_france_actors_df$unknown) 

# Classify Based on Stated Objective: 

unique_france_actors_df <- unique_france_actors_df %>%
  mutate(
    # LEFTIST GROUPS
    
    # Anti-Racism Groups
    left = if_else(str_detect(actor_clean_lower, "blm: black lives matter|truth for adama|mrap: movement against racism and for friendship between peoples|sos racism|international league against racism"), 1L, left),
    
    # Pro-Migrant Groups
    left = if_else(str_detect(actor_clean_lower, "education without borders|black vests|no border network|united against disposable immigration"), 1L, left),
    
    # Human Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "human rights league|amnesty international|oxfam|icrc: international committee of the red cross|cif: heart forward|doctors of the world|care international|act up-paris"), 1L, left),
    
    # Anti-War
    left = if_else(str_detect(actor_clean_lower, "la cimade|bake bidea|peace movement|women in black"), 1L, left),
    
    # Palestine Solidarity Groups 
    left = if_else(str_detect(actor_clean_lower, "palestinian group (france)|france-palestine solidarity association|bds: boycott, divestment and sanctions|samidoun: palestinian prisoner solidarity network"), 1L, left),
    
    # Anti-Austerity/Tenants Rights Groups
    left = if_else(str_detect(actor_clean_lower, "msf: doctors without borders|mncp: national movement of unemployed and precarious|angry liberal nurses"), 1L, left),
    
    # Antifascist Groups
    left = if_else(str_detect(actor_clean_lower, "antifa|reporters without borders"), 1L, left),
    # I am aware that RSF does not cleanly fit here, but it's mostly to save space 
    
    # Environmental Groups
    left = if_else(str_detect(actor_clean_lower, "velorution|alternatiba|anv-cop 21|greenpeace|extinction rebellion|we want poppies|friends of the earth|fridays for future|people for the ethical treatment of animals|earth uprisings|zero waste europe|students for climate|mothers rebellion|women's environmental network|scientist rebellion|last renovation|last generation|independent ecological movement|sea shepherd conversation society|stay grounded"), 1L, left),
    
    # Animal Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "l214 ethics & animals|paris animals zoopolis|one voice|world wildlife fund|animalist party"), 1L, left),
    
    # Womens Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "french movement for family planning|femen|rosies|rosa: for reproductive rights, against oppression, sexism & austerity"), 1L, left),
    
    # CENTRE GROUPS
    centre = if_else(str_detect(actor_clean_lower, "gj: yellow vests"), 1L, centre),
    
    # RIGHT GROUPS
    # Anti-Migrant Groups/ Pro-Police Action
    right = if_else(str_detect(actor_clean_lower, "angry wives of law enforcement officers"), 1L, right),
    
    # Nationalist Groups
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Anti-Abortion Groups
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )
# Unable to understand what Marchon Enfants! is and, therefore, where it goes. Also
# unclear about FaC: Let's Make Corsica. 
table(unique_france_actors_df$unknown)

# Classification of Labor Unions Based on Previous Party Alignment

unique_france_actors_df <- unique_france_actors_df %>%
  mutate(
    
    # LEFT
    left = if_else(str_detect(actor_clean_lower, "movement of the enterprises of france|general confederation of labor|national high school union|national union of journalists|national union of retirees and seniors|departmental association of parents and friends of people with mental disorders|independent union luxembourg|union of jewish students of france"), 1L, left),
    
    # Note; CGT cut links with the PCF, but still remains moderate left. 
    
    # Assoc. with Social Democratic 
    left = if_else(str_detect(actor_clean_lower, "federation of general student associations|magistrate's union"), 1L, left),
    
    # Assoc. with Socialist/Communist Parties 
    left = if_else(str_detect(actor_clean_lower, "democratic unitary solidarity|united federation of trade unions|workers'force|national union of students of france|french democratic confederation of labor|national labor confederation|kurdistan workers' party|federation of education, culture, and vocational training|national union of physical education"), 1L, left),
    
    # Associ. with Left-Nationalist groups
    left = if_else(str_detect(actor_clean_lower, "union of corsican workers|patriotic workers' commissions|abertzales workers' committees"), 1L, left),
    
    # CENTRE
    centre = if_else(str_detect(actor_clean_lower, "french confederation of christian workers|union of magistrates"), 1L, centre),
    # Often this is the Christian democratic unions 
    
    # RIGHT
    right = if_else(str_detect(actor_clean_lower, "national police alliance|national federation of farmers union|young farmers|rural coordination|national education staff union|prison guards union|association of french mayors|federation of young farmers"), 1L, right),
    #No idea if I should put the national union of autonomous trade unions here? They seem to vote more conservative? 
    #FSNEA seems to belong here based on its interests of large-scale, intensive, and export-oriented farming
    
    # Unknown
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )
table(unique_france_actors_df$unknown)

# Map partisanship back to original ACLED data

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
france_acled_df <- france_acled_df %>%
  mutate(
    # Actor 1
    actor1_partisan = map(actor1, ~map_partisanship(.x, unique_france_actors_df)),
    actor1_left = map_int(actor1_partisan, "left"),
    actor1_right = map_int(actor1_partisan, "right"),
    actor1_centre = map_int(actor1_partisan, "centre"),
    actor1_unknown = map_int(actor1_partisan, "unknown"),
    
    # Actor 2
    actor2_partisan = map(actor2, ~map_partisanship(.x, unique_france_actors_df)),
    actor2_left = map_int(actor2_partisan, "left"),
    actor2_right = map_int(actor2_partisan, "right"),
    actor2_centre = map_int(actor2_partisan, "centre"),
    actor2_unknown = map_int(actor2_partisan, "unknown"),
    
    # Associated Actor 1
    assoc_actor1_partisan = map(assoc_actor_1, ~map_partisanship(.x, unique_france_actors_df)),
    assoc_actor1_left = map_int(assoc_actor1_partisan, "left"),
    assoc_actor1_right = map_int(assoc_actor1_partisan, "right"),
    assoc_actor1_centre = map_int(assoc_actor1_partisan, "centre"),
    assoc_actor1_unknown = map_int(assoc_actor1_partisan, "unknown"),
    
    # Associated Actor 2
    assoc_actor2_partisan = map(assoc_actor_2, ~map_partisanship(.x, unique_france_actors_df)),
    assoc_actor2_left = map_int(assoc_actor2_partisan, "left"),
    assoc_actor2_right = map_int(assoc_actor2_partisan, "right"),
    assoc_actor2_centre = map_int(assoc_actor2_partisan, "centre"),
    assoc_actor2_unknown = map_int(assoc_actor2_partisan, "unknown")
  ) %>%
  select(-ends_with("_partisan"))  # Remove intermediate list columns

# Create event-level partisanship indicators

france_acled_df <- france_acled_df %>%
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
table(france_acled_df$event_partisan_type)

# Summary statistics
france_acled_df %>%
  count(event_partisan_type) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))

# View some examples of left vs right conflicts
france_acled_df %>%
  filter(event_partisan_type == "left_vs_right") %>%
  select(event_date, actor1, actor2, event_type, sub_event_type, event_partisan_type) %>%
  head(20) %>%
  View()

# Check what the most common unclassified actors are
unclassified_actors <- france_acled_df %>%
  filter(event_partisan_type == "unknown") %>%
  select(actor1, actor2, assoc_actor_1, assoc_actor_2) %>%
  pivot_longer(everything(), names_to = "actor_field", values_to = "actor") %>%
  filter(!is.na(actor)) %>%
  count(actor, sort = TRUE) %>%
  head(50)

print(unclassified_actors, n = 50)

View(france_acled_df) 

#################################################################################
# SAVING DOCUMENTS 
#################################################################################

write_csv(france_acled_df, "~/Downloads/france_acled_partisan_classification_2020_2024.csv")
write_csv(unique_france_actors_df, "~/Downloads/france_acled_classified_actors_2020_2024.csv")

