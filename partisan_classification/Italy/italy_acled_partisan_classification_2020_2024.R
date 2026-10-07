# Title: Classifying Partisanship in ACLED data (Italy)
# Author: Katelyn Nutley 
# Date: 10-12-2025

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)

acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/acled_all_countries_combined_2020_2024.csv")

# Subset to Italy 

italy_acled_df <- acled_df %>% 
  filter(country == "Italy") 

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
  italy_acled_df$actor1,
  italy_acled_df$actor2,
  italy_acled_df$assoc_actor_1,
  italy_acled_df$assoc_actor_2
)

# Create dataframe, split by semicolon, and deduplicate
unique_italy_actors_df <- data.frame(
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

View(unique_italy_actors_df)


# Step 1: Classify based on Government! 

cpds_data <- read_xlsx("~/Documents/GitHub/Police_Response/external datasets/cpds-1960-2023-update-2025.xlsx")
cpds_italy <- cpds_data %>%
  filter(country == "Italy") 
View(cpds_italy)

# Create separate left/right/centre/unknown columns and classify according to gov: 

unique_italy_actors_df <- unique_italy_actors_df %>%
  mutate(
    # Initialize all as 0
    left = 0L,
    right = 0L,
    centre = 0L,
    unknown = 0L,
    
    # Classify governments
    centre = if_else(actor_clean == "Former Government of Italy (2018-2022)", 1L, centre),
    centre = if_else(actor_clean == "Government of Italy (2018-2022)", 1L, centre),
    right = if_else(actor_clean == "Government of Italy (2022-)", 1L, right),
    right = if_else(actor_clean == "Former Government of Italy (2022-)", 1L, right),
    right = if_else(actor_clean == "Government of Hungary (2010-)", 1L, right),
    right = if_else(actor_clean == "Government of France (2017-)", 1L, right),
    right = if_else(actor_clean == "Government of Poland (2015-2023)", 1L, right),
    right = if_else(actor_clean == "Government of Greece (2019-)", 1L, right),
    left = if_else(actor_clean == "Government of Canada (2015-)", 1L, left),
    
    # Mark as unknown if nothing is classified yet
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, unknown)
  ) # I can throw spain in there because it's classified in CPDS; might remove it later 

unique_italy_actors_df %>%
  filter(str_detect(actor_clean_lower, "government of italy")) %>%
  select(actor_clean, left, right, centre, unknown) # love-lah 
table(unique_italy_actors_df$unknown) # Right, so 9 have been classified! 


# Step 2: Classify based on Mainstream Political Parties  

ppdb_data <- read_csv("~/Documents/GitHub/Police_Response/external datasets/PPDB_Round2_v4.csv")
ppdb_italy <- ppdb_data %>%
  filter(COUNTRY == "Italy") # So, the difference here is that the PPDB has 
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
unique_italy_actors_df <- unique_italy_actors_df %>%
  mutate(
    # Match The Republicans (right according to PPDB)/ New Force is more right than it is centre; 
    right = if_else(str_detect(actor_clean_lower, "forward italy|vox italy"), 1L, right),
    
    # Match the Nationalist/ Neofascist Groups (far right)
    right = if_else(str_detect(actor_clean_lower, "casapound italia|lega|new force|brothers of italy|aliud|youth league|tricolor flame|nationalist party|militant community of the twelve rays|italian popular wave|students' block|national youth|italexit|social idea movement|sud calls nord|sicilian national movement|confederation of identity movements"), 1L, right),
    
    # Christian dems
    right = if_else(str_detect(actor_clean_lower, "new christian democracy|popular alternative"), 1L, right),
    
    # Match broad, right leaning coalitions 
    right = if_else(str_detect(actor_clean_lower, "people of family|the good right|courage italy|let's change"), 1L, right),
    
    # Liberals
    centre = if_else(str_detect(actor_clean_lower, "italian radicals|more europe|italy alive|volt italy|volt europa|democratic center|us moderates|action|european liberal party"), 1L, centre),
    
    # Match Socialist/Communist Parties (Left)
    left = if_else(str_detect(actor_clean_lower, "italian communist party|marxist-leninist party|workers' communist party|communist youth|communist party|italian socialist party|political collective gramigna|federation of young socialists|changing course|communist struggle|communist front|communist refoundation party|resistance for communism"), 1L, left),
    
    # Match the Social Democrats (left) 
    left = if_else(str_detect(actor_clean_lower, "pd: democratic party|article one|italian left|party of the european left|young democrats|solidary democracy|for a caring society"), 1L, left),
    
    # Match the Greens (Left)
    left = if_else(str_detect(actor_clean_lower, "green europe|european animalist party|italian animalist party|greens and left alliance|greens and left alliance|greens/efa|european green party|italy in common|five star movement|federation of the greens"), 1L, left),
    
    # Broader Left Coalitions
    left = if_else(str_detect(actor_clean_lower, "free and equal|the left|gay party|left for|power to the people"), 1L, left),
    
    # Anticapitalist
    left = if_else(str_detect(actor_clean_lower, "anticapitalist left|attac"), 1L, left),
    
    # Left-Nationalists (liberation movement)
    left = if_else(str_detect(actor_clean_lower, "zapatista army of national liberation|zapatistas|sardigna natzione|alternativa|free italy|democracy and autonomy"), 1L, left),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )

table(unique_italy_actors_df$unknown) # 93 classified! 


# Step 3: Classify Based on Stated Objective: 

unique_italy_actors_df <- unique_italy_actors_df %>%
  mutate(
    # LEFTIST GROUPS
    
    # Anti-Racism Groups
    left = if_else(str_detect(actor_clean_lower, "black lives matter|black italians|catanese anti-racist network"), 1L, left),
    
    # Pro-Migrant Groups
    left = if_else(str_detect(actor_clean_lower, "bologna migrants network|sea-watch|no border network|mediterranea saving humans"), 1L, left),
    
    # Human Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "amnesty international|arcigay|parents of homosexuals|democratic lawyers|doctors without borders|end sars|caritas|free patrick|oxfam|unicef|world wide rally for freedom|rainbow families|committee of the red cross|unia"), 1L, left),
    
    # Anti-War
    left = if_else(str_detect(actor_clean_lower, "women in black|pkk|peace and disarmament"), 1L, left),
    
    # Palestine Solidarity Groups 
    left = if_else(str_detect(actor_clean_lower, "boycott, divestment and sanctions|young palestinians"), 1L, left),
    
    # Anti-Austerity/Tenants Rights Groups
    left = if_else(str_detect(actor_clean_lower, "alternative student opposition|pitchforks movement|right to health"), 1L, left),
    
    # Antifascist Groups
    left = if_else(str_detect(actor_clean_lower, "antifa|sardines movement|national association of italian partisans|aned"), 1L, left),
    
    # Environmental Groups
    left = if_else(str_detect(actor_clean_lower, "no tav movement|fridays for future|legambiente|no tap|extinction rebellion|world wildlife fund|zero waste europe|greenpeace|justice for taranto|critical mass|italian doctors for the environment|mountain wilderness|health and environment in taranto|rise up 4 climate justice|no grandi navi|no mose|no triv|environment and bicycle federation|last generation|shared line|scientist rebellion|climate strike|mothers against pfas|friends of the earth|parents from taranto"), 1L, left),
    
    # Animal Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "national authority for animal protection|100% animalisti|anti-vivisection league|ethical movement for the protection of animals|international organization for animal protection|bird protection|people for the ethical treatment of animals|animal save|animal rebellion|anonymous for the voiceless|people animals nature|animal liberation front"), 1L, left),
    
    # Womens Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "one billion rising|non una di meno|international women house|women's march|amica|laiga|women's international league for peace and freedom"), 1L, left),
    
    # Broad Left groups
    left = if_else(str_detect(actor_clean_lower, "italian anarchist federation|anarchist group"), 1L, left),
    
    # CENTRE GROUPS
    # centre = if_else(str_detect(actor_clean_lower, ""), 1L, centre),
    
    # RIGHT GROUPS
    # Anti-Migrant Groups/ Pro-Police Action
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Nationalist Groups
    right = if_else(str_detect(actor_clean_lower, "no euro movement|skinheads front|tricolor masks"), 1L, right),
    
    # Anti-Abortion Groups
    right = if_else(str_detect(actor_clean_lower, "pro life and family|standing sentinels|society for the protection of unborn children|no194"), 1L, right),
    
    # Broad Right 
    right = if_else(str_detect(actor_clean_lower, "let's remain free"), 1L, right),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )
# Revisit: Libera, Orange Vests, No Vax, Stop 5G, yellow vests 

table(unique_italy_actors_df$unknown) # 188 classified! 

temp_italy<- write_csv(unique_italy_actors_df,"temp_italy.csv")

# 4. Classification of Labor Unions Based on Previous Party Alignment

unique_italy_actors_df <- unique_italy_actors_df %>%
  mutate(
    
    # LEFT
    left = if_else(str_detect(actor_clean_lower, "adi|nursind|university students' union|students' union|italian labor union|determined researchers|link|riders union|sgb|spc|sua|7 November"), 1L, left),
    
    # Assoc. with Social Democratic 
    left = if_else(str_detect(actor_clean_lower, "arci: italian recreative and cultural association|high school students' network|unicobas"), 1L, left),
    
    # Assoc. with Socialist/Communist Parties 
    left = if_else(str_detect(actor_clean_lower, "basic unit confederation|cobas|base trade union|cisl|cgil|federconsumatori|people's union|fds|csdl"), 1L, left),
    
    # Associ. with Left-Nationalist groups
    left = if_else(str_detect(actor_clean_lower, "cagliari social forum"), 1L, left),
    
    # CENTRE
    centre = if_else(str_detect(actor_clean_lower, "acli|udc"), 1L, centre),
    
    # RIGHT
    right = if_else(str_detect(actor_clean_lower, "confcommercio: confederation of business, professional activities and autonomous work|coldiretti|confindustria|hospitality business movement|mio: hospitality business movement|ugl"), 1L, right),
    
    # Unknown
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )
table(unique_italy_actors_df$unknown) #226 identified


# 5. Map partisanship back to original ACLED data

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
italy_acled_df <- italy_acled_df %>%
  mutate(
    # Actor 1
    actor1_partisan = map(actor1, ~map_partisanship(.x, unique_italy_actors_df)),
    actor1_left = map_int(actor1_partisan, "left"),
    actor1_right = map_int(actor1_partisan, "right"),
    actor1_centre = map_int(actor1_partisan, "centre"),
    actor1_unknown = map_int(actor1_partisan, "unknown"),
    
    # Actor 2
    actor2_partisan = map(actor2, ~map_partisanship(.x, unique_italy_actors_df)),
    actor2_left = map_int(actor2_partisan, "left"),
    actor2_right = map_int(actor2_partisan, "right"),
    actor2_centre = map_int(actor2_partisan, "centre"),
    actor2_unknown = map_int(actor2_partisan, "unknown"),
    
    # Associated Actor 1
    assoc_actor1_partisan = map(assoc_actor_1, ~map_partisanship(.x, unique_italy_actors_df)),
    assoc_actor1_left = map_int(assoc_actor1_partisan, "left"),
    assoc_actor1_right = map_int(assoc_actor1_partisan, "right"),
    assoc_actor1_centre = map_int(assoc_actor1_partisan, "centre"),
    assoc_actor1_unknown = map_int(assoc_actor1_partisan, "unknown"),
    
    # Associated Actor 2
    assoc_actor2_partisan = map(assoc_actor_2, ~map_partisanship(.x, unique_italy_actors_df)),
    assoc_actor2_left = map_int(assoc_actor2_partisan, "left"),
    assoc_actor2_right = map_int(assoc_actor2_partisan, "right"),
    assoc_actor2_centre = map_int(assoc_actor2_partisan, "centre"),
    assoc_actor2_unknown = map_int(assoc_actor2_partisan, "unknown")
  ) %>%
  select(-ends_with("_partisan"))  # Remove intermediate list columns

# Create event-level partisanship indicators

italy_acled_df <- italy_acled_df %>%
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
table(italy_acled_df$event_partisan_type)

# Summary statistics
italy_acled_df %>%
  count(event_partisan_type) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))

# View some examples of left vs right conflicts
italy_acled_df %>%
  filter(event_partisan_type == "left_vs_right") %>%
  select(event_date, actor1, actor2, event_type, sub_event_type, event_partisan_type) %>%
  head(20) %>%
  View()

# Check what the most common unclassified actors are
unclassified_actors <- italy_acled_df %>%
  filter(event_partisan_type == "unknown") %>%
  select(actor1, actor2, assoc_actor_1, assoc_actor_2) %>%
  pivot_longer(everything(), names_to = "actor_field", values_to = "actor") %>%
  filter(!is.na(actor)) %>%
  count(actor, sort = TRUE) %>%
  head(50)

print(unclassified_actors, n = 50)

View(italy_acled_df)

#################################################################################
# SAVING DOCUMENTS 
#################################################################################

write_csv(italy_acled_df, "~/Downloads/italy_acled_partisan_classification_2020_2024.csv")
write_csv(unique_italy_actors_df, "~/Downloads/italy_acled_classified_actors_2020_2024.csv")



