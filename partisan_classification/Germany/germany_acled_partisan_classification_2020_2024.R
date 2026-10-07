# Title: Classifying Partisanship in ACLED data (Germany)
# Author: Katelyn Nutley 
# Date: 15-12-2025

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)

acled_df <- read_csv("~/Documents/GitHub/Police_Response/2020-2024/acled_all_countries_combined_2020_2024.csv")

# Subset to Germany

germany_acled_df <- acled_df %>% 
  filter(country == "Germany") 

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
  germany_acled_df$actor1,
  germany_acled_df$actor2,
  germany_acled_df$assoc_actor_1,
  germany_acled_df$assoc_actor_2
)

# Create dataframe, split by semicolon, and deduplicate
unique_germany_actors_df <- data.frame(
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

View(unique_germany_actors_df)

# Step 1: Classify based on Government! 

cpds_data <- read_xlsx("~/Documents/GitHub/Police_Response/external datasets/cpds-1960-2023-update-2025.xlsx")
cpds_germany <- cpds_data %>%
  filter(country == "Germany") 
View(cpds_germany)

# Create separate left/right/centre/unknown columns and classify according to gov: 

unique_germany_actors_df <- unique_germany_actors_df %>%
  mutate(
    # Initialize all as 0
    left = 0L,
    right = 0L,
    centre = 0L,
    unknown = 0L,
    
    # Classify governments
    centre = if_else(actor_clean == "Government of Germany (2005-2021)", 1L, centre),
    left = if_else(actor_clean == "Government of Germany (2021-2025)", 1L, left),
    right = if_else(actor_clean == "Government of the Czech Republic (2018-2021)", 1L, right),
    right = if_else(actor_clean == "Government of Poland (2015-2023)", 1L, right),
    centre = if_else(actor_clean == "Former Government of Austria (2019-2025)", 1L, centre),
    left = if_else(actor_clean == "Former Government of Germany (2021-2025)", 1L, left),
    # Mark as unknown if nothing is classified yet
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, unknown)
  ) # I can throw spain in there because it's classified in CPDS; might remove it later 

unique_germany_actors_df %>%
  filter(str_detect(actor_clean_lower, "government of germany")) %>%
  select(actor_clean, left, right, centre, unknown) # love-lah 
table(unique_germany_actors_df$unknown) # Right, so 6 have been classified! 

# Step 2: Classify based on Mainstream Political Parties  

ppdb_data <- read_csv("~/Documents/GitHub/Police_Response/external datasets/PPDB_Round2_v4.csv")
ppdb_germany <- ppdb_data %>%
  filter(COUNTRY == "Germany") # So, the difference here is that the PPDB has 
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
unique_germany_actors_df <- unique_germany_actors_df %>%
  mutate(
    # Match The Republicans (right according to PPDB)
    right = if_else(str_detect(actor_clean_lower, "pro chemnitz|fw: free voters"), 1L, right),
    
    # Match the Nationalist/ Neofascist Groups (far right)
    right = if_else(str_detect(actor_clean_lower, "afd: alternative for germany|ja: young alternative for germany|jn: young nationalists|the third path|poe: patriotic opposition europe|grey wolves|patriots nrw|nsp: new strength party|adpm: dawn of german patriots - central germany|djv: german youth forward|pegida: patriotic europeans against the islamisation of the occident|ibd: identitarian movement germany"), 1L, right),
    
    # Christian dems
    right = if_else(str_detect(actor_clean_lower, "csu: christian social union in bavaria|cdu: christian democratic union of germany|ju: young union"), 1L, right),
    
    # Match broad, right leaning coalitions 
    right = if_else(str_detect(actor_clean_lower, "the right|lkr: liberal conservative reformers"), 1L, right),
    
    # Liberals
    centre = if_else(str_detect(actor_clean_lower, "fdp: free democratic party|npd: national democratic party of germany|uid: union of international democrats|cdu: unitary democratic coalition|oedp: ecological democratic party|fdj: free german youth|jef: young european federalists|jl: young liberals|volt europa"), 1L, centre),
    
    # Match Socialist/Communist Parties (Left)
    left = if_else(str_detect(actor_clean_lower, "sjd: socialist youth germany - the falcons|dkp: german communist party|mlpd: marxist-leninist party of germany|rebell: youth league rebel|tkp/ml: communist party of turkey/marxist-leninist|wpi: worker-communist party of iran|pts: socialist workers' party|pkk: kurdistan workers' party|sdaj: socialist german workers youth|ypg: people's protection units"), 1L, left),
    
    # Match the Social Democrats (left) 
    left = if_else(str_detect(actor_clean_lower, "spd: social democratic party of germany|jusos: young socialists in the spd"), 1L, left),
    
    # Match the Greens (Left)
    left = if_else(str_detect(actor_clean_lower, "the greens|gj: green youth"), 1L, left),
    
    # Broader Left Coalitions
    left = if_else(str_detect(actor_clean_lower, "the left|the left.sds|alliance against the right|alliance germany|left youth solid|partei: the party|il: interventionist left|rise up|bpd: the grassroots democratic party of germany|pel: party of the european left|youth against the right"), 1L, left),
    
    # Anticapitalist
    # left = if_else(str_detect(actor_clean_lower, ""), 1L, left),
    
    # Left-Nationalists (liberation movement)
    left = if_else(str_detect(actor_clean_lower, "sahra wagenknecht alliance - reason and justice|fau: free workers union|alliance sahra wagenknecht"), 1L, left),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )

table(unique_germany_actors_df$unknown) # 64 classified! 

# Step 3: Classify Based on Stated Objective: 

unique_germany_actors_df <- unique_germany_actors_df %>%
  mutate(
    # LEFTIST GROUPS
    
    # Anti-Racism Groups
    left = if_else(str_detect(actor_clean_lower, "blm: black lives matter|black community hamburg|standup against racism|no need for racism|otfr: open convention against racism and fascism in tubingen and region"), 1L, left),
    
    # Pro-Migrant Groups
    left = if_else(str_detect(actor_clean_lower, "seebruecke|sea-watch|pro asyl|german council for refugees|igoj: initiative in remembrance of oury jalloh|no border network|abolish frontex|mission lifeline"), 1L, left),
    
    # Human Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "sea-eye|rsf: reporters without borders|rh: red aid|caritas|ai: amnesty international|msf: doctors without borders|si: solidarity international|icrc: international committee of the red cross|oxfam"), 1L, left),
    
    # Anti-War
    left = if_else(str_detect(actor_clean_lower, "lebenslaute|dfg-vk: the german peace community|peace plenum|ngpm: network of the german peace movement|pax christi"), 1L, left),
    
    # Palestine Solidarity Groups 
    left = if_else(str_detect(actor_clean_lower, "bds: boycott, divestment and sanctions|samidoun: palestinian prisoner solidarity network|ib: internationalist alliance|palestinian group"), 1L, left),
    
    # Anti-Austerity/Tenants Rights Groups
    left = if_else(str_detect(actor_clean_lower, "fga: fridays against old age poverty"), 1L, left),
    
    # Antifascist Groups
    left = if_else(str_detect(actor_clean_lower, "antifa|chemnitz nazifrei|indivisible|abc: anarchist black cross|nika: nationalism is not an alternative|colorful instead of brown|vvn-bda: association of persecutees of the nazi regime/federation of antifascists|migrantifa|heart instead of hate"), 1L, left),
    
    # Environmental Groups
    left = if_else(str_detect(actor_clean_lower, "robin wood|ende gelaende|critical mass|greenpeace|fff: fridays for future|bund: german federation for the environment and nature conservation|nabu: nature and biodiversity conservation union|xr: extinction rebellion|fossil free|environmental union|vcd: traffic club germany|replace coal|mission coal stop|stay on the ground|last generation|scientist rebellion|ecologists in action|wwf: world wildlife fund|end fossil: occupy|friends of the earth|attac"), 1L, left),
    
    # Animal Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "peta: people for the ethical treatment of animals|animal rebellion|ariwa: animal rights watch|together against the animal industry|animal save movement|animals united|soko animal protection|alf: animal liberation front|vegan strike group|animal equality|av: anonymous for the voiceless|human environment animal protection party"), 1L, left),
    
    # Womens Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "obr: one billion rising|fvc: women association courage|fk: kandel women's alliance|maria 2.0|reclaim the night|faz: feminist autonomous cell"), 1L, left),
    
    # Broad Left groups
    left = if_else(str_detect(actor_clean_lower, "campact|kdw: communication point democratic resistance|pulse of europe|ncri: national council of resistance of iran|pfs: pulse for stuttgart"), 1L, left),
    
    # CENTRE GROUPS
    # centre = if_else(str_detect(actor_clean_lower, ""), 1L, centre),
    
    # RIGHT GROUPS
    # Anti-Migrant Groups/ Pro-Police Action
    right = if_else(str_detect(actor_clean_lower, "thugida"), 1L, right),
    
    # Nationalist Groups
    right = if_else(str_detect(actor_clean_lower, "reich citizens|hogesa: hooligans against salafists|bpe: citizens' movement pax europa|bd: brotherhood germany|free saxony|the homeland|iboe: identitarian movement austria|steeler boys|daoe: the alliance for austria"), 1L, right),
    
    # Anti-Abortion Groups
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Broad Right 
    right = if_else(str_detect(actor_clean_lower, "falun gong group"), 1L, right),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )
# Revisit: Islamist groups - hizb ut-tahrir, muslim interaktiv, ansaar international, hamas

table(unique_germany_actors_df$unknown) # 162 classified! 

temp_germany<- write_csv(unique_germany_actors_df,"temp_germany.csv")

# 4. Classification of Labor Unions Based on Previous Party Alignment

unique_germany_actors_df <- unique_germany_actors_df %>%
  mutate(
    
    # LEFT
    left = if_else(str_detect(actor_clean_lower, "ver.di: united services union|cgt: general confederation of labor (france)|atik: confederation of workers from turkey in europe|si cobas: inter-category trade union"), 1L, left),
    # Cobas is left-wing in London
    
    # Assoc. with Social Democratic 
    left = if_else(str_detect(actor_clean_lower, "igm: industrial union of metalworkers|dgb: german trade union confederation|gew: german education union|awo: worker welfare|evg: railway and transport union|ngg: food, beverages and catering union"), 1L, left),
    # IGM and GEW associated with DGB (SD aligned)
    
    # Assoc. with Socialist/Communist Parties 
    # left = if_else(str_detect(actor_clean_lower, ""), 1L, left),
    
    # Associ. with Left-Nationalist groups
    # left = if_else(str_detect(actor_clean_lower, ""), 1L, left),
    
    # CENTRE
    centre = if_else(str_detect(actor_clean_lower, "dbb: german civil service federation"), 1L, centre),
    
    # RIGHT
    right = if_else(str_detect(actor_clean_lower, "lsv: land creates connection"), 1L, right),
    
    # Unknown
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )
# Review DGB affiliates? I didn't think that IGBCE was SD. 
table(unique_germany_actors_df$unknown) #173 identified 


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
germany_acled_df <- germany_acled_df %>%
  mutate(
    # Actor 1
    actor1_partisan = map(actor1, ~map_partisanship(.x, unique_germany_actors_df)),
    actor1_left = map_int(actor1_partisan, "left"),
    actor1_right = map_int(actor1_partisan, "right"),
    actor1_centre = map_int(actor1_partisan, "centre"),
    actor1_unknown = map_int(actor1_partisan, "unknown"),
    
    # Actor 2
    actor2_partisan = map(actor2, ~map_partisanship(.x, unique_germany_actors_df)),
    actor2_left = map_int(actor2_partisan, "left"),
    actor2_right = map_int(actor2_partisan, "right"),
    actor2_centre = map_int(actor2_partisan, "centre"),
    actor2_unknown = map_int(actor2_partisan, "unknown"),
    
    # Associated Actor 1
    assoc_actor1_partisan = map(assoc_actor_1, ~map_partisanship(.x, unique_germany_actors_df)),
    assoc_actor1_left = map_int(assoc_actor1_partisan, "left"),
    assoc_actor1_right = map_int(assoc_actor1_partisan, "right"),
    assoc_actor1_centre = map_int(assoc_actor1_partisan, "centre"),
    assoc_actor1_unknown = map_int(assoc_actor1_partisan, "unknown"),
    
    # Associated Actor 2
    assoc_actor2_partisan = map(assoc_actor_2, ~map_partisanship(.x, unique_germany_actors_df)),
    assoc_actor2_left = map_int(assoc_actor2_partisan, "left"),
    assoc_actor2_right = map_int(assoc_actor2_partisan, "right"),
    assoc_actor2_centre = map_int(assoc_actor2_partisan, "centre"),
    assoc_actor2_unknown = map_int(assoc_actor2_partisan, "unknown")
  ) %>%
  select(-ends_with("_partisan"))  # Remove intermediate list columns

# Create event-level partisanship indicators

germany_acled_df <- germany_acled_df %>%
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
table(germany_acled_df$event_partisan_type)

# Summary statistics
germany_acled_df %>%
  count(event_partisan_type) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))

# View some examples of left vs right conflicts
germany_acled_df %>%
  filter(event_partisan_type == "left_vs_right") %>%
  select(event_date, actor1, actor2, event_type, sub_event_type, event_partisan_type) %>%
  head(20) %>%
  View()

# Check what the most common unclassified actors are
unclassified_actors <- germany_acled_df %>%
  filter(event_partisan_type == "unknown") %>%
  select(actor1, actor2, assoc_actor_1, assoc_actor_2) %>%
  pivot_longer(everything(), names_to = "actor_field", values_to = "actor") %>%
  filter(!is.na(actor)) %>%
  count(actor, sort = TRUE) %>%
  head(50)

print(unclassified_actors, n = 50)

View(germany_acled_df)

#################################################################################
# SAVING DOCUMENTS 
#################################################################################

write_csv(germany_acled_df, "~/Downloads/germany_acled_partisan_classification_2020_2024.csv")
write_csv(unique_germany_actors_df, "~/Downloads/germany_acled_classified_actors_2020_2024.csv")





