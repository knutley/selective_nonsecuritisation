# Title: Classifying Partisanship in ACLED data (Spain)
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

# Subset to Spain

spain_acled_df <- acled_df %>% 
  filter(country == "Spain") 

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
  spain_acled_df$actor1,
  spain_acled_df$actor2,
  spain_acled_df$assoc_actor_1,
  spain_acled_df$assoc_actor_2
)

# Create dataframe, split by semicolon, and deduplicate
unique_spain_actors_df <- data.frame(
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

View(unique_spain_actors_df)

# Step 1: Classify based on Government! 

cpds_data <- read_xlsx("~/Documents/GitHub/Police_Response/external datasets/cpds-1960-2023-update-2025.xlsx")
cpds_spain <- cpds_data %>%
  filter(country == "Spain") 
View(cpds_spain)

# Create separate left/right/centre/unknown columns and classify according to gov: 

unique_spain_actors_df <- unique_spain_actors_df %>%
  mutate(
    # Initialize all as 0
    left = 0L,
    right = 0L,
    centre = 0L,
    unknown = 0L,
    
    # Classify governments
    left = if_else(actor_clean == "Government of Spain (2020-)", 1L, left),
    left = if_else(actor_clean == "Former Government of Spain (2020-)", 1L, left),
    left = if_else(actor_clean == "Government of Portugal (2015-2024)", 1L, left),
    # Mark as unknown if nothing is classified yet
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, unknown)
  ) # I can throw spain in there because it's classified in CPDS; might remove it later 

unique_spain_actors_df %>%
  filter(str_detect(actor_clean_lower, "government of spain")) %>%
  select(actor_clean, left, right, centre, unknown) # love-lah 
table(unique_spain_actors_df$unknown) # Right, so 3 have been classified! 


# Step 2: Classify based on Mainstream Political Parties  

ppdb_data <- read_csv("~/Documents/GitHub/Police_Response/external datasets/PPDB_Round2_v4.csv")
ppdb_spain <- ppdb_data %>%
  filter(COUNTRY == "Spain") # So, the difference here is that the PPDB has 
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
unique_spain_actors_df <- unique_spain_actors_df %>%
  mutate(
    # Match The Republicans (right according to PPDB)
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Match the Nationalist/ Neofascist Groups (far right)
    right = if_else(str_detect(actor_clean_lower, "fni-pnsoe: national-socialist identitarian|jxcat: together for catalonia|dn: national democracy|frontal bastion|denaes: foundation for the defense of the spanish nation|anova: renewal-nationalist brotherhood|cc: canarian coalition|spain 2000"), 1L, right),
    
    # Christian dems
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Match broad, right leaning coalitions 
    right = if_else(str_detect(actor_clean_lower, "pp: people's party|vox|fe de las jons: spanish phalanx of the councils of the national syndicalist offensive|fac: asturias forum|upn: navarrese people's union|do: ourense democracy"), 1L, right),
    
    # Liberals
    centre = if_else(str_detect(actor_clean_lower, "citizens party|upl: leonese people's union|soria ya|regionalist party of cantabria|idpa: initiative of the andalusian people|vv: come venezuela"), 1L, centre),
    
    # Match Socialist/Communist Parties (Left)
    left = if_else(str_detect(actor_clean_lower, "socialist party|forward: socialist organization of national liberation|gks: socialist youth coordinator|iu: united left|united we can|podemos|pce: communist party of spain|enrai|pcpe: communist party of the peoples of spain|ps: socialist party|mes: more for majorca|uce: communist unification of spain|fo: workers' front|pcp: portuguese communist party|nfp: new popular front|cdr: committees for the defence of the republic"), 1L, left),
    
    # Match the Social Democrats (left) 
    left = if_else(str_detect(actor_clean_lower, "psoe: spanish socialist workers' party|aa: forward andalusia|sinn fein|bcomu: barcelona in common|igv: interior galician alive|new canaries"), 1L, left),
    
    # Match the Greens (Left)
    left = if_else(str_detect(actor_clean_lower, "en comu podem|pavp: green alliance party|pev: green progress|cha: aragonese union|mp: more country|greens equo"), 1L, left),
    
    # Broader Left Coalitions
    left = if_else(str_detect(actor_clean_lower, "erpv: republican left of the valencian country|ir: republican left|ei: independentist left|compromise coalition|gdr: defence and resistance groups|inter-union confederation|anticapitalists|mcc: citizens' movement of cartagena|cpm: coalicion por melilla|smr: unite movement|ehfai: euskal herria anti-imperialist front|upe: united for extremadura|pud: democratic unitary platform|vtlp: valladolid takes the floor|two nwighbor alternative"), 1L, left),
    
    # Anticapitalist
    left = if_else(str_detect(actor_clean_lower, "anarchists|anarchist"), 1L, left),
    
    # Left-Nationalists (liberation movement)
    left = if_else(str_detect(actor_clean_lower, "erc: catalan republican left|ela: basque workers' association|basque group|pnv: basque nationalist party|catalan group|euskal herria bildu|cup: popular unity candidacy|sortu|bng: galician nationalist bloc|arran|elkarrekin podemos|elkarrekin donostia|republican left of the valencian country|ernai|teruel exists"), 1L, left),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )

table(unique_spain_actors_df$unknown) # 78 classified!

# Step 3: Classify Based on Stated Objective: 

unique_spain_actors_df <- unique_spain_actors_df %>%
  mutate(
    # LEFTIST GROUPS
    
    # Anti-Racism Groups
    left = if_else(str_detect(actor_clean_lower, "blm: black lives matter|sos racism|ucfr: unity against fascism and racism platform"), 1L, left),
    
    # Pro-Migrant Groups
    left = if_else(str_detect(actor_clean_lower, "cear: spanish commission for refugee aid|migrants"), 1L, left),
    
    # Human Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "rsf: reporters without borders|caritas|ai: amnesty international|icrc: international committee of the red cross|oxfam|ata: movement for amnesty and against repression"), 1L, left),
    
    # Anti-War
    left = if_else(str_detect(actor_clean_lower, "women in black"), 1L, left),
    
    # Palestine Solidarity Groups 
    left = if_else(str_detect(actor_clean_lower, "bds: boycott, divestment and sanctions|samidoun: palestinian prisoner solidarity network|palestinian group|rescop: solidarity network against the occupation of palestine"), 1L, left),
    
    # Anti-Austerity/Tenants Rights Groups
    left = if_else(str_detect(actor_clean_lower, "sos public health platform|october 3 platform|white tide|tide of residencies|cas: anti-privatization coordinator of public health|coespe: state coordinator for the defense of the public pension system|salamanca district movement|mdsp: table in defense of public healthcare|tenants' union|canary islands have a limit|inclusive education platform yes, special also|fadsp: spanish federation of associations for the defence of the public health service"), 1L, left),
    
    # Antifascist Groups
    left = if_else(str_detect(actor_clean_lower, "antifa|antifascist coordinator|safor antifascist collective|ceaqua: state coordinator of support to the argentine complaint against the crimes of francoism"), 1L, left),
    
    # Environmental Groups
    left = if_else(str_detect(actor_clean_lower, "steilas|green tide|fff: fridays for future|xr: extinction rebellion|ecologists in action|climate justice network|rxc: 2020 climate rebellion|greenpeace|scientist rebellion|wind farms yes, but not like this|futuro vegetal|critical mass|end fossil: occupy|friends of the earth"), 1L, left),
    
    # Animal Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "peta: people for the ethical treatment of animals|animal save movement|animal equality|animanaturalis|pacma: animalist party|wwf: world wildlife fund|nac: no to hunting platform|pan: people animals nature|cas international"), 1L, left),
    
    # Womens Rights Groups 
    left = if_else(str_detect(actor_clean_lower, "helpless women|femen|8M comission|8M Commission; Women (Spain)|women's revolt in the church|world march of women|madrid safe"), 1L, left),
    
    # Broad Left groups
    left = if_else(str_detect(actor_clean_lower, "omnium cultural"), 1L, left),
    
    # CENTRE GROUPS
    # centre = if_else(str_detect(actor_clean_lower, ""), 1L, centre),
    
    # RIGHT GROUPS
    # Anti-Migrant Groups/ Pro-Police Action
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Nationalist Groups
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Anti-Abortion Groups
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Broad Right 
    # right = if_else(str_detect(actor_clean_lower, ""), 1L, right),
    
    # Update unknown flag (only if nothing else matched)
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )
# Revisit: why are there so few right-wing protestors? 

table(unique_spain_actors_df$unknown) # 142 classified! 

temp_spain<- write_csv(unique_spain_actors_df,"temp_spain.csv")

# Step 4: Classification of Labor Unions Based on Previous Party Alignment

unique_spain_actors_df <- unique_spain_actors_df %>%
  mutate(
    
    # LEFT - Major Left-aligned unions
    
    # CCOO (Comisiones Obreras) - historically linked to PCE/Communist Party
    left = if_else(str_detect(actor_clean_lower, "ccoo|workers commissions|workers' commissions|comisiones obreras"), 1L, left),
    
    # UGT (Unión General de Trabajadores) - historically linked to PSOE/Social Democrats
    left = if_else(str_detect(actor_clean_lower, "ugt|general union of workers|union general de trabajadores"), 1L, left),
    
    # CGT (Confederación General del Trabajo) - anarcho-syndicalist
    left = if_else(str_detect(actor_clean_lower, "cgt|general confederation of labor|confederacion general del trabajo"), 1L, left),
    
    # CNT (Confederación Nacional del Trabajo) - anarcho-syndicalist
    left = if_else(str_detect(actor_clean_lower, "cnt:|national labor confederation|confederacion nacional del trabajo"), 1L, left),
    
    # USO (Unión Sindical Obrera) - left-leaning independent
    left = if_else(str_detect(actor_clean_lower, "uso:|workers' trade union|union sindical obrera"), 1L, left),
    
    # Basque nationalist unions (left-nationalist)
    left = if_else(str_detect(actor_clean_lower, "lab:|abertzales workers|esk:|left trade union convergence|steilas|ehne:|farmers and breeders of the basque"), 1L, left),
    
    # Catalan unions (left-nationalist)
    left = if_else(str_detect(actor_clean_lower, "csc:|catalan trade union|intersindical|iac-catac:|autonomous candidature of workers|ustec:|education workers of catalonia|inter-union-csc"), 1L, left),
    
    # Aragon unions (left-regional)
    left = if_else(str_detect(actor_clean_lower, "osta:|workers of aragon"), 1L, left),
    
    # Education sector unions (typically left)
    left = if_else(str_detect(actor_clean_lower, "stes|aspepc-sps:|secondary school teachers|education trade union|ustec"), 1L, left),
    
    # Healthcare worker unions (typically left)
    left = if_else(str_detect(actor_clean_lower, "health workers in action|mi15f:|temporary public workers"), 1L, left),
    
    # Student unions (left)
    left = if_else(str_detect(actor_clean_lower, "sepc:|students of the catalan|se:|students' union|student union"), 1L, left),
    
    # Solidarity/leftist unions
    left = if_else(str_detect(actor_clean_lower, "sut:|solidarity and workers unity|solidarity trade union|csi:|left common trade union"), 1L, left),
    
    # Metal/industrial workers (left)
    left = if_else(str_detect(actor_clean_lower, "ctm:|metal workers coordinator"), 1L, left),
    
    # Justice/administration workers (left)
    left = if_else(str_detect(actor_clean_lower, "staj:|justice administration workers"), 1L, left),
    
    # Farmers unions (typically left in Basque context, mixed elsewhere)
    left = if_else(str_detect(actor_clean_lower, "ehne:|farmers and breeders of the basque"), 1L, left),
    
    # CENTRE - Independent/professional unions
    
    # CSIF and other independent civil service unions
    centre = if_else(str_detect(actor_clean_lower, "csif|independent central of civil servants|independent trade union"), 1L, centre),
    
    # Healthcare professional unions (more conservative/professional)
    centre = if_else(str_detect(actor_clean_lower, "satse:|nursing union|cesm:|confederation of medical unions|sma:|andalusian medical union|health workers \\("), 1L, centre),
    
    # Portuguese unions appearing in Spain data
    centre = if_else(str_detect(actor_clean_lower, "sep:|portuguese nurses|health workers \\(portugal\\)|snes-fsu:|national union of secondary education|unsa:|national union of autonomous"), 1L, centre),
    
    # International/generic health workers
    centre = if_else(str_detect(actor_clean_lower, "health workers \\(international\\)|health workers \\(cuba\\)|aid workers"), 1L, centre),
    
    # RIGHT - Conservative/professional associations and police unions
    
    # Police unions (right)
    right = if_else(str_detect(actor_clean_lower, "cep:|confederation of police|sep:|spanish police|jupol|unified police union|police union|acaip:|penitentiary institutions"), 1L, right),
    
    # Farmers unions (typically centre-right/right in Spain except Basque)
    right = if_else(str_detect(actor_clean_lower, "upa:|small farmers and ranchers|udu:|farmers' and ranchers' unions|udp:|union of peasants|uccl:|farmers in castilla"), 1L, right),
    
    # UNKNOWN - Need more context
    
    # Civic/political unions that are ambiguous
    unknown = if_else(str_detect(actor_clean_lower, "hb:|civic union|union of free association|upp:|union for penagos") & left == 0 & right == 0 & centre == 0, 1L, unknown),
    
    # European Union (not a union in labor sense)
    unknown = if_else(str_detect(actor_clean_lower, "eu:|european union") & left == 0 & right == 0 & centre == 0, 1L, unknown),
    
    # Update unknown flag
    unknown = if_else(left == 0 & right == 0 & centre == 0, 1L, 0L)
  )

table(unique_spain_actors_df$unknown) # 185 classified! 


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
spain_acled_df <- spain_acled_df %>%
  mutate(
    # Actor 1
    actor1_partisan = map(actor1, ~map_partisanship(.x, unique_spain_actors_df)),
    actor1_left = map_int(actor1_partisan, "left"),
    actor1_right = map_int(actor1_partisan, "right"),
    actor1_centre = map_int(actor1_partisan, "centre"),
    actor1_unknown = map_int(actor1_partisan, "unknown"),
    
    # Actor 2
    actor2_partisan = map(actor2, ~map_partisanship(.x, unique_spain_actors_df)),
    actor2_left = map_int(actor2_partisan, "left"),
    actor2_right = map_int(actor2_partisan, "right"),
    actor2_centre = map_int(actor2_partisan, "centre"),
    actor2_unknown = map_int(actor2_partisan, "unknown"),
    
    # Associated Actor 1
    assoc_actor1_partisan = map(assoc_actor_1, ~map_partisanship(.x, unique_spain_actors_df)),
    assoc_actor1_left = map_int(assoc_actor1_partisan, "left"),
    assoc_actor1_right = map_int(assoc_actor1_partisan, "right"),
    assoc_actor1_centre = map_int(assoc_actor1_partisan, "centre"),
    assoc_actor1_unknown = map_int(assoc_actor1_partisan, "unknown"),
    
    # Associated Actor 2
    assoc_actor2_partisan = map(assoc_actor_2, ~map_partisanship(.x, unique_spain_actors_df)),
    assoc_actor2_left = map_int(assoc_actor2_partisan, "left"),
    assoc_actor2_right = map_int(assoc_actor2_partisan, "right"),
    assoc_actor2_centre = map_int(assoc_actor2_partisan, "centre"),
    assoc_actor2_unknown = map_int(assoc_actor2_partisan, "unknown")
  ) %>%
  select(-ends_with("_partisan"))  # Remove intermediate list columns

# Create event-level partisanship indicators

spain_acled_df <- spain_acled_df %>%
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
table(spain_acled_df$event_partisan_type)

# Summary statistics
spain_acled_df %>%
  count(event_partisan_type) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))

# View some examples of left vs right conflicts
spain_acled_df %>%
  filter(event_partisan_type == "left_and_right") %>%
  select(event_date, actor1, actor2, event_type, sub_event_type, event_partisan_type) %>%
  head(20) %>%
  View()

# Check what the most common unclassified actors are
unclassified_actors <- spain_acled_df %>%
  filter(event_partisan_type == "unknown") %>%
  select(actor1, actor2, assoc_actor_1, assoc_actor_2) %>%
  pivot_longer(everything(), names_to = "actor_field", values_to = "actor") %>%
  filter(!is.na(actor)) %>%
  count(actor, sort = TRUE) %>%
  head(50)

print(unclassified_actors, n = 50)

View(spain_acled_df)

#################################################################################
# SAVING DOCUMENTS 
#################################################################################

write_csv(spain_acled_df, "~/Downloads/spain_acled_partisan_classification_2020_2024.csv")
write_csv(unique_spain_actors_df, "~/Downloads/spain_acled_classified_actors_2020_2024.csv")








