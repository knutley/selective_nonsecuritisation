# Police_Response

Replication pipeline for *Selective Non-Securitisation: A Cross-National Quantitative Analysis of Police Response to Political Protest in France, Germany, Italy, Spain and the United Kingdom* (K. Nutley, Harvard University/ University of St Andrews).

This README lays out the pipeline in run order. 

## Pipeline order

```
1. data_collection/
   └── ACLED_api.R
   → pulls raw ACLED protest events (2020–2024) for FR/DE/IT/ES/UK via authenticated,
     paginated API calls; was done iteratively
   → output: acled_all_countries_combined.csv
2. partisan_classification/{country}/
   ├── {country}_acled_partisan_classification.R # script classifying actor partisanship
   │    → output: {country}_acled_partisan_classification.csv,
   │              {country}_acled_classified_actors.csv
   └── {country}_acled_partisan_classification_bootstrapped.R # script applying BART MNLI
   → output: {country}_acled_partisan_classification_bootstrapped_pre.csv,
             {country}_acled_partisan_classification_bootstrapped_post.csv
3. data/
   ├──{country}/
      ├── {country}_acled_partisan_classification.csv # original event data w/ classified partisans
      ├── {country}_acled_classified_actors.csv # discrete actors and their classifications
      ├── {country}_acled_partisan_classification_bootstrapped_pre.csv # NLP classifications, pre-correction
      └── {country}_acled_partisan_classification_bootstrapped_post.csv # NLP classifications, post-correction
   ├──combined/
      ├── acled_all_countries_combined_classed1.csv # dataset w/ just partisanship classification
      ├── acled_classified_police_presence1.csv # dataset w/ rule-based classification of presence
      ├── acled_classified_severity1.csv # dataset w/ improved presence and severity measures
      └── acled_merged_controls.csv # finalised dataset with partisanship, response, and controls 
4. response_classification/
    ├── classifying_binary_response.R # algorithmic detection of additional police responses
    │    → output: data/combined/acled_classified_police_presence1.csv
    ├── response_pos_set.xlsx # positive test set 
    ├── neg_pos_set.xlsx # test negative set 
    └── roberta_severity.py # builds on the algorithmic detection to find presence, arrest, and brutality
         → reads: data/combined/acled_classified_police_presence1.csv,
                 response_pos_set.xlsx, neg_pos_set.xlsx
         → output: data/combined/acled_classified_severity1.csv
5. control_variable/
    └── control_variable_construction.R
         → output: data/combined/acled_merged_controls.csv (final built dataset)
6. analysis/
    ├── model1_presence.R  [rename from corrected_response_model1.R]
    │    → output: results/table8_odds_ratios.csv, results/table13_admin2_clustered.csv,
    │              results/predicted_probabilities.csv,
    │              results/model1_partisan_coefs_across_specs.csv,
    │              models/model1_baseline_aligned.rds (+ 3 more .rds)
    └── model2_severity.R  [rename from corrected_response_models.R]
        → output: results/tost_equivalence.csv, results/heckman_arrest_full.csv,
                  results/heckman_brutality_full.csv, results/heckman_partisan_coefs.csv,
                  results/heckman_vs_nocorrection.csv,
                  results/table9_table10_descriptive_columns.csv,
                  models/heckman_arrest.rds, models/heckman_brutality.rds

