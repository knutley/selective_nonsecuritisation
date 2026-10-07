# Selective Non-Securitisation

Replication pipeline for *Selective Non-Securitisation: A Cross-National Quantitative Analysis of Police Response to Political Protest in France, Germany, Italy, Spain and the United Kingdom* (K. Nutley, Harvard University/ University of St Andrews).

This README lays out the pipeline in run order. 

## Pipeline order

```
1. data_collection/
   └── ACLED_api.R
   → pulls raw ACLED protest events (2020–2024) for FR/DE/IT/ES/UK via authenticated,
     paginated API calls; was done iteratively
   → output: acled_all_countries_combined_2020_2024.csv
2. partisan_classification/{country}/
   ├── {country}_acled_partisan_classification_2020_2024.R # script classifying actor partisanship
   │    → output: {country}_acled_partisan_classification_2020_2024.csv,
   │              {country}_acled_classified_actors_2020_2024.csv
   └── {country}_acled_partisan_class_nlp_2020_2024.R # script applying BART MNLI
   → output: {country}_acled_partisan_classification_bootstrapped{pre}_2020_2024.csv, # this was iterative, so this is just the penultimate iteration
             {country}_acled_partisan_classification_bootstrapped{post}_2020_2024.csv
3. data/
   ├──{country}/
      ├── {country}_acled_partisan_classification_2020_2024.csv # original event data w/ classified partisans
      ├── {country}_acled_classified_actors_2020_2024.csv # discrete actors and their classifications
      ├── {country}_acled_partisan_classification_bootstrapped{pre}_2020_2024.csv # NLP classifications, pre-correction
      └── {country_acled_partisan_classification_bootstrapped{post}_2020_2024.csv # NLP classifications, post-correction
   ├──combined/
      ├── acled_all_countries_combined_2020_2024.csv # dataset combined, but no partisan classification
      ├── acled_all_countries_combined_classed_2020_2024.csv # dataset with classified partisans
      ├── acled_classified_police_presence_2020_2024.csv # dataset w/ rule-based classification of presence; no RoBERTa classification yet 
      ├── acled_classified_severity_v3_2020_2024.csv # dataset with RoBERTa classification, but no thresholding yet!
      ├── acled_classified_severity_v3_rethresholded.csv # final response classification dataset w/ rethresholded RoBERTa
      ├── acled_merged_controls.csv # an older control dataset that I rely on b/c I didn't want to re-query OSM 
      └── acled_merged_controls_rethresholded_b3.csv # finalised dataset with partisanship, response, and controls 
4. response_classification/
    ├── algorithmic_presence_2020-2024.R # algorithmic detection of additional police responses only 
    │    → output: acled_classified_police_presence_2020_2024.csv
    └── roberta/
      ├── training_data/
         ├── police_manual_sample.xlsx # 252 hand-coded positives from algorithmic ID 
         └── contrastive_examples.csv # 40 boundary cases (there was some issue where the police offered size reports, especially)
      ├── stage_a_train_evaluate.py # trains + evaluates -> model
      ├── stage_b_full_classification.py # classifies the full dataset
          → output: acled_classified_severity_v3_2020_2024.csv
      ├── rethresholding_postclass.R
          → output: acled_classified_severity_v3_rethresholded.csv
      └── model/
         └── police_severity_classifier_roberta_v3/
            └── config.json, tokenizer.json, tokenizer_config.json, training_args.bin, model.safetensors # safe tensors is on Zenodo w/ DOI 
5. control_variables/
    |── spatial_controls.R # this builds off an earlier checkpoint acled_merged_controls.csv, because I didn't want to have to re-query OSM
          → output: acled_merged_controls_rethresholded_v3.csv
6. analysis
    |── 01_model1_police_presence.R # logit, country FE, presence at 0.9 threshold
      → output: results/table8_odds_ratios.csv, table13_admin2_clustered.csv,
        model1_presence_by_partisan_type.csv, model1_partisan_coefs_across_specs.csv,
        model1_predicted_probabilities.csv # I didn't include these in the repo, but please reach out if you'd like them 
    |── 02_heckman_severity.R # Heckman arrest/brutality; builds protest-load exclusion instrument and TOST
      → output: results/heckman_*.csv, tost_*.csv, exclusion_restriction_checks.csv,
        alternative_instruments.csv, table9_table10_descriptive_columns.csv
   ├── 03_hausman_fe_vs_re.R  # FE vs RE (Hausman + Mundlak)
   │    → output: results/hausman_results.csv
   
