# STAGE B: FULL-DATASET CLASSIFICATION
# Only run this after Stage A's per-label metrics (arrest_f1/precision/recall,
# brutality_f1/precision/recall) look reasonable. Uses the model saved by
# Stage A. Batched inference, flat 0.5 threshold (no recalibration hack -
# if this model is properly trained, 0.5 should work).
# -------------------------------------------------

import pandas as pd
from transformers import pipeline

LABEL_COLS = ["police_presence", "arrest", "brutality"]
SAVE_DIR = "./police_severity_classifier_roberta_v3"
SOURCE_PATH = "~/Documents/GitHub/Police_Response/2020-2024/acled_classified_police_presence_2020_2024.csv"
OUTPUT_PATH = "~/Documents/GitHub/Police_Response/2020-2024/acled_classified_severity_v3_2020_2024.csv"
THRESHOLD = 0.5
BATCH_SIZE = 32

print("=== LOADING FULL DATASET ===")
full_data = pd.read_csv(SOURCE_PATH, low_memory=False)
full_data = full_data.rename(columns={"police_presence": "police_presence_regex"})
full_data["notes"] = full_data["notes"].fillna("")
print(f"Loaded {len(full_data)} records")

clf = pipeline("text-classification", model=SAVE_DIR, tokenizer=SAVE_DIR, top_k=None)

notes_list = full_data["notes"].astype(str).str.strip().tolist()
notes_list = [n[:512] if n else "" for n in notes_list]

predictions = [None] * len(notes_list)
failed = 0

print("Classifying (batched)...")
for start in range(0, len(notes_list), BATCH_SIZE):
    if start % (BATCH_SIZE * 20) == 0:
        print(f"  Processed {start}/{len(notes_list)} records...")

    chunk = notes_list[start:start + BATCH_SIZE]
    chunk_idx = list(range(start, min(start + BATCH_SIZE, len(notes_list))))
    nonempty_positions = [i for i, n in enumerate(chunk) if n]

    if nonempty_positions:
        try:
            raw_batch = clf([chunk[i] for i in nonempty_positions])
        except Exception as e:
            print(f"  Warning: batch at {start} failed: {e}")
            raw_batch = None
            failed += len(nonempty_positions)
    else:
        raw_batch = []

    batch_results = {}
    if raw_batch is not None:
        for pos_in_batch, raw in zip(nonempty_positions, raw_batch):
            raw_sorted = sorted(raw, key=lambda x: x["label"])
            pred_dict = {}
            for i, col in enumerate(LABEL_COLS):
                score = raw_sorted[i]["score"]
                pred_dict[col] = int(score > THRESHOLD)
                pred_dict[f"{col}_prob"] = round(score, 4)
            batch_results[pos_in_batch] = pred_dict

    for i, global_idx in enumerate(chunk_idx):
        if i in batch_results:
            predictions[global_idx] = batch_results[i]
        else:
            pred_dict = {col: 0 for col in LABEL_COLS}
            pred_dict.update({f"{col}_prob": 0.0 for col in LABEL_COLS})
            predictions[global_idx] = pred_dict

print(f"\nClassification complete. Failed: {failed}")

pred_df = pd.DataFrame(predictions)
output_df = pd.concat([full_data.reset_index(drop=True), pred_df], axis=1)
output_df.to_csv(OUTPUT_PATH, index=False)

print(f"\nResults saved to {OUTPUT_PATH}")
print(f"\nPrediction summary (at {THRESHOLD} threshold):")
for col in LABEL_COLS:
    n = pred_df[col].sum()
    print(f"  {col.capitalize()}: {n} cases ({n/len(pred_df):.2%})")
