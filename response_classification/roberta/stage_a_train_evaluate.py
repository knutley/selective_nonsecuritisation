# STAGE A: TRAIN + EVALUATE ONLY (no full-dataset inference yet)
# Combines: your 252 hand-coded positive examples, 40 contrastive examples
# (18 arrest false-positives relabeled 0, 20 brutality false-positives
# relabeled 0, 2 genuine arrests confirmed as 1), and a MODERATE negative
# sample (400 rows, not ~10,400) to avoid the extreme class imbalance that
# forced the aggressive weighting which caused the earlier calibration mess.
# No pos_weight this time - the better-balanced, more targeted data should
# need much less correction. Stops after evaluation - check the per-label
# metrics below before running Stage B (full-dataset inference).
# -------------------------------------------------

import pandas as pd
from datasets import Dataset
from sklearn.model_selection import train_test_split
from transformers import (
    AutoTokenizer,
    AutoModelForSequenceClassification,
    Trainer,
    TrainingArguments,
    EarlyStoppingCallback,
)
from sklearn.metrics import f1_score, precision_score, recall_score, accuracy_score
import torch

LABEL_COLS = ["police_presence", "arrest", "brutality"]
MODEL = "roberta-base"
SAVE_DIR = "./police_severity_classifier_roberta_v3"

# ─────────────────────────────────────────────
# PART 1: LOAD AND COMBINE ALL DATA
# ─────────────────────────────────────────────

print("=== LOADING DATA ===")

pos = pd.read_excel("~/Documents/GitHub/Police_Response/2020-2024/police_manual_sample.xlsx")
pos = pos[["notes"] + LABEL_COLS]
for col in LABEL_COLS:
    pos[col] = pd.to_numeric(pos[col], errors="coerce")
pos = pos.dropna(subset=["notes"])
pos = pos[pos[LABEL_COLS].isin([0, 1]).all(axis=1)]
print(f"Hand-coded positive sample: {len(pos)} rows")

contrastive = pd.read_csv("~/Documents/GitHub/Police_Response/2020-2024/contrastive_examples.csv")
contrastive = contrastive[["notes"] + LABEL_COLS]
print(f"Contrastive examples: {len(contrastive)} rows")

full_data = pd.read_csv(
    "~/Documents/GitHub/Police_Response/2020-2024/acled_classified_police_presence_2020_2024.csv",
    low_memory=False,
)
neg = full_data[full_data["police_presence"] == False].sample(n=400, random_state=42).copy()
neg["police_presence"] = 0
neg["arrest"] = 0
neg["brutality"] = 0
neg = neg[["notes"] + LABEL_COLS]
print(f"Moderate negative sample: {len(neg)} rows")

df = pd.concat([pos, contrastive, neg], ignore_index=True).dropna(subset=["notes"])
for col in LABEL_COLS:
    df[col] = df[col].astype(int)

print(f"\nCombined dataset: {len(df)} rows")
print("\nLabel distribution:")
for col in LABEL_COLS:
    print(f"  {col.capitalize()}: {df[col].sum()} positive cases ({df[col].mean():.2%})")

# ─────────────────────────────────────────────
# PART 2: TRAIN / TEST SPLIT AND TOKENISATION
# ─────────────────────────────────────────────

train_df, test_df = train_test_split(
    df, test_size=0.2, random_state=42, stratify=df["police_presence"]
)
print(f"\nTrain size: {len(train_df)}, Test size: {len(test_df)}")

train_ds = Dataset.from_pandas(train_df.reset_index(drop=True))
test_ds  = Dataset.from_pandas(test_df.reset_index(drop=True))

print("\n=== TOKENIZING DATA ===")
tokenizer = AutoTokenizer.from_pretrained(MODEL)

def tokenize(batch):
    return tokenizer(batch["notes"], truncation=True, padding="max_length", max_length=256)

train_ds = train_ds.map(tokenize, batched=True)
test_ds  = test_ds.map(tokenize, batched=True)

def prepare_labels(batch):
    batch["labels"] = [
        [float(batch[col][i]) for col in LABEL_COLS]
        for i in range(len(batch[LABEL_COLS[0]]))
    ]
    return batch

train_ds = train_ds.map(prepare_labels, batched=True)
test_ds  = test_ds.map(prepare_labels, batched=True)
train_ds = train_ds.remove_columns(LABEL_COLS)
test_ds  = test_ds.remove_columns(LABEL_COLS)

# ─────────────────────────────────────────────
# PART 3: MODEL AND METRICS (per-label, no aggregate-only hiding)
# ─────────────────────────────────────────────

print("\n=== LOADING MODEL ===")
model = AutoModelForSequenceClassification.from_pretrained(
    MODEL, num_labels=len(LABEL_COLS), problem_type="multi_label_classification",
)

def compute_metrics(pred):
    logits, labels = pred
    probs  = torch.sigmoid(torch.tensor(logits))
    preds  = (probs > 0.5).int()
    labels = torch.tensor(labels).int()

    result = {
        "f1": f1_score(labels, preds, average="micro"),
        "accuracy": accuracy_score(labels, preds),
    }
    for i, col in enumerate(LABEL_COLS):
        result[f"{col}_f1"]        = f1_score(labels[:, i], preds[:, i], zero_division=0)
        result[f"{col}_precision"] = precision_score(labels[:, i], preds[:, i], zero_division=0)
        result[f"{col}_recall"]    = recall_score(labels[:, i], preds[:, i], zero_division=0)
    return result

# ─────────────────────────────────────────────
# PART 4: TRAIN (no class weighting - better-balanced data shouldn't need it)
# ─────────────────────────────────────────────

print("\n=== TRAINING MODEL ===")
training_args = TrainingArguments(
    output_dir="./police_severity_model_roberta_v3",
    eval_strategy="epoch",
    save_strategy="epoch",
    learning_rate=2e-5,
    per_device_train_batch_size=8,
    per_device_eval_batch_size=8,
    num_train_epochs=4,
    weight_decay=0.01,
    logging_steps=10,
    load_best_model_at_end=True,
    metric_for_best_model="f1",
    greater_is_better=True,
)

trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=train_ds,
    eval_dataset=test_ds,
    processing_class=tokenizer,
    compute_metrics=compute_metrics,
    callbacks=[EarlyStoppingCallback(early_stopping_patience=2)],
)

trainer.train()
metrics = trainer.evaluate()

print("\n" + "="*60)
print("FINAL EVALUATION METRICS - CHECK THESE BEFORE STAGE B")
print("="*60)
for k, v in sorted(metrics.items()):
    print(f"  {k}: {v}")
print("="*60)

trainer.save_model(SAVE_DIR)
tokenizer.save_pretrained(SAVE_DIR)
print(f"\nModel saved to {SAVE_DIR}")
print("\n>>> STOP HERE. Check arrest_f1/arrest_precision/arrest_recall and")
print(">>> brutality_f1/brutality_precision/brutality_recall above.")
print(">>> If they look reasonable (not 0, not suspiciously perfect), run")
print(">>> Stage B for full-dataset classification. If not, report back")
print(">>> the numbers before running anything further.")
