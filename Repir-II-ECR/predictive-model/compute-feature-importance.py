import pandas as pd

import numpy as np
from utils import *

# The best method was determined by the process described in compute-feature-importance.py
imputation_method = "mean-average_before_after_values"
latent_dim = 64
nepochs = 1
batch_size = 1
n_iter= 1

output_path = "model-results/"

columns_numeric = pd.read_csv(
    "datasets/original_columns_numeric.csv"
)["Column"].values

model = Model_LSTM(columns_numeric, output_path)

model.compute_feature_importance(
    imputation_method, latent_dim, nepochs, batch_size, n_iter)

print("End script")
