import pandas as pd
from utils import missing_imputation
import numpy as np

import os

os.chdir(os.getcwd() + "/predictive-modelv3")

imputation_method= "mean-average_before_after_values"

columns_numeric = pd.read_csv(
    "datasets/original_columns_numeric.csv")["Column"].values

columns_binary = pd.read_csv(
    "datasets/original_columns_binary.csv")["Column"].values

columns_nominal = pd.read_csv(
        "datasets/original_columns_nominal.csv")["Column"].values

dtypes={c:np.float64 for c in columns_numeric}

dtypes.update({c:str for c in columns_binary})

dtypes.update({c:str for c in columns_nominal})

dataset = pd.read_csv("datasets/test3.csv", dtype= dtypes)

trainingset_original = pd.read_csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-{method}.csv".format(
            method=imputation_method))

missing_imputation(dataset, trainingset_original)

# Checking
num_nans = dataset.size - dataset.count().sum()

if num_nans == 0:
    print("Completed imputation process")
else:
    print("Imputation process incompleted")



