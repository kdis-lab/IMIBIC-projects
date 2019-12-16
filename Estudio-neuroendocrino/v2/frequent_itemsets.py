import pandas as pd
import numpy as np
from mlxtend.frequent_patterns import apriori

import os

os.chdir("/media/ogreyesp/DATA/workspace/IMIBIC-projects/Estudio-neuroendocrino/v2")

method_name = "all_models"

df = pd.read_csv("results/{method}/datos-preproc/{method}.csv".format(method = method_name))

print(df.shape)

dfT = df.loc[(df["ROC"]>=0.8) & (df["Sens"] >= 0.8) & (df["Spec"] >= 0.8),:]

print(dfT.shape)

# Computing the universe of items
all_patterns = set()

dfT.apply(lambda row: all_patterns.update(row["Atts"].split(" ")), axis=1)

all_patterns = np.array(list(all_patterns))

#Converting the dataset to understanding format to execute aprior algorithm
a= dfT.apply(lambda row: np.isin(all_patterns, row["Atts"].split(" ")).tolist(), axis=1)

newdf = pd.DataFrame(a.values.tolist(), columns=all_patterns)

newdf.to_csv("results/{method}/datos-preproc/{method}2.csv".format(method = method_name), index=False)

#Run apriori
frequent_itemsets = apriori(
    newdf, min_support= 0.5, use_colnames=True)

frequent_itemsets.sort_values(
    "support",
    ascending= False,
    inplace=True)

# Save the itemsets
frequent_itemsets.to_csv("results/{method}/datos-preproc/frequent_itemsets.csv".format(method = method_name), index= False)