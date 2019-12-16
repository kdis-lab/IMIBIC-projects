# Saving the dataset of importance
import pandas as pd
from matplotlib import pyplot
import matplotlib.cm as cm
import numpy as np

df = pd.read_csv(
    "model-results/mean-average_before_after_values-CV-feature-importance.csv")

maximum = np.max(df.values[0, :])
minimum = np.min(df.values[0, :])

colors = cm.rainbow(((maximum-df.values[0, :])/(maximum-minimum)).tolist())

pyplot.figure(figsize=(14, 14))
ax = pyplot.subplot(111)

x= np.arange(df.values[0, :].size)

ax.bar( x,
       df.values[0, :], color=colors)

maximum = np.max(df.values[1, :])
minimum = np.min(df.values[1, :])

colors = cm.rainbow(((maximum-df.values[1, :])/(maximum-minimum)).tolist())

ax.bar(x, -df.values[1, :], color=colors)

pyplot.xticks(x, df.columns.values, rotation=60, ha='right')

pyplot.tight_layout(pad=0, w_pad=0.0, h_pad=0.0)

pyplot.savefig(
    "model-results/mean-average_before_after_values-CV-feature-importance.svg", format="svg")
