# Kfold cross validation determine the best imputation method

import GPy
import GPyOpt
import pandas as pd
from utils import *

columns_numeric = pd.read_csv(
    "datasets/original_columns_numeric.csv"
)["Column"].values

output_folder = "model-results/"

# bounds for hyper-parameters
# the bounds dict should be in order of continuous type and then discrete type

bounds = [{'name': 'imputation_method', 'type': 'discrete', 'domain': (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)},
          {'name': 'latent_dim',       'type': 'discrete',
              'domain': (32, 64, 128, 256)},
          {'name': 'epochs',          'type': 'discrete',
           'domain': (30, 50, 100)},
          {'name': 'batch_size',       'type': 'discrete',   'domain': (1, 4, 8, 16, 32)}]

model = Model_LSTM(columns_numeric, output_folder)

opt = GPyOpt.methods.BayesianOptimization(
    f= model.cross_validate, domain=bounds)

# optimize model
opt.run_optimization(max_iter=10)

# print optimized mnist model
print("Optimized Parameters:")
print(bounds[0]["name"], opt.x_opt[0])
print(bounds[1]["name"], opt.x_opt[1])
print(bounds[2]["name"], opt.x_opt[2])
print(bounds[3]["name"], opt.x_opt[3])

print("Optimized loss: {0}".format(opt.fx_opt))
