# Kfold cross validation determine the best imputation method

import pandas as pd
from utils import *

columns_numeric = pd.read_csv(
    "datasets/original_columns_numeric.csv"
)["Column"].values

output_folder = "model-results/"

model = Model_LSTM(columns_numeric, output_folder)

ip= model.imputation_method[0]

model.dataset = pd.read_csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-{method}-allnumerics.csv".format(
            method=ip))

model.num_features = model.dataset.shape[1]-1
# Optimum values
model.latent_dim = 64
model.nepochs = 50
model.batch_size = 1

# Apply standarization/normalization
scaler = StandardScaler()
features_train = model.dataset.loc[:, model.columns_numeric].values

scaler.fit(features_train)

model.dataset.loc[:, model.columns_numeric] = scaler.transform(features_train)

encoder_input_data_train, decoder_input_data_train, decoder_target_data_train, buckets_train= transform_dataset_different_timesteps(model.dataset)

data_sequence_train = DataSequence(encoder_input_data_train, decoder_input_data_train,
                                               decoder_target_data_train, model.batch_size, model.num_features, buckets_train)

print("Creating model")
model.model = model.create_model()

print("Training model")
model.fit_model(data_sequence_train)

model.save_model()