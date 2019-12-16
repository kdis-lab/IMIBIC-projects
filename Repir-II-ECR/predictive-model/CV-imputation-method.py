# Kfold cross validation determine the best imputation method

import tensorflow as tf
import pandas as pd
import numpy as np
from matplotlib import pyplot
from utils import *
from multiprocessing import Pool
from sklearn.preprocessing import StandardScaler
import os

output_path = "model-results/"

imputation_methods = ["mean-average_before_after_values", "mean-average_last_next_values", "mean-average_previous_values",
                      "mean-locf", "mean-nocb", "median-average_before_after_values", "median-average_last_next_values",
                      "median-average_previous_values", "median-locf", "median-nocb"]

columns_numeric = pd.read_csv(
    "datasets/original_columns_numeric.csv"
)["Column"].values

batch_size = 4
nepochs = 30

seed = 123

np.random.seed(seed)

for imputation_method in imputation_methods:

    name_method = output_path + imputation_method

    dataset = pd.read_csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-{method}-allnumerics.csv".format(
        method=imputation_method))

    nmax = dataset["Id"].value_counts().max()

    num_features = dataset.shape[1] - 1  # Id is not considered

    loss = []
    val_loss = []

    for nvisit in range(1, nmax-1):

        print("Fold:", nvisit)
        print("*" * 50)

        dataset_train, dataset_test = split_train_test(dataset, nvisit)

        print(dataset_train.shape, dataset_test.shape)

        # Apply standarization/normalization, first on training data, and the same traslation should be applied to the test data
        scaler = StandardScaler()
        features_train = dataset_train.loc[:, columns_numeric].values
        # The mean and std are computed over training data, and then they are translated to test data
        scaler.fit(features_train)

        dataset_train.loc[:, columns_numeric] = scaler.transform(
            features_train)

        dataset_test.loc[:, columns_numeric] = scaler.transform(
            dataset_test.loc[:, columns_numeric].values)

        # Transform the training and test sets
        encoder_input_data_train, decoder_input_data_train, decoder_target_data_train, buckets_train = transform_dataset_different_timesteps(
            dataset_train)

        data_sequence_train = DataSequence(encoder_input_data_train, decoder_input_data_train,
                                           decoder_target_data_train, batch_size, num_features, buckets_train)

        encoder_input_data_test, decoder_input_data_test, decoder_target_data_test, buckets_test = transform_dataset_different_timesteps(
            dataset_test)

        data_sequence_test = DataSequence(encoder_input_data_test, decoder_input_data_test,
                                          decoder_target_data_test, batch_size, num_features, buckets_test)

        with Pool(processes=1) as pool:
            history = pool.apply(
                execute_one_fold, (data_sequence_train, data_sequence_test, nepochs, num_features, name_method, nvisit))

        #lossT= (np.array(history['loss'])/nvisit).tolist()
        #val_lossT= (np.array(history['val_loss'])/nvisit).tolist()

        loss.append(history['loss'])
        val_loss.append(history['val_loss'])

        print(loss)
        print(val_loss)

        print("End fold")
        print("*" * 50)

    # Saving loss
    #################################################

    loss = np.array(loss)
    val_loss = np.array(val_loss)

    np.save(name_method + "-Loss-CV.npy", loss)
    np.save(name_method + "-Val-Loss-CV.npy", val_loss)

    # Plot history, averaging losses
    #################################################
    create_plot_CV(loss, val_loss, name_method +
                   "-CV-loss.svg")
