import tensorflow as tf
from tensorflow.keras.layers import Input, LSTM, Dense
from tensorflow.keras.utils import Sequence
from tensorflow.keras.utils import plot_model
from tensorflow.keras.callbacks import ModelCheckpoint
from tensorflow.keras.callbacks import EarlyStopping
from multiprocessing import Pool
from tensorflow.keras.models import Model, load_model
from keras import losses

from matplotlib import pyplot
import matplotlib.cm as cm

import pandas as pd
import numpy as np
from sklearn.utils import check_random_state

from sklearn.preprocessing import StandardScaler

from numba import cuda


def transform_dataset_different_timesteps(dataset):
    """
    dataset: is the original dataset with all numeric columns

    This function returns a tuple of lenght 4.
    encoder_input_data: list of list
    decoder_input_data: list of FG values
    decoder_target_data: list of FG values to predict
    buckets: dictionary with info of samples with the same timesteps. Key is the number of timesteps,
    and value is a list with the indexes of the samples which have this number of timesteps"""

    # Represents the encoder input data for each sample
    encoder_input_data = []
    # Represents the decoder input data for each sample
    decoder_input_data = []
    # Represents the decoder target data for each sample
    decoder_target_data = []

    # Each entry of this list represents the FG value to be predicted for each example
    y = []

    buckects = {}

    pos = 0

    # For each patient
    for index, rows in dataset.groupby("Id"):

        if rows.shape == 1:
            continue

        # Select all columns except the column 'Id'
        rows = rows.iloc[:, 1:]

        num_visits = rows.shape[0]

        for visit in range(1, num_visits):

            visit_data = []

            rows.iloc[:visit, :].apply(
                lambda row: visit_data.extend(list(row.values)), axis=1)

            # El decoder recibira el ultimo FG conocido. Es decir el primer FG pasado al decoder,
            # es el ultimo valor de FG que se le pasa al encoder (el último FG de visita). El resto de valores de FG no son pasados al encoder.

            FG_values = rows["Filtrado_glomerular"].values[(visit-1):].tolist()

            encoder_input_data.append(visit_data)
            # El decoder_input_data tendra los valores de FG que se quieren predecir +1, el adicional es el ultimo
            # valor conocido de FG
            decoder_input_data.append(FG_values[:-1])
            # El decoder_target_data tendra los valores de FG que se quieren predecir
            decoder_target_data.append(FG_values[1:])

            key = (visit, len(FG_values)-1)
            if key not in buckects:
                buckects[key] = [pos]
            else:
                buckects[key].append(pos)

            pos += 1

    # decoder_target_data = sequence.pad_sequences(decoder_target_data, dtype="float32", padding='post', value= -1000).tolist()

    return encoder_input_data, decoder_input_data, decoder_target_data, buckects


def transform_data_to_predict(input_data, nVisits):
    """
    input_data: is the original dataset with all numeric columns
    """

    # Select all columns except the column 'Id'
    rows = input_data.iloc[-nVisits:, 1:]

    visit_data = []

    num_features = rows.shape[1]

    rows.apply(lambda row: visit_data.extend(list(row.values)), axis=1)

    last_FG_value = rows.iloc[-1, -1]

    timesteps_visitas = int(len(visit_data) / num_features)

    visit_data = np.array(visit_data).reshape(
        (1, timesteps_visitas, num_features))

    return visit_data, last_FG_value


def transform_data_to_predictv2(input_data):
    """
    Esta version descompone todos los registros y devuelve una tripla ((nsamples, 1, num_feartures), current_FG, next_FG)

    input_data: is the original dataset with all numeric columns

    """

    lastFGs = []
    nextFGs = []

    allVisit = []

    num_features = input_data.shape[1] - 1

    # For each patient
    for index, rows in input_data.groupby("Id"):

        # Select all columns except the column 'Id'
        visit_data = rows.iloc[0, 1:].values
        #rows.iloc[0, 1:].apply(lambda row: visit_data.extend(list(row.values)), axis=1)

        allVisit.append(visit_data)

        lastFGs.append(rows.iloc[0, -1])

        nextFGs.append(rows.iloc[1, -1])

    nsamples = len(allVisit)
    allVisit = np.array(allVisit).reshape((nsamples, 1, num_features))

    lastFGs = np.array(lastFGs).reshape((nsamples, 1, 1))

    nextFGs = np.array(nextFGs)

    return allVisit, lastFGs, nextFGs


def split_train_test(dataset, nvisits):

    train = pd.DataFrame([], columns=dataset.columns.tolist())
    test = pd.DataFrame([], columns=dataset.columns.tolist())

    # For each patient
    for index, rows in dataset.groupby("Id"):

        rows_training = rows.iloc[: nvisits + 1, :]
        rows_test = rows.iloc[: nvisits + 2, :]

        train = train.append(rows_training)

        if rows_test.shape[0] > rows_training.shape[0]:
            test = test.append(rows_test)

    return train, test


def split_train_testv2(dataset, nvisits):
    """Esta version se diferencia de la otra en que el test solo esta formado por la siguiente visita"""

    train = pd.DataFrame([], columns=dataset.columns.tolist())
    test = pd.DataFrame([], columns=dataset.columns.tolist())

    # For each patient
    for index, rows in dataset.groupby("Id"):

        nvisitsT = nvisits

        if rows.shape[0] < nvisits:

            nvisitsT = rows.shape[0] - 1

        rows_training = rows.iloc[: nvisitsT + 1, :]
        train = train.append(rows_training)

        if nvisitsT < (rows.shape[0]-1):

            # Select the next visit
            rows_test = rows.iloc[nvisits: nvisits + 2, :]
            test = test.append(rows_test)

    return train, test


def data_generator(encoder_X, decoder_X, decoder_y, num_features):

    i = 0
    nsamples = len(encoder_X)

    while True:

        enc_x = encoder_X[i]  # get the list representing the predictive vars
        dec_x = decoder_X[i]
        dec_y = decoder_y[i]

        i += 1

        timesteps_encoder = int(len(enc_x) / num_features)
        timesteps_decoder = len(dec_x)

        enc_x = np.array(enc_x).reshape((1, timesteps_encoder, num_features))
        dec_x = np.array(dec_x).reshape((1, timesteps_decoder, 1))
        dec_y = np.array(dec_y).reshape((1, timesteps_decoder, 1))

        yield [enc_x, dec_x], dec_y

        # Reset the counter
        if i == nsamples:
            i = 0

# Define an input sequence and process it


def create_model(num_features, print_summary=False, plot_model=False):

    latent_dim = 256

    encoder_inputs = Input(shape=(None, num_features), name="encoder_input")
    encoder = LSTM(latent_dim, return_state=True, name="encoder_layer")
    encoder_outputs, state_h, state_c = encoder(encoder_inputs)
    # We discard encoder_outputs and only keep the states.
    encoder_states = [state_h, state_c]

    # Set up the decoder, using encoder_states as initial state.
    decoder_inputs = Input(shape=(None, 1), name="decoder_input")
    # We set up our decoder to return full output sequences,
    # and to return internal states as well. We don't use the
    # return states in the training model, but we will use them in inference.
    decoder_lstm = LSTM(latent_dim, return_sequences=True,
                        return_state=True,  name="decoder_layer")
    decoder_outputs, _, _ = decoder_lstm(decoder_inputs,
                                         initial_state=encoder_states)

    decoder_dense = Dense(1, name="dense_layer")
    decoder_outputs = decoder_dense(decoder_outputs)

    # Define the model that will turn
    # encoder_input_data & decoder_input_data into decoder_target_data
    model = Model([encoder_inputs, decoder_inputs], decoder_outputs)

    if print_summary:
        print(model.summary())

    model.compile(loss="logcosh", optimizer='adam')

    if plot_model:
        plot_model(model, to_file='model.png', show_shapes=True)

    return model


def fit_model_generator(model, name_model, encoder_input_data, decoder_input_data, decoder_target_data, num_features, nsamples, nepochs):
    """
    Function that fits a model.
    """
    # Application checkpointing is a fault tolerance technique for long running processes.
    checkpoint = ModelCheckpoint(
        name_model + ".best.hdf5",
        monitor='val_loss',
        verbose=0,
        save_best_only=True,
        mode='min',
        period=1)

    return model.fit_generator(data_generator(encoder_input_data, decoder_input_data, decoder_target_data, num_features),
                               steps_per_epoch=nsamples, epochs=nepochs, use_multiprocessing=False, callbacks=[checkpoint])


def fit_model_sequence(model, name_model, dataTrain, dataTest, nepochs):
    """
    Function that fits a model.
    """
    # Application checkpointing is a fault tolerance technique for long running processes.
    # checkpoint = ModelCheckpoint(
    #    name_model + ".best.hdf5",
    #    monitor='val_loss',
    #    verbose=0,
    #    save_best_only=True,
    #    mode='min',
    #    period=1)

    return model.fit_generator(dataTrain, epochs=nepochs, verbose=1, validation_data=dataTest, use_multiprocessing=False)


def create_plot_CV(training_loss, test_loss, path_name, print_std=False):

    # Plot history, averaging losses
    #################################################
    train_scores_mean = np.mean(training_loss, axis=0)
    train_scores_std = np.std(training_loss, axis=0)
    test_scores_mean = np.mean(test_loss, axis=0)
    test_scores_std = np.std(test_loss, axis=0)

    # Start a new figure
    pyplot.figure()
    pyplot.plot(train_scores_mean, label='train')
    pyplot.plot(test_scores_mean, label='test')

    if print_std:

        pyplot.fill_between(list(range(0, training_loss.shape[1])), train_scores_mean - train_scores_std,
                            train_scores_mean + train_scores_std, alpha=0.1, color="b")

        pyplot.fill_between(list(range(0, training_loss.shape[1])), test_scores_mean - test_scores_std,
                            test_scores_mean + test_scores_std, alpha=0.1, color="r")

    pyplot.legend()
    pyplot.savefig(path_name + "-CV-loss.svg", format="svg")


def execute_one_fold(dataTrain, dataTest, nepochs, num_features, path_to_save, fold):

    config = tf.ConfigProto()
    config.gpu_options.allow_growth = True
    tf.Session(config=config)

    # Construir el modelo LSTM
    # None allow that samples can have different timesteps
    model = create_model(num_features)

    history = fit_model_sequence(
        model, path_to_save + "-Fold-" + str(fold), dataTrain, dataTest, nepochs)

    return history.history


class DataSequence(Sequence):

    def __init__(self, x_encoder, x_decoder, y_decoder, batch_size, num_features, buckets):

        self.batch_size = batch_size
        self.num_features = num_features
        self.X_encoder = []
        self.X_decoder = []
        self.y_decoder = []

        for key, list_indexes in buckets.items():

            permut = np.random.permutation(list_indexes)

            split_indices = [int(self.batch_size*(i+1))
                             for i in np.arange(np.floor(permut.size / self.batch_size))]

            list_arrays = np.array_split(permut, split_indices)

            for l in list_arrays:

                if len(l) == 0:
                    break

                x_encoder_array = []
                x_decoder_array = []
                y_decoder_array = []

                for i in l:
                    x_encoder_array.append(x_encoder[i])
                    x_decoder_array.append(x_decoder[i])
                    y_decoder_array.append(y_decoder[i])

                # Array of shape (nsamples, nvisits, nfeatures) -> (batch_size, timesteps, nfeatures)
                x_encoder_array = np.array(x_encoder_array).reshape(
                    (l.size, key[0], self.num_features))
                x_decoder_array = np.array(x_decoder_array).reshape(
                    (l.size, key[1], 1))
                y_decoder_array = np.array(y_decoder_array).reshape(
                    (l.size, key[1], 1))

                self.X_encoder.append(x_encoder_array)
                self.X_decoder.append(x_decoder_array)
                self.y_decoder.append(y_decoder_array)

    def __len__(self):
        return len(self.y_decoder)

    def size(self):
        return len(self.y_decoder)

    def __getitem__(self, idx):

        if self.X_encoder[idx].shape[0] == 0:
            import sys
            sys.exit("Alert: Batch of size 0")

        return [self.X_encoder[idx], self.X_decoder[idx]], self.y_decoder[idx]

    def subset(self, indexes):

        empty_data = DataSequence(
            [], [], [], self.batch_size, self.num_features, {})

        for index in indexes:

            [X_encoder_temp, X_decoder_temp], y_decoder_temp = self[index]

            empty_data.X_encoder.append(X_encoder_temp)
            empty_data.X_decoder.append(X_decoder_temp)
            empty_data.y_decoder.append(y_decoder_temp)

        return empty_data


class Model_LSTM():

    def __init__(self, columns_numeric, output_folder, gpu=False):

        print("Constructor")
        self.columns_numeric = columns_numeric
        self.imputation_method = {0: "mean-average_before_after_values", 1: "mean-average_last_next_values", 2: "mean-average_previous_values",
                                  3: "mean-locf", 4: "mean-nocb", 5: "median-average_before_after_values", 6: "median-average_last_next_values",
                                  7: "median-average_previous_values", 8: "median-locf", 9: "median-nocb"}
        self.output_folder = output_folder
        self.gpu = False

    def create_model(self):

        if self.gpu:
            config = tf.ConfigProto()
            config.gpu_options.allow_growth = True
            self.sess = tf.Session(config=config)

        encoder_inputs = Input(
            shape=(None, self.num_features), name="encoder_input")

        encoder = LSTM(self.latent_dim,
                       return_state=True, name="encoder_layer")

        encoder_outputs, state_h, state_c = encoder(encoder_inputs)

        # We discard encoder_outputs and only keep the states.
        encoder_states = [state_h, state_c]

        # Set up the decoder, using encoder_states as initial state.
        decoder_inputs = Input(shape=(None, 1), name="decoder_input")
        # We set up our decoder to return full output sequences,
        # and to return internal states as well. We don't use the
        # return states in the training model, but we will posteriorly use them in inference.
        decoder_lstm = LSTM(self.latent_dim, return_sequences=True,
                            return_state=True,  name="decoder_layer")

        decoder_outputs, _, _ = decoder_lstm(decoder_inputs,
                                             initial_state=encoder_states)

        decoder_dense = Dense(1, name="dense_layer")
        decoder_outputs = decoder_dense(decoder_outputs)

        # Define the model that will turn
        # encoder_input_data & decoder_input_data into decoder_target_data
        model = Model([encoder_inputs, decoder_inputs], decoder_outputs)

        model.compile(loss="logcosh", optimizer='adam',
                      metrics=[losses.mean_squared_error])

        return model

    def fit_model(self, dataTrain, dataTest=None, early_stopping=False):

        # simple early stopping
        callbacks = []
        if early_stopping:

            es = EarlyStopping(monitor='val_loss', mode='min',
                               verbose=1, patience=50, restore_best_weights=True)

            callbacks.append(es)

        return self.model.fit_generator(
            dataTrain, epochs=self.nepochs, verbose=1, validation_data=dataTest, callbacks=callbacks)

    def evaluate_model(self, dataTest):

        return self.model.evaluate(dataTest)

    def predict(self, dataTest):

        return self.model.predict(dataTest)

    def save_model(self):

        print("Saving model...")
        self.model.save(self.output_folder + "model.hdf5")

    def cross_validate(self, x):

        loss = []
        val_loss = []

        method_temp = self.imputation_method[int(x[:, 0])]

        self.dataset = pd.read_csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-{method}-allnumerics.csv".format(
            method=method_temp))

        nmax = self.dataset["Id"].value_counts().max()

        self.num_features = self.dataset.shape[1]-1

        self.latent_dim = int(x[:, 1])
        self.nepochs = int(x[:, 2])
        self.batch_size = int(x[:, 3])

        for nvisit in range(1, nmax-1):

            dataset_train, dataset_test = split_train_test(
                self.dataset, nvisit)

            # Apply standarization/normalization, first on training data, and the same traslation should be applied to the test data
            scaler = StandardScaler()
            features_train = dataset_train.loc[:, self.columns_numeric].values
            # The mean and std are computed over training data, and then they are translated to test data
            scaler.fit(features_train)

            dataset_train.loc[:, self.columns_numeric] = scaler.transform(
                features_train)

            dataset_test.loc[:, self.columns_numeric] = scaler.transform(
                dataset_test.loc[:, self.columns_numeric].values)

            # Transform the training and test sets
            encoder_input_data_train, decoder_input_data_train, decoder_target_data_train, buckets_train = transform_dataset_different_timesteps(
                dataset_train)

            data_sequence_train = DataSequence(encoder_input_data_train, decoder_input_data_train,
                                               decoder_target_data_train, self.batch_size, self.num_features, buckets_train)

            encoder_input_data_test, decoder_input_data_test, decoder_target_data_test, buckets_test = transform_dataset_different_timesteps(
                dataset_test)

            data_sequence_test = DataSequence(encoder_input_data_test, decoder_input_data_test,
                                              decoder_target_data_test, self.batch_size, self.num_features, buckets_test)

            self.model = self.create_model()

            # with Pool(processes=1) as pool:
            #    pool.apply(fit_model_sequence, (self.model, "", data_sequence_train, None, self.nepochs))

            history = self.fit_model(data_sequence_train, data_sequence_test)

            loss.append(history.history['loss'])
            val_loss.append(history.history['val_loss'])

            if self.gpu:
                self.sess.close()
                cuda.select_device(0)

        loss = np.array(loss)
        val_loss = np.array(val_loss)

        # output_folder-imputation_method-latent_dim-nepochs-batch_size
        name_method = "{0}-{1}-{2}-{3}-{4}".format(
            self.output_folder, method_temp, self.latent_dim, self.nepochs, self.batch_size)

        np.save(name_method + "-Loss-CV.npy", loss)
        np.save(name_method + "-Val-Loss-CV.npy", val_loss)

        # Plot history, averaging losses
    #################################################
        create_plot_CV(loss, val_loss, name_method +
                       "-CV-losses.svg")

        mean_val_loss = np.mean(val_loss, axis=0)

        overall_loss = mean_val_loss.min() + np.mean(loss,
                                                     axis=0)[mean_val_loss.argmin()]

        print(x, overall_loss)

        # We want a model with a minimum training and test loss
        return overall_loss

    def cross_validatev2(self):
        """Esta funcion hace un cross val tal y como plantea en https://towardsdatascience.com/time-series-nested-cross-validation-76adba623eb9, 
        con el objetivo de comparar el modelo con los de state-of-the-art"""

        test_loss = []

        method_temp = "mean-average_before_after_values"

        self.dataset = pd.read_csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-{method}-allnumerics.csv".format(
            method=method_temp))

        nmax = self.dataset["Id"].value_counts().max()

        self.num_features = self.dataset.shape[1]-1

        self.latent_dim = 64
        self.nepochs = 50
        self.batch_size = 1

        for nvisit in range(1, nmax-1):

            dataset_train, dataset_test = split_train_test(
                self.dataset, nvisit)

            # Apply standarization/normalization, first on training data, and the same traslation should be applied to the test data
            scaler = StandardScaler()
            features_train = dataset_train.loc[:, self.columns_numeric].values
            # The mean and std are computed over training data, and then they are translated to test data
            scaler.fit(features_train)

            dataset_train.loc[:, self.columns_numeric] = scaler.transform(
                features_train)

            dataset_test.loc[:, self.columns_numeric] = scaler.transform(
                dataset_test.loc[:, self.columns_numeric].values)

            # Transform the training and test sets
            encoder_input_data_train, decoder_input_data_train, decoder_target_data_train, buckets_train = transform_dataset_different_timesteps(
                dataset_train)

            data_sequence_train = DataSequence(encoder_input_data_train, decoder_input_data_train,
                                               decoder_target_data_train, self.batch_size, self.num_features, buckets_train)

            encoder_input_data_test, decoder_input_data_test, decoder_target_data_test, buckets_test = transform_dataset_different_timesteps(
                dataset_test)

            data_sequence_test = DataSequence(encoder_input_data_test, decoder_input_data_test,
                                              decoder_target_data_test, self.batch_size, self.num_features, buckets_test)

            self.model = self.create_model()

            history = self.fit_model(data_sequence_train, data_sequence_test)

            test_loss.append(
                np.sqrt(np.min(history.history["val_mean_squared_error"])))

            #allVisit, lastFGs, nextFGs= transform_data_to_predictv2(dataset_test)

            #p= self.predict([allVisit, lastFGs]).flatten()

            #test_loss.append(np.sqrt(np.mean((p - nextFGs)**2)))

        # return np.mean(test_loss), np.std(test_loss)
        return test_loss

    def compute_feature_importance(self, imputation_method, latent_dim, nepochs, batch_size, n_iter):

        self.dataset = pd.read_csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-{method}-allnumerics.csv".format(
            method=imputation_method))

        nmax = self.dataset["Id"].value_counts().max()

        self.num_features = self.dataset.shape[1]-1

        self.latent_dim = latent_dim
        self.nepochs = nepochs
        self.batch_size = batch_size

        global_feature_importances_over = None
        global_feature_importances_under = None
        global_column_names = None

        for nvisit in range(1, nmax-1):

            dataset_train, dataset_test = split_train_test(
                self.dataset, nvisit)

            # Apply standarization/normalization, first on training data, and the same traslation should be applied to the test data
            scaler = StandardScaler()
            features_train = dataset_train.loc[:, self.columns_numeric].values
            # The mean and std are computed over training data, and then they are translated to test data
            scaler.fit(features_train)

            dataset_train.loc[:, self.columns_numeric] = scaler.transform(
                features_train)

            dataset_test.loc[:, self.columns_numeric] = scaler.transform(
                dataset_test.loc[:, self.columns_numeric].values)

            # Transform the training and test sets
            encoder_input_data_train, decoder_input_data_train, decoder_target_data_train, buckets_train = transform_dataset_different_timesteps(
                dataset_train)

            data_sequence_train = DataSequence(encoder_input_data_train, decoder_input_data_train,
                                               decoder_target_data_train, self.batch_size, self.num_features, buckets_train)

            self.model = self.create_model()

            self.fit_model(data_sequence_train)

            column_names, feature_importances_over, feature_importances_under = get_score_importances(
                data=dataset_test, model=self, n_iter=n_iter)

            if global_column_names is None:

                global_feature_importances_over = feature_importances_over
                global_feature_importances_under = feature_importances_under
                global_column_names = column_names
            else:
                global_feature_importances_over += feature_importances_over
                global_feature_importances_under += feature_importances_under

            self.sess.close()
            cuda.select_device(0)
            # cuda.close()

        # Averaging the importance
        global_feature_importances_over /= (nmax-2)
        global_feature_importances_under /= (nmax-2)

        # Saving the dataset of importance
        df = pd.DataFrame(global_feature_importances_over.reshape(1, global_feature_importances_over.shape[0]),
                          columns=global_column_names.tolist())

        df2 = pd.DataFrame(global_feature_importances_under.reshape(1, global_feature_importances_under.shape[0]),
                           columns=global_column_names.tolist())

        df = df.append(df2)

        df.to_csv(self.output_folder + imputation_method +
                  "-CV-feature-importance.csv", index=False)

        maximum = np.max(df.values[0, :])
        minimum = np.min(df.values[0, :])

        colors = cm.rainbow(
            ((maximum-df.values[0, :])/(maximum-minimum)).tolist())

        pyplot.figure(figsize=(14, 14))
        ax = pyplot.subplot(111)

        ax.bar(np.arange(df.values[0, :].size),
               df.values[0, :], color=colors)

        maximum = np.max(df.values[1, :])
        minimum = np.min(df.values[1, :])

        colors = cm.rainbow(
            ((maximum-df.values[1, :])/(maximum-minimum)).tolist())

        ax.bar(np.arange(df.values[1, :].size),
               df.values[1, :], color=colors)

        pyplot.xticks(np.arange(df.values.size),
                      df.columns.values, rotation=60, ha='right')

        pyplot.tight_layout(pad=0, w_pad=0.0, h_pad=0.0)

        pyplot.savefig(self.output_folder + imputation_method +
                       "-CV-feature-importance.svg", format="svg")


def compute_feature_importance2(self, imputation_method, latent_dim, nepochs, batch_size, n_iter):

    self.dataset = pd.read_csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-{method}-allnumerics.csv".format(
        method=imputation_method))

    nmax = self.dataset["Id"].value_counts().max()

    self.num_features = self.dataset.shape[1]-1

    self.latent_dim = latent_dim
    self.nepochs = nepochs
    self.batch_size = batch_size

    global_feature_importances = None
    global_column_names = None

    for nvisit in range(1, nmax-1):

        dataset_train, dataset_test = split_train_test(
            self.dataset, nvisit)

        # Apply standarization/normalization, first on training data, and the same traslation should be applied to the test data
        scaler = StandardScaler()
        features_train = dataset_train.loc[:, self.columns_numeric].values
        # The mean and std are computed over training data, and then they are translated to test data
        scaler.fit(features_train)

        dataset_train.loc[:, self.columns_numeric] = scaler.transform(
            features_train)

        dataset_test.loc[:, self.columns_numeric] = scaler.transform(
            dataset_test.loc[:, self.columns_numeric].values)

        # Transform the training and test sets
        encoder_input_data_train, decoder_input_data_train, decoder_target_data_train, buckets_train = transform_dataset_different_timesteps(
            dataset_train)

        data_sequence_train = DataSequence(encoder_input_data_train, decoder_input_data_train,
                                           decoder_target_data_train, self.batch_size, self.num_features, buckets_train)

        self.model = self.create_model()

        self.fit_model(data_sequence_train)

        column_names, feature_importances = get_score_importances(
            data=dataset_test, model=self, n_iter=n_iter)

        if global_feature_importances is None:

            global_feature_importances = feature_importances
            global_column_names = column_names
        else:
            global_feature_importances += feature_importances

        self.sess.close()
        cuda.select_device(0)
        # cuda.close()

    # Averaging the importance
    global_feature_importances /= (nmax-2)

    # Saving the dataset of importance
    df = pd.DataFrame(global_feature_importances.reshape(1, global_feature_importances.shape[0]),
                      columns=global_column_names.tolist())

    df.to_csv(self.output_folder + imputation_method +
              "-CV-feature-importance.csv", index=False)

    maximum = np.max(df.values)
    minimum = np.min(df.values)

    colors = cm.rainbow(
        ((maximum-df.values[0, :])/(maximum-minimum)).tolist())

    pyplot.figure(figsize=(14, 14))
    pyplot.bar(np.arange(df.values[0, :].size),
               df.values[0, :], color=colors)
    pyplot.xticks(np.arange(df.values.size),
                  df.columns.values, rotation=60, ha='right')

    pyplot.tight_layout(pad=0, w_pad=0.0, h_pad=0.0)

    pyplot.savefig(self.output_folder + imputation_method +
                   "-CV-feature-importance.svg", format="svg")


""" Function to be used in the prediction needed to compute the feature weights"""


def score_func2(data_to_predict, model):
    "data: pandas dataframe to be predicted"
    "model: model to be used for prediction"

    encoder_input_data_test, decoder_input_data_test, decoder_target_data_test, buckets_test = transform_dataset_different_timesteps(
        data_to_predict)

    data_sequence_test = DataSequence(encoder_input_data_test, decoder_input_data_test,
                                      decoder_target_data_test, model.batch_size, model.num_features, buckets_test)

    value = model.evaluate_model(data_sequence_test)

    return 1/value


def score_func(data_to_predict, model):
    "data: pandas dataframe to be predicted"
    "model: model to be used for prediction"

    encoder_input_data_test, decoder_input_data_test, decoder_target_data_test, buckets_test = transform_dataset_different_timesteps(
        data_to_predict)

    data_sequence_test = DataSequence(encoder_input_data_test, decoder_input_data_test,
                                      decoder_target_data_test, model.batch_size, model.num_features, buckets_test)

    over_global = 0
    under_global = 0

    for i in range(0, data_sequence_test.size()):

        item = data_sequence_test.__getitem__(i)
        values_predicted = model.predict(item[0]).flatten()
        actual_values = item[1].flatten()

        score = values_predicted - actual_values

        for s in range(0, score.size):

            temp = score[s]

            if(temp >= 0):
                over_global += np.log(np.cosh(temp))
            else:
                under_global += np.log(np.cosh(temp))

    print(over_global, under_global)

    return 1/over_global, 1/under_global


""" Function to generate random longitudinal datasets"""


def iter_shuffled(data, random_state=None):
    """
    data: is a pandas dataframe
    Return an iterator of X matrices which have one or more columns shuffled.
    After each iteration yielded matrix is mutated inplace, so
    if you want to use multiple of them at the same time, make copies.
    By default, all columns are shuffled once, i.e. columns_to_shuffle
    is ``range(X.shape[1])``.
    """
    rng = check_random_state(random_state)

    # shuffle all columns, except Id
    columns_to_shuffle = range(1, data.shape[1])

    column_names = data.columns.values

    data_res = data.copy()  # copy the dataframe

    basals = ["Sexo", "Peso_nacimiento", "Semanas_gestacion", "Dx_histologico", "Patologia_extrarrenal", "Antecedentes_familiares", "Edad_diagnostico",
              "Raza_Caucasiana", "Raza_Hispana", "Raza_Negra", "Raza_Oriental", "Raza_Otra", "Raza_Árabe", "Grupo_diagnostico_EG", "Grupo_diagnostico_ESAR", "Grupo_diagnostico_ETI",
              "Grupo_diagnostico_NHF", "Grupo_diagnostico_TRD"]

    nobasals = ["Peso", "Talla",	"SC", "IMC", "Urea", "Cr", "HTA", "PAS", "PAD", "Hto", "Hb", "Ferritina", "Indice_transferrina", "Ca", "P", "PTH", "X25.OH", "Na", "K",
                "HCO3", "BUN", "Estatinas", "Quelantes_calcio", "Quelantes_libres_calcio", "Vitamina_D3", "dihidroxivintamina", "hidroxivitamina", "otra_vitamina", "Fe", "EPO",
                "Bicarbonato", "Resinas_intercambio", "Cloruro_Na", "HGH", "Acido_folico", "Alopurinol", "Sonda_nasogastrica", "Suplementos_nutricionales", "Fluorhidrocortisona",
                "hipotensor_IECA_ARA", "IECA", "ARA", "Aporte_calcio", "Tiempo_evolucion", "Edad_visita", "Proteinuria_No realizada", "Proteinuria_Normal", "Proteinuria_Patológica",
                "Microalbuminuria_No realizada", "Microalbuminuria_Normal", "Microalbuminuria_Patológica", "Filtrado_glomerular"]

    for column in columns_to_shuffle:

        col_name = column_names[column]

        print("Altering ", col_name)

        if col_name in basals:
            basal_var = True
        else:
            basal_var = False

        if col_name in ["Raza_Caucasiana", "Grupo_diagnostico_EG"]:
            categorical_basal = True
        else:
            categorical_basal = False

        if col_name == "Proteinuria_No realizada":
            col_name == ["Proteinuria_No realizada", "Proteinuria_Normal",
                         "Proteinuria_Patológica"]  # a categorical var

        if col_name == "Microalbuminuria_No realizada":
            col_name = ["Microalbuminuria_No realizada",
                        "Microalbuminuria_Normal", "Microalbuminuria_Patológica"]

        if col_name == "Raza_Caucasiana":
            col_name = ["Raza_Caucasiana", "Raza_Hispana",
                        "Raza_Negra", "Raza_Oriental", "Raza_Otra", "Raza_Árabe"]

        if col_name == "Grupo_diagnostico_EG":
            col_name = ["Grupo_diagnostico_EG", "Grupo_diagnostico_ESAR",
                        "Grupo_diagnostico_ETI", "Grupo_diagnostico_NHF", "Grupo_diagnostico_TRD"]

        if col_name in ["Proteinuria_Normal", "Proteinuria_Patológica", "Microalbuminuria_Normal", "Microalbuminuria_Patológica", "Raza_Hispana", "Raza_Negra", "Raza_Oriental", "Raza_Otra", "Raza_Árabe",
                        "Grupo_diagnostico_ESAR", "Grupo_diagnostico_ETI", "Grupo_diagnostico_NHF", "Grupo_diagnostico_TRD"]:
            continue  # do nothing

        for index, rows in data_res.groupby("Id"):

            if basal_var:

                if categorical_basal:

                    value = np.zeros(len(col_name))
                    pos = np.random.randint(len(col_name), size=1)[0]
                    value[pos] = 1

                    data_res.loc[(data_res["Id"] == index).values,
                                 col_name] = value

                else:

                    unique_values = np.unique(data_res[col_name].values)

                    value = np.random.choice(
                        unique_values, 1, replace=False)[0]

                    data_res.loc[(data_res["Id"] == index).values,
                                 col_name] = value

            else:  # Longitudinal var
                   # Generate a permutation
                values = rows[col_name].sample(
                    frac=1, replace=False, random_state=rng).values

                # update the new values
                data_res.loc[(data_res["Id"] == index).values,
                             col_name] = values

        yield data_res

        # restore the original data column
        data_res[col_name] = data[col_name].copy()


"""Function to compute the feature importance and to explain the overall behavior of a model.
Feature importances are computed as score decrease when a feature is not available."""


def get_score_importances(data,
                          model,
                          n_iter=10,
                          random_state=None
                          ):
    """
    data: is a pandas dataframe
    n_iter: iterations of the basic algorithm is done, each iteration
    starting from a different random seed.
    model: model to be used in prediction
    Return (feature_name, importance) array of feature importance
    """

    rng = check_random_state(random_state)

    base_score = score_func(data, model)

    # score_decreases is a list of length n_iter with feature importance arrays
    # (each array is of shape n_features)
    scores_changes_over = []
    scores_changes_under = []

    for i in range(n_iter):

        scores_shuffled = [score_func(data_shuffled, model)
                           for data_shuffled in iter_shuffled(data, random_state=rng)]

        scores_shuffled_over = []
        scores_shuffled_under = []

        for tup in scores_shuffled:

            scores_shuffled_over.append(tup[0])
            scores_shuffled_under.append(tup[1])

        scores_shuffled_over = np.array(scores_shuffled_over)
        scores_shuffled_under = np.array(scores_shuffled_under)

        scores_changes_over.append(
            np.abs(scores_shuffled_over - base_score[0]))
        scores_changes_under.append(
            np.abs(scores_shuffled_under - base_score[1]))

    feature_importances_over = np.mean(scores_changes_over, axis=0)
    feature_importances_under = np.mean(scores_changes_under, axis=0)

    list_names = np.array(["Sexo", "Peso_nacimiento", "Semanas_gestacion", "Dx_histologico", "Patologia_extrarrenal", "Antecedentes_familiares", "Edad_diagnostico",
                           "Peso", "Talla", "SC", "IMC", "Urea", "Cr", "HTA", "PAS", "PAD", "Hto", "Hb", "Ferritina", "Indice_transferrina", "Ca", "P", "PTH",
                           "X25.OH", "Na", "K", "HCO3", "BUN", "Estatinas", "Quelantes_calcio", "Quelantes_libres_calcio", "Vitamina_D3", "dihidroxivintamina",
                           "hidroxivitamina", "otra_vitamina", "Fe", "EPO", "Bicarbonato", "Resinas_intercambio", "Cloruro_Na", "HGH", "Acido_folico",
                           "Alopurinol", "Sonda_nasogastrica", "Suplementos_nutricionales", "Fluorhidrocortisona", "hipotensor_IECA_ARA", "IECA", "ARA",
                           "Aporte_calcio", "Tiempo_evolucion", "Edad_visita", "Raza", "Grupo_diagnostico", "Proteinuria", "Microalbuminuria", "Filtrado_glomerular"])

    return list_names, feature_importances_over, feature_importances_under


def get_score_importances2(data,
                           model,
                           n_iter=10,
                           random_state=None
                           ):
    """
    data: is a pandas dataframe
    n_iter: iterations of the basic algorithm is done, each iteration
    starting from a different random seed.
    model: model to be used in prediction
    Return (feature_name, importance) array of feature importance
    """

    rng = check_random_state(random_state)

    base_score = score_func(data, model)

    # score_decreases is a list of length n_iter with feature importance arrays
    # (each array is of shape n_features)
    scores_decreases = []

    for i in range(n_iter):

        scores_shuffled = np.array(
            [score_func(data_shuffled, model) for data_shuffled in iter_shuffled(data, random_state=rng)])

        scores_decreases.append(-scores_shuffled + base_score)

    feature_importances = np.mean(scores_decreases, axis=0)

    list_names = np.array(["Sexo", "Peso_nacimiento", "Semanas_gestacion", "Dx_histologico", "Patologia_extrarrenal", "Antecedentes_familiares", "Edad_diagnostico",
                           "Peso", "Talla", "SC", "IMC", "Urea", "Cr", "HTA", "PAS", "PAD", "Hto", "Hb", "Ferritina", "Indice_transferrina", "Ca", "P", "PTH",
                           "X25.OH", "Na", "K", "HCO3", "BUN", "Estatinas", "Quelantes_calcio", "Quelantes_libres_calcio", "Vitamina_D3", "dihidroxivintamina",
                           "hidroxivitamina", "otra_vitamina", "Fe", "EPO", "Bicarbonato", "Resinas_intercambio", "Cloruro_Na", "HGH", "Acido_folico",
                           "Alopurinol", "Sonda_nasogastrica", "Suplementos_nutricionales", "Fluorhidrocortisona", "hipotensor_IECA_ARA", "IECA", "ARA",
                           "Aporte_calcio", "Tiempo_evolucion", "Edad_visita", "Raza", "Grupo_diagnostico", "Proteinuria", "Microalbuminuria", "Filtrado_glomerular"])

    return list_names, feature_importances


def loadModel(path_name, latent_dim):

    try:
        print(path_name)
        # Restore the model and construct the encoder and decoder.
        model = load_model(path_name)

        # Create encoder model
        encoder_inputs = model.input[0]   # input_encoder
        # encoder_layer
        encoder_outputs, state_h_enc, state_c_enc = model.layers[2].output
        encoder_states = [state_h_enc, state_c_enc]

        encoder = Model(encoder_inputs, encoder_states)

        # Create decoder model
        decoder_inputs = model.input[1]   # input_decoder
        decoder_state_input_h = Input(
            shape=(latent_dim,), name='decoder_input_h')
        decoder_state_input_c = Input(
            shape=(latent_dim,), name='decoder_input_c')
        decoder_states_inputs = [decoder_state_input_h, decoder_state_input_c]

        decoder_lstm = model.layers[3]  # decoder_layer
        decoder_outputs, state_h_dec, state_c_dec = decoder_lstm(
            decoder_inputs, initial_state=decoder_states_inputs)

        decoder_states = [state_h_dec, state_c_dec]

        decoder_dense = model.layers[4]  # Dense layer

        decoder_outputs = decoder_dense(decoder_outputs)

        decoder = Model([decoder_inputs] + decoder_states_inputs,
                        [decoder_outputs] + decoder_states)

        print("Model loaded")
        return encoder, decoder

    except IOError as errorMessage:
        print("The model does not exit or it could not be loaded")
        print(errorMessage)
        return None, None


def predict_FGs(encoder_model, decoder_model, input_data, last_known_FG, maxi=10):

    # Encode the input as state vectors.
    states_value = encoder_model.predict(input_data)

    # Generate empty target sequence of length 1.
    target_seq = np.zeros((1, 1, 1))

    # Populate the first FG of target sequence with the last known FG.
    target_seq[0, 0, 0] = last_known_FG

    predicted_values = np.empty((maxi))

    i = 0

    while i < maxi:

        output_tokens, h, c = decoder_model.predict(
            [target_seq] + states_value)

        predicted = output_tokens.ravel()[0]

        predicted_values[i] = predicted

        # Update the target sequence.
        target_seq = np.zeros((1, 1, 1))
        target_seq[0, 0, 0] = predicted

        # Update states
        states_value = [h, c]

        i += 1

    return predicted_values


def preprocess_data(dataset, columns_nominal, columns_binary):

    # Delete some unuseful columns
    #########################################################
    dataset.drop(columns=["Lugar_nacimiento",
                          "Lugar_residencia"], axis=1, inplace=True)

    # Replace some values for shorten
    dataset["Grupo_diagnostico"].replace({"Enfermedades sistémicas que afectan al riñón": "ESAR", "Enfermedades tubulo-intersticiales": "ETI",
                                          "Trastornos renales diversos": "TRD", "Nefropatías hereditarias / familiares": "NHF", "Enfermedades glomerulares": "EG"}, inplace=True)

    # All nominal atts with only two possible values are converted to binary var
    for binary_var in columns_binary:
        dataset[binary_var] = dataset[binary_var].apply(
            {'Hombre': 1, 'Mujer': 0, "No": 0, "Sí": 1}.get)

    new_columns = ["Id", "Sexo", "Peso_nacimiento", "Semanas_gestacion", "Dx_histologico", "Patologia_extrarrenal", "Antecedentes_familiares",
                   "Edad_diagnostico", "Peso", "Talla", "SC", "IMC", "Urea", "Cr", "HTA", "PAS", "PAD", "Hto", "Hb", "Ferritina", "Indice_transferrina",
                   "Ca", "P", "PTH", "X25.OH", "Na", "K", "HCO3", "BUN", "Estatinas", "Quelantes_calcio", "Quelantes_libres_calcio", "Vitamina_D3",
                   "dihidroxivintamina", "hidroxivitamina", "otra_vitamina", "Fe", "EPO", "Bicarbonato", "Resinas_intercambio",
                   "Cloruro_Na", "HGH", "Acido_folico", "Alopurinol", "Sonda_nasogastrica", "Suplementos_nutricionales",
                   "Fluorhidrocortisona", "hipotensor_IECA_ARA", "IECA", "ARA", "Aporte_calcio", "Tiempo_evolucion", "Edad_visita",
                   "Raza_Caucasiana", "Raza_Hispana", "Raza_Negra", "Raza_Oriental", "Raza_Otra", "Raza_Árabe", "Grupo_diagnostico_EG",
                   "Grupo_diagnostico_ESAR", "Grupo_diagnostico_ETI", "Grupo_diagnostico_NHF", "Grupo_diagnostico_TRD", "Proteinuria_No realizada", "Proteinuria_Normal", "Proteinuria_Patológica", "Microalbuminuria_No realizada", "Microalbuminuria_Normal", "Microalbuminuria_Patológica", "Filtrado_glomerular"]

    df = pd.DataFrame(columns=new_columns)
    newData = []

    for index, row in dataset.iterrows():

        dictio = {}

        for col in new_columns:

            if "Raza" in col:

                for raza in ["Raza_Caucasiana", "Raza_Hispana", "Raza_Negra", "Raza_Oriental", "Raza_Otra", "Raza_Árabe"]:

                    if row["Raza"] in raza:
                        dictio[raza] = 1
                    else:
                        dictio[raza] = 0

                continue

            if "Grupo_diagnostico" in col:

                for grupo in ["Grupo_diagnostico_EG", "Grupo_diagnostico_ESAR", "Grupo_diagnostico_ETI", "Grupo_diagnostico_NHF", "Grupo_diagnostico_TRD"]:

                    if row["Grupo_diagnostico"] in grupo:
                        dictio[grupo] = 1
                    else:
                        dictio[grupo] = 0

                continue

            if "Proteinuria" in col:

                for prote in ["Proteinuria_No realizada", "Proteinuria_Normal", "Proteinuria_Patológica"]:

                    if row["Proteinuria"] in prote:
                        dictio[prote] = 1
                    else:
                        dictio[prote] = 0

                continue

            if "Microalbuminuria" in col:

                for micro in ["Microalbuminuria_No realizada", "Microalbuminuria_Normal", "Microalbuminuria_Patológica"]:

                    if row["Microalbuminuria"] in micro:
                        dictio[micro] = 1
                    else:
                        dictio[micro] = 0

                continue

            dictio[col] = row[col]

        newData.append(dictio)

    df = df.append(newData)

    return df


def missing_imputation_col(dataset, col, dataset_original, longitudinal_var=False):

    # Esta funcion implementa la imputacion mean_average _before_after_values
    count_nan = len(dataset) - dataset.iloc[:, col].count()

    column_names = dataset.columns.values

    datasetN = dataset.copy()

    print("Number of missing values before imputation: ", count_nan)

    if count_nan == 0:

        print("Nothing to do...")

        return datasetN.iloc[:, col]

    if not longitudinal_var:

        valuesP = datasetN.iloc[:, col]

        if count_nan == len(dataset):

                valuesP = dataset_original.iloc[:, col]

        if datasetN.dtypes[col] == np.int or datasetN.dtypes[col] == np.float:

            datasetN.iloc[:, col] = datasetN.iloc[:, col].fillna(valuesP.mean())

        if dataset.dtypes[col] == object:

            datasetN.iloc[:, col] = datasetN.iloc[:, col].fillna(valuesP.mode()[0])

    else:
        # For each patient
        for index, rows in datasetN.groupby("Id"):

            values = rows.iloc[:, col]

            count_nan = len(rows) - values.count()

            valuesP = values

            # Extreme case. All patient's values are NA. In this case, all the population is taken.
            if count_nan == len(rows):

                valuesP = dataset_original.iloc[:, col]

            if datasetN.dtypes[col] == np.int or datasetN.dtypes[col] == np.float:

                values = values.fillna(valuesP.mean())

            if dataset.dtypes[col] == object:

                valuesT = valuesP.mode()

                if len(valuesT) > 1 and count_nan < len(rows):
                    # En el caso de que valueT tenga mas de un valor, esto puede ocurrir solamente con la moda,
                    # se va asignar la ultima moda conocida antes del missing, o la primer moda conocida despues del missing

                    indexesNaN = np.where(values.isnull().values)[0]

                    knownIndexesMode = np.where(
                        np.isin(values.values, valuesT))[0]

                    # For each nan value
                    for index in indexesNaN:

                        lowerIndexes = np.where(index > knownIndexesMode)[0]
                        higherIndexes = np.where(index < knownIndexesMode)[0]

                        if len(lowerIndexes) > 0:
                            values[index] = values[knownIndexesMode[lowerIndexes[-1]]]
                        else:
                            values[index] = values[knownIndexesMode[higherIndexes[0]]]
                else:

                    values = values.fillna(valuesT[0])

            # Updating
            datasetN.loc[datasetN.Id == rows.Id[0], column_names[col]] = values

    # Checking the imputation
    count_nan = len(datasetN) - datasetN.iloc[:, col].count()

    print("Number of missing values after imputation: ", count_nan)

    if count_nan != 0:
        print("The imputation was not completed in var ", column_names[col])

    return datasetN.iloc[:, col]


def missing_imputation(dataset, dataset_original):

    cols = dataset.shape[1]

    column_names = dataset.columns.values

    longitudinal = False

    for c in range(cols):

        # Los valores omitidos para las fechas y variables que se calculan a partir de otras no se calculan en este momento
        if(column_names[c].startswith(("Id", "IMC", "SC", "BUN", "Filtrado_glomerular"))):

            print("Jumping ", column_names[c])
            continue

        if c > 11:

            print("Longitudinal var", column_names[c])
            longitudinal = True

        else:

            print("Global var", column_names[c])

        dataset.iloc[:, c] = missing_imputation_col(dataset, c, dataset_original, longitudinal)

    # Ahora se estiman los valores de variables dependientes

    # Computing IMC

    indexes = dataset.IMC.isnull()

    if indexes.sum() > 0:

        values = dataset.loc[indexes, ["Talla", "Peso"]]

        dataset.loc[indexes, "IMC"] = values.Peso * 10000 / (values.Talla ** 2)

    # Computing SC

    indexes = dataset.SC.isnull()

    if indexes.sum() > 0:

        values = dataset.loc[indexes, ["Talla", "Peso"]]

        dataset.loc[indexes, "SC"] = np.sqrt((values.Peso * values.Talla/3600))

    # Computing BUN

    indexes = dataset.BUN.isnull()

    if indexes.sum() > 0:

        values = dataset.loc[indexes, "Urea"]

        dataset.loc[indexes, "BUN"] = values / 2.1428

    # Checking
    num_nans = dataset.size - dataset.count().sum()

    if num_nans == 0:
        print("Completed imputation process")
    else:
        print("Imputation process incompleted")

    # Faltaria por imputar el Filtrado_glomerular, pero como este lo normal es que no este vacio y se calcula automaticamente por el sistema entonces es de esperar que no llegue vacio, de lo contrario son un monton de formulas a tener en cuenta.
