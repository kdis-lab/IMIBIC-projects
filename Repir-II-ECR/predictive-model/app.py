# https://towardsdatascience.com/deploying-keras-models-using-tensorflow-serving-and-flask-508ba00f1037
# https://stackoverflow.com/questions/10434599/how-to-get-data-received-in-flask-request

import base64
import json
from io import BytesIO

from tensorflow import keras
import tensorflow as tf

import numpy as np
import requests
from flask import Flask, flash, request, jsonify
import numpy as np
from sklearn.preprocessing import StandardScaler
from utils import *

app = Flask(__name__)

session = tf.Session()

keras.backend.set_session(session)

outputFolder= "model-results/"
imputation_method= "mean-average_before_after_values"
latent_dim = 64
modelName = "model.hdf5"

encoder_model = None
decoder_model = None

# This is used to get the original values of filtrado glomerular
columns_numeric = pd.read_csv(
    "datasets/original_columns_numeric.csv")["Column"].values

columns_binary = pd.read_csv(
    "datasets/original_columns_binary.csv")["Column"].values

columns_nominal = pd.read_csv(
        "datasets/original_columns_nominal.csv")["Column"].values

dtypes={c:np.float64 for c in columns_numeric}

dtypes.update({c:str for c in columns_binary})

dtypes.update({c:str for c in columns_nominal})
           
trainingset_original = pd.read_csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-{method}.csv".format(
            method=imputation_method), dtype= dtypes)
    
scalerFG = StandardScaler()
scalerFG.fit(trainingset_original["Filtrado_glomerular"].values.reshape(-1,1))

scalerAllVars = StandardScaler()
scalerAllVars.fit(trainingset_original.loc[:, columns_numeric])

# Testing URL
@app.route('/hello/', methods=['GET', 'POST'])
def hello_world():
    return 'Hello, World!'

# predict
@app.route('/predict_values/', methods=['GET', 'POST'])
def predict():

    try:
        # Use the last nVisits to predict nFG values to the future
        nVisits = int(request.form.get("nvisits", 1))
        nFG= int(request.form.get("nFG", 1))

        print(nVisits)
        print(nFG)

        global session

        with session.as_default():
            with session.graph.as_default():
    
                if request.method == "POST":

                    # check if the post request has the file part
                    if 'file' not in request.files:
                        return 'No file part'
                    
                    fileInput = request.files['file']

                    # if user does not select file, browser also
                    # submit an empty part without filename
                    if fileInput.filename == '':
                        return 'No selected file'
            
                global encoder_model
                global decoder_model

                if encoder_model is None and decoder_model is None:
                    encoder_model, decoder_model= loadModel(outputFolder + modelName, latent_dim)
                
                input_data = pd.read_csv(fileInput, dtype= dtypes)

                # First conduct the imputation process
                missing_imputation(input_data, trainingset_original)

                # Transform nominal and binary vars
                input_data = preprocess_data(input_data, columns_nominal, columns_binary)

                # Apply standarization/normalization
                input_data.loc[:, columns_numeric] = scalerAllVars.transform(input_data.loc[:, columns_numeric].values)

                visits, lastFG = transform_data_to_predict(input_data, nVisits)
                
                 # Encode the input as state vectors.
                states_value = encoder_model.predict(visits)
                    
                # Generate empty target sequence of length 1.
                target_seq = np.zeros((1, 1, 1))
                
                # Populate the first FG of target sequence with the last known FG.
                target_seq[0, 0, 0] = lastFG
                
                predicted_values = np.empty((nFG))
                
                i= 0
                
                while i < nFG:
                    
                    output_tokens, h, c = decoder_model.predict(
                        [target_seq] + states_value)

                    predicted = output_tokens.ravel()[0]
                    
                    predicted_values[i] = predicted

                    # Update the target sequence.
                    target_seq = np.zeros((1, 1, 1))
                    target_seq[0, 0, 0] = predicted

                    # Update states
                    states_value = [h, c]
                    
                    i+=1

        return jsonify(scalerFG.inverse_transform(predicted_values).tolist())
    
    except Exception as ex:
        
        print(ex, "Ahhh")
        return "Prediction error"