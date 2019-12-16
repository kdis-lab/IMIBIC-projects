# Forward cross val para comparar con el estado del arte
import pandas as pd
from utils import *

columns_numeric = pd.read_csv(
    "datasets/original_columns_numeric.csv"
)["Column"].values

output_folder = "model-results/"

model = Model_LSTM(columns_numeric, output_folder)

#m, s = model.cross_validatev2()
#print("RMSE:", m, "+-" , s)

test_loss = model.cross_validatev2()

print("RESULTS")

for i in test_loss:

    print(i)