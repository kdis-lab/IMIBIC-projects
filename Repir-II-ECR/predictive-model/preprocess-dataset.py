import pandas as pd
from sklearn.preprocessing import StandardScaler

name_dataset = "datos-demograficos-visitas-tratamientos"

imputation_methods = ["mean-average_before_after_values", "mean-average_last_next_values", "mean-average_previous_values",
                      "mean-locf", "mean-nocb", "median-average_before_after_values", "median-average_last_next_values", "median-average_previous_values",
                      "median-locf", "median-nocb"]

columns_nominal = pd.read_csv(
    "datasets/original_columns_nominal.csv"
)["Column"].values

columns_binary = pd.read_csv(
    "datasets/original_columns_binary.csv"
)["Column"].values

for imputation_method in imputation_methods:

    # Load data
    #########################################################

    dataset = pd.read_csv(
        "datasets/datos-demograficos-visitas-tratamientos-missing-imputation-" +
        imputation_method + ".csv"
    )

    # Delete some unuseful columns
    #########################################################
    dataset.drop(columns=["Lugar_nacimiento",
                          "Lugar_residencia"], axis=1, inplace=True)

    # Replace some values for shorten
    dataset["Grupo_diagnostico"].replace({"Enfermedades sistémicas que afectan al riñón": "ESAR", "Enfermedades tubulo-intersticiales": "ETI",
                                          "Trastornos renales diversos": "TRD", "Nefropatías hereditarias / familiares": "NHF", "Enfermedades glomerulares": "EG"}, inplace=True)

    # All nominal attributes are converted to dummy vars
    ##########################################################
    dataset = pd.get_dummies(
        dataset,
        columns=columns_nominal,
        drop_first=False)

    # All nominal atts with only two possible values are converted to binary var
    for binary_var in columns_binary:
        dataset[binary_var] = dataset[binary_var].apply(
            {'Hombre': 1, 'Mujer': 0, "No": 0, "Sí": 1}.get)

    # The column Filtrado_glomerular is moved to the end of the dataset, since
    # the columns are desrodered after converting vars
    ####################################################################

    list_columns = dataset.columns.tolist()

    pos = list_columns.index("Filtrado_glomerular")

    newList = list_columns[0:pos] + list_columns[pos+1:] + [list_columns[pos]]

    # Reorder columns
    dataset = dataset[newList]

    dataset.to_csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-" +
                   imputation_method + "-allnumerics.csv", index=False)