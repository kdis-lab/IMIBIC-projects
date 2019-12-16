import numpy as np
from matplotlib import pyplot

imputation_methods = ["mean-average_before_after_values", "mean-average_last_next_values", "mean-average_previous_values",
                      "mean-locf", "mean-nocb", "median-average_before_after_values", "median-average_last_next_values", "median-average_previous_values",
                      "median-locf", "median-nocb"]

output_path = "model-results/"

for imputation_method in imputation_methods:

    # Load the val_loss
    loss = np.load(output_path + imputation_method + "-Loss-CV.npy")
    val_loss = np.load(output_path + imputation_method + "-Val-Loss-CV.npy")

    print(imputation_method)

    print("Numpy array loaded")

    train_scores_mean = np.mean(loss, axis=0)
    train_scores_std = np.std(loss, axis=0)
    test_scores_mean = np.mean(val_loss, axis=0)
    test_scores_std = np.std(val_loss, axis=0)

    pyplot.figure()
    pyplot.plot(train_scores_mean, label='train')
    pyplot.plot(test_scores_mean, label='test')

    #pyplot.fill_between(list(range(0, loss.shape[1])), train_scores_mean - train_scores_std,
    #                    train_scores_mean + train_scores_std, alpha=0.1, color="b")

    #pyplot.fill_between(list(range(0, loss.shape[1])), test_scores_mean - test_scores_std,
    #                    test_scores_mean + test_scores_std, alpha=0.1, color="r")

    pyplot.legend()
    pyplot.savefig(output_path + imputation_method + "-CV-lossv2.svg", format="svg")