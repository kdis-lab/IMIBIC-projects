import numpy as np

imputation_methods = ["mean-average_before_after_values", "mean-average_last_next_values", "mean-average_previous_values",
                      "mean-locf", "mean-nocb", "median-average_before_after_values", "median-average_last_next_values", "median-average_previous_values",
                      "median-locf", "median-nocb"]

output_path = "model-results/"

for imputation_method in imputation_methods:

    # Load the val_loss
    loss = np.load(output_path + imputation_method + "-Loss-CV.npy")
    val_loss = np.load(output_path + imputation_method + "-Val-Loss-CV.npy")

    print(imputation_method)

    train_scores_mean = np.mean(loss, axis=0)
    train_scores_std = np.std(loss, axis=0)
    test_scores_mean = np.mean(val_loss, axis=0)
    test_scores_std = np.std(val_loss, axis=0)

    print(train_scores_mean.mean(), train_scores_std.mean())
    print(test_scores_mean.mean(), test_scores_std.mean())

    print((train_scores_mean.mean() + test_scores_mean.mean())/2, (train_scores_std.mean() + test_scores_std.mean())/2)

    print("*" * 50)