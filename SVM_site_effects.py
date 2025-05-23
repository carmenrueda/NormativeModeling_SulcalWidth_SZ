import os
import pandas as pd
import numpy as np
from sklearn.svm import SVC
from sklearn.model_selection import cross_val_score, KFold
from sklearn.metrics import make_scorer, balanced_accuracy_score
from sklearn.preprocessing import StandardScaler
from google.colab import drive

# Mount Google Drive
drive.mount('/content/drive')

"""# **Load z-scores data**"""

# Load z-scores from training and test data
train_z = pd.read_csv('/content/drive/MyDrive/Carmen_Rueda/Width_Thickness_Adjusted/80clinical/Zscores/train_width_zscores.csv', header=0)
test_z = pd.read_csv('/content/drive/MyDrive/Carmen_Rueda/Width_Thickness_Adjusted/80clinical/Zscores/test_width_zscores.csv', header=0)

# Combine both to allow evaluation of train and test data using the same structure
train_test_join = pd.concat([train_z, test_z])

z_scores_rom_train = train_test_join
z_scores_rom_test = train_test_join

# Print shape for verification
print('Train:', z_scores_rom_train.shape)
print('Test:', z_scores_rom_test.shape)

"""# **Utility functions**"""

def read_data(train_path: pd.DataFrame, test_path: pd.DataFrame):
    """
    Reads and preprocesses training and test dataframes.

    Drops non-feature columns and returns cleaned dataframes.
    """
    train = train_path.drop(columns=["ID", "dcode", "scanner"])
    test = test_path.drop(columns=["ID", "dcode", "scanner"])
    return train, test

def generate_site_data_labels(data: pd.DataFrame):
    """
    Generates one-hot label lists for each site and standardizes features.

    Returns:
        - train_data: standardized feature matrix (numpy array)
        - label_list: list of binary label arrays (per site)
        - label_names: site names
    """
    label_list = []
    label_names = []

    for site in data["site"].unique():
        labels = data["site"] == site
        label_list.append(labels.astype(int).to_list())
        label_names.append(site)

    # Drop site column and standardize features
    data.drop(columns=["site"], inplace=True)
    scaler = StandardScaler()
    train_data = scaler.fit_transform(data)

    return train_data, label_list, label_names

"""# **Main script**"""

def main():
    """
    Main function to evaluate site predictability via SVM.

    It performs independent SVM classification for each site in both train and test data
    and prints average balanced accuracy using 2-fold cross-validation.
    """
    # Load and preprocess data
    train_df, test_df = read_data(z_scores_rom_train, z_scores_rom_test)
    train_data, train_label_list, train_label_names = generate_site_data_labels(train_df)
    test_data, test_label_list, test_label_names = generate_site_data_labels(test_df)

    # Define balanced accuracy scorer
    balanced_accuracy = make_scorer(balanced_accuracy_score)

    # Evaluate train data
    train_res_dict = {}
    for train_labels, label_name in zip(train_label_list, train_label_names):
        print(f"Training set number of samples for {label_name}: {sum(train_labels)}")
        scores = cross_val_score(SVC(kernel='linear', class_weight='balanced'),
                                 train_data,
                                 train_labels,
                                 cv=KFold(shuffle=True, n_splits=2),
                                 scoring=balanced_accuracy)
        train_res_dict[label_name] = scores.mean()

    # Evaluate test data
    test_res_dict = {}
    for test_labels, label_name in zip(test_label_list, test_label_names):
        print(f"Test set number of samples for {label_name}: {sum(test_labels)}")
        scores = cross_val_score(SVC(kernel='linear', class_weight='balanced'),
                                 test_data,
                                 test_labels,
                                 cv=KFold(shuffle=True, n_splits=2),
                                 scoring=balanced_accuracy)
        test_res_dict[label_name] = scores.mean()

    # Print results
    print("\nTrain Results:")
    for label_name, score in train_res_dict.items():
        print(f"{label_name}: {score:.4f}")

    print("\nTest Results:")
    for label_name, score in test_res_dict.items():
        print(f"{label_name}: {score:.4f}")

# Run main function
if __name__ == '__main__':
    main()
