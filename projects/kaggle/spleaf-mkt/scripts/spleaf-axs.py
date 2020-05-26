__author__ = 'Amit Sharma'

import pandas as pd
import numpy as np
import os
import glob
from sklearn import cross_validation, ensemble, preprocessing, metrics
from sklearn.metrics import roc_curve, auc
from sklearn.grid_search import GridSearchCV
import collections
import matplotlib.pyplot as plt


import matplotlib

matplotlib.style.use('ggplot')

# Prints the feature importance sorted descending for a model
def featureImp(model, df, n=10):
    index = np.argsort(model.feature_importances_)[::-1]
    print df.columns[index].values[:n] + ':' + model.feature_importances_[index].astype(str)[:n]

# Print Grid Search results
def getBestModel(model, param_grid):
    print("Best score: %0.3f" % model.best_score_)
    print("Best parameters set:")
    best_parameters = model.best_estimator_.get_params()
    for param_name in sorted(param_grid.keys()):
        print("\t%s: %r" % (param_name, best_parameters[param_name]))
    return model.best_estimator_

# Returns true for encoded columns
def isEncodedCol(df, col):
    encodedCols = []
    encode = (df[col].dtype != 'int64') & (df[col].dtype != 'float64')
    encode = encode | (col in encodedCols)
    return encode

# Encode columns to categorical values
def encodeCols(df):
    print "Encoding cols..."
    # Encode categorical cols
    for col in df.columns:
        if (isEncodedCol(df, col)):
            lbl = preprocessing.LabelEncoder()
            lbl.fit(df[col])
            df[col] = lbl.transform(df[col])
    return df

# Create train/test split files for internal benchmarking
def createCVFiles(train, targetCol, testSize=0.2):
    # Split train test data sets
    trainX, testX, trainY, testY = cross_validation.train_test_split( \
        train, train[targetCol], test_size=testSize, random_state=0)
    trainX.to_csv("trainX.csv", index=False)
    testX.drop([targetCol], axis=1).to_csv("testX.csv", index=False)
    testY.to_csv("testY.csv", index=False)

def imputeMissingVals(df):
    # Impute missing values
    for impCol in df.columns:
        if (df[impCol].dtype == 'int64') or (df[impCol].dtype == 'float64'):
            #imputedVal = df[impCol].value_counts().index[0]
            #df[impCol] = df[impCol].fillna(imputedVal)
            df[impCol] = df[impCol].fillna(0)
        elif df[impCol].dtype == 'object':
            #imputedVal = df[impCol].value_counts().index[0]
            #df[impCol] = df[impCol].fillna(imputedVal)
            df[impCol] = df[impCol].fillna('NAValue')
    return df

def preProcess(df):
    df = imputeMissingVals(df)
    df = encodeCols(df)
    return df

if __name__ == '__main__':

    forSubmission = False

    os.chdir("/home/devel/axs/work/kaggle/spleaf-mkt/scripts")

    # load trainAlling and test datasets
    train = pd.read_csv('../input/train10K.csv')
    train.index = train.ID

    if (forSubmission):
        # Load the test dat
        test = pd.read_csv('../input/testMini.csv', index_col='ID')
        test['target'] = 0

        # Add the test data for pre processing (factor leveling)
        trainTest = pd.concat([train, test])
        trainTest = preProcess(trainTest)

        # Scoop out the train data back
        train = trainTest[:train.shape[0]]
        test = trainTest[train.shape[0]:]
        print(train.shape, test.shape)
    else:
        train = preProcess(train)

    yAll = train['target']
    train = train.drop(['target'], axis=1)

    # Split train test data sets
    trainX, testX, trainY, testY = cross_validation.train_test_split( \
        train, yAll, test_size=0.2, random_state=0)

    if forSubmission:
        trainX, trainY = train, yAll
        testX = test

    #Define scorer
    #rmsle_scorer = metrics.make_scorer(rmsle, greater_is_better=False)

    print "Running model..."
    from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
    #model = RandomForestClassifier(n_estimators=500, max_features=None, max_depth=None).fit(trainX, trainY)
    gbmParams = {'n_estimators': 1200, 'max_depth': 3, 'subsample': 0.5,
          'learning_rate': 0.01, 'min_samples_leaf': 1, 'random_state': 3}
    model = GradientBoostingClassifier(**gbmParams).fit(trainX, trainY)

    print "...Completed!"

    preds = model.predict(testX)

    if forSubmission:
        print "Creating submission file..."
        # Create your first submission file
        submission = pd.DataFrame({"id": testX.index, "cost": preds})
        submission.to_csv("cat-tube-axs-results.csv", columns=['id', 'cost'], index=False)
    else:
        fpr, tpr, thresholds = metrics.roc_curve(testY, preds, pos_label=1)
        print "AUC Score: ", metrics.auc(fpr, tpr)


    print "Done!"



