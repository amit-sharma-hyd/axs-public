__author__ = 'sharmaam'

import pandas as pd
import numpy as np
import os
import glob
from sklearn import cross_validation, ensemble, preprocessing, metrics
from sklearn.grid_search import GridSearchCV
import collections
import matplotlib.pyplot as plt
from sklearn.cross_validation import train_test_split

import matplotlib

matplotlib.style.use('ggplot')

# Prints the feature importance sorted descending for a model
def featureImp(model, df, n=10):
    index = np.argsort(model.feature_importances_)[::-1]
    print df.columns[index].values[:n] + ':' + model.feature_importances_[index].astype(str)[:n]


# Percentage Root Mean Squared Error (RMSPE)
def rmspe(pred, actual):
    sse = np.sum(pow((pred-actual)/pred, 2))
    sseMean = sse / len(pred)
    rmspe = np.sqrt(sseMean)
    return rmspe


# Print Grid Search results
def getBestModel(model):
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


def preProcess(df):

    # Remove columns not being handled currently
    df = df.drop(['Customers'], axis=1)

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

    '''
    print "Cleaning too many categories..."
    ### Clean variables with too many categories
    for col in df.columns:
        if (df[col].dtype != 'int64') & (df[col].dtype != 'float64'):
            top30 = np.array(collections.Counter(df[col]).most_common(60))[:, 0]
            notInTop30 = [(not x in top30) for x in df[col]]
            df.ix[notInTop30, col] = 'rareValue'

    '''

    df = encodeCols(df)

    return df

if __name__ == '__main__':

    forSubmission = False
    gbr = False

    os.chdir("/home/devel/axs/work/kaggle/rossmann/scripts")

    # load trainAlling and test datasets
    trainAll = pd.read_csv('../input/train.csv', parse_dates=[2, ])

    # Take a random split for temp training purpose
    trainAll, dummy = train_test_split(trainAll, test_size = 0.5)


    yAll = trainAll.Sales

    trainAll['Month'] = trainAll.Date.dt.month

    grpByCust = trainAll.groupby(["Store", "Month"]).mean()['Customers']
    grpByCust.name = 'Avg_Cust'
    trainAll = trainAll.join(grpByCust, on=['Store', 'Month'])

    stores = pd.read_csv('../input/store.csv')
    trainAll = trainAll.merge(stores, on='Store')

    trainAll = preProcess(trainAll)

    #Split train-test data
    train, test = train_test_split(trainAll, test_size = 0.2)

    testY = test.Sales
    trainY = train.Sales
    trainX = train.drop(['Sales', 'PromoInterval'], axis=1)
    testX = test.drop(['Sales', 'PromoInterval'], axis=1)
    model = ensemble.RandomForestRegressor(n_estimators=100, criterion='mse', oob_score=True, max_features=None, max_depth=None).fit(trainX, trainY)
    print "...Completed!"

    # Grid based turning
    # from sklearn import linear_model
    # model = linear_model.LinearRegression(fit_intercept=True, normalize=True, copy_X=True).fit(trainX, trainY)
    # Alternative model
    # rfModel = ensemble.RandomForestRegressor(criterion='mse')
    # param_grid = dict(max_features=[40,50,60,70,80,90])
    # model = GridSearchCV(rfModel, param_grid=param_grid, scoring=rmsle_scorer, verbose=10).fit(trainX, trainY)
    # model = getBestModel(model)


    preds = model.predict(testX)

    if forSubmission:
        print "Creating submission file..."
        # Create your first submission file
        submission = pd.DataFrame({"id": testXId, "cost": preds})
        submission.to_csv("cat-tube-axs-results.csv", columns=['id', 'cost'], index=False)
    else:
        print "PRMSE Score: ", rmspe(preds, testY)

    print "Done!"
