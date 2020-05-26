__author__ = 'sharmaam'

import pandas as pd
import numpy as np
import os
import glob
from sklearn import cross_validation, ensemble, preprocessing, metrics
from sklearn.grid_search import GridSearchCV
import collections
import matplotlib.pyplot as plt

import matplotlib

matplotlib.style.use('ggplot')

# Prints the feature importance sorted descending for a model
def featureImp(model, df, n=10):
    index = np.argsort(model.feature_importances_)[::-1]
    print df.columns[index].values[:n] + ':' + model.feature_importances_[index].astype(str)[:n]


# Root Mean Squared Logarithmic Error (RMSLE)
def rmsle(pred, actual):
    sle = np.sum(pow(np.log(pred + 1) - np.log(actual + 1), 2))
    sleMean = sle / len(pred)
    rmsle = np.sqrt(sleMean)
    return rmsle


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
    encodedCols = ['year', 'dayofweek']
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

def countSpecs(df):
    specCounter = np.zeros(df.spec1.size)
    for i in range(1, 11):
        arr = [0 if (pd.isnull(x)) else 1 for x in df['spec' + str(i)]]
        specCounter = specCounter + arr
    df['numSpecs'] = np.array(specCounter)
    return df

def countComponents(df):
    compCounter = np.zeros(df.spec1.size)
    for i in range(1, 9):
        arr = [0 if (pd.isnull(x)) else 1 for x in df['quantity_' + str(i)]]
        compCounter = compCounter + arr
    df['numComponents'] = np.array(compCounter)
    return df



def preProcess(df):
    '''
    # Bin quantity
    df['qtyGrp'] = pd.cut(df.quantity, bins=100, labels=range(100))
    df['qtyGrpSmall'] = [x if x <= 50 else 99 for x in df.quantity]

    df['auGrp'] = pd.cut(df.annual_usage, bins=100, labels=range(100))
    #df['auGrpSmall'] = [x if x <= 10 else 99 for x in df.quantity]

    df['logQty'] = np.log(df.quantity+1)
    '''

    # create some new features
    df['year'] = df.quote_date.dt.year.astype(int)

    # Temp test to see pred quality at year level
    #df = df[(df.year == 2011) | (df.year == 2012)| (df.year == 2013)| (df.year == 2014)]
    #df = df[(df.year == 2013)]

    df['month'] = df.quote_date.dt.month.astype(int)
    df['dayofyear'] = df.quote_date.dt.dayofyear.astype(int)
    df['dayofweek'] = df.quote_date.dt.dayofweek.astype(int)
    df['day'] = df.quote_date.dt.day.astype(int)

    df['weekNo'] = pd.cut(df.dayofyear, bins=52, labels=range(52))
    # df['minOrderQtyGrp'] = pd.cut(df.min_order_quantity, bins=[-10, 1, 2, 1000])
    df['surplusQty'] = df.quantity - df.min_order_quantity

    #df['yearsAgo'] = np.exp(1/(2018-df['year']))
    df['yearsAgo'] = 2018 - df['year']

    print 'Initial shape df.shape: ', df.shape
    # Merge with tube data
    fileDF = pd.read_csv('../input/tube.csv')
    df = pd.merge(df, fileDF, on='tube_assembly_id', how='left')

    print 'After merge with tube df.shape: ', df.shape

    fileDF = pd.read_csv('../input/bill_of_materials.csv')
    df = pd.merge(df, fileDF, on='tube_assembly_id', how='left')

    print "After merging with BOM df.shape: ", df.shape

    # Add component type
    # Merge with component type
    fileDF = pd.read_csv('../input/components.csv')
    # Create a series for lookup
    compTypeLookup = pd.Series(fileDF.component_type_id.values, index=fileDF.component_id.values)
    for i in range(1, 9):
        colName = 'component_id_' + str(i)
        dfCol = df[colName].copy()
        dfCol[pd.isnull(dfCol)] = '9999'
        df[colName + '_type'] = compTypeLookup[dfCol].values

    # Add features as applicable
    df['innerDia'] = df['diameter'] - df['wall']
    df['vol'] = df['length'] * (df['diameter'] * df['diameter']) - (df['innerDia'] * df['innerDia'])
    df['area'] = df['length'] * df['diameter']
    df['bendDensity'] = df['num_bends'] / (df['length'] + 0.1)


    # Bin some variables
    # df['bendDensityBin'] = pd.qcut(df['bendDensity'], np.arange(0,1.01,0.1))
    #df['invAnnUsage'] = 1 / (df.annual_usage + 0.01)

    # df['annual_usage'] = np.log(df['annual_usage'])
    # df['sq_annual_usage'] = df['annual_usage'] * df['annual_usage']


    # df['volBin'] = pd.cut(df.vol, bins=50, labels=range(50))
    # df['logQty'] = np.log(df.quantity)
    df['sqQty'] = df.quantity * df.quantity
    #df['qtyBin'] = pd.qcut(df.quantity, np.arange(0,1.01,0.25))
    #df = df.drop(['quantity'], axis=1)

    print "Merging data from files..."
    # Merge data from other files
    inpFiles = glob.glob('../input/*.csv')
    #Filter out the comp_ files - we shall merge them later
    inpFiles = [x for x in inpFiles if x.find('comp_') == -1]
    loop = True
    while loop == True:
        loop = False
        for inpFile in inpFiles:
            fileDF = pd.read_csv(inpFile)
            commonCols = list(set(df.columns) & set(fileDF.columns))
            if (len(commonCols) == 1):
                commonCol = commonCols[0]
                if (df[commonCol].dtype != 'int64') & ((df[commonCol].dtype != 'float64')):
                    print 'Merging with file: ', inpFile + ' on col: ' + commonCol
                    df = pd.merge(df, fileDF, on=commonCol, how='left')
                    print 'After merge df.shape: ', df.shape
                    loop = True

    print 'After first level merge :', df.shape

    # Join with Component files
    inpFiles = glob.glob('../input/comp_*.csv')
    for inpFile in inpFiles:
        fileDF = pd.read_csv(inpFile)
        newColNames = [inpFile[inpFile.index('comp_')+5:].replace('.csv', '') + '_' + colName for colName in fileDF.columns]
        print newColNames
        fileDF.columns = newColNames
        for i in range(1,2):
            lKeyColName = 'component_id_' + str(i)
            print 'Merging on component_id with ', inpFile
            compType = inpFile[inpFile.index('comp_')+5:].replace('.csv', '')
            rightKey = compType + '_component_id'
            df = pd.merge(df, fileDF, left_on=lKeyColName, right_on=rightKey, how='left')
            print 'After merge df.shape: ', df.shape

    print 'After component info merge :', df.shape

    df = df.drop(['quote_date'], axis=1)

    # Count number of specs
    df = countSpecs(df)
    df = countComponents(df)

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

    print "Cleaning too many categories..."
    ### Clean variables with too many categories
    for col in df.columns:
        if (df[col].dtype != 'int64') & (df[col].dtype != 'float64'):
            top30 = np.array(collections.Counter(df[col]).most_common(60))[:, 0]
            notInTop30 = [(not x in top30) for x in df[col]]
            df.ix[notInTop30, col] = 'rareValue'

    df = encodeCols(df)

    return df


def getYearWiseResults(preds, testY, years):
    res = []
    for year in years:
        filterIndex = testX[testX.year == year].index
        err = rmsle(preds[filterIndex], testY[filterIndex])
        res.append(err)
    return res


if __name__ == '__main__':

    forSubmission = False
    gbr = False

    #os.chdir("C:\\axs\\work\\kaggle\\cat-tube-pricing\\scripts")

    # load trainAlling and test datasets
    trainAll = pd.read_csv('../input/train_set.csv', parse_dates=[2, ])
    # trainAll = pd.read_csv('../input/train_set.csv')
    trainAll['id'] = np.multiply(np.add(range(len(trainAll)), 1), -1)

    # Load the test data
    test = pd.read_csv('../input/test_set.csv', parse_dates=[3, ])
    #test = pd.read_csv('../input/test_set.csv')
    test['cost'] = 0

    # Add the test data for pre processing
    trainTest = pd.concat([trainAll, test], ignore_index=True)
    trainTest = preProcess(trainTest)

    # Scoop out the train data back
    trainAll = trainTest[trainTest.id < 0].copy()
    test = trainTest[trainTest.id > 0].copy()

    yAll = np.log(trainAll['cost'])
    #yAll = np.log(trainAll['cost'] / trainAll['quantity'])

    test = test.drop(['cost', 'quantity'], axis=1)
    trainAll = trainAll.drop(['id', 'cost', 'quantity'], axis=1)
    #trainAll['costPerUnit'] = trainAll['cost'] / trainAll['quantity']
    #yAll = np.log(trainAll['costPerUnit'])
    # Drop columns we can't deal with right now
    #trainAll = trainAll.drop(['cost', 'costPerUnit'], axis=1)

    #trainAll = trainAll.drop(['component_id_1'], axis=1)
    #trainAll = trainAll.drop(['length'], axis=1)

    # Split train test data sets
    trainX, testX, trainY, testY = cross_validation.train_test_split( \
        trainAll, yAll, test_size=0.2, random_state=0)
    # Convert the cost back to normal from log for the final score check
    testY = np.exp(testY)

    trainX[:1000].to_csv("./trainX.csv")

    testXId = ""

    if forSubmission:
        trainX, trainY = trainAll, yAll
        testX = test
        testXId = testX.id
        testX = testX.drop(['id'], axis=1)

    # Let's filter out extreme quantities to see if we get a better accuracy
    #trainAll = trainAll[trainAll.quantity<10]

    #Define scorer
    rmsle_scorer = metrics.make_scorer(rmsle, greater_is_better=False)

    print "Running model..."

    if gbr:
        # GBR Run model
        gbr = ensemble.GradientBoostingRegressor(max_depth=1, random_state=0)
        param_grid = dict(n_estimators=[2500], learning_rate=[0.5])
        model = GridSearchCV(gbr, param_grid=param_grid, scoring=rmsle_scorer, verbose=10).fit(trainX, trainY)
        model = getBestModel(model)
    else:
        model = ensemble.RandomForestRegressor(n_estimators=500, criterion='mse', oob_score=True, max_features=60, max_depth=None).fit(trainX, trainY)

        # Grid based turning
        # from sklearn import linear_model
        # model = linear_model.LinearRegression(fit_intercept=True, normalize=True, copy_X=True).fit(trainX, trainY)
        # Alternative model
        # rfModel = ensemble.RandomForestRegressor(criterion='mse')
        # param_grid = dict(max_features=[40,50,60,70,80,90])
        # model = GridSearchCV(rfModel, param_grid=param_grid, scoring=rmsle_scorer, verbose=10).fit(trainX, trainY)
        # model = getBestModel(model)

    print "...Completed!"

    preds = model.predict(testX)
    preds = np.exp(preds)
    finalPreds = np.copy(preds)
    #preds = np.multiply(preds, testX.quantity)

    if forSubmission:
        print "Creating submission file..."
        # Create your first submission file
        submission = pd.DataFrame({"id": testXId, "cost": preds})
        submission.to_csv("cat-tube-axs-results.csv", columns=['id', 'cost'], index=False)
    else:
        print "RMSLE Score: ", rmsle(preds, testY)
        plotYearWiseResults = False
        if plotYearWiseResults:
            # Check year wise RMSLE
            testX = testX.reset_index(drop=True)
            testY = testY.reset_index(drop=True)
            results = pd.DataFrame()
            topYears = testX.year.value_counts()[:5].index
            results['Unbiased'] = getYearWiseResults(finalPreds, testY, topYears)
            for year in topYears:
                trainX_ = trainX[(trainX.year == year)]
                trainY_ = trainY[(trainX.year == year)]
                model = ensemble.RandomForestRegressor(n_estimators=10, criterion='mse', max_depth=None).fit(trainX_,
                                                                                                             trainY_)
                preds = model.predict(testX)
                preds = np.exp(preds)
                results[year] = getYearWiseResults(preds, testY, topYears)

            results.plot()
            #print(grid_search.best_estimator_)
            #scores = cross_validation.cross_val_score(clf, trainX, trainY, cv=5)
            #print scores

    print "Done!"
