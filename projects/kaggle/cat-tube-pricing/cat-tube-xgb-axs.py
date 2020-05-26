#forked from Gilberto Titericz Junior

import pandas as pd
import numpy as np
from sklearn import ensemble, preprocessing
from sklearn import cross_validation
import xgboost as xgb
import glob


# Root Mean Squared Logarithmic Error (RMSLE)
def rmsle(pred, actual):
    sle = np.sum(pow(np.log(pred + 1) - np.log(actual + 1), 2))
    sleMean = sle / len(pred)
    rmsle = np.sqrt(sleMean)
    return rmsle

def addCompTypeCols(df):
    fileDF = pd.read_csv('../input/components.csv')
    # Create a series for lookup
    compTypeLookup = pd.Series(fileDF.component_type_id.values, index=fileDF.component_id.values)
    for i in range(1, 9):
        colName = 'component_id_' + str(i)
        dfCol = df[colName].copy()
        dfCol[pd.isnull(dfCol)] = '9999'
        df[colName + '_type'] = compTypeLookup[dfCol].values
    return df

def addTubeEndFormCols(df):
    fileDF = pd.read_csv('../input/tube_end_form.csv')
    # Create a series for lookup
    formingLookup = pd.Series(fileDF.forming.values, index=fileDF.end_form_id.values)
    df['end_a_forming'] = formingLookup[df['end_a']].values
    df['end_x_forming'] = formingLookup[df['end_x']].values
    return df

def addCompDetails(df):
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
        return df

def addCols(df):
    df = addCompTypeCols(df)
    df = addTubeEndFormCols(df)
    df = addCompDetails(df)
    # Add features as applicable
    #df['innerDia'] = df['diameter'] - df['wall']
    #df['vol'] = df['length'] * (df['diameter'] * df['diameter']) - (df['innerDia'] * df['innerDia'])
    #df['area'] = df['length'] * df['diameter']
    #df['bendDensity'] = df['num_bends'] / (df['length'] + 0.1)
    df['surplusQty'] = df.quantity - df.min_order_quantity
    #df['yearsAgo'] = 2018 - df['year'].astype(int)
    return df


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

submit = False

print("Load data")
# load training and test datasets
train = pd.read_csv('../input/train_set.csv', parse_dates=[2,])

if (submit):
    test = pd.read_csv('../input/test_set.csv', parse_dates=[3,])
else :
    print("Splitting data set")
    # Split train test data sets
    trainX, testX, trainY, testY = cross_validation.train_test_split( \
        train, train.cost, test_size=0.2, random_state=0)
    train = trainX.copy()
    test = testX.copy()
    test['id'] = np.add(range(len(testY)), 1)
    test = test.drop(['cost'], axis=1)
    print(train.shape, test.shape, testY.shape)
    print("Test train data ready")
    print(trainX.columns)

print("Reading other files")
tube_data = pd.read_csv('../input/tube.csv')
bill_of_materials_data = pd.read_csv('../input/bill_of_materials.csv')
specs_data = pd.read_csv('../input/specs.csv')

print("train columns")
print(train.columns)
print("test columns")
print(test.columns)
print("tube.csv df columns")
print(tube_data.columns)
print("bill_of_materials.csv df columns")
print(bill_of_materials_data.columns)
print("specs.csv df columns")
print(specs_data.columns)

print(specs_data[2:3])

train = pd.merge(train, tube_data, on ='tube_assembly_id', how='left')
train = pd.merge(train, bill_of_materials_data, on ='tube_assembly_id', how='left')
train = pd.merge(train, specs_data, on ='tube_assembly_id', how='left')
test = pd.merge(test, tube_data, on ='tube_assembly_id', how='left')
test = pd.merge(test, bill_of_materials_data, on ='tube_assembly_id', how='left')
test = pd.merge(test, specs_data, on ='tube_assembly_id', how='left')

print("new train columns")
print(train.columns)
print(train[1:10])
print(train.columns.to_series().groupby(train.dtypes).groups)


# create some new features
train['year'] = train.quote_date.dt.year
train['month'] = train.quote_date.dt.month
#train['dayofyear'] = train.quote_date.dt.dayofyear
#train['dayofweek'] = train.quote_date.dt.dayofweek
#train['day'] = train.quote_date.dt.day

test['year'] = test.quote_date.dt.year
test['month'] = test.quote_date.dt.month
#test['dayofyear'] = test.quote_date.dt.dayofyear
#test['dayofweek'] = test.quote_date.dt.dayofweek
#test['day'] = test.quote_date.dt.day

#df['yearsAgo'] = np.exp(1/(2018-df['year']))
#df['yearsAgo'] = 2018 - df['year']

# AXS
train = addCols(train)
test = addCols(test)

# drop useless columns and create labels
idx = test.id.values.astype(int)
test = test.drop(['id', 'tube_assembly_id', 'quote_date'], axis = 1)
labels = train.cost.values
#'tube_assembly_id', 'supplier', 'bracket_pricing', 'material_id', 'end_a_1x', 'end_a_2x', 'end_x_1x', 'end_x_2x', 'end_a', 'end_x'
#for some reason material_id cannot be converted to categorical variable
train = train.drop(['quote_date', 'cost', 'tube_assembly_id'], axis = 1)

train['material_id'].replace(np.nan,' ', regex=True, inplace= True)
test['material_id'].replace(np.nan,' ', regex=True, inplace= True)
for i in range(1,9):
    column_label = 'component_id_'+str(i)
    print(column_label)
    train[column_label].replace(np.nan,' ', regex=True, inplace= True)
    test[column_label].replace(np.nan,' ', regex=True, inplace= True)

train.fillna(0, inplace = True)
test.fillna(0, inplace = True)

print("train columns")
print(train.columns)

#train = encodeCols(train)
#test = encodeCols(test)

for col in train.columns.values:
        if ((train[col].dtype != 'int64') & (train[col].dtype != 'float64')) | ((test[col].dtype != 'int64') & (test[col].dtype != 'float64')):
            print(col, list(train[col][1:5]) + list(test[col][1:5]))
            lbl = preprocessing.LabelEncoder()
            lbl.fit(list(train[col]) + list(test[col]))
            train[col] = lbl.transform(train[col].astype(str))
            test[col] = lbl.transform(test[col].astype(str))

# convert data to numpy array
train = np.array(train)
test = np.array(test)


'''
# label encode the categorical variables
for i in range(train.shape[1]):
    if i in [0,3,5,11,12,13,14,15,16,20,22,24,26,28,30,32,34]:
        print(i,list(train[1:5,i]) + list(test[1:5,i]))
        lbl = preprocessing.LabelEncoder()
        lbl.fit(list(train[:,i]) + list(test[:,i]))
        train[:,i] = lbl.transform(train[:,i])
        test[:,i] = lbl.transform(test[:,i])
'''

# object array to float
train = train.astype(float)
test = test.astype(float)

# i like to train on log(1+x) for RMSLE ;)
# The choice is yours :)
label_log = np.log1p(labels)

# fit a random forest model

params = {}
params["objective"] = "reg:linear"
params["eta"] = 0.02
params["min_child_weight"] = 6
params["subsample"] = 0.7
params["colsample_bytree"] = 0.6
params["scale_pos_weight"] = 0.8
params["silent"] = 1
params["max_depth"] = 8
params["max_delta_step"]=2


plst = list(params.items())

xgtrain = xgb.DMatrix(train, label=label_log)
xgtest = xgb.DMatrix(test)


scale=10
print("-----------------------------------------------------------")
print("Running with a scale factor: ", scale)
print("-----------------------------------------------------------")

print('1500')


num_rounds = 1500/scale
model = xgb.train(plst, xgtrain, num_rounds)
preds1 = model.predict(xgtest)


print('3000')

num_rounds = 3000/scale
model = xgb.train(plst, xgtrain, num_rounds)
preds2 = model.predict(xgtest)

print('4000')

num_rounds = 4000/scale
model = xgb.train(plst, xgtrain, num_rounds)
preds4 = model.predict(xgtest)

label_log = np.power(labels,1.0/16.0)

xgtrain = xgb.DMatrix(train, label=label_log)
xgtest = xgb.DMatrix(test)

print('power 1/16 4000')

num_rounds = 4000/scale
model = xgb.train(plst, xgtrain, num_rounds)
preds3 = model.predict(xgtest)

preds = 0.4*np.expm1(preds4)+.1*np.expm1(preds1)+0.1*np.expm1(preds2)+0.4*np.power(preds3,16)

if (submit==False):
    from sklearn import linear_model
    regr = linear_model.LinearRegression()
    trainDF = pd.concat([pd.DataFrame(np.expm1(preds4)), pd.DataFrame(np.expm1(preds1)),
                         pd.DataFrame(np.expm1(preds2)), pd.DataFrame(np.power(preds3,16))], axis=1)
    regr.fit(trainDF, testY)
    newPreds = regr.regr.coef_[0]*np.expm1(preds4) + regr.coef_[1]*np.expm1(preds1) + regr.coef_[2]*np.expm1(preds2) + regr.coef_[3]*np.power(preds3,16)

if (submit) :
    preds = pd.DataFrame({"id": idx, "cost": preds})
    preds.to_csv('benchmark.csv', index=False)
    print("Done!")
else:
    actual = testY
    sle = sum(pow(np.log(preds + 1) - np.log(actual + 1), 2))
    sleMean = sle / len(preds)
    rmsle = np.sqrt(sleMean)
    print ("RMSLE Score:", rmsle)
