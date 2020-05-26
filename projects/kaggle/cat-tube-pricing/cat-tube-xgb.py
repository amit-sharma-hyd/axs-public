#forked from Gilberto Titericz Junior

import pandas as pd
import numpy as np
from sklearn import ensemble, preprocessing
from sklearn import cross_validation
import xgboost as xgb

print("Load data")
# load training and test datasets
train = pd.read_csv('../input/train_set.csv', parse_dates=[2,])
#test = pd.read_csv('../input/test_set.csv', parse_dates=[3,])

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
test = pd.merge(test, tube_data, on ='tube_assembly_id', how='left')
test = pd.merge(test, bill_of_materials_data, on ='tube_assembly_id', how='left')

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

# convert data to numpy array
train = np.array(train)
test = np.array(test)


# label encode the categorical variables
for i in range(train.shape[1]):
    if i in [0,3,5,11,12,13,14,15,16,20,22,24,26,28,30,32,34]:
        print(i,list(train[1:5,i]) + list(test[1:5,i]))
        lbl = preprocessing.LabelEncoder()
        lbl.fit(list(train[:,i]) + list(test[:,i]))
        train[:,i] = lbl.transform(train[:,i])
        test[:,i] = lbl.transform(test[:,i])


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


#preds = pd.DataFrame({"id": idx, "cost": preds})
#preds.to_csv('benchmark.csv', index=False)

actual = testY
sle = sum(pow(np.log(preds + 1) - np.log(actual + 1), 2))
sleMean = sle / len(preds)
rmsle = np.sqrt(sleMean)
print ("RMSLE Score:", rmsle)

