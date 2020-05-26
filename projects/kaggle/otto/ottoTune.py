from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from numpy import genfromtxt, savetxt
import numpy as np
import scipy as sp
import pyUtils as myUtils

def runRF(rf, trainX, trainY, testX, testY):
    print("Applying Random Forest...")
    rf.fit(trainX, trainY)
    pred = rf.predict(testX)
    ll = myUtils.logloss(np.array(testY), pred)
    print("Random Forest LogLoss: %f" %ll)
    return pred

def main():
    #create the training & test sets, skipping the header row with [1:]
    dataset = genfromtxt(open('C:\\axs\\work\\kaggle\\otto\\train.csv','r'), delimiter=',',dtype=None)[1:]
    #target = [x[:,-1] for x in dataset]
    targetAll = dataset[:,-1]
    targetAll = [x[6:7] for x in targetAll]
    trainAll = np.delete(dataset, np.s_[0,len(dataset[0])-1], 1)
    trainAll = trainAll.astype(np.int)
    targetAll = [int(item) for item in targetAll]
    targetAll = np.array(targetAll)

    #Multiclass split
    trainX, testX, trainY, testY = myUtils.multiclass_split(trainAll, targetAll, range(1,10), 0.33)

    print("Applying Gradient Boost...")
    #create and train the random forest
    #multi-core CPUs can use: rf = RandomForestClassifier(n_estimators=100, n_jobs=2)
    rf = RandomForestClassifier(n_estimators=500)
    rf.fit(trainX, trainY)
    pred = rf.predict_proba(testX)
    ll = myUtils.multiclass_log_loss(testY, pred)
    print("Overall RF LogLoss: %f" %ll)


    print("Applying Gradient Boost...")
    #create and train the random forest
    #multi-core CPUs can use: rf = RandomForestClassifier(n_estimators=100, n_jobs=2)
    gbF = GradientBoostingClassifier(n_estimators=750, learning_rate=0.25,
                                     max_depth=1, random_state=0).fit(trainX, trainY)
    pred = gbF.predict_proba(testX)
    ll = myUtils.multiclass_log_loss(testY, pred)
    print("Gradient Boost LogLoss: %f" %ll)


if __name__=="__main__":
    main()