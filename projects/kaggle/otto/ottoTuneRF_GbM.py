from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix
from sklearn.ensemble import GradientBoostingClassifier
from numpy import genfromtxt, savetxt
import numpy as np
import scipy as sp
import pyUtils as myUtils
import collections

def runRF(rf, trainX, trainY, train2X, train2Y):
    print("Applying Random Forest...")
    rf.fit(trainX, trainY)
    pred = rf.predict(train2X)
    ll = myUtils.logloss(np.array(train2Y), pred)
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


    trainAllX, testAllX, trainAllY, testAllY = myUtils.multiclass_split(trainAll, targetAll, range(1,10), 0.33)


    #Multiclass split
    trainX, train2X, trainY, train2Y = myUtils.multiclass_split(trainAllX, trainAllY, range(1,10), 0.33)

    '''
    print("Applying RF...")
    #create and train the random forest
    #multi-core CPUs can use: rf = RandomForestClassifier(n_estimators=100, n_jobs=2)
    rf = RandomForestClassifier(n_estimators=500)
    rf.fit(trainX, trainY)
    pred = rf.predict_proba(train2X)
    ll = myUtils.multiclass_log_loss(train2Y, pred)
    print("Overall RF LogLoss: %f" %ll)
    '''

    print collections.Counter(trainY)

    print("Applying class wise ...")
    #create and train the random forest
    #multi-core CPUs can use: rf = RandomForestClassifier(n_estimators=100, n_jobs=2)
    rf = RandomForestClassifier(n_estimators=1000)
    #Back up the original target columns
    trainY_Orig = np.copy(trainY)
    train2Y_Orig = np.copy(train2Y)

    targetClasses = np.unique(train2Y)
    targetClassesTest = np.unique(testAllY)
    predAllRF = np.zeros((train2X.shape[0], len(targetClasses)))
    predAllRFTest = np.zeros((testAllX.shape[0], len(targetClassesTest)))
    for targetClass in targetClasses:
        print "------------------------------------"
        print "Processing class %d..." % targetClass
        # Restore from original target variabels
        trainY = np.copy(trainY_Orig)
        train2Y = np.copy(train2Y_Orig)

        #Set target values to binary classification as per current class
        trainY = map(lambda x:1 if x==targetClass else 0, trainY)
        train2Y = map(lambda x:1 if x==targetClass else 0, train2Y)
        trainYFreq = collections.Counter(trainY)
        print trainYFreq
        wRatio = trainYFreq[1]*1./trainYFreq[0]
        sampleWeight = np.array([wRatio if x==0 else 1 for x in trainY])

        # Fit the model
        rf.fit(trainX, trainY, sample_weight=sampleWeight)
        pred = rf.predict_proba(train2X)
        predAllRF[:,targetClass-1] = pred[:,1]
        predClass = rf.predict(train2X)
        print "... Log loss= %f" % myUtils.logloss(train2Y, pred[:,1])
        print confusion_matrix(train2Y, predClass)

        predTest = rf.predict_proba(testAllX)
        predAllRFTest[:,targetClass-1] = predTest[:,1]

    # Normalize the pred probs
    predAllRF = predAllRF/predAllRF.sum(axis=1)[:,None]
    predAllRFTest = predAllRFTest/predAllRFTest.sum(axis=1)[:,None]


    ll = myUtils.multiclass_log_loss(train2Y_Orig, predAllRF)
    print("Training Data: Overall class wise RF LogLoss: %f" %ll)

    llTest = myUtils.multiclass_log_loss(testAllY, predAllRFTest)
    print("Test Data: Overall class wise RF LogLoss: %f" %llTest)


    print("Applying Gradient Boost...")
    #create and train the random forest
    #multi-core CPUs can use: rf = RandomForestClassifier(n_estimators=100, n_jobs=2)
    gbF = GradientBoostingClassifier(n_estimators=100, learning_rate=0.1,
                                     max_depth=1, random_state=0).fit(predAllRF, train2Y_Orig)
    pred = gbF.predict_proba(predAllRFTest)
    ll = myUtils.multiclass_log_loss(testAllY, pred)
    print("Gradient Boost LogLoss: %f" %ll)

if __name__=="__main__":
    main()