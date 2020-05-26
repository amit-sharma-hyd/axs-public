from sklearn.ensemble import RandomForestClassifier
from numpy import genfromtxt, savetxt
import numpy as np

def main():
    #create the training & test sets, skipping the header row with [1:]
    dataset = genfromtxt(open('C:\\axs\\work\\kaggle\\otto\\train.csv','r'), delimiter=',',dtype=None)[1:]
    #target = [x[:,-1] for x in dataset]
    target = dataset[:,-1]
    target = [x[6:7] for x in target]
    train = np.delete(dataset, np.s_[0,len(dataset[0])-1], 1)
    train = train.astype(np.int)
    target = [int(item) for item in target]


    #create and train the random forest
    #multi-core CPUs can use: rf = RandomForestClassifier(n_estimators=100, n_jobs=2)
    rf = RandomForestClassifier(n_estimators=500)


    test = genfromtxt(open('C:\\axs\\work\\kaggle\\otto\\test.csv','r'), delimiter=',', dtype='f8')[1:]
    test = test.astype(np.int)
    testIDs = test[:,0]
    test = test[:,1:]

    predOut = np.copy(testIDs)
    predOut = predOut.reshape(len(predOut),1)
    #Try for each class separately
    for targetClass in range(1,10):
        print("Processing for: %d" % targetClass)
        #Extract class specific train data
        targetCol = np.copy(target)
        targetCol = map(lambda x:1 if x==targetClass else 0, targetCol)

        rf.fit(train, targetCol)
        predClass = rf.predict_proba(test)
        predClass = predClass[:,1]
        predClass = predClass.reshape(len(predClass),1)
        predOut = np.concatenate((predOut,predClass),axis=1)
        print(predOut[10])

    colHeaders = 'id,Class_1,Class_2,Class_3,Class_4,Class_5,Class_6,Class_7,Class_8,Class_9'
    colFormats = '%d,%f,%f,%f,%f,%f,%f,%f,%f,%f'
    savetxt('C:\\axs\\work\\kaggle\\otto\\pySubmission3.csv', predOut, delimiter=',', fmt=colFormats, header=colHeaders)

if __name__=="__main__":
    main()