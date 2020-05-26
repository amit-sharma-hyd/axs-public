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
    rf = RandomForestClassifier(n_estimators=100)
    rf.fit(train, target)

    test = genfromtxt(open('C:\\axs\\work\\kaggle\\otto\\test.csv','r'), delimiter=',', dtype='f8')[1:]
    test = test.astype(np.int)
    testIDs = test[:,0]
    test = test[:,1:]

    pred = rf.predict_proba(test)
    pred = np.insert(pred,0,testIDs,axis=1)
    print([pred[5]])
    print(len(pred[0]))

    colHeaders = 'id,Class_1,Class_2,Class_3,Class_4,Class_5,Class_6,Class_7,Class_8,Class_9'
    colFormats = '%d,%f,%f,%f,%f,%f,%f,%f,%f,%f'
    #print(pred[1:10])
    savetxt('C:\\axs\\work\\kaggle\\otto\\pySubmission2.csv', pred, delimiter=',', fmt=colFormats, header=colHeaders)

if __name__=="__main__":
    main()