import numpy as np
import scipy as sp
from sklearn.cross_validation import train_test_split
import collections

def logloss(act, pred):
    epsilon = 1e-15
    pred = sp.maximum(epsilon, pred)
    pred = sp.minimum(1-epsilon, pred)
    ll = sum(act*sp.log(pred) + sp.subtract(1,act)*sp.log(sp.subtract(1,pred)))
    ll = ll * -1.0/len(act)
    return ll

def multiclass_log_loss(y_true, y_pred, eps=1e-15):
    """Multi class version of Logarithmic Loss metric.
    https://www.kaggle.com/wiki/MultiClassLogLoss

    idea from this post:
    http://www.kaggle.com/c/emc-data-science/forums/t/2149/is-anyone-noticing-difference-betwen-validation-and-leaderboard-error/12209#post12209

    Parameters
    ----------
    y_true : array, shape = [n_samples]
    y_pred : array, shape = [n_samples, n_classes]

    Returns
    -------
    loss : float
    """
    predictions = np.clip(y_pred, eps, 1 - eps)

    # normalize row sums to 1
    predictions /= predictions.sum(axis=1)[:, np.newaxis]

    actual = np.zeros(y_pred.shape)
    rows = actual.shape[0]
    actual[np.arange(rows), y_true.astype(int)-1] = 1
    vsota = np.sum(actual * np.log(predictions))
    return -1.0 / rows * vsota

def multiclass_split(train, target, classes, testSize):
    retTrainX = retTestX = retTrainY = retTestY  = []
    for i in classes:
        trainClass = train[target==i]
        trainX, testX, trainY, testY = train_test_split(trainClass, np.repeat(i, len(trainClass)), test_size=testSize, random_state=42)
        if (i==classes[0]):
            retTrainX, retTestX, retTrainY, retTestY  =  trainX, testX, trainY, testY
        else:
            retTrainX = np.vstack([retTrainX,trainX])
            retTestX = np.vstack([retTestX, testX])
            retTrainY = np.concatenate([retTrainY, trainY])
            retTestY = np.concatenate([retTestY, testY])
    return (retTrainX, retTestX, retTrainY, retTestY)

def main():
    SIZE = 300
    train = train = np.random.rand(SIZE,2)
    target = np.random.randint(3, size=SIZE)
    trainX, testX, trainY, testY = multiclass_split(train, target, range(0,3), 0.33)
    print collections.Counter(trainY)
    print collections.Counter(testY)

if __name__=="__main__":
    main()