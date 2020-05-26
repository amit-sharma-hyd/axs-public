
# coding: utf-8

# In[1]:

import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import SVC
from sklearn.decomposition import TruncatedSVD
from sklearn.preprocessing import StandardScaler
from sklearn import decomposition, pipeline, metrics, grid_search

#get_ipython().magic(u'matplotlib inline')
#import matplotlib.pyplot as plt

#pd.set_option('display.mpl_style', 'default') 
pd.set_option('display.width', 5000) 
pd.set_option('display.max_columns', 60) 

train = pd.read_csv('../input/train.csv')

# create labels. drop useless columns
y = train.median_relevance.values
train = train.drop(['median_relevance', 'relevance_variance'], axis=1)


# In[2]:

counts = train['query'].value_counts()
len(counts) #261 unique queries


# In[3]:

'''
# Plot the top 10 queries - not really required now but wanted to check how plots work ;)
data = counts[:10]
plt.rc('xtick', labelsize=8) 
plt.rc('ytick', labelsize=8) 
xpos = np.arange(len(data))
plt.bar(xpos, data, align='center', alpha=0.4)
plt.xlabel('Query')
plt.ylabel('Frequency')
plt.title('Query Hist')
plt.xticks(xpos, data.index)
plt.show()
'''


# In[4]:

# The following 3 functions have been taken from Ben Hamner's github repository
# https://github.com/benhamner/Metrics
def confusion_matrix(rater_a, rater_b, min_rating=None, max_rating=None):
    """
    Returns the confusion matrix between rater's ratings
    """
    assert(len(rater_a) == len(rater_b))
    if min_rating is None:
        min_rating = min(rater_a + rater_b)
    if max_rating is None:
        max_rating = max(rater_a + rater_b)
    num_ratings = int(max_rating - min_rating + 1)
    conf_mat = [[0 for i in range(num_ratings)]
                for j in range(num_ratings)]
    for a, b in zip(rater_a, rater_b):
        conf_mat[a - min_rating][b - min_rating] += 1
    return conf_mat


def histogram(ratings, min_rating=None, max_rating=None):
    """
    Returns the counts of each type of rating that a rater made
    """
    if min_rating is None:
        min_rating = min(ratings)
    if max_rating is None:
        max_rating = max(ratings)
    num_ratings = int(max_rating - min_rating + 1)
    hist_ratings = [0 for x in range(num_ratings)]
    for r in ratings:
        hist_ratings[r - min_rating] += 1
    return hist_ratings


def quadratic_weighted_kappa(y, y_pred):
    """
    Calculates the quadratic weighted kappa
    axquadratic_weighted_kappa calculates the quadratic weighted kappa
    value, which is a measure of inter-rater agreement between two raters
    that provide discrete numeric ratings.  Potential values range from -1
    (representing complete disagreement) to 1 (representing complete
    agreement).  A kappa value of 0 is expected if all agreement is due to
    chance.
    quadratic_weighted_kappa(rater_a, rater_b), where rater_a and rater_b
    each correspond to a list of integer ratings.  These lists must have the
    same length.
    The ratings should be integers, and it is assumed that they contain
    the complete range of possible ratings.
    quadratic_weighted_kappa(X, min_rating, max_rating), where min_rating
    is the minimum possible rating, and max_rating is the maximum possible
    rating
    """
    rater_a = y
    rater_b = y_pred
    min_rating=None
    max_rating=None
    rater_a = np.array(rater_a, dtype=int)
    rater_b = np.array(rater_b, dtype=int)
    assert(len(rater_a) == len(rater_b))
    if min_rating is None:
        min_rating = min(min(rater_a), min(rater_b))
    if max_rating is None:
        max_rating = max(max(rater_a), max(rater_b))
    conf_mat = confusion_matrix(rater_a, rater_b,
                                min_rating, max_rating)
    num_ratings = len(conf_mat)
    num_scored_items = float(len(rater_a))

    hist_rater_a = histogram(rater_a, min_rating, max_rating)
    hist_rater_b = histogram(rater_b, min_rating, max_rating)

    numerator = 0.0
    denominator = 0.0

    for i in range(num_ratings):
        for j in range(num_ratings):
            expected_count = (hist_rater_a[i] * hist_rater_b[j]
                              / num_scored_items)
            d = pow(i - j, 2.0) / pow(num_ratings - 1, 2.0)
            numerator += d * conf_mat[i][j] / num_scored_items
            denominator += d * expected_count / num_scored_items

    return (1.0 - numerator / denominator)


# In[5]:

train['query'] = train['query'].astype('str')
train['product_title'] = train['product_title'].astype('str')

# Lowercase everything
train['query'] = train['query'].str.lower()
train['product_title'] = train['product_title'].str.lower()


# In[6]:

def intersect(row):
    return (set(row['query'].split()) & set(row['product_title'].split()))

train['query_title'] = train.apply(intersect,axis=1)
train['query_title_nwords'] = train['query_title'].str.len()
train['title_nwords'] = train['product_title'].str.split().str.len()
train['query_nwords'] = train['query'].str.split().str.len()
train['query_title_match'] = train['query_title_nwords']/train['query_nwords']


# In[27]:

X=train[['query', 'query_nwords','title_nwords','query_title_nwords','query_title_match']]
X['query_len'] = train['query'].str.len()
X['ptitle_len'] = train['product_title'].str.len()
X['query_title_match'] = (X['query_title_nwords']+0.00001)/X['query_nwords']
X['ptitle_size_over_query'] = X['ptitle_len']/X['query_len']
X['query'] = X['query'].astype(str)

# Create dummy columns for categorical variable as scikit does not understand factors
X = pd.concat([X,pd.get_dummies(X['query'])],axis=1)

X = X.drop(['query'], axis=1)


# In[28]:

from sklearn.cross_validation import train_test_split
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

trainX, testX, trainY, testY = multiclass_split(X, y, range(1,5), 0.33)

trainX = pd.DataFrame.from_dict(trainX) 
testX = pd.DataFrame.from_dict(testX)


# In[32]:
print "Running Random Forest"
from sklearn.ensemble import RandomForestClassifier
rf = RandomForestClassifier(n_estimators=200)
rf.fit(trainX, trainY)
pred = rf.predict(testX)
print quadratic_weighted_kappa(testY, pred)


# In[34]:

'''
from sklearn.ensemble import GradientBoostingClassifier
gbF = GradientBoostingClassifier(n_estimators=200, learning_rate=0.5,
                                 max_depth=1, random_state=0).fit(trainX, trainY)
pred = gbF.predict(testX)
print quadratic_weighted_kappa(testY, pred)
'''


# In[ ]:

from sklearn import datasets
from sklearn.multiclass import OneVsRestClassifier
from sklearn.svm import SVC

print ("Running SVM")
svc = OneVsRestClassifier(SVC(kernel='linear'))
svc.fit(trainX[[1,2,3,4,5,6,7]], trainY)
pred = svc.predict(testX[[1,2,3,4,5,6,7]])
print quadratic_weighted_kappa(testY, pred)


