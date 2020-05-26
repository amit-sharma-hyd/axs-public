
# coding: utf-8

# In[36]:

"""
Beyond Beating the Benchmark 
Search Results Relevance @ Kaggle
__author__ : Amit Sharma

"""
import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import SVC
from sklearn.decomposition import TruncatedSVD
from sklearn.preprocessing import StandardScaler
from sklearn import decomposition, pipeline, metrics, grid_search
from sklearn import cross_validation
from sklearn.ensemble import GradientBoostingClassifier
from bs4 import BeautifulSoup


pd.set_option('display.width', 5000) 
pd.set_option('display.max_columns', 60) 

train = pd.read_csv('C:\\axs\\work\\kaggle\\crowdflower\\input\\train.csv')
test = pd.read_csv('C:\\axs\\work\\kaggle\\crowdflower\\input\\test.csv')

# we dont need ID columns
idx = test.id.values.astype(int)
train = train.drop('id', axis=1)
test = test.drop('id', axis=1)

# create labels. drop useless columns
y = train.median_relevance.values
train = train.drop(['median_relevance', 'relevance_variance'], axis=1)


# In[37]:

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

kappa_scorer = metrics.make_scorer(quadratic_weighted_kappa, greater_is_better = True)


# In[38]:

def query_title_common(row):
    return (set(row['query'].split()) & set(row['product_title'].split()))

def query_desc_common(row):
    return (set(row['query'].split()) & set(row['pdesc'].split()))

def addFeatures(df):
    df['query'] = df['query'].astype('str')
    df['product_title'] = df['product_title'].astype('str')
    df['product_description'] = df['product_description'].astype('str')
    # Lowercase everything
    df['query'] = df['query'].str.lower()
    df['product_title'] = df['product_title'].str.lower()
    df['product_description'] = df['product_description'].str.lower()

    df['pdesc'] = map(lambda x: BeautifulSoup(''.join(x)).text, df['product_description'])

    df['query_title'] = df.apply(query_title_common,axis=1)
    df['query_title_nwords'] = df['query_title'].str.len()
    df['title_nwords'] = df['product_title'].str.split().str.len()
    df['query_nwords'] = df['query'].str.split().str.len()
    df['query_title_match'] = df['query_title_nwords']/df['query_nwords']

    df['query_desc'] = df.apply(query_desc_common,axis=1)
    df['query_desc_nwords'] = df['query_desc'].str.len()
    df['desc_nwords'] = df['pdesc'].str.split().str.len()
    df['query_nwords'] = df['query'].str.split().str.len()
    df['query_desc_match'] = df['query_desc_nwords']/df['query_nwords']

    X=pd.DataFrame(df[['query', 'product_title', 'pdesc', 'query_nwords','title_nwords',
             'query_title_nwords','query_title_match', 'desc_nwords','query_desc_nwords','query_desc_match']])
    X['query_len'] = df['query'].str.len()
    X['ptitle_len'] = df['product_title'].str.len()
    X['query_title_match'] = (X['query_title_nwords']+0.00001)/X['query_nwords']
    X['ptitle_size_over_query'] = X['ptitle_len']/X['query_len']
    X['query'] = X['query'].astype(str)


    X['query_len'] = df['query'].str.len()
    X['pdesc_len'] = df['pdesc'].str.len()
    X['query_desc_match'] = (X['query_desc_nwords']+0.00001)/X['query_nwords']
    X['pdesc_size_over_query'] = X['pdesc_len']/X['query_len']

    # Create dummy columns for categorical variable as scikit does not understand factors
    dummyDF = pd.get_dummies(X['query'])
    dummyDF= dummyDF.astype(bool)
    X = pd.concat([X,dummyDF],axis=1)
    
    return (X)


# In[39]:

train = addFeatures(train)
test = addFeatures(test)


# In[40]:

#Use SVM based learning 
# do some lambda magic on text columns
traindata = list(train.apply(lambda x:'%s %s' % (x['query'],x['product_title']),axis=1))
testdata = list(test.apply(lambda x:'%s %s' % (x['query'],x['product_title']),axis=1))

# the infamous tfidf vectorizer (Do you remember this one?)
tfv = TfidfVectorizer(min_df=3,  max_features=None, 
        strip_accents='unicode', analyzer='word',token_pattern=r'\w{1,}',
        ngram_range=(1, 5), use_idf=1,smooth_idf=1,sublinear_tf=1,
        stop_words = 'english')

# Fit TFIDF
tfv.fit(traindata)
X =  tfv.transform(traindata) 
X_test = tfv.transform(testdata)

# Initialize SVD
svd = TruncatedSVD()

# Initialize the standard scaler 
scl = StandardScaler()

# We will use SVM here..
svm_model = SVC()

# Create the pipeline
clf = pipeline.Pipeline([('svd', svd),
                         ('scl', scl),
                         ('svm', svm_model)])

# Create a parameter grid to search for best parameters for everything in the pipeline
param_grid = {'svd__n_components' : [400],
              'svm__C': [12]}

# Model Code - Run the model

# Kappa Scorer 
kappa_scorer = metrics.make_scorer(quadratic_weighted_kappa, greater_is_better = True)

# Initialize Grid Search Model
model = grid_search.GridSearchCV(estimator = clf, param_grid=param_grid, scoring=kappa_scorer,
                                 verbose=10, n_jobs=1, iid=True, refit=True, cv=2)

# Fit Grid Search Model
model.fit(X, y)
print("Best score: %0.3f" % model.best_score_)
print("Best parameters set:")
best_parameters = model.best_estimator_.get_params()
for param_name in sorted(param_grid.keys()):
    print("\t%s: %r" % (param_name, best_parameters[param_name]))

# Get best model
best_model = model.best_estimator_


trainPred = best_model.predict(X)
testPred = best_model.predict(X_test)

trainX = pd.DataFrame(train)
testX = pd.DataFrame(test)
train['model1_Pred'] = trainPred
test['model1_Pred'] = testPred


# In[41]:

#TODO pdesc not used yet
train = train.drop(['query', 'product_title', 'pdesc'], axis=1)
test = test.drop(['query', 'product_title', 'pdesc'], axis=1)


# In[42]:

clf = GradientBoostingClassifier(n_estimators=500, learning_rate=0.5, max_depth=1, random_state=0).fit(train, y)
scores = cross_validation.cross_val_score(clf, train, y, scoring=kappa_scorer, cv=10)
scores


# In[43]:

test[:3]


# In[44]:

preds = clf.predict(test)
# Submission code - final nail in the coffin
# Create your first submission file
submission = pd.DataFrame({"id": idx, "prediction": preds})
submission.to_csv("C:\\axs\\work\\kaggle\\crowdflower\\cflower_submit.csv", index=False)

