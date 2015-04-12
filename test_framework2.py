import os
import pandas as pd
import numpy as np
os.chdir('C:\\Users\\Paul\\GitHub\\Metabolite-Prediction')

positive = pd.read_csv('Tyrosine/positive_train2.csv')
negative = pd.read_csv('Tyrosine/negative_train2.csv')

ppm = positive.ix[:,0] # First column are the variable names
train = np.concatenate((positive.ix[:,0:(np.size(positive,axis=1)-2)],negative.ix[:,0:(np.size(negative,axis=1)-2)]),axis=1) # combine them except the first column
train = train.T
train = pd.DataFrame(train) # Now convert it to a data frame
train['label'] = 0 # Now add in a label column initially set to all 0's
train.loc[0:93,'label'] = 1 # Now set the first 94 samples to 1 to indicate they are positive

train.columns = ["X"+str(x) for x in list(ppm)] + ['label'] # Now correct the column names

features = train

from sklearn.tree import DecisionTreeClassifier
collist = [x for x in features.columns if x not in ('label')]


k = 3
positive_accuracy = 0.
negative_accuracy = 0.
train_accuracy = 0.
accuracy = 0.
random_order = np.random.permutation(np.size(features,axis=0)) #sample(1:nrow(features),nrow(features))
features = features.ix[random_order,:]
size = int(round(np.size(features,axis=0)/k))
for i in range(k):
  start = size*(i-1)
  #end = min(nrow(train),size + size*(i-1))
  if i == k:
    end = size(features,axis=0)
  else:
    end = size + size*(i-1)
  
  test_inxs = list(range(start,end))
  test_set = features.ix[test_inxs,:]
  train_inxs = [ix for ix in features.index if ix not in test_inxs]
  train_set = features.ix[train_inxs,:]
  
  clf = DecisionTreeClassifier()
  X = features[collist].values
  y = features["label"].values
  clf.fit(X[train_inxs,:], y[train_inxs])

  X_test = X[test_inxs,:]
  predictions = clf.predict(X_test)
  
  correct_predictions = y[test_inxs]
  accuracy = accuracy + float(np.size(np.where(predictions == correct_predictions)))/len(predictions)
  cixs = np.where(correct_predictions == 1)
  if np.size(cixs,axis=1) > 0:
    positive_accuracy = positive_accuracy + float(np.size(np.where(predictions[cixs] == correct_predictions[cixs]),axis=1))/np.size(cixs,axis=1)
  nixs = np.where(correct_predictions == 0)
  negative_accuracy = negative_accuracy + float(np.size(np.where(predictions[nixs] == correct_predictions[nixs]),axis=1))/np.size(nixs,axis=1)

  #predictions = predict(fit,train.set,type='vector')-1 # They use 1 to indicate class 1 which is actually 0
  predictions = clf.predict(X[train_inxs,:]) # They use 1 to indicate class 1 which is actually 0
  correct_predictions = train_set['label']
  train_accuracy = train_accuracy + float(np.size(np.where(predictions == correct_predictions)))/len(predictions)

accuracy = accuracy / k
train_accuracy = train_accuracy / k
positive_accuracy = positive_accuracy / k
negative_accuracy = negative_accuracy / k
print 'The training accuracy is',train_accuracy
print 'The accuracy is',accuracy
print 'The negative accuracy is',negative_accuracy
print 'The positive accuracy is',positive_accuracy
