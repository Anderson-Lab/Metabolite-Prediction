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

train.columns = list(ppm) + ['label'] # Now correct the column names
