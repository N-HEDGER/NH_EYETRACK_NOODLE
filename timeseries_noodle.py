# Load data into Pandas dataframe
import pandas as pd
import matplotlib.pyplot as plt
import os
import numpy as np


PATH='/Users/nicholashedger/Documents/GitHub/NH_EYETRACK_NOODLE'


def load_data(PATH,FILE):
	csvpath=os.path.join(PATH,FILE)
	return pd.read_csv(csvpath)


TIMESERIES=load_data(PATH,'timeseries.csv')
SCRAMLAB=load_data(PATH,'sclabels.csv')
SCLAB=load_data(PATH,'sidelabels.csv')


TIMESERIES=TIMESERIES.drop('Unnamed: 0',1)
SCRAMLAB=SCRAMLAB.drop('Unnamed: 0',1)
SCLAB=SCLAB.drop('Unnamed: 0',1)
TIMESERIES=TIMESERIES.drop('ps',1)

LABELS=SCRAMLAB
LABELS2=pd.DataFrame.as_matrix(LABELS)
LABELS3=np.squeeze(LABELS2)


from tsfresh import extract_relevant_features

features_filtered_direct = extract_relevant_features(TIMESERIES, LABELS3,
                                                     column_id='id', column_sort='time')


from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score

X_train, X_test, y_train, y_test = train_test_split(features_filtered_direct, LABELS3, test_size=.2)

from sklearn.tree import DecisionTreeClassifier

cl = DecisionTreeClassifier()
cl.fit(X_train, y_train)

scoreCV=cross_val_score(cl,X_train,y_train,cv=10,scoring='accuracy')

from sklearn.metrics import classification_report
print(classification_report(y_test, cl.predict(X_test))

attributes=list(features_filtered_direct)

sorted(zip(cl.feature_importances_,attributes),reverse=True)



def indices_of_top_k(arr, k):
    return np.sort(np.argpartition(np.array(arr), -k)[-k:])


top_k=indices_of_top_k(cl.feature_importances_,10)



toplabels=np.array(attributes)[top_k]
topkmat=np.array(features_filtered_direct)[:,top_k]


X_train, X_test, y_train, y_test = train_test_split(topkmat, LABELS3, test_size=.2)

from sklearn.ensemble import RandomForestClassifier

cl2 = RandomForestClassifier()
cl2.fit(X_train, y_train)

scoreCV2=cross_val_score(cl2,X_train,y_train,cv=10,scoring='accuracy')

from sklearn.metrics import classification_report
print(classification_report(y_test, cl2.predict(X_test))



param_grid = [
    # try 12 (3×4) combinations of hyperparameters
    {'n_estimators': [3, 10, 30], 'max_features': [2, 4, 6, 8]},
    # then try 6 (2×3) combinations with bootstrap set as False
    {'bootstrap': [False], 'n_estimators': [3, 10], 'max_features': [2, 3, 4]},
  ]



from sklearn.model_selection import GridSearchCV
grid_search = GridSearchCV(cl2, param_grid, cv=5,scoring='accuracy')

grid_search.fit(X_train, y_train)



cvres = grid_search.cv_results_
for mean_score, params in zip(cvres["mean_test_score"], cvres["params"]):
    print(mean_score, params)

print(classification_report(y_test, grid_search.best_estimator_.predict(X_test)))




