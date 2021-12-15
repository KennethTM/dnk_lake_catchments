import numpy as np
import pandas as pd
import sklearn
from sklearn.model_selection import cross_validate, KFold, RandomizedSearchCV
from lightgbm import LGBMRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.compose import TransformedTargetRegressor
from sklearn.linear_model import LinearRegression, ElasticNet
from sklearn.neighbors import KNeighborsRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.cross_decomposition import PLSRegression
from sklearn.neural_network import MLPRegressor
from sklearn.svm import SVR
from xgboost import XGBRegressor

from model_setup import *

df_train = pd.read_csv("./data/data_train_preproc.csv")
df_test = pd.read_csv("./data/data_test_preproc.csv")

cv_inner = KFold(n_splits=5, shuffle=True, random_state=999)
cv_outer = KFold(n_splits=10, shuffle=True, random_state=999)

metrics = ("r2", "neg_mean_squared_error", "neg_mean_absolute_error")

random_iters = 50

lm = LinearRegression(n_jobs=1)

lm_log = TransformedTargetRegressor(regressor=LinearRegression(), func=np.log1p, inverse_func=np.expm1)

elastic = ElasticNet()
elastic_tune = RandomizedSearchCV(elastic, elastic_param, n_iter = random_iters, cv = cv_inner, refit=True, verbose=True)

knn = KNeighborsRegressor(n_jobs=1)
knn_tune = RandomizedSearchCV(knn, knn_param, n_iter = random_iters, cv = cv_inner, refit=True, verbose=True)

rpart = DecisionTreeRegressor()
rpart_tune = RandomizedSearchCV(rpart, rpart_param, n_iter = random_iters, cv = cv_inner, refit=True, verbose=True)

plsr = PLSRegression(scale=True)
plsr_tune = RandomizedSearchCV(plsr, plsr_param, n_iter = random_iters, cv = cv_inner, refit=True, verbose=True)

mlp = MLPRegressor()
mlp_tune = RandomizedSearchCV(mlp, mlp_param, n_iter = random_iters, cv = cv_inner, refit=True, verbose=True)

svm = SVR()
svm_tune = RandomizedSearchCV(svm, svm_param, n_iter = random_iters, cv = cv_inner, refit=True, verbose=True)

lgbm = LGBMRegressor(silent=True, n_jobs=1)
lgbm_tune = RandomizedSearchCV(lgbm, lgbm_param, n_iter = random_iters, cv = cv_inner, refit=True, verbose=True)

rf = RandomForestRegressor(n_jobs=1)
rf_tune = RandomizedSearchCV(rf, rf_param, n_iter = random_iters, cv = cv_inner, refit=True, verbose=True)

xgb = XGBRegressor(nthread=1)
xgb_tune = RandomizedSearchCV(xgb, xgb_param, n_iter = random_iters, cv = cv_inner, refit=True, verbose=True)

estimator_list = [lm, lm_log, elastic_tune, knn_tune, rpart_tune, plsr_tune, mlp_tune, svm_tune, lgbm_tune, rf_tune, xgb_tune]



#create dicts for results and coerce to data.frame

response_results = []

for response in response_vars[:1]:

    response_rm = [i for i in response_vars if i != response]

    df_train_sub = df_train.drop(response_rm, axis=1).dropna()
    df_test_sub = df_test.drop(response_rm, axis=1).dropna()

    X_train = df_train_sub.drop(response, axis=1)
    Y_train = df_train_sub[response]

    estimator_results = []

    for estimator in estimator_list:

        cv_scores = cross_validate(estimator, X_train, Y_train, cv = cv_outer, scoring = metrics, return_train_score=True, n_jobs=10)

        scores_df = pd.DataFrame.from_dict(cv_scores)

        estimator_results.append(scores_df)

    response_results.append(estimator_results)

results_df = pd.DataFrame.from_records(response_results)