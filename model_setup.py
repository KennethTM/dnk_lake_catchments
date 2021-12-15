from scipy.stats import randint, uniform, loguniform

response_vars = ["alk", "chl_a", "color", "ph", "tn", "tp", "secchi", "pco2"]

knn_param = {'n_neighbors': randint(2, 50)}

rpart_param = {"max_depth": randint(5, 30),
              "max_features": uniform(0.1, 1),
              "min_samples_leaf": randint(1, 15),
              "criterion": ["gini", "entropy"],
              "ccalpha": uniform(0, 1)}

plsr_param = {'n_components': randint(2, 30)}

mlp_param = {
   'hidden_layer_sizes': [(50, 50, 50), (50, 100, 50), (100, 1)],
   'activation': ['relu'],
   'batch_size': [64, 128, 256],
   'alpha': uniform(0.0001, 0.05),
   'max_iter': randint(200, 2000),
   'learning_rate': ['constant', 'adaptive'],
   'solver': ['sgd', 'adam']
}

svm_param = {'C': loguniform(1e0, 1e3),
            'gamma': loguniform(1e-4, 1e-3),
            'kernel': ['linear', 'poly', 'rbf', 'sigmoid']
} 

lgbm_param ={'num_leaves': randint(5, 50), 
             'min_child_samples': randint(10, 500), 
             'min_child_weight': [1e-5, 1e-3, 1e-2, 1e-1, 1, 1e1, 1e2, 1e3, 1e4],
             'subsample': uniform(0.1, 0.9), 
             'colsample_bytree': uniform(0.1, 0.9),
             'reg_alpha': [0, 1e-1, 1, 2, 5, 7, 10, 50, 100],
             'reg_lambda': [0, 1e-1, 1, 5, 10, 20, 50, 100],
             'n_estimators': randint(50, 2000),
             'boosting_type': ['gbdt', 'dart', 'goss'],
             'objective': ["l1", "l2"],
             'metric': ["l1", "l2"]}

rf_param = {'n_estimators': randint(50, 2000),
            'max_features': uniform(0.1, 0.9),
            'max_depth': randint(10, 100),
            'min_samples_split': randint(2, 10),
            'min_samples_leaf': randint(2, 20),
            'bootstrap': [True, False],
            'criterion': ["squared_error", "absolute_error"]}

xgb_param = {
        'n_estimators': randint(50, 2000),
        "max_depth": randint(1, 9),
        "min_child_weight": randint(1, 20),
        "subsample": uniform(0.1, 0.9),
        "eta": loguniform(1e-4, 1e-1),
     }

elastic_param = {
    'l1_ratio': uniform(0, 1),
    'normalize': [True, False],
    'selection': ['cyclic', 'random'],
    'alpha': loguniform(1e-4, 1e2)
}