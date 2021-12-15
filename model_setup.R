#Script containing settings and data used for modeling

source("libs_and_funcs.R")

library(mlr)

#Load data
data_train <- read_csv(paste0(getwd(), "/data/data_train_preproc.csv"))
data_test <- read_csv(paste0(getwd(), "/data/data_test_preproc.csv"))

#Define resample for inner and outer loops
cv_outer = makeResampleDesc("RepCV", folds = 5, reps = 5)
#cv_outer = makeResampleDesc("CV", iters = 10)
cv_inner = makeResampleDesc("CV", iters = 5)

#Tune method
tune_random = makeTuneControlRandom(budget=25)

#tune_mbo = makeTuneControlMBO(budget = 100)

#Measures to report
regr_measures = list(rmse, rsq, mae, timeboth)

#Define hyperparameter search spaces for model training
ps.randomforest = makeParamSet(
  makeIntegerParam("mtry", lower = 2, upper = 50), 
  makeIntegerParam("num.trees", lower = 100, upper = 2000),
  makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
  makeIntegerParam("min.node.size", lower = 1, upper = 20),
  makeDiscreteParam("splitrule", c("variance","extratrees","maxstat")),
  makeLogicalParam("replace", TRUE)
)

ps.fnn = makeParamSet(makeIntegerParam("k", lower = 1, upper = 100))

ps.rpart = makeParamSet(makeNumericParam("cp", lower = 0, upper = 1),
                        makeIntegerParam("maxdepth", lower = 3, upper = 30),
                        makeIntegerParam("minbucket", lower = 5, upper = 50),
                        makeIntegerParam("minsplit", lower = 5, upper = 50))

ps.nnet = makeParamSet(
  makeIntegerParam("size", lower = 1, upper = 20),
  makeNumericParam("decay", lower = -5, upper = 1, trafo = function(x){10^x}),
  makeIntegerParam("maxit", lower = 100, upper = 500)
)

ps.svm = makeParamSet(
  makeNumericParam("cost", -10, 10, trafo = function(x) 2^x),
  makeNumericParam("gamma", -10, 10, trafo = function(x) 2^x)
)

ps.plsr = makeParamSet(
  makeIntegerParam("ncomp", lower = 2, upper = 50),
  makeLogicalParam("scale", FALSE)
)

ps.xgboost <- makeParamSet(
  makeIntegerParam("nrounds", lower = 100, upper = 2000),
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  makeNumericParam("eta", lower = -5, upper = 0, trafo = function(x){10^x}),
  makeNumericParam("subsample", lower = 0.1, upper = 1),
  makeNumericParam("min_child_weight", lower = 0, upper = 20),
  makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x){2^x}),
  makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x){2^x}),
  makeDiscreteParam("booster", c("gbtree","gblinear","dart"))
)
