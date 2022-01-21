#Script containing settings and data used for modeling

source("libs_and_funcs.R")

#Load data
data_preproc <- readRDS(paste0(getwd(), "/data/data_preproc.rds"))

data_train <- data_preproc$train
data_test <- data_preproc$test
data_recipe <- data_preproc$data_recipe

#Define resample for inner and outer loops
cv_outer = makeResampleDesc("RepCV", reps=6, folds=5)
#cv_outer = makeResampleDesc("CV", iters = 5)
cv_inner = makeResampleDesc("CV", iters = 4)

#Tune method
tune_random = makeTuneControlRandom(budget = 30)

#Measures to report
regr_measures = list(rmse, rsq, mae, timeboth)

#Define hyperparameter search spaces for model training
ps.randomforest = makeParamSet(
  makeIntegerParam("mtry", lower = 2, upper = 50), 
  makeIntegerParam("num.trees", lower = 250, upper = 2500),
  makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
  makeIntegerParam("min.node.size", lower = 1, upper = 20)
)

ps.fnn = makeParamSet(makeIntegerParam("k", lower = 1, upper = 100))

ps.elastic = makeParamSet(makeNumericParam("alpha", lower = 0, upper = 1),
                          makeNumericParam("s", lower = 0, upper = 500),
                          makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x){2^x}))

ps.rpart = makeParamSet(makeNumericParam("cp", lower = 0, upper = 1),
                        makeIntegerParam("maxdepth", lower = 3, upper = 30),
                        makeIntegerParam("minbucket", lower = 5, upper = 50),
                        makeIntegerParam("minsplit", lower = 5, upper = 50))

ps.nnet = makeParamSet(
  makeIntegerParam("size", lower = 2, upper = 20),
  makeNumericParam("decay", lower = -5, upper = 1, trafo = function(x){10^x})
)

ps.svm = makeParamSet(
  makeNumericParam("cost", -10, 10, trafo = function(x) 2^x),
  makeNumericParam("gamma", -10, 10, trafo = function(x) 2^x)
)

ps.plsr = makeParamSet(
  makeIntegerParam("ncomp", lower = 2, upper = 50)
)

#Define learners
lrn.nofeats = makeLearner("regr.featureless")

lrn.lm = makeLearner("regr.lm")

lrn.elastic = makeTuneWrapper("regr.glmnet", resampling = cv_inner, par.set = ps.elastic, control = tune_random)

lrn.fnn = makeTuneWrapper("regr.fnn", resampling = cv_inner, par.set = ps.fnn, control = tune_random)

lrn.rpart = makeTuneWrapper("regr.rpart", resampling = cv_inner, par.set = ps.rpart, control = tune_random) 

lrn.plsr = makeTuneWrapper("regr.plsr", resampling = cv_inner, par.set = ps.plsr, control = tune_random) 

lrn.nnet = makeTuneWrapper(makeLearner("regr.nnet", maxit=400, MaxNWts = 2000), resampling = cv_inner, par.set = ps.nnet, control = tune_random)

lrn.svm = makeTuneWrapper("regr.svm", resampling = cv_inner, par.set = ps.svm, control = tune_random)

lrn.ranger = makeTuneWrapper(makeLearner("regr.ranger", num.threads=5), resampling = cv_inner, par.set = ps.randomforest, control = tune_random) 
