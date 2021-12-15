source("model_setup.R")

library(parallelMap)

#Define learners
lrn.nofeats = makeLearner("regr.featureless")

lrn.lm = makeLearner("regr.lm") 

lrn.fnn = makeTuneWrapper("regr.fnn", resampling = cv_inner, par.set = ps.fnn, control = tune_random)

lrn.rpart = makeTuneWrapper("regr.rpart", resampling = cv_inner, par.set = ps.rpart, control = tune_random) 

lrn.plsr = makeTuneWrapper("regr.plsr", resampling = cv_inner, par.set = ps.plsr, control = tune_random) 

lrn.nnet = makeTuneWrapper("regr.nnet", resampling = cv_inner, par.set = ps.nnet, control = tune_random)

lrn.svm = makeTuneWrapper("regr.svm", resampling = cv_inner, par.set = ps.svm, control = tune_random)

lrn.ranger = makeTuneWrapper("regr.ranger", resampling = cv_inner, par.set = ps.randomforest, control = tune_random) 

lrn.xgboost = makeTuneWrapper(makeLearner("regr.xgboost", nthread = 1), resampling = cv_inner, par.set = ps.xgboost, control = tune_random) 

lrn.list = list(lrn.nofeats, lrn.lm, lrn.fnn, lrn.rpart, lrn.plsr, lrn.nnet, lrn.svm, lrn.ranger, lrn.xgboost)

bmr_result_list <- list()


#Training loop for lake data
for(i in response_vars){
  print(paste0("-----Processing ", i, " as the response variable-----"))
  
  response_remove <- setdiff(response_vars, i)
  
  data_train_var <- data_train %>%
    select(-all_of(response_remove)) %>% 
    as.data.frame() %>% 
    na.omit()
  
  task_train <- makeRegrTask(data=data_train_var, target=i)
  
  parallelStart(mode = "socket", cpus=8, mc.set.seed = TRUE, level = "mlr.resample")
  
  benchmark_regr = benchmark(learners = lrn.list,
                             tasks = task_train,
                             resamplings = cv_outer,
                             measures = regr_measures,
                             models = FALSE,
                             keep.pred = FALSE)
  
  parallelStop()
  
  aggr_df <- getBMRAggrPerformances(benchmark_regr, as.df = TRUE) %>% 
    mutate(response = i)
  
  resamples_df <- getBMRPerformances(benchmark_regr, as.df = TRUE) %>% 
    mutate(response = i)
  
  print(aggr_df)
  
  bmr_result_list[[i]] <- list("aggr" = aggr_df, "resamples" = resamples_df)
  
}

saveRDS(bmr_result_list, paste0(getwd(), "/data/", "model_bmr.rds"))


getParamSet("regr.nnet")








# library(mlr3)
# library(mlr3misc)
# library(mlr3tuning)
# library(mlr3extralearners)
# 
# tsk_regr = TaskRegr$new("tsk_regr", backend = data_train_var, target = "pco2")
# 
# rsmp_outer_random = rsmp("cv", folds = 10)
# rsmp_inner_random = rsmp("cv", folds = 5)
# 
# rsmp_outer_random$instantiate(tsk_regr)
# 
# lrn_lightgbm = lrn("regr.lightgbm", num_threads=8)
# 
# lgbm_ps = ParamSet$new(
#   params = list(
#     ParamInt$new("num_leaves", lower = 5, upper = 50),
#     ParamInt$new("min_data_in_leaf", lower = 10, upper = 200),
#     ParamDbl$new("bagging_fraction", lower = 0.1, upper = 1),
#     ParamDbl$new("feature_fraction", lower = 0.1, upper = 1),
#     ParamFct$new("objective", levels=c("regression_l1", "regression", "poisson")),
#     ParamFct$new("boosting", levels=c('gbdt', 'dart', 'goss')),
#     ParamFct$new("metric", levels=c("l1", "l2")),
#     ParamInt$new("num_iterations", lower = 50, upper = 500)
#   )
# )
# 
# 
# lrn_wrap = AutoTuner$new(
#   lrn_lightgbm,
#   rsmp_inner_random,
#   msr("regr.mae"),
#   terminator = trm("evals", n_evals = 25),
#   tuner = tnr("random_search"),
#   search_space = lgbm_ps
# )
# 
# measures_regr = list(msr("regr.mae"), msr("regr.rmse"), msr("regr.rsq"))
# 
# tmp=mlr3::resample(tsk_regr, lrn_wrap, rsmp_outer_random)
# tmp$aggregate(measures_regr)
# 
