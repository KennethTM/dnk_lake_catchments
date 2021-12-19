#Benchmark candidate predictive models

source("model_setup.R")

library(parallelMap)

#Define learners
lrn.nofeats = makeLearner("regr.featureless")

lrn.lm = makeLearner("regr.lm")

lrn.elastic = makeTuneWrapper("regr.glmnet", resampling = cv_inner, par.set = ps.elastic, control = tune_random)

lrn.fnn = makeTuneWrapper("regr.fnn", resampling = cv_inner, par.set = ps.fnn, control = tune_random)

lrn.rpart = makeTuneWrapper("regr.rpart", resampling = cv_inner, par.set = ps.rpart, control = tune_random) 

lrn.plsr = makeTuneWrapper("regr.plsr", resampling = cv_inner, par.set = ps.plsr, control = tune_random) 

lrn.nnet = makeTuneWrapper("regr.nnet", resampling = cv_inner, par.set = ps.nnet, control = tune_random)

lrn.svm = makeTuneWrapper("regr.svm", resampling = cv_inner, par.set = ps.svm, control = tune_random)

lrn.ranger = makeTuneWrapper("regr.ranger", resampling = cv_inner, par.set = ps.randomforest, control = tune_random) 

#xgboost nthread=more? ranger num.threads
#glmnet lambda vs s
#stacked learner from 3 best or different learners, elastic, plsr, nnet, ranger
lrn.xgboost = makeTuneWrapper(makeLearner("regr.xgboost", nthread = 1), resampling = cv_inner, par.set = ps.xgboost, control = tune_random) 

lrn.list = list(lrn.nofeats, lrn.lm, lrn.elastic, lrn.fnn, lrn.rpart, lrn.plsr, lrn.nnet, lrn.svm, lrn.ranger, lrn.xgboost)
#lrn.list = list(lrn.nofeats, lrn.lm, lrn.elastic, lrn.fnn, lrn.rpart, lrn.plsr)

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
  
  parallelStart(mode = "socket", cpus=10, mc.set.seed = TRUE, level = "mlr.resample")
  
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

#getParamSet("regr.ranger")
