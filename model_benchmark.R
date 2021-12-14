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

response_vars_2 <- paste0(response_vars, "_response")

bmr_result_list <- list()

#Training loop for lake data
for(i in response_vars_2){
  print(paste0("-----Processing ", i, " as the response variable-----"))
  
  response_var <- i
  response_remove <- setdiff(response_vars_2, response_var)
  
  data_train_var <- data_train %>%
    select(-all_of(response_remove)) %>% 
    as.data.frame()
  
  task_train <- makeRegrTask(data=data_train_var, target=response_var)
  
  parallelStart(mode = "socket", cpus=8, mc.set.seed = TRUE, level = "mlr.resample")
  
  benchmark_regr = benchmark(learners = lrn.list,
                             tasks = task_train,
                             resamplings = cv_outer,
                             measures = regr_measures,
                             models = FALSE,
                             keep.pred = FALSE)
  
  parallelStop()
  
  aggr_df <- getBMRAggrPerformances(benchmark_regr, as.df = TRUE) %>% 
    mutate(response = response_var)
  
  resamples_df <- getBMRPerformances(benchmark_regr, as.df = TRUE) %>% 
    mutate(response = response_var)
  
  print(aggr_df)
  
  bmr_result_list[[response_var]] <- list("aggr" = aggr_df, "resamples" = resamples_df)
  
}

saveRDS(bmr_result_list, paste0(getwd(), "/data/", "model_bmr.rds"))


#getParamSet("regr.ranger")

#log10 responses transform, tjek NA/infs values

