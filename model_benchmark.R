#Benchmark candidate predictive models

source("model_setup.R")

#ADD SVM TO STACK - OR DROP STACK? OR NOCV STACK? REPCV IF NO STACK

lrn.list = list(lrn.nofeats, lrn.lm, lrn.elastic, lrn.fnn, lrn.rpart, lrn.plsr, lrn.nnet, lrn.svm, lrn.ranger, lrn.stacked)

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
  
  parallelStart(mode = "socket", cpus=10, mc.set.seed = TRUE, level = "mlr.tuneParams")
  
  benchmark_regr = benchmark(learners = lrn.list,
                             tasks = task_train,
                             resamplings = cv_outer,
                             measures = regr_measures,
                             models = FALSE,
                             keep.pred = FALSE)
  
  parallelStop()
  
  aggr_df <- getBMRAggrPerformances(benchmark_regr, as.df = TRUE)
  
  resamples_df <- getBMRPerformances(benchmark_regr, as.df = TRUE)
  
  print(aggr_df)
  
  bmr_result_list[[i]] <- list("aggr" = aggr_df, "resamples" = resamples_df)
  
}

saveRDS(bmr_result_list, paste0(getwd(), "/data/", "model_bmr.rds"))

#getParamSet("regr.nnet")
