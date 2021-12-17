#Train the best performing model for each response variable on the entire training data
#Predict on all lakes
#Assess variable importance

source("model_setup.R")

library(parallelMap);library(recipes)

#Create list with best learners for each response variable
#lrn.ranger = makeTuneWrapper("regr.ranger", resampling = cv_inner, par.set = ps.randomforest, control = tune_mbo) 
lrn.ranger = makeLearner("regr.ranger") 

best_models <- list("alk" = lrn.ranger, "chl_a" = lrn.ranger, "color" =lrn.ranger, "ph" = lrn.ranger,
                    "tn" = lrn.ranger, "tp" = lrn.ranger, "secchi" = lrn.ranger, "pco2" = lrn.ranger)

fitted_models <- list()
test_predictions <- list()
test_performance <- list()

#Training loop for lake data
for(i in response_vars){
  print(paste0("-----Processing ", i, " as the response variable-----"))
  
  response_remove <- setdiff(response_vars, i)
  
  data_train_var <- data_train %>%
    select(-all_of(response_remove)) %>% 
    as.data.frame() %>% 
    na.omit()
  
  task_train <- makeRegrTask(data=data_train_var, target=i)
  
  data_test_var <- data_test %>%
    select(-all_of(response_remove)) %>% 
    as.data.frame() %>% 
    na.omit()
  
  task_test <- makeRegrTask(data=data_test_var, target=i)
  
  #Train model
  #parallelStart(mode = "socket", cpus=10, mc.set.seed = TRUE, level = "mlr.tuneParams")
  
  fit <- train(best_models[[i]], task = task_train)
  
  #parallelStop()
  
  #Predict on test set
  pred <- predict(fit, task_test)
  
  #Asses performance on test set
  perf <- performance(pred, measures = list(rmse, mae, rsq))
  
  
  #Feature importance
  #ALE effects
  
  
  fitted_models[[i]] <- fit
  test_predictions[[i]] <- pred
  test_performance[[i]] <- perf
  
}

saveRDS(fitted_models, paste0(getwd(), "/data/", "fitted_models.rds"))
saveRDS(test_predictions, paste0(getwd(), "/data/", "test_predictions.rds"))
saveRDS(test_performance, paste0(getwd(), "/data/", "test_performance.rds"))

#Predict for all lakes in denmark
all_features <- readRDS(paste0(getwd(), "/data/", "all_features.rds"))
preproc_recipe <- readRDS(paste0(getwd(), "/data/", "preproc_recipe_fit.rds"))

#Combine data
data_all <- all_features$df_other %>% 
  left_join(all_features$df_buffer_50) %>% 
  left_join(all_features$df_buffer_250) %>% 
  left_join(all_features$df_catch)

#Preproces data in the same way as for modeling
data_all_preproc <- bake(preproc_recipe, new_data = data_all)

#Predict
predict_all <- lapply(fitted_models, \(model){predict(model, newdata=as.data.frame(data_all_preproc))$data})

#Convert to data frame and save
predict_all_df <- bind_cols(predict_all) %>% 
  set_names(names(fitted_models)) %>% 
  bind_cols(select(data_all, contains("id"))) %>% 
  mutate_at(vars(chl_a, color, ph, tn, tp, secchi, pco2), ~10^.x) %>% 
  mutate_at(vars(alk), ~(10^.x)-1) %>% 
  relocate(contains("_id"))

write_csv(predict_all_df, paste0(getwd(), "/data/", "predict_all_df.csv"))
