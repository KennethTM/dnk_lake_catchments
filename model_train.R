#Train the best performing model for each response variable on the entire training data
#Predict on all lakes
#Assess variable importance

source("model_setup.R")

#Use best model found in model selectin and increase tuning budget for final training
tune_random_final = makeTuneControlRandom(budget = 100)
lrn.ranger_final = makeTuneWrapper(makeLearner("regr.ranger", num.threads=5), resampling = cv_inner, par.set = ps.randomforest, control = tune_random_final) 

#Create list with best learners for each response variable
best_models <- list("alk" = lrn.ranger_final, "chl_a" = lrn.ranger_final, "color" =lrn.ranger_final, "ph" = lrn.ranger_final,
                    "tn" = lrn.ranger_final, "tp" = lrn.ranger_final, "secchi" = lrn.ranger_final, "pco2" = lrn.ranger_final)

models <- list()
predictions <- list()
performance <- list()
importance <- list()
ale <- list()

#Training loop for lake data
for(i in response_vars){
  print(paste0("-----Processing ", i, " as the response variable-----"))
  
  response_remove <- setdiff(response_vars, i)
  
  data_train_var <- data_train %>%
    dplyr::select(-all_of(response_remove)) %>% 
    as.data.frame() %>% 
    na.omit()
  
  task_train <- makeRegrTask(data=data_train_var, target=i)
  
  data_test_var <- data_test %>%
    dplyr::select(-all_of(response_remove)) %>% 
    as.data.frame() %>% 
    na.omit()
  
  task_test <- makeRegrTask(data=data_test_var, target=i)
  
  #Train model
  parallelStart(mode = "socket", cpus=10, mc.set.seed = TRUE, level = "mlr.tuneParams")
  
  fit <- mlr::train(best_models[[i]], task = task_train)
  
  parallelStop()
  
  #Predict on test set
  pred <- predict(fit, task_test)
  
  #Asses performance on test set
  perf <- performance(pred, measures = list(mlr::rmse, mlr::mae, mlr::rsq))
  
  #Feature importance
  model_iml <- Predictor$new(fit, data = data_test_var, y = i)
  
  feature_importance <- FeatureImp$new(model_iml, loss = "rmse")
  
  #ALE effects
  ale_effects <- FeatureEffects$new(model_iml, features = feature_importance$results$feature, method = "ale", grid.size = 30)
  
  models[[i]] <- fit
  predictions[[i]] <- pred
  performance[[i]] <- perf
  importance[[i]] <- feature_importance$results
  ale[[i]] <- ale_effects$results
}

model_results <- list("models" = models,
                      "predictions" = predictions,
                      "performance" = performance,
                      "importance" = importance,
                      "ale" = ale)

#Save all results from training and evaluation og models
saveRDS(model_results, paste0(getwd(), "/data/", "model_results.rds"))

#Predict for all lakes in denmark
all_features <- readRDS(paste0(getwd(), "/data/", "all_features.rds"))

#Combine data
data_all <- all_features$df_other %>% 
  left_join(all_features$df_buffer) %>% 
  left_join(all_features$df_catch)

#Preproces data in the same way as for modeling
data_all_preproc <- bake(data_recipe, new_data = data_all) %>% 
  as.data.frame()

#Predict
predict_all <- lapply(model_results$models, \(model){predict(model, newdata=data_all_preproc)$data})

#Convert to data frame and save
predict_all_df <- bind_cols(predict_all) %>% 
  set_names(names(model_results$models)) %>% 
  bind_cols(dplyr::select(data_all, contains("id"))) %>% 
  mutate_at(dplyr::vars(chl_a, color, ph, tn, tp, secchi, pco2), ~10^.x) %>% 
  mutate_at(dplyr::vars(alk), ~(10^.x)-1) %>% 
  relocate(contains("_id"))

write_csv(predict_all_df, paste0(getwd(), "/data/", "all_predict.csv"))
