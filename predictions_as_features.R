#Train models for each of the eight response variables using the remaining seven as predictors (values predicted by the models)

source("model_setup.R")

#Transform response variables
response_df <- readRDS(paste0(getwd(), "/data/", "response_vars.rds")) %>% 
  mutate_at(vars(chl_a, color, ph, tn, tp, secchi, pco2), ~log10(.x)) %>% 
  mutate_at(vars(alk), ~log10(.x + 1))

#Load and subset predictions from final models for all Danish lakes
all_predict_subset <- read_csv(paste0(getwd(), "/data/", "all_predict.csv")) %>% 
  select(-lake_id, -basin_id, -catch_id) %>% 
  filter(gml_id %in% response_df$gml_id)
names(all_predict_subset)[-1] <- paste0(names(all_predict_subset)[-1], "_predicted")

#Adjust ranger hyperparameters to a reduced number of predictors
ps.randomforest_small = makeParamSet(
  makeIntegerParam("mtry", lower = 2, upper = 7), 
  makeIntegerParam("num.trees", lower = 250, upper = 2500),
  makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
  makeIntegerParam("min.node.size", lower = 1, upper = 20)
)

#Create ranger learner
lrn.ranger_small = makeTuneWrapper(makeLearner("regr.ranger", num.threads=5), resampling = cv_inner, par.set = ps.randomforest_small, control = tune_random) 

cv_outer = makeResampleDesc("CV", iters = 5)

cv_resampling <- list()

#Training loop for lake data
for(i in response_vars){
  print(paste0("-----Processing ", i, " as the response variable-----"))
  
  response_remove <- c(setdiff(response_vars, i), paste0(i, "_predicted"))
  
  data_train_var <- response_df %>%
    left_join(all_predict_subset) %>% 
    dplyr::select(-all_of(response_remove), -gml_id) %>% 
    as.data.frame() %>% 
    na.omit()
  
  task_train <- makeRegrTask(data=data_train_var, target=i)
  
  #Train model
  parallelStart(mode = "socket", cpus=5, mc.set.seed = TRUE, level = "mlr.resample")
  
  response_cv = resample(learner = lrn.ranger_small, task = task_train, resampling = cv_outer, measures = regr_measures)
  
  parallelStop()
  
  cv_resampling[[i]] <- response_cv$measures.test
}

#Save all results from training and evaluation og models
saveRDS(cv_resampling, paste0(getwd(), "/data/", "predictions_as_features_results.rds"))
