source("libs_and_funcs.R")

response_df <- readRDS(paste0(getwd(), "/data/", "response_vars.rds"))
all_features <- readRDS(paste0(getwd(), "/data/", "all_features.rds"))

data_raw <- response_df %>% 
  left_join(all_features$df_buffer) %>% 
  left_join(all_features$df_catch) %>% 
  left_join(all_features$df_other) %>% 
  select(-lake_id, -basin_id, -catch_id, -gml_id) %>% 
  mutate_at(vars(chl_a, color, ph, tn, tp, secchi, pco2), ~log10(.x)) %>% 
  mutate_at(vars(alk), ~log10(.x + 1))

summary(data_raw)

#Split data set
data_split <- initial_split(data_raw, prop = 4/5)
data_train <- training(data_split)
data_test <- testing(data_split)

preproc_recipe <- recipe(alk + chl_a + color + ph + tn + tp + secchi + pco2 ~ . , data = data_train) %>% 
  step_impute_median(all_predictors()) %>% 
  step_rm(lake_bbox_width_m, lake_bbox_height_m) %>% 
  step_nzv(all_predictors()) %>% 
  step_sqrt(contains("mean.soil_"), contains("mean.clc_")) %>% 
  step_YeoJohnson(all_predictors(), -contains("mean.soil_"), -contains("mean.clc_"), -ice_covered, -lake_stream_connect) %>% 
  step_center(all_predictors(), -ice_covered, -lake_stream_connect) %>% 
  step_scale(all_predictors(), -ice_covered, -lake_stream_connect) %>% 
  step_corr(all_predictors(), -ice_covered, -lake_stream_connect, threshold = 0.7, method = "spearman")

recipe_fit <- prep(preproc_recipe, training = data_train)

data_train_preproc <- bake(recipe_fit, new_data = data_train)
data_test_preproc <- bake(recipe_fit, new_data = data_test)

# #Plot response variables
# data_train_preproc %>%
#   select(all_of(response_vars)) %>%
#   gather(variable, value) %>%
#   ggplot(aes(value))+
#   geom_histogram() +
#   facet_wrap(variable~., scales = "free")
# 
#Plot predictor variables
# data_train_preproc %>%
#   select(-all_of(response_vars)) %>%
#   gather(variable, value) %>%
#   ggplot(aes(value))+
#   geom_histogram() +
#   facet_wrap(variable~., scales = "free")

#Save preprocessed data and recipe
data_preproc <- list("train" = data_train_preproc, "test" = data_test_preproc, "data_recipe" = recipe_fit)

saveRDS(data_preproc, paste0(getwd(), "/data/data_preproc.rds"))
