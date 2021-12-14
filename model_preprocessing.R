source("libs_and_funcs.R")

library(rsample);library(recipes)

response_df <- readRDS(paste0(getwd(), "/data/", "response_df.rds"))
all_features <- readRDS(paste0(getwd(), "/data/", "all_features.rds"))

data_raw <- response_df %>% 
  left_join(all_features$df_buffer_50) %>% 
  left_join(all_features$df_buffer_250) %>% 
  left_join(all_features$df_catch) %>% 
  left_join(all_features$df_other) %>% 
  na.omit() %>% 
  select(-lake_id, -basin_id, -catch_id, -gml_id)

summary(data_raw)

#Split data set
data_split <- initial_split(data_raw, prop = 4/5)
data_train <- training(data_split)
data_test <- testing(data_split)

preproc_recipe <- recipe(alk_response + chl_a_response + color_response + 
                           ph_response + tn_response + tp_response + 
                           secchi_response + pco2_response ~ . , data = data_train) %>% 
  step_zv(all_predictors()) %>% 
  step_log(catch_lake_area_m2, catch_stream_length_m, base = 10, offset=1) %>% 
  step_log(catch_area_m2, contains("_dist"), contains("_shoreline"), base = 10) %>% 
  step_range(all_predictors()) #%>% 
  #step_YeoJohnson(all_predictors()) %>% 
  #step_center(all_predictors(), -ice_covered) %>% 
  #step_scale(all_predictors(), -ice_covered) %>%
  #step_pca(contains("mean.clc_"), threshold = 0.9, prefix = "PC_clc_") %>% 
  #step_pca(contains("mean.soil_"), threshold = 0.9, prefix = "PC_soil_") %>% 
  #step_log(contains("_response"), base = 10)

recipe_fit <- prep(preproc_recipe, training = data_train)

data_train_preproc <- bake(recipe_fit, new_data = data_train)
data_test_preproc <- bake(recipe_fit, new_data = data_test)

# #Plot response variables
# data_train_preproc %>%
#   select(contains("_response")) %>%
#   gather(variable, value) %>%
#   ggplot(aes(value))+
#   geom_histogram() +
#   facet_wrap(variable~., scales = "free")
# 
# #Plot predictor variables
# data_train_preproc %>%
#   select(-contains("_response")) %>%
#   gather(variable, value) %>%
#   ggplot(aes(value))+
#   geom_histogram() +
#   facet_wrap(variable~., scales = "free")

#Save preprocessed data for modelling
data_preproc <- list("train" = data_train_preproc, "test" = data_train_preproc)
saveRDS(data_preproc, paste0(getwd(), "/data/", "data_preproc.rds"))
