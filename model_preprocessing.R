source("libs_and_funcs.R")

library(rsample);library(recipes)

response_df <- readRDS(paste0(getwd(), "/data/", "response_df.rds"))
all_features <- readRDS(paste0(getwd(), "/data/", "all_features.rds"))

data_raw <- response_df %>% 
  left_join(all_features$df_buffer_50) %>% 
  left_join(all_features$df_buffer_250) %>% 
  left_join(all_features$df_catch) %>% 
  left_join(all_features$df_other) %>% 
  select(-lake_id, -basin_id, -catch_id, -gml_id)

summary(data_raw)

#Split data set
data_split <- initial_split(data_raw, prop = 4/5)
data_train <- training(data_split)
data_test <- testing(data_split)

preproc_recipe <- recipe(alk + chl_a + color + ph + tn + tp + secchi + pco2 ~ . , data = data_train) %>% 
  step_impute_median(all_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_log(catch_lake_area_m2, catch_stream_length_m, base = 10, offset=1) %>% 
  step_log(catch_area_m2, contains("_dist"), contains("_shoreline"), base = 10) %>% 
  step_range(all_predictors())

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
# #Plot predictor variables
# data_train_preproc %>%
#   select(-all_of(response_vars)) %>%
#   gather(variable, value) %>%
#   ggplot(aes(value))+
#   geom_histogram() +
#   facet_wrap(variable~., scales = "free")

#Save preprocessed data for modelling
write_csv(data_train_preproc, paste0(getwd(), "/data/data_train_preproc.csv"))
write_csv(data_test_preproc, paste0(getwd(), "/data/data_test_preproc.csv"))
