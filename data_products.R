source("libs_and_funcs.R")

#Export to data_products folder
data_products_dir <- paste0(getwd(), "/data_products/")

#Catchments (sqlite, shp)
#Simple
catch_all_no_lake <- st_read(gis_database, "catch_all_no_lake") %>% 
  select(gml_id)

st_write(catch_all_no_lake, paste0(data_products_dir, "catchments_simple.sqlite")) 

#Lakes
lakes <- st_read(gis_database, "lakes_grass") %>% 
  select(gml_id)

st_write(lakes, paste0(data_products_dir, "lakes.sqlite")) 

#Models (list rds)
model_results <- readRDS(paste0(getwd(), "/data/", "model_results.rds"))

#Save list of models (both as mlr and "raw" format e.g. as in the underlying packages)
mlr_format <- model_results$models
raw_format <- lapply(mlr_format, \(x) getLearnerModel(x, more.unwrap = TRUE))

model_list <- list("raw" = raw_format, "mlr" = mlr_format)

saveRDS(model_list, paste0(getwd(), "/data_products/", "models.rds"))

#Predictions (csv)
all_predict_subset <- read_csv(paste0(getwd(), "/data/", "all_predict.csv")) %>% 
  select(-lake_id, -basin_id, -catch_id) 

write_csv(all_predict_subset, paste0(getwd(), "/data_products/", "predictions.csv"))
