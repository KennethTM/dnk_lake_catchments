source("libs_and_funcs.R")
library(rmapshaper)

#Lakes
lakes <- st_read(gis_database, "lakes_grass")
lakes_gml_ids <- lakes %>% 
  st_drop_geometry() %>% 
  select(gml_id, lake_id, basin_id = basin_grass_id)

#Load and combine delineated catchments
catchments_path <- list.files(paste0(getwd(), "/data/catchments_sub"), full.names = TRUE, pattern = "*.shp")

catch_list <- lapply(catchments_path, st_read)

#Add gml_id, remove some duplicate rows and write to database
catch_all <- do.call(rbind, catch_list) %>% 
  left_join(lakes_gml_ids) %>% 
  filter(!duplicated(.)) %>% 
  arrange(basin_id, lake_id) %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON")

st_write(catch_all, gis_database, "catchments_raw", delete_layer = TRUE)

# #Simplify and write to database
# catch_all_simple <- catch_all %>% 
#   ms_simplify(keep = 0.2, keep_shapes = TRUE, sys = TRUE) %>% 
#   st_make_valid() %>% 
#   st_cast("MULTIPOLYGON")
# 
# st_write(catch_all_simple, gis_database, "catchments_simple", delete_layer = TRUE)

#save shape, exec in cmd line

####


lakes %>% 
  filter(!(lake_id %in% catch_all$lake_id)) %>% 
  st_write("lakes_not_delin.sqlite")

#st_buffer

#ms_erase for catchment and buffers

#exactextract cli