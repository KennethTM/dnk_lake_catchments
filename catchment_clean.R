source("libs_and_funcs.R")
library(rmapshaper)

#lakes
lakes <- st_read(gis_database, "lakes_grass")
lakes_gml_ids <- lakes %>% 
  st_drop_geometry() %>% 
  select(gml_id = lake_name, lake_id, basin_id = basin_grass_id)

#clean delineated catchments
catchments_path <- list.files(paste0(getwd(), "/data/catchments_sub"), full.names = TRUE, pattern = "*.shp")

catch_list <- lapply(catchments_path, st_read)

#add gml_id and save raw catchments
catch_all <- do.call(rbind, catch_list) %>% 
  left_join(lakes_gml_ids) %>% 
  arrange(basin_id, lake_id)
  
st_write(catch_all, gis_database, "catch_all_raw", delete_layer = TRUE)

#simplify, clean, add gml_id and save
clean_and_simplify <- function(sf, keep){
  sf %>% 
    ms_simplify(keep_shapes = TRUE, keep = keep, sys = TRUE) %>% 
    st_make_valid() %>% 
    st_cast("MULTIPOLYGON")
}

catch_clean_list <- lapply(catch_list, clean_and_simplify, keep = 0.1)

catch_all_clean <- do.call(rbind, catch_clean_list) %>%
  left_join(lakes_gml_ids) %>%
  arrange(basin_id, lake_id)

st_write(catch_all_clean, gis_database, "catch_all_simple", delete_layer = TRUE)

