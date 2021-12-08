source("libs_and_funcs.R")

library(rmapshaper)
library(nngeo)

#Lakes
lakes <- st_read(gis_database, "lakes_grass")
lakes_gml_ids <- lakes %>% 
  st_drop_geometry() %>% 
  select(gml_id, lake_id, basin_id = basin_grass_id)

#Load and combine delineated catchments
catchments_path <- list.files(paste0(getwd(), "/data/catchments_sub"), full.names = TRUE, pattern = "*.shp")

catch_list <- lapply(catchments_path, st_read)

catch_list_simple <- lapply(catch_list, function(sf){ms_simplify(sf, keep = 0.1, keep_shapes = TRUE, snap = TRUE, sys = TRUE)})

#Add gml_id, remove some duplicate rows and write to database
catch_all <- do.call(rbind, catch_list) %>% 
  left_join(lakes_gml_ids) %>% 
  filter(!duplicated(.)) %>% 
  arrange(basin_id, lake_id) %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON")

st_write(catch_all, gis_database, "catchments_raw", delete_layer = TRUE)

catch_all_simple <- do.call(rbind, catch_list_simple) %>% 
  left_join(lakes_gml_ids) %>% 
  filter(!duplicated(.)) %>% 
  arrange(basin_id, lake_id) %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON")

st_write(catch_all_simple, gis_database, "catchments_simple", delete_layer = TRUE)


#####

#Remove lake polygon from catchment
lakes_no_hole <- lakes %>% 
  st_remove_holes() 

catch_all_no_lake <- ms_erase(catch_all_simple, lakes_no_hole, sys = TRUE)

st_write(catch_all_no_lake, gis_database, "catch_all_no_lake", delete_layer = TRUE)





# #Write catchments to .shp file for mapshaper
# catchments_shp <- paste0(getwd(), "/data/catch_all.shp")
# 
# st_write(catch_all, catchments_shp)
# 
# #Use mapshaper cmd line functions to simplify polygons
# catchments_simple_shp <- paste0(getwd(), "/data/catch_all_simple.shp")
# 
# simplify_cmd <- paste("mapshaper-xl 8gb", catchments_shp, "snap", "-simplify", "10%", "keep-shapes", "-o", catchments_simple_shp)
# system(simplify_cmd) 
# 
# #Read simplified polygons and write to database
# catch_all_simple <- st_read(catchments_simple_shp) %>%
#   st_snap(tolerance = 5) %>% 
#   st_make_valid() %>%
#   st_cast("MULTIPOLYGON")
# 
# st_write(catch_all_simple, gis_database, "catchments_simple", delete_layer = TRUE)

#file.remove(c(catchments_shp, catchments_simple_shp))


# #catchment without lake
# left_join(lakes, catch_all_simple) %>% 
#   slice(1:10) %>% 
#   mutate(geometry = ifelse(is.na(catch_id), NA, st_difference(geometry1, geometry2)) %>% 
#   select(-#OLD geoms) %>% 
#   st_set_geometry(geometry) %>% 
#   st_cast("MULTIPOLYGON")

catch_all <- st_read(gis_database, "catchments_raw")
catch_all_simple <- st_read(gis_database, "catchments_simple")


# lakes_50 <- lakes %>% 
#   slice(1:10) %>% 
#   mutate(geometry = st_difference(st_make_valid(st_buffer(st_remove_holes(geometry), 50)), st_make_valid(geometry))) %>% 
#   st_cast("MULTIPOLYGON")

#st_remove_holes, st_buffer, 

#ms_erase for catchment and buffers

#exactextract cli