source("libs_and_funcs.R")

#Lakes
lakes <- st_read(gis_database, "lakes_grass") %>% 
  rename(basin_id = basin_grass_id)

lakes_gml_ids <- lakes %>% 
  st_drop_geometry() %>% 
  select(gml_id, lake_id, basin_id)

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

#Remove lake polygon from catchment
lakes_no_hole <- lakes %>% 
  filter(lake_id %in% catch_all_simple$lake_id) %>% 
  arrange(basin_id, lake_id) %>% 
  st_remove_holes() 

st_write(lakes_no_hole, gis_database, "lakes_no_hole", delete_layer = TRUE)

#Get difference between lake and catchment
geoms_no_lake <- mapply(function(c, l){st_difference(c, l)}, catch_all_simple$GEOMETRY, lakes_no_hole$GEOMETRY)

catch_all_no_lake <- catch_all_simple
catch_all_no_lake$GEOMETRY <- st_sfc(geoms_no_lake)
catch_all_no_lake_clean <- catch_all_no_lake %>% 
  st_set_crs(st_crs(catch_all_simple)) %>% 
  filter(!st_is_empty(GEOMETRY)) %>% #remove one empty geometry
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON")

st_write(catch_all_no_lake_clean, gis_database, "catch_all_no_lake", delete_layer = TRUE)

#Function to get lake buffer area without the lake itself
lake_geom_50 <- lapply(lakes_no_hole$GEOMETRY, function(l){st_difference(st_buffer(l, 50), l)})
lake_geom_250 <- lapply(lakes_no_hole$GEOMETRY, function(l){st_difference(st_buffer(l, 250), l)})

lakes_50 <- lakes_no_hole
lakes_50$GEOMETRY <- st_sfc(lake_geom_50)
lakes_50_clean <- lakes_50 %>% 
  st_set_crs(st_crs(lakes_no_hole)) %>% 
  st_make_valid() %>% 
  st_cast("POLYGON")

lakes_250 <- lakes_no_hole
lakes_250$GEOMETRY <- st_sfc(lake_geom_250)
lakes_250_clean <- lakes_250 %>% 
  st_set_crs(st_crs(lakes_no_hole)) %>% 
  st_make_valid() %>% 
  st_cast("POLYGON")

st_write(lakes_50_clean, gis_database, "lakes_50", delete_layer = TRUE)
st_write(lakes_250_clean, gis_database, "lakes_250", delete_layer = TRUE)
