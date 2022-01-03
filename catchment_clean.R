source("libs_and_funcs.R")

#Lakes
lakes <- st_read(gis_database, "lakes_grass")

lakes_gml_ids <- lakes %>% 
  st_drop_geometry() %>% 
  select(gml_id, lake_id, basin_id)

#Load and combine delineated catchments
catchments_path <- list.files(paste0(getwd(), "/data/catchments_sub"), full.names = TRUE, pattern = "*.shp")

catch_list <- lapply(catchments_path, function(path){
  st_read(path) %>% 
    st_make_valid() %>% 
    st_cast("MULTIPOLYGON")
})

catch_all_raw <- do.call(rbind, catch_list)

catch_all <- catch_all_raw %>% 
  left_join(lakes_gml_ids) %>% 
  arrange(basin_id, lake_id)

#Write to database and shapefile
st_write(catch_all, gis_database, "catchments_raw", delete_layer = TRUE)

catchment_path <- paste0(getwd(), "/data/catchments_raw.shp")
st_write(catch_all, catchment_path)

#Use mapshaper cmd line functions to simplify polygons
catchment_simple_path <- paste0(getwd(), "/data/catch_all_simple.shp")

simplify_cmd <- paste("mapshaper-xl 16gb", catchment_path, "-simplify", "10%", "keep-shapes", "-o", catchment_simple_path)
system(simplify_cmd)

#Read simplified catchment and write to database
catch_all_simple_raw <- st_read(catchment_simple_path)

catch_all_simple <- catch_all_simple_raw %>% 
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
geoms_no_lake <- mapply(function(c, l){st_difference(c, l)}, catch_all_simple$geometry, lakes_no_hole$geom)

catch_all_no_lake <- catch_all_simple
catch_all_no_lake$geometry <- st_sfc(geoms_no_lake)
catch_all_no_lake_clean <- catch_all_no_lake %>% 
  st_set_crs(st_crs(catch_all_simple)) %>% 
  filter(!st_is_empty(geometry)) %>% #remove one empty geometry
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON")

st_write(catch_all_no_lake_clean, gis_database, "catch_all_no_lake", delete_layer = TRUE)

#Function to get lake buffer areas without the lake itself
buffers <- c(50, 100, 250)

for(b in buffers){
  lake_geom <- lapply(lakes_no_hole$geom, function(l){st_difference(st_buffer(l, b), l)})
  
  lakes_buf <- lakes_no_hole
  lakes_buf$geom <- st_sfc(lake_geom)
  lakes_buf_clean <- lakes_buf %>% 
    st_set_crs(st_crs(lakes_no_hole)) %>% 
    st_make_valid() %>% 
    st_cast("POLYGON")
  
  st_write(lakes_buf_clean, gis_database, paste0("lakes_", b), delete_layer = TRUE)
}
