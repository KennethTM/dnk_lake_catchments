source("libs_and_funcs.R")

#Extract features

#Feature extraction for catchment and lake buffers
catch_all_no_lake <- st_read(gis_database, "catch_all_no_lake")
lakes <- st_read(gis_database, "lakes_grass")
lakes_50 <- st_read(gis_database, "lakes_50")
lakes_100 <- st_read(gis_database, "lakes_100")
lakes_250 <- st_read(gis_database, "lakes_250")
dk_border <- st_read(gis_database, layer = "dk_border")

#Topographical features
topo_features <- list.files(rawdata_path, pattern = "*_10m.tif", full.names = TRUE)
topo_stack <- stack(topo_features[!grepl("dhym*", topo_features)])

poly_list <- list(catch_all_no_lake, lakes_50, lakes_100, lakes_250)
suffix_list <- list(".catch", ".buffer_50", ".buffer_100", ".buffer_250")

topo_mean_df <- mapply(function(poly, suffix){
  df <- exact_extract(topo_stack, poly, fun="mean", max_cells_in_memory = 2e+09)
  names(df) <- paste0(names(df), suffix)
  return(df)}, 
  poly_list, suffix_list, SIMPLIFY = FALSE)

topo_range_df <- mapply(function(poly, suffix){
  df <- exact_extract(topo_stack[[c("dtm_10m", "dsm_10m", "slope_10m")]], poly, fun=c("min", "max"), max_cells_in_memory = 2e+09)
  names(df) <- paste0(names(df), suffix)
  return(df)}, 
  poly_list, suffix_list, SIMPLIFY = FALSE)

#Create clc stack and extract
clc_raster <- raster(paste0(rawdata_path, "corine_land_cover.tif"))
clc_id_unique <- na.omit(unique(clc_raster[]))
clc_stack <- stack(lapply(clc_id_unique, \(x){(clc_raster == x)}))
names(clc_stack) <- paste0("clc_", clc_id_unique)

clc_catch <- exact_extract(clc_stack, catch_all_no_lake, fun="mean")

#Create soil stack and extract
soil_raster <- raster(paste0(rawdata_path, "geus_soil.tif"))
soil_id_unique <- na.omit(unique(soil_raster[]))
soil_stack <- stack(lapply(soil_id_unique, \(x){(soil_raster == x)}))
names(soil_stack) <- paste0("soil_", soil_id_unique)

soil_catch <- exact_extract(soil_stack, catch_all_no_lake, fun="mean")

#Climate
target_extent <- as.numeric(st_bbox(dk_border))

#Stack climate rasters and extract
climate_stack <- stack(list.files(rawdata_path, pattern = "worldclim*", full.names = TRUE))
names(climate_stack) <- gsub("worldclim_", "", names(climate_stack))
climate_catch <- exact_extract(climate_stack, catch_all_no_lake, fun="mean")

#Other features
dk_iceage <- st_read(gis_database, layer = "dk_iceage")

dk_iceage_line <- dk_iceage %>% 
  st_cast("LINESTRING") %>% 
  st_intersection(dk_border) %>% 
  st_collection_extract("LINESTRING")

lakes_centroid <- st_centroid(st_geometry(lakes))

lake_attr <- lakes %>%
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY)),
         lake_shoreline_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))),
         lake_dev_ind = lake_shoreline_m/(2*sqrt(pi*lake_area_m2))) %>% 
  mutate(ice_covered = as.numeric(st_intersects(dk_iceage, lakes_centroid, sparse = FALSE)),
         ice_line_dist = as.numeric(st_distance(dk_iceage_line, lakes_centroid, by_element=TRUE)),
         shoreline_dist = as.numeric(st_distance(st_cast(dk_border, "MULTILINESTRING"), lakes_centroid, by_element=TRUE))) %>% 
  st_drop_geometry()

# lakes_bbox_dims <- lakes %>%
#   mutate(lake_bbox_width_m = st_dims_by_feature(GEOMETRY, mode = "width"),
#          lake_bbox_height_m = st_dims_by_feature(GEOMETRY, mode = "height")) %>%
#   st_drop_geometry()

catch_attr <- catch_all_no_lake %>% 
  mutate(catch_area_m2 = as.numeric(st_area(GEOMETRY)),
         catch_shoreline_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))),
         catch_dev_ind = catch_shoreline_m/(2*sqrt(pi*catch_area_m2))) %>% 
  st_drop_geometry()

#Catchment lake area and stream length
streams <- st_read(gis_database, "streams")

streams_centroid <- streams %>% 
  mutate(stream_length_m = as.numeric(st_length(geometry))) %>% 
  select(stream_length_m) %>% 
  st_centroid()

lakes_centroid <- lakes %>% 
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY))) %>% 
  select(lake_area_m2) %>% 
  st_centroid()

catch_lake_area <- catch_all_no_lake %>%
  st_join(lakes_centroid) %>%
  st_drop_geometry() %>%
  select(basin_id, lake_id, catch_id, gml_id, lake_area_m2) %>%
  group_by(basin_id, lake_id, catch_id, gml_id) %>%
  summarise(catch_lake_area_m2 = sum(lake_area_m2)) %>% 
  ungroup()

catch_stream_length <- catch_all_no_lake %>%
  st_join(streams_centroid) %>%
  st_drop_geometry() %>%
  select(basin_id, lake_id, catch_id, gml_id, stream_length_m) %>%
  group_by(basin_id, lake_id, catch_id, gml_id) %>%
  summarise(catch_stream_length_m = sum(stream_length_m)) %>% 
  ungroup()

#Lake-streams intersections
lake_stream_intersect <- lakes %>% 
  mutate(lake_stream_connect_n = lengths(st_intersects(GEOMETRY, streams)),
         lake_stream_connect = ifelse(lake_stream_connect_n == 0, 0, 1)) %>% 
  st_drop_geometry() %>%
  select(basin_id, lake_id, gml_id, lake_stream_connect_n, lake_stream_connect)

#Collect/combine dataframes in list and save/write csv
df_other <- lake_attr %>% 
  left_join(catch_attr) %>% 
  left_join(catch_lake_area) %>% 
  left_join(catch_stream_length) %>% 
  left_join(lake_stream_intersect) %>% 
  left_join(lakes_bbox_dims) %>% 
  mutate(catch_stream_length_m = ifelse(is.na(catch_stream_length_m), 0, catch_stream_length_m),
         catch_lake_area_m2 = ifelse(is.na(catch_lake_area_m2), 0, catch_lake_area_m2),
         lake_catch_area_ratio = lake_area_m2/catch_area_m2)

df_catch <- cbind(st_drop_geometry(catch_all_no_lake), topo_mean_df[[1]], topo_range_df[[1]], clc_catch, soil_catch, climate_catch) %>% 
  mutate(range.dtm_10m.catch = max.dtm_10m.catch - min.dtm_10m.catch,
         range.dsm_10m.catch = max.dsm_10m.catch - min.dsm_10m.catch) %>% 
  select(-contains("min."), -contains("max."))

df_buffer <- cbind(st_drop_geometry(lakes_50), do.call(cbind, topo_mean_df[2:4]), do.call(cbind, topo_range_df[2:4])) %>% 
  as_tibble() %>% 
  mutate(range.dtm_10m.buffer_50 = max.dtm_10m.buffer_50 - min.dtm_10m.buffer_50,
         range.dtm_10m.buffer_100 = max.dtm_10m.buffer_100 - min.dtm_10m.buffer_100,
         range.dtm_10m.buffer_250 = max.dtm_10m.buffer_250 - min.dtm_10m.buffer_250,
         range.dsm_10m.buffer_50 = max.dsm_10m.buffer_50 - min.dsm_10m.buffer_50,
         range.dsm_10m.buffer_100 = max.dsm_10m.buffer_100 - min.dsm_10m.buffer_100,
         range.dsm_10m.buffer_250 = max.dsm_10m.buffer_250 - min.dsm_10m.buffer_250) %>% 
  select(-contains("max.dtm_10m."), -contains("min.dtm_10m."),
         -contains("max.dsm_10m."), -contains("min.dsm_10m."),
         -contains("min.slope_10m."))

all_features <- list("df_buffer" = df_buffer, "df_catch" = df_catch, "df_other" = df_other)
saveRDS(all_features, paste0(getwd(), "/data/", "all_features.rds"))
