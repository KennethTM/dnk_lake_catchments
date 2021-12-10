source("libs_and_funcs.R")

#Extract features
#catch: mean - slope, curvature, aspect, elev, dsm, soil, clc, climate
#buffer 50: mean - slope, curvature, aspect, elev, dsm
#buffer 50: max - slope
#buffer 250: mean - slope, curvature, aspect, elev, dsm
#other: ice cover, dist to ice, dist to coastline, lake area, lake circum, lake shoreline idx, catch area, catch circum, lake area, stream length

#Feature extraction for catchment and lake buffers
catch_all_no_lake <- st_read(gis_database, "catch_all_no_lake")
lakes <- st_read(gis_database, "lakes_grass") %>% 
  rename(basin_id = basin_grass_id)
lakes_50 <- st_read(gis_database, "lakes_50")
lakes_250 <- st_read(gis_database, "lakes_250")
dk_border <- st_read(gis_database, layer = "dk_border")

#Topograhical features
topo_features <- list.files(rawdata_path, pattern = "*_10m.tif", full.names = TRUE)
topo_stack <- stack(topo_features)

topo_catch <- exact_extract(topo_stack, catch_all_no_lake, fun="mean", max_cells_in_memory = 2e+09)
names(topo_catch) <- paste0(names(topo_catch), ".catch")

topo_50 <- exact_extract(topo_stack, lakes_50, fun="mean", max_cells_in_memory = 2e+09)
names(topo_50) <- paste0(names(topo_50), ".buffer_50")

topo_250 <- exact_extract(topo_stack, lakes_250, fun="mean", max_cells_in_memory = 2e+09)
names(topo_250) <- paste0(names(topo_250), ".buffer_250")

topo_max_slope_50 <- exact_extract(topo_stack[["slope_10m"]], lakes_50, fun="max", max_cells_in_memory = 2e+09, force_df=TRUE)
names(topo_max_slope_50) <- "max.slope_10m.buffer_50"

df_buffer_50 <- cbind(st_drop_geometry(lakes_50), topo_50, topo_max_slope_50)
df_buffer_250 <- cbind(st_drop_geometry(lakes_250), topo_250)

#Create clc stack and extract
clc_raster <- raster(paste0(rawdata_path, "corine_land_cover.tif"))
clc_id_unique <- na.omit(unique(clc_raster[]))
clc_stack <- stack(lapply(clc_id_unique, function(x){(clc_raster == x)}))
names(clc_stack) <- paste0("clc_", clc_id_unique)

clc_catch <- exact_extract(clc_stack, catch_all_no_lake, fun="mean")

#Create soil stack and extract
soil_raster <- raster(paste0(rawdata_path, "geus_soil.tif"))
soil_id_unique <- na.omit(unique(soil_raster[]))
soil_stack <- stack(lapply(soil_id_unique, function(x){(soil_raster == x)}))
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

lake_attr <- lakes %>%
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY)),
         lake_shoreline_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))),
         lake_dev_ind = lake_shoreline_m/(2*sqrt(pi*lake_area_m2))) %>% 
  mutate(ice_covered = as.numeric(st_intersects(dk_iceage, st_centroid(GEOMETRY), sparse = FALSE)),
         ice_line_dist = as.numeric(st_distance(dk_iceage_line, st_centroid(GEOMETRY), by_element=TRUE)),
         shoreline_dist = as.numeric(st_distance(st_cast(dk_border, "MULTILINESTRING"), st_centroid(GEOMETRY), by_element=TRUE))) %>% 
  st_drop_geometry()

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

#Collect/combine dataframes in list and save/write csv
df_other <- lake_attr %>% 
  left_join(catch_attr) %>% 
  left_join(catch_lake_area) %>% 
  left_join(catch_stream_length) %>% 
  mutate(catch_stream_length_m = ifelse(is.na(catch_stream_length_m), 0, catch_stream_length_m),
         catch_lake_area_m2 = ifelse(is.na(catch_lake_area_m2), 0, catch_lake_area_m2))

df_catch <- cbind(st_drop_geometry(catch_all_no_lake), topo_catch, clc_catch, soil_catch, climate_catch)

all_features <- list("df_buffer_50" = df_buffer_50, "df_buffer_250" = df_buffer_250, "df_catch" = df_catch, "df_other" = df_other)
saveRDS(all_features, paste0(getwd(), "/data/", "all_features.rds"))


####250 buffer clc??
