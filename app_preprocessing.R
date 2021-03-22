source("libs_and_funcs.R")
library(rmapshaper)

#preprocessing of geodata for app

basin_id <- 19

catchments_raw <- st_read(paste0(getwd(), "/data/catchments_sub/basin_catchments_", basin_id, ".shp")) 
#st_write(catchments_raw, gis_database, "basin_catchments_19", delete_layer =  TRUE)

catchments <- catchments_raw %>% 
  ms_simplify(keep_shapes = TRUE) %>% 
  #st_simplify(dTolerance=2, preserveTopology=TRUE) %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON") %>% 
  mutate(area=as.numeric(st_area(geometry)),
         area_label = paste0("Catchment area: ", round(area, 0), " m<sup>2</sup>")) %>% 
  st_transform(4326)

lakes <- st_read(gis_database, "lakes") %>% 
  filter(lake_id %in% catchments$lake_id) %>% 
  ms_simplify(keep_shapes = TRUE) %>% 
  #st_simplify(dTolerance=2, preserveTopology=TRUE) %>% 
  st_make_valid() %>% 
  st_cast("POLYGON") %>% 
  mutate(area=as.numeric(st_area(GEOMETRY)),
         label = paste0("Lake area: ", round(area, 0), " m<sup>2</sup>")) %>% 
  st_transform(4326) %>% 
  select(lake_id, label)

basins <- st_read(gis_database, "basins") %>% 
  ms_simplify(keep_shapes = TRUE) %>% 
  #st_simplify(dTolerance=25, preserveTopology=TRUE) %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_transform(4326) %>% 
  select(basin_id) %>% 
  filter(basin_id == 19)

basins_buf <- basins %>% 
  st_transform(dk_epsg) %>% 
  st_buffer(500) %>% 
  st_transform(4326) %>% 
  st_cast("MULTIPOLYGON") %>% 
  filter(basin_id == 19)

#extract landuse
clc <- st_read(gis_database, layer = "corine_land_cover")
soil <- st_read(gis_database, layer = "geus_soil")

#Define rater template
raster_template <- raster(catchments_raw, res = 50)

#Rasterize soil and clc vectors
raster_soil <- fasterize(soil, raster_template, field = "tsym_id", fun = "first")
raster_clc <- fasterize(clc, raster_template, field = "clc_code", fun = "first")

#Extract landcover attributes
clc_id_unique <- na.omit(unique(raster_clc[]))

raster_clc_values <- lapply(clc_id_unique, function(id){
  
  raster_val <- (raster_clc == id)
  
  extract_val <- exact_extract(raster_val, catchments, fun = "mean")
  
  df <- as.data.frame(extract_val)
  names(df) <- paste0("clc_", id)
  
  return(df)
  
})

raster_clc_values_df <- bind_cols(raster_clc_values)

# #Extract soil attributes
# soil_id_unique <- na.omit(unique(raster_soil[]))
# 
# raster_soil_values <- lapply(soil_id_unique, function(id){
# 
#   raster_val <- (raster_soil == id)
# 
#   extract_val <- exact_extract(raster_val, catchment, fun = "mean")
# 
#   df <- as.data.frame(extract_val)
#   names(df) <- paste0("soil_", id)
# 
#   return(df)
# 
# })
# 
# raster_soil_values_df <- bind_cols(raster_soil_values)

clc_codes <- clc %>% 
  st_drop_geometry() %>% 
  select(clc_code, contains("label")) %>% 
  distinct()

clc_box <- data.frame(catch_id = catchments$catch_id, raster_clc_values) %>%
  gather(cat, value, -catch_id) %>% 
  filter(value != 0) %>%
  mutate(clc_code = parse_number(cat)) %>% 
  left_join(clc_codes) %>% 
  group_by(catch_id, label2) %>% 
  summarise(value = sum(value)) %>% 
  mutate(cat_label = paste0(label2, ": ", round(value*100, 0), " %")) %>% 
  summarise(clc_label = paste0(cat_label, collapse = "<br>"))

catchments_with_cover <- catchments %>% 
  left_join(clc_box) %>% 
  mutate(label = paste0(area_label, "<br><br>", clc_label)) %>% 
  select(lake_id, label)

geo_list <- list("catchments" = catchments_with_cover, "lakes" = lakes,"basins" = basins, "basins_buf" = basins_buf)
saveRDS(geo_list, paste0(getwd(), "/geo_list.rds"))
