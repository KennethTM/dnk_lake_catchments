source("libs_and_funcs.R")
library(mapview)

geo_list <- readRDS(paste0(getwd(), "/geo_list.rds"))

grass_basins <- st_read(gis_database, layer = "grass_basins")

mapview(grass_basins)

grass_sub_id <- 47

grass_basins_sub <- grass_basins %>% 
  filter(basin_id == grass_sub_id) %>% 
  st_transform(4326)

catchment_centroids <- st_centroid(geo_list$catchments)

catchment_centroids_sub <- st_intersection(grass_basins_sub, catchment_centroids)

catchments_sub <- geo_list$catchments %>% 
  filter(lake_id %in% catchment_centroids_sub$lake_id)

lakes_sub <- geo_list$lakes %>%
  filter(lake_id %in% catchment_centroids_sub$lake_id)

st_write(catchments_sub, "catchment.geojson", delete_dsn = TRUE)
st_write(lakes_sub, "lakes.geojson", delete_dsn = TRUE)

