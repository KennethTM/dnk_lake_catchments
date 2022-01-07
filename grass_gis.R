source("libs_and_funcs.R")

dk_border <- st_read(gis_database, "dk_border")

dhym_10m_labels <- paste0(getwd(), "/rawdata/dhym_10m_labels.tif")

dhym_labels_raster <- raster(dhym_10m_labels)

#Find grass install
grass_path <- findGRASS()

linkGRASS7(dhym_labels_raster, default_GRASS7 = grass_path,
           gisdbase = paste0(getwd(), "/grass_database"), 
           location = "dhym_10m", gisdbase_exist = TRUE)

#Import dem
execGRASS("r.in.gdal", flags = c("overwrite", "o"), parameters = list(input = dhym_10m_labels, output = "labels"))

#Convert raster to vector file
execGRASS("r.to.vect", flags = c("overwrite", "s"), 
          parameters = list(input = "labels",
                            output = "labels",
                            type = "area"))

#Brug grass.gis v.clean
execGRASS("v.clean", flags = c("overwrite"), 
          parameters = list(input = "labels",
                            output = "labels_clean",
                            tool = rep("rmarea", 7),
                            threshold = 10^seq(2, 8, 1)))

#Import vector file to R and write to gis_database
use_sf()
grass_basins <- readVECT("labels_clean") %>% 
  st_set_crs(dk_epsg) %>% 
  mutate(basin_id = 1:n()) %>% 
  select(basin_id) %>% 
  st_intersection(dk_border) %>% 
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>% 
  select(basin_id)

#Missing danish islands due to erased polygons
grass_basins_union <- st_union(grass_basins)

dk_border_split <- dk_border %>% 
  select(GEOMETRY) %>% 
  st_cast("POLYGON")

dk_border_missing <- dk_border_split[grass_basins_union, , op = st_disjoint] %>% 
  mutate(basin_id = 1000+(1:n())) %>% 
  rename(geom = GEOMETRY) %>% 
  st_cast("MULTIPOLYGON")

#buffer around basins (not islands, those are joined on later)
grass_basins_buffer <- grass_basins %>%
  st_buffer(1000) %>%
  st_intersection(dk_border) %>% 
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>% 
  select(basin_id)

grass_basins_all <- rbind(grass_basins, dk_border_missing)
grass_basins_buffer_all <- rbind(grass_basins_buffer, dk_border_missing)

st_write(grass_basins_all, gis_database, layer = "basins_grass", delete_layer = TRUE)
st_write(grass_basins_buffer_all, gis_database, layer = "basins_grass_buffer", delete_layer = TRUE)

#Read lake polygons
lakes <- st_read(paste0(rawdata_path, "DK_StandingWater.gml"))

lakes_clean <- lakes %>%
  select(gml_id) %>%
  mutate(lake_id = 1:n()) %>%
  st_zm() %>%
  st_transform(dk_epsg)
 
#Join basins ids to lakes
lakes_centroid_basins_id <- lakes_clean %>%
  st_centroid() %>%
  st_join(grass_basins_all) %>%
  st_drop_geometry()

lakes_clean_basin_id <- lakes_clean %>%
  left_join(lakes_centroid_basins_id) %>%
  filter(!is.na(basin_id))

#Write to database
st_write(lakes_clean_basin_id, gis_database, "lakes_grass", delete_layer = TRUE)
