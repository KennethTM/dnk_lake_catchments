source("libs_and_funcs.R")

dhym_10m_labels <- paste0(getwd(), "/data/dhym_10m_labels.tif")

dhym_labels_raster <- raster(dhym_10m_labels)

#Find grass install
grass_path <- findGRASS()

link2GI::linkGRASS7(dhym_labels_raster,
                    default_GRASS7 = grass_path,
                    gisdbase = paste0(getwd(), "/grass_database"), 
                    location = "dhym_10m",
                    gisdbase_exist = FALSE)

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
  mutate(basin_grass_id = 1:n()) %>% 
  select(basin_grass_id) %>% 
  st_make_valid()

grass_basins_buffer <- grass_basins %>%
  st_buffer(1000) %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid()

st_write(grass_basins, gis_database, layer = "basins_grass", delete_layer = TRUE)
st_write(grass_basins_buffer, gis_database, layer = "basins_grass_buffer", delete_layer = TRUE)

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
  st_join(grass_basins) %>%
  st_drop_geometry()

lakes_clean_basin_grass_id <- lakes_clean %>%
  left_join(lakes_centroid_basins_id) %>%
  filter(!is.na(basin_grass_id))

#Write to database
st_write(lakes_clean_basin_grass_id, gis_database, "lakes_grass", delete_layer = TRUE)





dk_border %>% 
  st_cast("POLYGON") %>% 
  mutate(area=as.numeric(st_area(geometry))) %>% 
  filter(area < 10^8)

# missing_lakes <-lakes_clean %>%
#   left_join(lakes_centroid_basins_id) %>%
#   filter(is.na(basin_grass_id))
