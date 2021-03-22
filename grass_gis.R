source("libs_and_funcs.R")

dhym_10m <- paste0(getwd(), "/data/dhym_10m.tif")

dhym_raster <- raster(dhym_10m)

#Find grass install
grass_path <- findGRASS()

link2GI::linkGRASS7(dhym_raster,
                    default_GRASS7 = grass_path,
                    gisdbase = paste0(getwd(), "/grass_database"), 
                    location = "dhym_10m",
                    gisdbase_exist = TRUE)

#Import dem
execGRASS("r.in.gdal", flags = c("overwrite", "o"), parameters = list(input = dhym_10m, output = "dem"))

#Extract streams and flow directions using r.watershed
execGRASS("r.watershed", flags = c("overwrite", "b", "s", "a", "m"),
          parameters = list(elevation = "dem",
                            stream = "dem_stream",
                            accumulation="dem_acc",
                            drainage = "dem_drain",
                            threshold = 10^3,
                            memory = 8000))

#Determine major watersheds using r.stream.basins
execGRASS("r.stream.basins", flags = c("overwrite", "l", "c", "m"), 
          parameters = list(direction = "dem_drain",
                            stream_rast = "dem_stream",
                            basins = "dem_basins",
                            memory = 8000))

#Convert raster to vector file
execGRASS("r.to.vect", flags = c("overwrite", "s"), 
          parameters = list(input = "dem_basins",
                            output = "dem_basins",
                            type = "area"))


#Brug grass.gis v.clean
execGRASS("v.clean", flags = c("overwrite"), 
          parameters = list(input = "dem_basins",
                            output = "dem_basins_clean",
                            tool = rep("rmarea", 7),
                            threshold = 10^seq(2, 8, 1)))

#Import vector file to R and write to gis_database
use_sf()
grass_basins <- readVECT("dem_basins_clean") %>% 
  st_set_crs(dk_epsg) %>% 
  mutate(basin_id = 1:n()) %>% 
  select(basin_id)

st_write(grass_basins, gis_database, layer = "grass_basins", delete_layer = TRUE)
