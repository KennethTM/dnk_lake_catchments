source("libs_and_funcs.R")

#Script for comparing outputs with that of Taudem

lakes_grass <- st_read(gis_database, "lakes_grass")

taudem_test_path <- paste0(getwd(), "/data/taudem/")

dem_path <- paste0(taudem_test_path, "basin_8.tif")

mpi_settings <- "mpiexec -n 8 "
taudem_path <- "/usr/local/taudem/"

#Fill
taudem_fel <- paste0(mpi_settings, taudem_path, "pitremove ", 
                     dem_path)
system(taudem_fel)

#Flowdirs
taudem_flowdir <- paste0(mpi_settings, taudem_path, "d8flowdir ",
                         " -p ", paste0(taudem_test_path, "basin_8p.tif"),
                         " -sd8 ", paste0(taudem_test_path, "basin_8sd8.tif"),
                         " -fel ", paste0(taudem_test_path, "basin_8fel.tif"))
system(taudem_flowdir)

#Delin lakes

set.seed(99)
taudem_lakes <- lakes_grass %>% 
  sample_n(10)

#Prepare lake boundary vectors for watershed delineation
lake_boundary <- taudem_lakes %>% 
  st_remove_holes() %>% 
  select(basin_grass_id)

st_write(lake_boundary, paste0(taudem_test_path, "lake_boundary.sqlite"), delete_dsn = TRUE)

#Rasterize lake boundaries
gdal_rasterize(paste0(taudem_test_path, "lake_boundary.sqlite"),
               paste0(taudem_test_path, "lake_boundary.tif"),
               a = "basin_grass_id", 
               #te = target_extent,
               tr = c(1.6, 1.6), 
               co = "COMPRESS=LZW", 
               a_nodata = 0)

lake_boundary_raster <- raster(paste0(taudem_test_path, "lake_boundary.tif"))

lake_boundary_pour_points <- rasterToPoints(lake_boundary_raster)

lake_boundary_pour_points_sf <- st_as_sf(as.data.frame(lake_boundary_pour_points), 
                                         coords = c("x", "y"), crs = dk_epsg) %>% 
  rename(basin_grass_id = lake_boundary)

ids <- unique(taudem_lakes$basin_grass_id)

for(i in ids){
  
  print(paste0("Delineating watershed ", which(ids == i), " of ", length(ids), " in total"))
  
  #Write lake boundary points to file
  lake_boundary_pour_points_sf %>%
    filter(basin_grass_id == i) %>%
    st_write(paste0(taudem_test_path, "boundary_id_", i, ".sqlite"))
  
  #Delineate watershed draining to lake boundary points
  taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                        " -p ", paste0(taudem_test_path, "basin_8p.tif"),
                        " -o ", paste0(taudem_test_path, "boundary_id_", i, ".sqlite"),
                        " -gw ", paste0(taudem_test_path, "watershed_id_", i, ".tif"))
  system(taudem_gage)
  
  polygonize <- paste0("pkpolygonize ",
                       " -i ", paste0(taudem_test_path, "watershed_id_", i, ".tif"),
                       " -m ", paste0(taudem_test_path, "watershed_id_", i, ".tif"),
                       " -o ", paste0(taudem_test_path, "watershed_id_", x, ".sqlite"))
  system(polygonize)
  
  file.remove(paste0(taudem_test_path, "boundary_id_", i, ".sqlite"))
  file.remove(paste0(taudem_test_path, "watershed_id_", i, ".tif"))
}

