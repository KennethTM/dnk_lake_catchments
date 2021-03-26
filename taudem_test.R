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
  filter(basin_grass_id == 8) %>% 
  sample_n(10)

#Prepare lake boundary vectors for watershed delineation
lake_boundary <- taudem_lakes %>% 
  nngeo::st_remove_holes() %>% 
  select(lake_id)

st_write(lake_boundary, paste0(taudem_test_path, "lake_boundary.sqlite"), delete_dsn = TRUE)

target_extent <- as.numeric(st_bbox(raster(dem_path)))

#Rasterize lake boundaries
gdal_rasterize(paste0(taudem_test_path, "lake_boundary.sqlite"),
               paste0(taudem_test_path, "lake_boundary.tif"),
               a = "lake_id", 
               te = target_extent,
               tr = c(1.6, 1.6), 
               co = "COMPRESS=LZW", 
               a_nodata = 0)

lake_boundary_raster <- raster(paste0(taudem_test_path, "lake_boundary.tif"))

lake_boundary_pour_points <- rasterToPoints(lake_boundary_raster)

lake_boundary_pour_points_sf <- st_as_sf(as.data.frame(lake_boundary_pour_points), 
                                         coords = c("x", "y"), crs = dk_epsg) %>% 
  rename(lake_id = lake_boundary)

ids <- unique(taudem_lakes$lake_id)

for(i in ids){
  
  print(paste0("Delineating watershed ", which(ids == i), " of ", length(ids), " in total"))
  
  #Write lake boundary points to file
  lake_boundary_pour_points_sf %>%
    filter(lake_id == i) %>%
    st_write(paste0(taudem_test_path, "boundary_id_", i, ".sqlite"), delete_dsn = TRUE)
  
  #Delineate watershed draining to lake boundary points
  taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                        " -p ", paste0(taudem_test_path, "basin_8p.tif"),
                        " -o ", paste0(taudem_test_path, "boundary_id_", i, ".sqlite"),
                        " -gw ", paste0(taudem_test_path, "watershed_id_", i, ".tif"))
  system(taudem_gage)
  
  polygonize <- paste0("pkpolygonize ",
                       " -i ", paste0(taudem_test_path, "watershed_id_", i, ".tif"),
                       " -m ", paste0(taudem_test_path, "watershed_id_", i, ".tif"),
                       " -o ", paste0(taudem_test_path, "watershed_id_", i, ".sqlite"))
  system(polygonize)
  
  file.remove(paste0(taudem_test_path, "boundary_id_", i, ".sqlite"))
  file.remove(paste0(taudem_test_path, "watershed_id_", i, ".tif"))
}

#Collect catchments
lake_watershed_list <- lapply(ids, function(lake){
  lake_path <- paste0(taudem_test_path, "watershed_id_", lake, ".sqlite")
  
  st_read(lake_path) %>% 
    add_column(lake_id = lake)
})

#Union and clean catchment polygons
lake_watershed_list_clean <- lapply(lake_watershed_list, function(sf){
  sf %>% 
    st_transform(dk_epsg) %>% 
    st_make_valid() %>% 
    st_union() %>% 
    st_as_sf() %>% 
    st_cast("POLYGON") %>% 
    nngeo::st_remove_holes() %>% 
    add_column(lake_id = sf$lake_id[1])
  })

#Bind catchment polygons
lake_watershed_sf <- do.call(what = sf:::rbind.sf, args = lake_watershed_list_clean)

#write for inspection
st_write(lake_watershed_sf, paste0(taudem_test_path, "taudem_catchments.sqlite"))

#write subset for comparison
st_read(paste0(catchments_sub_path, "basin_catchments_8.shp")) %>% 
  filter(lake_id %in% taudem_lakes$lake_id) %>% 
  st_write(paste0(taudem_test_path, "python_catchments.sqlite"))
