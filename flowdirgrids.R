source("libs_and_funcs.R")

#Read from database
basins_grass_buffer <- st_read(gis_database, "basins_grass_buffer")
lakes_grass <- st_read(gis_database, "lakes_grass")
dk_border <- st_read(gis_database, "dk_border")

basins_grass_buffer_coastline <- basins_grass_buffer %>% 
  st_intersection(st_geometry(dk_border)) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid()

st_write(basins_grass_buffer_coastline, gis_database, layer = "basins_grass_buffer_coastline", delete_layer = TRUE)

#Write lake and basin shapefiles to sub-folders
basin_ids <- sort(unique(lakes_grass$basin_grass_id))

lapply(basin_ids, function(id){
  lakes_grass %>%
    filter(basin_grass_id == id) %>%
    st_write(paste0(lakes_sub_path, "basin_lakes_", id, ".shp"), delete_dsn = TRUE)

  basins_grass_buffer_coastline %>%
    filter(basin_grass_id == id) %>%
    st_write(paste0(basin_sub_path, "basin_", id, ".shp"), delete_dsn = TRUE)
})

#Cut dems from buffered basin polygons
for(i in basin_ids){
  gdalwarp(srcfile = dhym,
           dstfile = paste0(flowdir_sub_path, "basin_", i, ".tif"),
           cutline = gis_database,
           cl = "basins_grass_buffer_coastline",
           cwhere = paste0("basin_grass_id='", i, "'"),
           crop_to_cutline = TRUE,
           overwrite = TRUE,
           dstnodata = -9999,
           co = c("COMPRESS=LZW", "BIGTIFF=YES"),
           tr = c(1.6, 1.6),
           multi = TRUE,
           wm = 6000)
}

for(i in basin_ids){
  if(file.exists(paste0(flowdir_sub_path, "basin_", i, "-breach-flowdirs-lzw.tif"))){
    next
  }
  
  print("Breaching...")
  rd_breach_call <- paste0(flowdir_sub_path, "rd_depressions_breach.exe ",
                           paste0(flowdir_sub_path, "basin_", i, ".tif"),
                           " ",
                           paste0(flowdir_sub_path, "basin_", i, "-breach.tif"),
                           " COMPLETE NOEPS NOFILL 0 0")
  system(rd_breach_call)
  
  print("Fill and flowdirection...")
  rd_flowdir_call <- paste0(flowdir_sub_path, "rd_d8_flowdirs.exe ", 
                            paste0(flowdir_sub_path, "basin_", i, "-breach.tif"),
                            " ",
                            paste0(flowdir_sub_path, "basin_", i, "-breach"))
  system(rd_flowdir_call)
  
}

for(i in basin_ids){
  print("Compressing...")
  gdal_translate(src_dataset = paste0(flowdir_sub_path, "basin_", i, "-breach-flowdirs.tif"),
                 dst_dataset = paste0(flowdir_sub_path, "basin_", i, "-breach-flowdirs-lzw.tif"),
                 co = "COMPRESS=LZW")
  
  print("File cleanup...")
  if(file.exists(paste0(flowdir_sub_path, "basin_", i, "-breach-flowdirs-lzw.tif"))){
    file.remove(paste0(flowdir_sub_path, "basin_", i, ".tif"),
                paste0(flowdir_sub_path, "basin_", i, "-breach.tif"),
                paste0(flowdir_sub_path, "basin_", i, "-breach-flowdirs.tif"))
  }
}

