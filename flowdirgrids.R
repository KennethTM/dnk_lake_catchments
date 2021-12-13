source("libs_and_funcs.R")

#Read from database
basins_grass_buffer <- st_read(gis_database, "basins_grass_buffer")
lakes_grass <- st_read(gis_database, "lakes_grass")
dk_border <- st_read(gis_database, "dk_border")

#Write lake and basin shapefiles to sub-folders
basin_ids <- sort(unique(lakes_grass$basin_grass_id))

lapply(basin_ids, function(id){
  lakes_grass %>%
    filter(basin_grass_id == id) %>%
    st_write(paste0(lakes_sub_path, "basin_lakes_", id, ".shp"), delete_dsn = TRUE)

  basins_grass_buffer %>%
    filter(basin_grass_id == id) %>%
    st_write(paste0(basin_sub_path, "basin_", id, ".shp"), delete_dsn = TRUE)
})

#Cut dems from buffered basin polygons
for(i in basin_ids){
  
  if(file.exists(paste0(flowdir_sub_path, "basin_", i, ".tif"))){
    print(paste0("Skipping basin ", i))
    next
  }
  
  print(paste0("Basin ", i))
  
  gdalwarp(srcfile = dhym,
           dstfile = paste0(flowdir_sub_path, "basin_", i, ".tif"),
           cutline = gis_database,
           cl = "basins_grass_buffer",
           cwhere = paste0("basin_grass_id='", i, "'"),
           crop_to_cutline = TRUE,
           overwrite = TRUE,
           dstnodata = -9999,
           co = c("COMPRESS=LZW", "BIGTIFF=YES"),
           tr = c(1.6, 1.6),
           tap = TRUE,
           multi = TRUE,
           wm = 8000,
           wo = "NUM_THREADS=ALL_CPUS")
}

for(i in basin_ids){
  
  rd_breach_call <- paste(paste0(richdem_apps_path, "rd_depressions_breach.exe"),
                          paste0(flowdir_sub_path, "basin_", i, ".tif"),
                          paste0(flowdir_sub_path, "basin_", i, "-breach.tif"),
                          "COMPLETE NOEPS NOFILL 0 0")
  
  if(!file.exists(paste0(flowdir_sub_path, "basin_", i, "-breach.tif"))){
    print(paste0("Breaching for basin ", i))
    system(rd_breach_call)
  }
  
  print(paste0("Fill and flowdirection for basin ", i))
  rd_flowdir_call <- paste(paste0(richdem_apps_path, "rd_d8_flowdirs.exe"), 
                            paste0(flowdir_sub_path, "basin_", i, "-breach.tif"),
                            paste0(flowdir_sub_path, "basin_", i, "-breach"))
  system(rd_flowdir_call)
  
}

for(i in basin_ids){
  
  print(paste0("Compressing for basin ", i))
  gdal_translate(src_dataset = paste0(flowdir_sub_path, "basin_", i, "-breach-flowdirs.tif"),
                 dst_dataset = paste0(flowdir_sub_path, "basin_", i, "-breach-flowdirs-lzw.tif"),
                 co = "COMPRESS=LZW")
  
  print(paste0("File cleanup for basin ", i))
  if(file.exists(paste0(flowdir_sub_path, "basin_", i, "-breach-flowdirs-lzw.tif"))){
    file.remove(paste0(flowdir_sub_path, "basin_", i, ".tif"),
                paste0(flowdir_sub_path, "basin_", i, "-breach.tif"),
                paste0(flowdir_sub_path, "basin_", i, "-breach-flowdirs.tif"))
  }
}


gdalwarp(srcfile = dhym,
         dstfile = paste0("basin_47.tif"),
         cutline = gis_database,
         cl = "basins_grass_buffer",
         cwhere = paste0("basin_grass_id='47'"),
         crop_to_cutline = TRUE,
         overwrite = TRUE,
         dstnodata = -9999,
         co = c("COMPRESS=LZW", "BIGTIFF=YES"),
         tr = c(1.6, 1.6),
         tap = TRUE,
         multi = TRUE,
         wm = 8000,
         wo = "NUM_THREADS=ALL_CPUS")
