source("libs_and_funcs.R")

#Read from database
#Write to database
basins_buffer <- st_read(gis_database, "basins_buffer")

dhym <- paste0(getwd(), "/data/dhym_rain.vrt")

ids <- unique(basins_buffer$basin_id)

#idx of largest area poly
basins_buffer %>% 
  mutate(area=as.numeric(st_area(GEOMETRY))) %>% 
  arrange(desc(area)) %>% 
  slice(1)

for(i in 325){
  gdalwarp(srcfile = dhym,
           dstfile = paste0(flowdir_sub_path, "basin_", i, ".tif"),
           cutline = gis_database,
           cl = "basins_buffer",
           cwhere = paste0("basin_id='", i, "'"),
           crop_to_cutline = TRUE,
           overwrite = TRUE,
           co = c("COMPRESS=LZW", "BIGTIFF=YES"),
           tr = c(1.6, 1.6),
           multi = TRUE,
           wm = 4000)
}
