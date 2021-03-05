source("libs_and_funcs.R")

#Read and clean rawdata files
main_basins <- st_read(paste0(rawdata_path, "vp2b2013hovedvandoplande.shp"))

main_basins_break <- main_basins %>% 
  st_cast("POLYGON")

main_basins_clean <- main_basins_break %>% 
  select(basin_name = PLANOPLAND) %>% 
  mutate(basin_id = 1:n()) %>% 
  st_zm() %>% 
  st_transform(dk_epsg)

main_basins_clean_buffer <- main_basins_clean %>% 
  st_buffer(5000) #%>% 
  #st_cast("MULTIPOLYGON")

lakes <- st_read(paste0(rawdata_path, "DK_StandingWater.gml"))

lakes_clean <- lakes %>% 
  select(lake_name = gml_id) %>% 
  mutate(lake_id = 1:n()) %>% 
  st_zm() %>% 
  st_transform(dk_epsg)

lakes_centroid_basins_id <- lakes_clean %>% 
  st_centroid() %>% 
  st_join(main_basins_clean) %>% 
  st_drop_geometry()

lakes_clean_basin_id <- lakes_clean %>% 
  left_join(lakes_centroid_basins_id) %>% 
  filter(!is.na(basin_id))

#Write to database
st_write(main_basins_clean, gis_database, "basins", delete_layer =  TRUE)
st_write(main_basins_clean_buffer, gis_database, "basins_buffer", delete_layer = TRUE)
st_write(lakes_clean_basin_id, gis_database, "lakes", delete_layer = TRUE)

#Write lake and basin shapefiles to sub-folders
basin_idx <- unique(lakes_clean_basin_id$basin_id)

lapply(basin_idx, function(idx){
  lakes_clean_basin_id %>% 
    filter(basin_id == idx) %>% 
    st_write(paste0(lakes_sub_path, "basin_lakes_", idx, ".shp"))
  
  main_basins_clean_buffer %>% 
    filter(basin_id == idx) %>% 
    st_write(paste0(basin_sub_path, "basin_", idx, ".shp"))
})

#Create vrt for hydro dem
dem_files <- list.files(paste0(rawdata_path, "DHYM_RAIN"), pattern = "*.ZIP", full.names = TRUE)

dem_asc_files <- sapply(dem_files, function(x){
  zip_files <- unzip(x, list = TRUE)
  asc_file <- zip_files$Name[grepl("*.asc", zip_files$Name)]
  asc_path <- paste0(x, "/", asc_file)
  return(asc_path)
})

gdalbuildvrt(paste0("/vsizip/", dem_asc_files), 
             paste0(getwd(), "/data/dhym_rain.vrt"),
             allow_projection_difference = TRUE,
             a_srs = paste0("EPSG:", dk_epsg))
