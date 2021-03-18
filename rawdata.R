source("libs_and_funcs.R")

#Read and clean rawdata files
main_basins <- st_read(paste0(rawdata_path, "vp2b2013hovedvandoplande.shp"))

#main_basins_break <- main_basins %>% 
#  st_cast("POLYGON")

main_basins_clean <- main_basins %>% 
  select(basin_name = PLANOPLAND) %>% 
  mutate(basin_id = 1:n()) %>% 
  st_zm() %>% 
  st_transform(dk_epsg)

main_basins_clean_buffer <- main_basins_clean %>% 
  st_buffer(500) #%>% 
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
             a_srs = paste0("EPSG:", dk_epsg),
             )

#Read soil and landcover layers and write to database
soil_path <- paste0(rawdata_path, "Jordart_200000_Shape/jordart_200000_ids.shp")

soil <- st_read(soil_path) %>%
  st_transform(dk_epsg) %>%
  set_names(str_to_lower(names(.)))

soil %>%
  st_write(gis_database, layer = "geus_soil", delete_layer = TRUE)

clc_path <- paste0(rawdata_path, "DK_CORINE_SHP_UTM32-WGS84/CLC12_DK.shp")
clc_legend <- read.csv(paste0(rawdata_path, "DK_CORINE_SHP_UTM32-WGS84/clc_legend.csv"), colClasses = "character")

clc <- st_read(clc_path) %>%
  st_transform(dk_epsg) %>%
  left_join(clc_legend, by = c("CODE_12"="CLC_CODE")) %>% 
  set_names(str_to_lower(names(.))) %>%
  mutate(clc_code = parse_number(as.character(code_12)))

clc %>%
  st_write(gis_database, layer = "corine_land_cover", delete_layer = TRUE)
