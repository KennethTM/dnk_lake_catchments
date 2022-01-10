source("libs_and_funcs.R")

#Denmark border
dk_border_raw <- raster::getData("GADM", country = "DNK", level = 0, path = rawdata_path)

dk_border <- dk_border_raw %>%
  st_as_sf() %>% 	
  st_transform(dk_epsg)	

dk_border %>% 
  st_write(gis_database, layer = "dk_border", delete_layer = TRUE)

#Create vrt for hydro dem tiles (1.6 meter resolution)
dem_files <- list.files(paste0(rawdata_path, "DHYM_RAIN"), pattern = "*.ZIP", full.names = TRUE)

dem_asc_files <- sapply(dem_files, function(x){
  zip_files <- unzip(x, list = TRUE)
  asc_file <- zip_files$Name[grepl("*.asc", zip_files$Name)]
  asc_path <- paste0(x, "/", asc_file)
  return(asc_path)
})

gdalbuildvrt(paste0("/vsizip/", dem_asc_files), 
             dhym,
             allow_projection_difference = TRUE,
             a_srs = paste0("EPSG:", dk_epsg))

#Create national 10 m dem for drainage basin delineation (minimum function for resampling)
gdalwarp(srcfile = dhym,
         dstfile = paste0(getwd(), "/rawdata/dhym_10m.tif"),
         cutline = gis_database,
         cl = "dk_border",
         crop_to_cutline = TRUE,
         overwrite = TRUE,
         dstnodata = -9999,
         r = "min",
         co = c("COMPRESS=LZW", "BIGTIFF=YES"),
         tr = c(10, 10),
         multi = TRUE,
         wm = 4000)

#Create national 10 m dem (terrain model, e.g. without buildings, trees etc.) 
#for computing geomorphometrical variables (average function for resampling)
gdalwarp(srcfile = dhym,
         dstfile = paste0(getwd(), "/rawdata/dtm_10m.tif"),
         cutline = gis_database,
         cl = "dk_border",
         crop_to_cutline = TRUE,
         overwrite = TRUE,
         dstnodata = -9999,
         r = "average",
         co = c("COMPRESS=LZW", "BIGTIFF=YES"),
         tr = c(10, 10),
         multi = TRUE,
         wm = 8000,
         wo = "NUM_THREADS=ALL_CPUS")

#Create national 10 dem (surface model, e.g. height of objects AND terrain)
dsm_files <- list.files(paste0(rawdata_path, "DSM/UTM32"), pattern = "*.zip", full.names = TRUE)

dsm_asc_files <- sapply(dsm_files, function(x){
  zip_files <- unzip(x, list = TRUE)
  asc_file <- zip_files$Name[grepl("*.asc", zip_files$Name)]
  asc_path <- paste0(x, "/", asc_file)
  return(asc_path)
})

dsm <- paste0(getwd(), "/rawdata/dsm.vrt")

gdalbuildvrt(paste0("/vsizip/", dsm_asc_files), 
             dsm,
             allow_projection_difference = TRUE,
             a_srs = paste0("EPSG:", dk_epsg))

gdalwarp(srcfile = dsm,
         dstfile = paste0(getwd(), "/rawdata/dsm_10m_raw.tif"),
         cutline = gis_database,
         cl = "dk_border",
         crop_to_cutline = TRUE,
         overwrite = TRUE,
         dstnodata = -9999,
         r = "average",
         co = c("COMPRESS=LZW", "BIGTIFF=YES"),
         tr = c(10, 10),
         multi = TRUE,
         wm = 8000,
         wo = "NUM_THREADS=ALL_CPUS")

#Breaching 10 m dem 
dhym_10m_breach <- paste(paste0(richdem_apps_path, "rd_depressions_breach.exe"),
                         paste0(getwd(), "/rawdata/dhym_10m.tif"),
                         paste0(getwd(), "/rawdata/dhym_10m_breach_raw.tif"),
                         "COMPLETE NOEPS NOFILL 0 0")

system(dhym_10m_breach)

gdal_translate(paste0(getwd(), "/rawdata/dhym_10m_breach_raw.tif"),
               paste0(getwd(), "/rawdata/dhym_10m_breach.tif"),
               co = "COMPRESS=LZW")

file.remove(paste0(getwd(), "/rawdata/dhym_10m_breach_raw.tif"))

#Label watersheds in 10 m dem using richdem functionality in "rd_label_watersheds.exe
dhym_10m_labels <- paste(paste0(richdem_apps_path, "rd_label_watersheds.exe"),
                         paste0(getwd(), "/rawdata/dhym_10m_breach.tif"),
                         paste0(getwd(), "/rawdata/dhym_10m_labels_raw.tif"))

system(dhym_10m_labels)

gdal_translate(paste0(getwd(), "/rawdata/dhym_10m_labels_raw.tif"),
               paste0(getwd(), "/rawdata/dhym_10m_labels.tif"),
               co = "COMPRESS=LZW")

file.remove(paste0(getwd(), "/rawdata/dhym_10m_labels_raw.tif"))

#Read soil and landcover layers and write to database
soil_path <- paste0(rawdata_path, "Jordart_200000_Shape/jordart_200000_ids.shp")

soil <- st_read(soil_path) %>%
  st_transform(dk_epsg) %>%
  set_names(str_to_lower(names(.)))

soil %>%
  st_write(gis_database, layer = "geus_soil", delete_layer = TRUE)

#Soil rasterize
gdal_rasterize(src_datasource = gis_database,
               dst_filename = paste0(rawdata_path, "geus_soil.tif"),
               a = "tsym_id",
               tr = c(100, 100),
               co = "COMPRESS=LZW",
               a_nodata = -9999,
               l = "geus_soil")

clc_path <- paste0(rawdata_path, "DK_CORINE_SHP_UTM32-WGS84/CLC12_DK.shp")
clc_legend <- read.csv(paste0(rawdata_path, "DK_CORINE_SHP_UTM32-WGS84/clc_legend.csv"), colClasses = "character")

clc <- st_read(clc_path) %>%
  st_transform(dk_epsg) %>%
  left_join(clc_legend, by = c("CODE_12"="CLC_CODE")) %>% 
  set_names(str_to_lower(names(.))) %>%
  mutate(clc_code = parse_number(as.character(code_12)))

clc %>%
  st_write(gis_database, layer = "corine_land_cover", delete_layer = TRUE)

#Corine land cover raster
gdal_rasterize(src_datasource = gis_database,
               dst_filename = paste0(rawdata_path, "corine_land_cover.tif"),
               a = "clc_code",
               tr = c(100, 100),
               co = "COMPRESS=LZW",
               a_nodata = -9999,
               l = "corine_land_cover")

#Warp worldclim annual temperature
gdalwarp(paste0(rawdata_path, "wc2.0_bio_30s_01.tif"),
         paste0(rawdata_path, "worldclim_airt.tif"), 
         tr = c(900, 900),
         t_srs = "EPSG:25832",
         r = "bilinear",
         te = target_extent,
         overwrite = TRUE,
         co = "COMPRESS=LZW",
         dstnodata = -9999)

#Warp worldclim annual precip
gdalwarp(paste0(rawdata_path, "wc2.0_bio_30s_12.tif"),
         paste0(rawdata_path, "worldclim_precip.tif"), 
         tr = c(900, 900),
         t_srs = "EPSG:25832",
         r = "bilinear",
         te = target_extent,
         overwrite = TRUE,
         co = "COMPRESS=LZW",
         dstnodata = -9999)

#Write streams to database
ogr2ogr(paste0(rawdata_path, "DK_Watercourse.gml"),
        gis_database,
        nln = "streams",
        update = TRUE,
        overwrite = TRUE,
        t_srs = paste0("EPSG:", dk_epsg),
        dim = 2,
        select = "gml_id")

#Ice line
ice_poly <- st_read(paste0(rawdata_path, "/Isrande/Isrand_poly.shp")) %>%
  slice(1) %>%
  st_transform(dk_epsg) %>%
  st_crop(dk_border)

ice_poly %>% 
  st_write(gis_database, layer = "dk_iceage", delete_layer = TRUE)
