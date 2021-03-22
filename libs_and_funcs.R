library(raster);library(sf);library(gdalUtils);library(tidyverse);library(lwgeom);library(fasterize);library(exactextractr)
library(rgrass7);library(link2GI)

rawdata_path <- paste0(getwd(), "/rawdata/")

lakes_sub_path <- paste0(getwd(), "/data/lakes_sub/")
basin_sub_path <- paste0(getwd(), "/data/basin_sub/")
catchments_sub_path <- paste0(getwd(), "/data/catchments_sub/")
flowdir_sub_path <- paste0(getwd(), "/data/flowdir_sub/")

dhym <- paste0(getwd(), "/data/dhym_rain.vrt")

processed_catchments <- parse_number(list.files(catchments_sub_path, pattern = "*.shp"))

dk_epsg <- 25832
gis_database <- paste0(getwd(), "/data/gis_database.sqlite")
