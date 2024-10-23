library(dplyr);library(sf);library(terra);library(exactextractr);library(foreign)

# Extra features from stream co2 project
base_path <- "../denmark_stream_co2/"

# Basemap04
bsm_dbf <- read.dbf(paste0(base_path, "data/features/Basemap04_public_geotiff/Basemap04_2021/lu_agg_2021.tif.vat.dbf"))

bsm_categories <- bsm_dbf |> 
  as_tibble() |> 
  mutate(label_danish = trimws(gsub('[[:digit:]]+', '', C_02))) |> 
  select(value = VALUE, label_danish)

#write.csv(bsm_categories, "vp3/features/bsm_categories.csv", row.names = FALSE)

bsm <- rast(paste0(base_path, "data/features/Basemap04_public_geotiff/Basemap04_2021/lu_agg_2021.tif"))

#Create boolean rasters of basemap variables
for(i in bsm_categories$value){
  print(i)
  
  bool_rast <- (bsm == i)
  
  writeRaster(bool_rast, 
              paste0("vp3/features/bsm/", i, ".tif"),
              datatype="INT1U")
}

#Load rasters
clay_files <- list.files(paste0(base_path, "data/features"), pattern="clay_*", full.names = TRUE)
chalk_file <- paste0(base_path, "data/features/chalkdepth_DKM.tif")
phraetic_file <- paste0(base_path, "data/features/dkm_2020_100m_phreatic_all_mean.tif")
bsm_files <- list.files("vp3/features/bsm", pattern="*.tif", full.names = TRUE)

clay <- rast(clay_files)

chalk <- rast(chalk_file)
names(chalk) <- "chalkdepth"

phraetic <- rast(phraetic_file)
names(phraetic) <- "phraetic"

bsm_features <- rast(bsm_files)
names(bsm_features) <- paste0("bsm_", sub(".tif", "", basename(bsm_files)))

# Catchments
catchments <- st_read("data_products/sqlite_format/catchments_simple.sqlite")

# Extract features
phraetic_vals <- exact_extract(phraetic, catchments, "mean")
chalk_vals <- exact_extract(chalk, catchments, "mean")
clay_vals <- exact_extract(clay, catchments, "mean", max_cells_in_memory=1e+09)

layer_idx <- 1:nlyr(bsm_features)
layer_chunks <- split(layer_idx, ceiling(seq_along(layer_idx)/5))
bsm_vals_list <- lapply(layer_chunks, \(x) exact_extract(bsm_features[[x]], catchments, "mean", max_cells_in_memory=1e+08))

bsm_vals <- bind_cols(bsm_vals_list)

# Combine and write to file
all_vals <- bind_cols(
  gml_id=catchments$gml_id,
  phraetic=phraetic_vals,
  chalk=chalk_vals,
  clay_vals,
  bsm_vals
)

saveRDS(all_vals, "vp3/features.rds")
