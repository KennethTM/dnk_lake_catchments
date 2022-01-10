#Calculate terrain metrics from 10 m dem and create surface with elevations above terrain only
import richdem as rd
import os
import rasterio as rio
import numpy as np

dtm_path = os.path.join(os.getcwd(), "rawdata", "dtm_10m.tif")
dsm_path = os.path.join(os.getcwd(), "rawdata", "dsm_10m_raw.tif")
dsm_minus_dtm_path = os.path.join(os.getcwd(), "rawdata", "dsm_10m.tif")

#Substract dtm_10m from dsm_10m
with rio.open(dtm_path) as dtm:
    dtm_meta = dtm.profile
    dtm_array = dtm.read(1)
    dtm_array[dtm_array == -9999] = np.nan

with rio.open(dsm_path) as dsm:
    dsm_meta = dsm.profile
    dsm_array = dsm.read(1)
    dsm_array[dsm_array == -9999] = np.nan

with rio.open(dsm_minus_dtm_path, "w", **dsm_meta) as dst:
    diff_array = dsm_array-dtm_array
    diff_array[np.isnan(diff_array)] = -9999
    dst.write(diff_array, 1)

#Calculate terrain metrics
dem = rd.LoadGDAL(dtm_path)

modes_dict = {"slope": "slope_percentage",
              "aspect": "aspect",
              "curvature": "curvature",
              "planform_curvature": "planform_curvature",
              "profile_curvature": "profile_curvature"}
              
for k, v in modes_dict.items():
  print("Processing " + k)
  metric = rd.TerrainAttribute(dem, attrib = v)
  metric_name = k + "_10m_raw.tif"
  metric_path = os.path.join(os.getcwd(), "rawdata", metric_name)
  rd.SaveGDAL(metric_path, metric)
  
  metric_compress_name = k + "_10m.tif"
  metric_compress_path = os.path.join(os.getcwd(), "rawdata", metric_compress_name)

  with rio.open(metric_path) as src:
    src_meta = src.profile
    src_meta["compress"] = "lzw"
    src_array = src.read(1)
    
    with rio.open(metric_compress_path, "w", **src_meta) as dst:
      dst.write(src_array, 1)
  
  os.remove(metric_path)

#Calculate Terrain Ruggedness Index
#Alg. req. equires GDAL 3.3
dtm_tri_path = os.path.join(os.getcwd(), "rawdata", "dtm_tri_10m.tif")
dsm_tri_path = os.path.join(os.getcwd(), "rawdata", "dsm_tri_10m.tif")

tri_cmd = "gdaldem TRI {} {} -alg Riley -co COMPRESS=LZW" 

os.system(tri_cmd.format(dtm_path, dtm_tri_path))
os.system(tri_cmd.format(dsm_minus_dtm_path, dsm_tri_path))
