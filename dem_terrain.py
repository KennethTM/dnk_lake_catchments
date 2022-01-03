#Calculate terrain metrics from 10 m dem

import richdem as rd
import os
import rasterio as rio

dem = rd.LoadGDAL(os.path.join(os.getcwd(), "rawdata", "dk_dtm_10m.tif"))

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
