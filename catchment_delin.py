import rasterio as rio
from rasterio import features
import numpy as np
import geopandas as gp
from shapely.geometry import shape
from catchment_functions.py import *

def catchment_delin(flowdir, shp, flowdirmap = richdemmap):
  
  with rio.open(flowdir) as ds:
    grid = ds.read(1)
    grid_meta = ds.meta
    
  lake_shp = gp.GeoDataFrame.from_file(shp)
  
  n_row = len(lake_shp.index)
  
  result_dict = {"basin_id": [], "lake_id": [], "catch_id": [], "geometry": []}
  
  for i, row in lake_shp.iterrows():
    basin_id = row["basin_id"]
    lake_id = row["lake_id"]
    catch_id = lake_id + 888*10**8
    
    print("Delineating catchment {}/{} in basin {}...".format(i, n_row, basin_id))
    
    target = features.rasterize(row["geometry"], out_shape = grid.shape, transform = grid_meta["transform"], default_value = 1)
    
    catch = d8_catchment(target_grid = target, flowdir_grid = grid, flowdir_mapping = richdemmap, target_val = 1)
    
    catch_vect = features.shapes(catch, mask = (catch == 1), transform = rid_meta["transform"])
    
    geom, val = list(catch_vect)[0]
    
    result_dict["basin_id"].append(basin_id)
    result_dict["lake_id"].append(lake_id)
    result_dict["catch_id"].append(catch_id)
    result_dict["geometry"].append(shape(geom))


