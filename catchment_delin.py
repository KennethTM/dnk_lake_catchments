import rasterio as rio
from rasterio import features
import numpy as np
import geopandas as gp
from shapely.geometry import shape
from catchment_functions import *
import os

def lake_catchment_delin(grid, grid_meta, poly, lake_id, basin_id, flowdirmap):
  catch_id = lake_id + 888*10**8
    
  target = features.rasterize([(poly, 1)], out_shape = grid.shape, transform = grid_meta["transform"])
  
  catch = d8_catchment(target_grid = target, flowdir_grid = grid, flowdir_mapping = richdemmap, target_val = 1, recursionlimit = 20000)
  
  catch_vect = features.shapes(catch, mask = (catch == 1), transform = grid_meta["transform"])
  
  geom, val = list(catch_vect)[0]
  
  lake_catch_dict = {"basin_id": basin_id, "lake_id": lake_id, "catch_id": catch_id, "geometry": shape(poly)}
  
  return(lake_catch_dict)

def basin_catchment_delin(grid_path, lake_path, flowdirmap = richdemmap):
  
  with rio.open(grid_path) as ds:
    grid = ds.read(1)
    grid_meta = ds.meta
    
  lake_shp = gp.GeoDataFrame.from_file(lake_path)
  
  n_row = len(lake_shp.index)
  
  result_list = []
  
  #i, row = next(iter(lake_shp.iterrows()))
  for i, row in lake_shp.iterrows():
    basin_id = row["basin_id"]
    lake_id = row["lake_id"]
    poly = row["geometry"]
    
    print("Delineating catchment {} of {} in basin {}...".format(i, n_row, basin_id), flush = True)
    
    try:
      result_dict = lake_catchment_delin(grid=grid, grid_meta=grid_meta, poly=poly, lake_id=lake_id, basin_id=basin_id, flowdirmap = richdemmap)
      result_list.append(result_dict)
    except:
      pass
    
  return(result_list, grid_meta["crs"])



#run using example basin
flowdir_path = os.path.join(os.getcwd(), "data", "flowdir_sub", "basin_19-flowdirs.tif")
lakes_path = os.path.join(os.getcwd(), "data", "lakes_sub", "basin_lakes_19.shp")

catch_list, crs = basin_catchment_delin(flowdir_path, lakes_path)

catch_geo = gp.GeoDataFrame(catch_list, crs = crs)
catch_geo.to_file(os.path.join(os.getcwd(), "data", "catchments_sub", "basin_catchments_19.shp"))


