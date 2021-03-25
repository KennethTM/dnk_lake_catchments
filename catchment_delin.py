import os
from catchment_functions import *
import geopandas as gp

#loop over basins
ids = list(range(112))[1:]

####mangler at delineate for basin 18
for i in ids[17:18]:
  basin = str(i)
  
  #Paths to flowdir grid and lake shapefile
  flowdir_path = os.path.join(os.getcwd(), "data", "flowdir_sub", "basin_"+basin+"-breach-flowdirs-lzw.tif")
  lakes_path = os.path.join(os.getcwd(), "data", "lakes_sub", "basin_lakes_"+basin+".shp")
  
  grid, grid_meta = read_grid(flowdir_path)
  
  lake_shp = gp.GeoDataFrame.from_file(lakes_path)
  
  n_row = len(lake_shp.index)
  
  result_list = []
  
  #i, row = next(iter(lake_shp[374:376].iterrows()))
  for i, row in lake_shp.iterrows():  
    basin_id = row["bsn_gr_"] #basin_id
    lake_id = row["lake_id"]
    poly = row["geometry"]
    
    if lake_id == 124679:
      continue
  
    try:
      print("Delineating catchment for lake_id {}: Lake {} of {} in basin {}...".format(lake_id, i, n_row, basin_id), flush=True)
      result_dict = lake_catchment_delin(grid=grid, grid_meta=grid_meta, poly=poly, lake_id=lake_id, basin_id=basin_id, flowdirmap = richdemmap)
      result_list.append(result_dict)
    except:
      print("Failed to delineate catchment for lake_id {}: Lake {} of {} in basin {}!".format(lake_id, i, n_row, basin_id), flush=True)
      pass
  
  catch_geo = gp.GeoDataFrame(result_list, crs = grid_meta["crs"])
  catch_geo.to_file(os.path.join(os.getcwd(), "data", "catchments_sub", "basin_catchments_"+basin+".shp"))

#Delineating catchment for lake_id 124679: Lake 1436 of 1686 in basin 18...
