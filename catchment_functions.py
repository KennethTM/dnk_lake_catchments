import numpy as np
import sys
import rasterio as rio
from shapely.geometry import shape
from rasterio import features
import catchment_cython
import geopandas as gp
import os
from collections import deque

#Flowdirection maps

#richdem flowcoordinate system
#234
#105
#876
richdemmap = (3, 4, 5, 6, 7, 8, 1, 2)

#taudem flowcoordinate system
#432
#501
#678
taudemmap = (3, 2, 1, 8, 7, 6, 5, 4)

def basin_catchment_delin(basin_id):
  
  basin = str(basin_id)
  
  #Paths to flowdir grid and lake shapefile
  flowdir_path = os.path.join(os.getcwd(), "data", "flowdir_sub", "basin_"+basin+"-breach-flowdirs-lzw.tif")
  lakes_path = os.path.join(os.getcwd(), "data", "lakes_sub", "basin_lakes_"+basin+".shp")
  
  grid, grid_meta = read_grid(flowdir_path)
  
  lake_shp = gp.GeoDataFrame.from_file(lakes_path)
  
  n_row = len(lake_shp.index)
  
  result_list = []
  
  #(lake_shp[283:285])
  for i, row in (lake_shp[0:5]).iterrows(): 
    basin_id = row["bsn_gr_"]
    lake_id = row["lake_id"]
    poly = row["geometry"]
  
    try:
      print("Delineating catchment for lake_id {}: Lake {} of {} in basin {}...".format(lake_id, i, n_row, basin_id), flush=True)
      result_dict = lake_catchment_delin(grid=grid, grid_meta=grid_meta, poly=poly, lake_id=lake_id, basin_id=basin_id, flowdirmap=richdemmap)
      result_list.append(result_dict)
    except:
      print("Failed to delineate catchment for lake_id {}: Lake {} of {} in basin {}!".format(lake_id, i, n_row, basin_id), flush=True)
      pass
  
  catch_geo = gp.GeoDataFrame(result_list, crs = grid_meta["crs"])
  catch_geo.to_file(os.path.join(os.getcwd(), "data", "catchments_sub", "basin_catchments_"+basin+".shp"))

def lake_catchment_delin(grid, grid_meta, poly, lake_id, basin_id, flowdirmap):
  catch_id = lake_id + 888*10**8

  target = features.rasterize([(poly, 1)], out_shape = grid.shape, transform = grid_meta["transform"], dtype = np.uint8)

  #catch = d8_catchment(target_grid = target, flowdir_grid = grid, flowdir_mapping = richdemmap, target_val = 1)
  #catch = catchment_cython.catchment_from_d8(target, grid) #using richdem map #used for the first app. 80% of catchments
  #catch = catchment_from_d8_bfs(target, grid) #using richdem map
  catch = catchment_cython.catchment_from_d8_bfs(target, grid) 

  catch_vect = features.shapes(catch, mask = (catch == 1), transform = grid_meta["transform"], connectivity=8)

  geom, _ = list(catch_vect)[0]

  lake_catch_dict = {"basin_id": basin_id, "lake_id": lake_id, "catch_id": catch_id, "geometry": shape(geom)}

  return(lake_catch_dict)

#Modified from https://github.com/mdbartos/pysheds/blob/master/pysheds/grid.py
def zero_rim(data, nodata=0):

    data[:,0] = nodata
    data[:,-1] = nodata
    data[0,:] = nodata
    data[-1,:] = nodata
    
    return None

def read_grid(grid_path):
    
    with rio.open(grid_path) as ds:
        grid = ds.read(1)
        grid_meta = ds.meta
        grid[grid == 255] = 0
        
    zero_rim(grid)
    
    return(grid, grid_meta)

def catchment_from_d8_bfs(target, flowdir):

  ys, xs = target.nonzero()
  outlets = [[i, j] for i,j in zip(ys, xs)]
  q = deque(outlets)

  neighbor_idx = [[0, -1], [-1, -1], [-1, 0], [-1, 1], [0, 1], [1, 1], [1, 0], [1, -1]]
  neighbor_dir = [5, 6, 7, 8, 1, 2, 3, 4]

  while q:
    y, x = q.popleft()

    for i, fdir in zip(neighbor_idx, neighbor_dir):
      dy, dx = i
      next_y, next_x = y + dy, x + dx

      if flowdir[next_y, next_x] == fdir and target[next_y, next_x] == 0:
        target[next_y, next_x] = 1
        q.append([next_y, next_x])

  return(target)


#write raster for debug
#with rio.open("tmp_target2.tif", "w", **grid_meta) as ds:
#tr  ds.write(target.astype(rio.uint8), 1)

# def select_surround_ravel(i, shape):
# 
#     offset = shape[1]
#     
#     return np.array([i + 0 - offset,
#                      i + 1 - offset,
#                      i + 1 + 0,
#                      i + 1 + offset,
#                      i + 0 + offset,
#                      i - 1 + offset,
#                      i - 1 + 0,
#                      i - 1 - offset]).T

#pysheds function need order:
#[N, NE, E, SE, S, SW, W, NW]

# Implementation 1
# def d8_catchment(target_grid, flowdir_grid, flowdir_mapping, target_val = 1, recursionlimit = 1000):
# 
#     sys.setrecursionlimit(recursionlimit)
# 
#     def d8_catchment_search(cells):
# 
#         collect.extend(cells)
#         selection = select_surround_ravel(cells, flowdir_grid.shape)
#         next_idx = selection[(flowdir_grid.flat[selection] == flowdir_mapping_reorder)]
# 
#         if next_idx.any():
#             return d8_catchment_search(next_idx)
# 
#     #zero_rim(flowdir_grid)
# 
#     pour_point = np.ravel_multi_index(np.where(target_grid == target_val), flowdir_grid.shape)
#     flowdir_mapping_reorder = np.array(flowdir_mapping)[[4, 5, 6, 7, 0, 1, 2, 3]].tolist()
#     pour_point = np.array([pour_point])
# 
#     collect = []
#     d8_catchment_search(pour_point)
# 
#     collect_np = np.hstack((collect[0], np.array(collect[1:])))
# 
#     outcatch = np.zeros(flowdir_grid.shape, dtype="uint8")
# 
#     outcatch.flat[collect_np] = 1
# 
#     sys.setrecursionlimit(1000)
# 
#     return outcatch

# # Implementation 2
# def d8_catchment(target_grid, flowdir_grid, flowdir_mapping, target_val = 1):
# 
#     pour_point = np.ravel_multi_index(np.where(target_grid == target_val), flowdir_grid.shape)
#     flowdir_mapping_reorder = np.array(flowdir_mapping)[[4, 5, 6, 7, 0, 1, 2, 3]].tolist()
#     cells = np.array([pour_point])
# 
#     search = True
#     while search:
#         selection = select_surround_ravel(cells, flowdir_grid.shape)
#         cells = selection[(flowdir_grid.flat[selection] == flowdir_mapping_reorder)]
#         #collect.extend(cells)
#         target_grid.flat[cells] = target_val
# 
#         if not cells.any():
#             search = False
# 
#     return target_grid

# Implementation 3
# def d8_catchment(target_grid, flowdir_grid, flowdir_mapping, target_val = 1, recursionlimit = 5000):
# 
#     sys.setrecursionlimit(recursionlimit)
# 
#     def d8_catchment_search(cells):
#         
#         nonlocal outcatch
#         nonlocal flowdir_grid
# 
#         outcatch.flat[cells] = 1
#         selection = select_surround_ravel(cells, flowdir_grid.shape)
#         next_idx = selection[(flowdir_grid.flat[selection] == flowdir_mapping_reorder)]
# 
#         if next_idx.any():
#             return d8_catchment_search(next_idx)
# 
#     #zero_rim(flowdir_grid)
# 
#     pour_point = np.ravel_multi_index(np.where(target_grid == target_val), flowdir_grid.shape)
#     flowdir_mapping_reorder = np.array(flowdir_mapping)[[4, 5, 6, 7, 0, 1, 2, 3]].tolist()
#     pour_point = np.array([pour_point])
# 
#     outcatch = np.zeros(flowdir_grid.shape, dtype="uint8")
#     d8_catchment_search(pour_point)
# 
#     sys.setrecursionlimit(1000)
# 
#     return outcatch
