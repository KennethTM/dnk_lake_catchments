import numpy as np
import sys
import rasterio as rio
import rasterio
from shapely.geometry import shape
from rasterio import features
import geopandas as gp
import os
from collections import deque
from numba import njit

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
  catchment_path = os.path.join(os.getcwd(), "data", "catchments_sub", "basin_catchments_"+basin+".shp")

  grid, grid_meta = read_grid(flowdir_path)
  
  lake_shp = gp.GeoDataFrame.from_file(lakes_path)
  
  n_row = len(lake_shp.index)
  
  for i, row in lake_shp.iterrows(): 
    basin_id = row["basin_id"]
    lake_id = row["lake_id"]
    poly = row["geometry"]
  
    try:
      print("Delineating catchment for lake_id {}: Lake {} of {} in basin {}...".format(lake_id, i+1, n_row, str(basin_id)), flush=True)
      result_dict = lake_catchment_delin(grid=grid, grid_meta=grid_meta, poly=poly, lake_id=lake_id, basin_id=basin_id, flowdirmap=richdemmap)

      catch_geo = gp.GeoDataFrame([result_dict], crs = grid_meta["crs"])
    
      if not os.path.exists(catchment_path):
        catch_geo.to_file(catchment_path)
      else:
        catch_geo.to_file(catchment_path, mode = "a")

    except:
      print("Failed to delineate catchment for lake_id {}: Lake {} of {} in basin {}!".format(lake_id, i+1, n_row, str(basin_id)), flush=True)
      pass


def lake_catchment_delin(grid, grid_meta, poly, lake_id, basin_id, flowdirmap):
  
  catch_id = lake_id + 888*10**8

  target = features.rasterize([(poly, 1)], out_shape = grid.shape, transform = grid_meta["transform"], dtype = np.uint8)

  catch = catchment_from_d8_vec(target, grid)

  catch_vect = features.shapes(catch, mask = (catch == 1), transform = grid_meta["transform"], connectivity=8)

  geom, _ = list(catch_vect)[0]

  lake_catch_dict = {"basin_id": basin_id, "lake_id": lake_id, "catch_id": catch_id, "geometry": shape(geom)}

  return(lake_catch_dict)


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
  outlets = [(i, j) for i,j in zip(ys, xs)]
  q = deque(outlets)

  neighbor_idx = ((0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1))
  neighbor_dir = (5, 6, 7, 8, 1, 2, 3, 4)

  while q:
    y, x = q.popleft()

    for i, fdir in zip(neighbor_idx, neighbor_dir):
      dy, dx = i
      next_y, next_x = y + dy, x + dx

      if flowdir[next_y, next_x] == fdir and target[next_y, next_x] == 0:
        target[next_y, next_x] = 1
        q.append((next_y, next_x))

  return(target)


#Vectorized function using numba
@njit
def catchment_from_d8_vec(target, flowdir):

  rows, cols = target.shape

  target_flat = target.reshape(-1)
  flowdir_flat = flowdir.reshape(-1)

  neighbor_dir = np.array([5, 6, 7, 8, 1, 2, 3, 4])
  neighbor_offsets = np.array([-1, -1 - cols, - cols, 1 - cols, 1, 1 + cols, cols, -1 + cols])

  front_ind = np.flatnonzero(target_flat)

  while front_ind.size > 0:

    front_neighbors = neighbor_offsets + np.expand_dims(front_ind, 1) 
    front_neighbors_flat = front_neighbors.reshape(-1)

    eval_flow = ((flowdir_flat[front_neighbors_flat]).reshape(front_neighbors.shape) == neighbor_dir).reshape(-1) & (target_flat[front_neighbors_flat] == 0)

    has_flow = front_neighbors_flat[eval_flow]

    target_flat[has_flow] = 1

    front_ind = has_flow

  return(target)
