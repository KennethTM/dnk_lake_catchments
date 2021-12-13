from numba.decorators import jit
import numpy as np
import sys
import rasterio as rio
import rasterio
from shapely.geometry import shape
from rasterio import features
#import catchment_cython
import geopandas as gp
import os
from collections import deque
from numba import jit

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
  #catchment_path = os.path.join(os.getcwd(), "data", "catchments_sub", "basin_catchments_"+basin+".shp")
  catchment_path = os.path.join(os.getcwd(), "data", "basin_catchments_"+basin+".shp")

  grid, grid_meta = read_grid(flowdir_path)
  
  lake_shp = gp.GeoDataFrame.from_file(lakes_path)
  
  n_row = len(lake_shp.index)
  
  #result_list = []
  
  #(lake_shp[283:285])
  for i, row in (lake_shp[0:5]).iterrows(): 
  #for i, row in lake_shp.iterrows(): 
    basin_id = row["bsn_gr_"]
    lake_id = row["lake_id"]
    poly = row["geometry"]
  
    try:
      print("Delineating catchment for lake_id {}: Lake {} of {} in basin {}...".format(lake_id, i+1, n_row, str(basin_id)), flush=True)
      result_dict = lake_catchment_delin(grid=grid, grid_meta=grid_meta, poly=poly, lake_id=lake_id, basin_id=basin_id, flowdirmap=richdemmap)
      #result_list.append(result_dict)

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

  #catch = d8_catchment(target_grid = target, flowdir_grid = grid, flowdir_mapping = richdemmap, target_val = 1)
  #catch = catchment_cython.catchment_from_d8(target, grid) #using richdem map #used for the first app. 80% of catchments
  #catch = catchment_cython.catchment_from_d8_bfs(target, grid) 
  #catch = catchment_from_d8_bfs(target, grid) #using richdem map
  catch = catchment_from_d8_vec(target, grid)

  #catch_crop, new_meta = crop_catch(catch, grid_meta)

  catch_vect = features.shapes(catch, mask = (catch == 1), transform = grid_meta["transform"], connectivity=8)
  #catch_vect = features.shapes(catch_crop, mask = (catch_crop == 1), transform = new_meta["transform"], connectivity=8)

  geom, _ = list(catch_vect)[0]

  lake_catch_dict = {"basin_id": basin_id, "lake_id": lake_id, "catch_id": catch_id, "geometry": shape(geom)}

  return(lake_catch_dict)




'''
def crop_catch(catch, grid_meta):

  xmin, ymin, xmax, ymax = bbox_from_mask(catch)

  catch_crop = catch[ymin:ymax, xmin:xmax]

  crop_trans = rasterio.Affine.translation(xmin, ymin)

  new_meta = grid_meta.copy()

  new_trans = new_meta["transform"] * crop_trans

  new_meta["width"] = catch_crop.shape[1]
  new_meta["height"] = catch_crop.shape[0]
  new_meta["transform"] = new_trans

  return(catch_crop, new_meta)


def bbox_from_mask(m):
    pos = np.where(m)
    xmin = np.min(pos[1])
    xmax = np.max(pos[1])
    ymin = np.min(pos[0])
    ymax = np.max(pos[0])
    return (xmin, ymin, xmax, ymax)

'''



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





'''
def neighbor_ind(x, offset):

  neighbor_idx = np.array([
    x - 1 + 0,
    x - 1 - offset,
    x + 0 - offset,
    x + 1 - offset,
    x + 1 + 0,
    x + 1 + offset,
    x + 0 + offset,
    x - 1 + offset
  ])

  return(neighbor_idx.T.flatten())

'''

#draft for vectorized function

@jit
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
