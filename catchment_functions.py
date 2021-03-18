import numpy as np
import sys
import rasterio as rio

#Flowdirection maps
#pysheds function need order:
#[N, NE, E, SE, S, SW, W, NW]

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
        
    zero_rim(grid)
    
    return(grid, grid_meta)

def select_surround_ravel(i, shape):

    offset = shape[1]
    
    return np.array([i + 0 - offset,
                     i + 1 - offset,
                     i + 1 + 0,
                     i + 1 + offset,
                     i + 0 + offset,
                     i - 1 + offset,
                     i - 1 + 0,
                     i - 1 - offset]).T

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

# Implementation 2
def d8_catchment(target_grid, flowdir_grid, flowdir_mapping, target_val = 1, recursionlimit = 1000):

    pour_point = np.ravel_multi_index(np.where(target_grid == target_val), flowdir_grid.shape)
    flowdir_mapping_reorder = np.array(flowdir_mapping)[[4, 5, 6, 7, 0, 1, 2, 3]].tolist()
    cells = np.array([pour_point])

    search = True
    while search:
        selection = select_surround_ravel(cells, flowdir_grid.shape)
        cells = selection[(flowdir_grid.flat[selection] == flowdir_mapping_reorder)]
        #collect.extend(cells)
        target_grid.flat[cells] = target_val

        if not cells.any():
            search = False

    return target_grid

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
