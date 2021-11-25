from catchment_functions import basin_catchment_delin
import time
import os
from multiprocessing import Pool

#loop over basins

#all ids
basin_dir = os.path.join(os.getcwd(), "data", "flowdir_sub")
all_ids = [int(i.split("-")[0].split("_")[-1]) for i in os.listdir(basin_dir)]

#processed ids
basin_dir = os.path.join(os.getcwd(), "data", "catchments_sub")
proc_ids = [int(i.split("_")[-1][0:-4]) for i in os.listdir(basin_dir) if ".shp" in i]

#ids to process
ids = [i for i in all_ids if i not in proc_ids]

pool = Pool(processes=3)
pool.imap_unordered(basin_catchment_delin, ids)

#basin_catchment_delin(1170)

'''
#Test 
t0 = time.time()
basin_catchment_delin(87)
t1 = time.time()
print(t1 - t0) #py bfs = 8.5, cy bfs = 8,3
'''
