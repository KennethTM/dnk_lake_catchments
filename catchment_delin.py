from catchment_functions import *
import time
import os
from multiprocessing import Pool

#loop over basins
ids = list(range(112))[1:]

t0 = time.time()

#pool = Pool()
#pool.map(basin_catchment_delin, ids[:4]) 

for i in ids[:4]: #[17:18]
  basin_catchment_delin(i)
  
t1 = time.time()
print(t1-t0)
