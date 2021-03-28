import numpy as np
cimport numpy as np
cimport cython

DTYPE = np.uint8
ctypedef np.uint8_t DTYPE_t


def catchment_from_d8(target, flowdir):

    ys, xs = target.nonzero()
        
    rows, cols = flowdir.shape
    
    return _catchment_from_d8(ys, xs, rows, cols, flowdir)
    

@cython.boundscheck(False) 
@cython.wraparound(False)
cdef np.ndarray[DTYPE_t, ndim=2] _catchment_from_d8(np.ndarray[np.int64_t, ndim=1] ys, 
                                                    np.ndarray[np.int64_t, ndim=1] xs,
                                                    int rows, int cols, 
                                                    np.ndarray[DTYPE_t, ndim=2] flowdir):

    cdef np.ndarray[DTYPE_t, ndim=2] catchment = np.zeros((rows,cols), dtype=DTYPE)
    
    cdef int k
    
    for k in range(ys.shape[0]):
      _flow_edge(ys[k], xs[k], flowdir, catchment)
      
    return catchment
    
@cython.boundscheck(False) 
@cython.wraparound(False)
cdef np.ndarray[DTYPE_t, ndim=2] _flow_edge(int y, int x, 
                                            np.ndarray[DTYPE_t, ndim=2] flowdir, 
                                            np.ndarray[DTYPE_t, ndim=2] catchment):
                                            
    catchment[y, x] = 1

    if flowdir[y  ,x-1] == 5 and catchment[y  ,x-1]<1 : 
        _flow_edge(y  ,x-1, flowdir, catchment)

    if flowdir[y-1,x-1] == 6 and catchment[y-1,x-1]<1 : 
        _flow_edge(y-1,x-1, flowdir, catchment)

    if flowdir[y-1,x  ] == 7 and catchment[y-1,x  ]<1 : 
        _flow_edge(y-1,x  , flowdir, catchment)

    if flowdir[y-1,x+1] == 8 and catchment[y-1,x+1]<1 : 
        _flow_edge(y-1,x+1, flowdir, catchment)

    if flowdir[y  ,x+1] == 1 and catchment[y  ,x+1]<1 : 
        _flow_edge(y  ,x+1, flowdir,catchment)

    if flowdir[y+1,x+1] == 2 and catchment[y+1,x+1]<1 : 
        _flow_edge(y+1,x+1, flowdir,catchment)

    if flowdir[y+1,x  ] == 3 and catchment[y+1,x  ]<1 : 
        _flow_edge(y+1,x  , flowdir,catchment)

    if flowdir[y+1,x-1] == 4 and catchment[y+1,x-1]<1 : 
        _flow_edge(y+1,x-1, flowdir,catchment)
        
    return catchment