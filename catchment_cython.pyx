import numpy as np
cimport numpy as np
cimport cython

DTYPE = np.uint8
ctypedef np.uint8_t DTYPE_t

def catchment_from_d8(target, flowdir):

  return _catchment_from_d8(target, flowdir)

@cython.boundscheck(False) 
@cython.wraparound(False)
cdef np.ndarray[DTYPE_t, ndim=2] _catchment_from_d8(np.ndarray[DTYPE_t, ndim=2] target, np.ndarray[DTYPE_t, ndim=2] flowdir):

    cdef np.ndarray[np.int64_t, ndim=1] ys
    cdef np.ndarray[np.int64_t, ndim=1] xs
    cdef int k
    
    ys, xs = target.nonzero()

    for k in range(ys.shape[0]):
      _flow_edge(ys[k], xs[k], flowdir, target)
      
    return target


@cython.boundscheck(False) 
@cython.wraparound(False)
cdef np.ndarray[DTYPE_t, ndim=2] _flow_edge(int y, int x, 
                                           np.ndarray[DTYPE_t, ndim=2] flowdir,
                                           np.ndarray[DTYPE_t, ndim=2] target):
                                            
    target[y, x] = 1

    if flowdir[y  ,x-1] == 5 and target[y  ,x-1]<1 : 
        _flow_edge(y  ,x-1, flowdir, target)

    if flowdir[y-1,x-1] == 6 and target[y-1,x-1]<1 : 
        _flow_edge(y-1,x-1, flowdir, target)

    if flowdir[y-1,x  ] == 7 and target[y-1,x  ]<1 : 
        _flow_edge(y-1,x  , flowdir, target)

    if flowdir[y-1,x+1] == 8 and target[y-1,x+1]<1 : 
        _flow_edge(y-1,x+1, flowdir, target)

    if flowdir[y  ,x+1] == 1 and target[y  ,x+1]<1 : 
        _flow_edge(y  ,x+1, flowdir, target)

    if flowdir[y+1,x+1] == 2 and target[y+1,x+1]<1 : 
        _flow_edge(y+1,x+1, flowdir, target)

    if flowdir[y+1,x  ] == 3 and target[y+1,x  ]<1 : 
        _flow_edge(y+1,x  , flowdir, target)

    if flowdir[y+1,x-1] == 4 and target[y+1,x-1]<1  : 
        _flow_edge(y+1,x-1, flowdir, target)
        
    return target