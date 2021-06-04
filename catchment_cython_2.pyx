import numpy as np
cimport numpy as np
cimport cython

np.import_array()

ctypedef np.uint8_t DTYPE_t

def catchment_from_d8(target, flowdir):

    ys, xs = target.nonzero()
    visited = np.zeros((target.shape[0], target.shape[1]), dtype = np.uint8)

    return _catchment_from_d8(target, flowdir, visited, ys, xs)


@cython.boundscheck(False) 
@cython.wraparound(False)
cdef np.ndarray[DTYPE_t, ndim=2] _catchment_from_d8(np.ndarray[DTYPE_t, ndim=2] target, 
                                                    np.ndarray[DTYPE_t, ndim=2] flowdir,
                                                    np.ndarray[DTYPE_t, ndim=2] visited,
                                                    np.ndarray[np.int64_t, ndim=1] ys,
                                                    np.ndarray[np.int64_t, ndim=1] xs):

    cdef int k
    
    for k in range(ys.shape[0]):
      _flow_edge(ys[k], xs[k], flowdir, visited)

    target[visited] = 1
      
    return target



@cython.boundscheck(False) 
@cython.wraparound(False)
cdef np.ndarray[DTYPE_t, ndim=2] _flow_edge(int y, int x, 
                                           np.ndarray[DTYPE_t, ndim=2] flowdir,
                                           np.ndarray[DTYPE_t, ndim=2] visited):
    
    message = str(x) + " " + str(y)
    print message

    if visited[y, x] or flowdir[y, x] == 0 or y < 0 or y >= flowdir.shape[0] or x < 0 or x >= flowdir.shape[1]:
        return visited

    visited[y, x] = 1

    if flowdir[y  ,x-1] == 5: 
        _flow_edge(y  ,x-1, flowdir, visited)

    if flowdir[y-1,x-1] == 6: 
        _flow_edge(y-1,x-1, flowdir, visited)

    if flowdir[y-1,x  ] == 7: 
        _flow_edge(y-1,x  , flowdir, visited)

    if flowdir[y-1,x+1] == 8: 
        _flow_edge(y-1,x+1, flowdir, visited)

    if flowdir[y  ,x+1] == 1: 
        _flow_edge(y  ,x+1, flowdir, visited)

    if flowdir[y+1,x+1] == 2: 
        _flow_edge(y+1,x+1, flowdir, visited)

    if flowdir[y+1,x  ] == 3: 
        _flow_edge(y+1,x  , flowdir, visited)

    if flowdir[y+1,x-1] == 4: 
        _flow_edge(y+1,x-1, flowdir, visited)

    return visited
