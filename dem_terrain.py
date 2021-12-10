#Calculate terrain metrics from 10 m dem

import richdem as rd
import os

dem = rd.LoadGDAL(os.path.join(os.getcwd(), "rawdata", "dk_dtm_10m.tif"))

slope = rd.TerrainAttribute(dem, attrib='slope_percentage')
aspect = rd.TerrainAttribute(dem, attrib='aspect')
curvature = rd.TerrainAttribute(dem, attrib='curvature')

rd.SaveGDAL(os.path.join(os.getcwd(), "rawdata", "slope_10m.tif"), slope)
rd.SaveGDAL(os.path.join(os.getcwd(), "rawdata", "aspect_10m.tif"), aspect)
rd.SaveGDAL(os.path.join(os.getcwd(), "rawdata", "curvature_10m.tif"), curvature)
