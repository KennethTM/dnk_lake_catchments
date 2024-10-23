# Project VP3 notes

## Goal

Prediction of lake water quality/ecological status at scale to assess influence of differing land use.

## Notes

Catchment/buffer features from Martinsen & Sand-Jensen 2022.

Additional features:
* Update land use/land cover data with Basemap04
* Combine Basemap features with DSM map, e.g. forest or buildup height
* Basemap tree cover type
* GEUS layers, i.e. depth to groundwater, chalk
* Other features from stream CO2 project?
* Predicted lake mean depth
* Coordinates

Preprocessing:
* PCA of soil/land use variables
* Landuse/soil as absolute areas or percentage cover

Modeling:
* Log training errors
* Increa reguralization
* Xgboost
* Other metrics, i.e. ROC AUC
* Basins embeddings using neural networks

Ideas:
* Do phosphorus model for summer concentrations
  - What years? What thresholds for ecological status classes (depends on mean depth)
* Use VP3 ecological status classes for lakes.
  Using simple Random Forest models:
  - Use overall score
  - For P, N or overall chemistry score
  Using neural nets:
  - Use all scores simultenously
  Other input features:
  - Use vision models on catchment and/or lake imagery (enables visual interpretability processing) (could be used for streams as well)
  - Fuse vision input and catchment features
  
## Analysis

* Explore impacts of changing land use in catchment and/buffers on predictions (class shifts/P concentration reductions)
* Apply other interpretability measures
