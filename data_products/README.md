# Data and analysis products for manuscript:

DOI: https://doi.org/10.1016/j.scitotenv.2022.158090

Journal: Science of the Total Environment

Title: Predicting water quality from geospatial lake, catchment, and buffer zone characteristics in temperate lowland lakes

Authors: Kenneth Thorø Martinsen* and Kaj Sand-Jensen

Affiliations: Freshwater Biological Laboratory, Biological Institute, University of Copenhagen, Universitetsparken 4, 3rd floor, 2100 Copenhagen, Denmark

*Corresponding author: Kenneth Thorø Martinsen, kenneth.martinsen@bio.ku.dk

## Contents:

This repository contains files (GIS files available in both '.shp' and '.sqlite' formats in sub-folders) associated with the above manuscript:

*catchments_simple* Catchment boundaries of 180.377 lakes identified by the "gml_id" feature. Catchments are derived from a high-resolution digital elevation model (1.6 m resolution) followed by simplification, retaining approx. 10% of the original points.

*lakes* 180.378 lake polygons identified by the "gml_id" feature.

*predictions.csv* CSV text file containing predictions of water quality (alk/alkalinity [meq L^-1], chl_a/chlorophyll a [ug L^-1], color [mg Pt L^-1], ph/pH [pH], tn/total nitrogen [mg L^-1], tp/total phosphorus [mg L^-1], secchi/Secchi depth [m], pco2/CO2 partial pressure [uatm]) for 180.378 lakes identified by the "gml_id" feature. Estimates represents annual averages of surface water concentrations.

*models.rds* R-object file which can be read in R using the readRDS() function. The contains a nested R list object: The first level keys are "raw" or "mlr", and second level keys "alk", "chl_a", "color", "ph", "tn", "tp", "secchi", "pco2"). The list contains trained predictive models for each of the eight water quality variables, either as implemented in the MLR R-package ("mlr") or in the underlying R-packages ("raw").

## Raw data

Raw data sources are publicly available and cited in the main text. The lake polygons in this repository are included in the "INSPIRE - HYDROGRAFI" dataset from SDFE, Agency for Datasupply and Efficiency ("Styrelsen for Dataforsyning og Effektivisering" in Danish), downloaded October 2021. This dataset is derived from "GeoDanmark-data" ("Styrelsen for Dataforsyning og Effektivisering og Danske kommuner" in Danish):

General terms:
https://sdfe.dk/Media/637703280490685559/Vilkaar_for_brug_af_frie_geografiske_data_2021.pdf

GeoDanmark-data terms:
https://www.geodanmark.dk/wp-content/uploads/2020/03/Vilk%C3%A5r-for-brug-af-frie-geografiske-data.pdf
