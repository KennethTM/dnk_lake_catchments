#Load libraries

library(data.table);library(Hmisc);library(mgcv);library(seacarb)
library(raster);library(sf);library(gdalUtils);library(tidyverse)
library(exactextractr);library(rgrass7);library(link2GI)
library(nngeo);library(readxl);library(lubridate);library(parallelMap)
library(recipes);library(iml);library(mlr)
library(rsample);library(scales);library(patchwork);library(RColorBrewer)

set.seed(9999)

#Define some paths, constants, and functions

rawdata_path <- paste0(getwd(), "/rawdata/")

richdem_apps_path <- paste0(getwd(), "/richdem_apps/")

lakes_sub_path <- paste0(getwd(), "/data/lakes_sub/")
basin_sub_path <- paste0(getwd(), "/data/basin_sub/")
catchments_sub_path <- paste0(getwd(), "/data/catchments_sub/")
flowdir_sub_path <- paste0(getwd(), "/data/flowdir_sub/")

processed_catchments <- parse_number(list.files(catchments_sub_path, pattern = "*.shp"))

dk_epsg <- 25832
gis_database <- paste0(getwd(), "/data/gis_database.sqlite")

dhym <- paste0(getwd(), "/rawdata/dhym_rain.vrt")

response_vars <- c("alk", "chl_a", "color", "ph", "tn", "tp", "secchi", "pco2")

labels_df <- data.frame(variable = c("alk", "chl_a", "color", "pco2", "ph", "secchi", "tn", "tp"), 
                        label_table = c("Alkalinity", "Chlorophyll a", "Color", "pCO2", "pH", "Secchi depth", "Total nitrogen", "Total phosphorus"),
                        label_unit = factor(c("Alkalinity~'('*meq.~L^{-1}*')'", "Chlorophyll~italic(a)~'('*mu*g~L^{-1}*')'", "Color~'('*mg~Pt~L^{-1}*')'", "pCO[2]~'('*mu*atm*')'", "pH~'('*pH*')'", "Secchi~'('*m*')'", "TN~'('*mg~L^{-1}*')'", "TP~'('*mg~L^{-1}*')'"),
                                            levels = c("Alkalinity~'('*meq.~L^{-1}*')'", "Chlorophyll~italic(a)~'('*mu*g~L^{-1}*')'", "Color~'('*mg~Pt~L^{-1}*')'", "pCO[2]~'('*mu*atm*')'", "pH~'('*pH*')'", "Secchi~'('*m*')'", "TN~'('*mg~L^{-1}*')'", "TP~'('*mg~L^{-1}*')'")),
                        label_no_unit = factor(c("Alk.", "Chl.~italic(a)", "Color", "pCO[2]", "pH", "Secchi", "TN", "TP"),
                                            levels = c("Alk.", "Chl.~italic(a)", "Color", "pCO[2]", "pH", "Secchi", "TN", "TP")),
                        label_long_no_unit = factor(c("Alkalinity", "Chlorophyll~italic(a)", "Color", "pCO[2]", "pH", "Secchi~depth", "Total~nitrogen", "Total~phosphorus"),
                                               levels = c("Alkalinity", "Chlorophyll~italic(a)", "Color", "pCO[2]", "pH", "Secchi~depth", "Total~nitrogen", "Total~phosphorus")))

model_labels_df <- data.frame(model = c("featureless", "lm", "glmnet", "fnn", "rpart", "plsr", "nnet", "svm", "ranger"),
                              label = c("AVG", "LM", "ELAST", "NN", "TREE", "PLSR", "NNET", "SVM", "RF"))

#Figure sizing. For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.
#ggplot theme
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)

#Function to compute polygon bbox width and height for each sf feature
st_dims_by_feature <- function(x, mode = "height") {
  x <- st_geometry(x)

  if(mode == "height"){
    f <- \(x){b <- st_bbox(x); return(b[["ymax"]] - b[["ymin"]])}
  }else if(mode == "width"){
    f <- \(x){b <- st_bbox(x); return(b[["xmax"]] - b[["xmin"]])}
  }else{
    return(NULL)
  }

  do.call("c", lapply(x, f))
}
