#Summary statistics used in manuscript 

source("libs_and_funcs.R")

catch_all_no_lake <- st_read(gis_database, "catch_all_no_lake")
lakes <- st_read(gis_database, "lakes_grass")
dk_border <- st_read(gis_database, layer = "dk_border")

dk_area <- as.numeric(st_area(dk_border))
lake_area <- as.numeric(st_area(st_union(lakes)))
catch_area <- as.numeric(st_area(st_union(catch_all_no_lake)))

lake_area_prop <- lake_area/dk_area*100
catch_area_prop <- catch_area/dk_area*100

lakes_area <- lakes %>% 
  mutate(area = as.numeric(st_area(GEOMETRY)))

n_small_lakes <- lakes_area %>% 
  filter(area < 10000) %>% 
  nrow()

prop_small_lakes <- n_small_lakes/nrow(lakes)*100

mean(lakes_area$area)
median(lakes_area$area)

catch_area <- catch_all_no_lake %>% 
  mutate(area = as.numeric(st_area(GEOMETRY)))

mean(catch_area$area)
median(catch_area$area)

catch_to_lake_area <- st_drop_geometry(catch_area) %>% 
  rename(catch_area = area) %>% 
  left_join(st_drop_geometry(lakes_area)) %>% 
  mutate(lake_catch_prop = area/catch_area) %>% 
  filter(gml_id != "dk.hy-p.1093392866")

mean(catch_to_lake_area$lake_catch_prop)
median(catch_to_lake_area$lake_catch_prop)

all_predict <- read_csv(paste0(getwd(), "/data/", "all_predict.csv"))
summary(all_predict)