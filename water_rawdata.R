#Processing lake water rawdata

source("libs_and_funcs.R")

library(data.table);library(seacarb);library(zoo)

#Lake chemistry, profile (water temperature) and secchi depth raw data 
lake_chem <- read_excel(paste0(rawdata_path, "lake_chemistry_2000_2019.xlsx"))
lake_chem <- as.data.table(lake_chem)

lake_profile <- read_excel(paste0(rawdata_path, "lake_profile_2000_2019.xlsx"))
lake_profile <- as.data.table(lake_profile)

lake_secchi <- read_excel(paste0(rawdata_path, "lake_field_2000_2019.xlsx"))
lake_secchi <- as.data.table(lake_secchi)

translate_vars <- data.table(Parameter = c("pH", "Farvetal-Pt", "Alkalinitet,total TA", "Nitrogen,total N", "Phosphor, total-P", "Vandtemperatur", "Sigtdybde", "Chlorophyl (ukorrigeret)", "Chlorophyl A"),
                             variable = c("ph", "color", "alk", "tn", "tp", "wtr", "secchi", "chl_a", "chl_a"),
                             units = c("ph", "mg_pt_l", "mmol_l", "mg_l", "mg_l", "degrees", "m", "ug_l", "ug_l"))

#Clean names and variables for chemistry data
lake_chem_clean <- lake_chem[translate_vars, on = "Parameter", nomatch = 0L
                             ][, .(site_id = ObservationsStedNr,
                                   x_coord = as.numeric(Xutm_Euref89_Zone32), y_coord = as.numeric(Yutm_Euref89_Zone32),
                                   date = ymd(Startdato), depth_sample = type.convert(`GennemsnitsDybde i m`, as.is = TRUE, dec=","),
                                   variable, value = Resultat)]

#Cast to wide format with a column for each variable
#Keep surface samples only
lake_chem_clean_wide <- dcast(lake_chem_clean[order(site_id, date, depth_sample)], 
                              site_id + x_coord + y_coord + date ~ variable, fun.aggregate = first, fill = NA, value.var = "value")

#Set key for chemistry table to speed up later processing
setkey(lake_chem_clean_wide, site_id, date)

#Clean names and variables for profile data
lake_profile_clean <- lake_profile[translate_vars, on = "Parameter", nomatch = 0L
                                   ][, .(site_id = ObservationsStedNr, 
                                         x_coord = as.numeric(Xutm_Euref89_Zone32), y_coord = as.numeric(Yutm_Euref89_Zone32),
                                         date = ymd(Startdato), depth_sample = type.convert(MåledybdeM, as.is = TRUE, dec=","),
                                         variable, value = Resultat)]

#Cast to wide format with a column for each variable
#Keep surface samples only
lake_profile_clean_wide <- dcast(lake_profile_clean[order(site_id, date, depth_sample)], 
                                 site_id + date ~ variable, value.var = "value", fun = first, fill = NA)
lake_profile_clean_wide <- setnames(lake_profile_clean_wide, "ph", "ph_profile")

#Set key for profile table
setkey(lake_profile_clean_wide, site_id, date)

#Clean names and variables for secchi data
lake_secchi_clean <- lake_secchi[translate_vars, on = "Parameter", nomatch = 0L
                                 ][, .(site_id = ObservationsStedNr, x_coord = as.numeric(Xutm_Euref89_Zone32_ZONE32), 
                                       y_coord = as.numeric(Yutm_Euref89_Zone32_ZONE32), date = ymd(Startdato), 
                                       variable, value = Resultat)]

#Cast to wide format with a column for each variable
lake_secchi_clean_wide <- dcast(lake_secchi_clean[order(site_id, date)], site_id + date ~ variable, value.var = "value", fun = mean, fill = NA)

#Set key for secchi table
setkey(lake_secchi_clean_wide, site_id, date)

#Join chemistry, profile and secchi data
lake_all <- lake_profile_clean_wide[lake_chem_clean_wide
                                    ][lake_secchi_clean_wide
                                      ][, ph := fcoalesce(ph, ph_profile)
                                        ][, ph_profile := NULL
                                          ][!is.na(x_coord), ]

#Calculate inorganic carbon system from pH, temperature and alkalinity
lake_carb <- carb(flag = 8, lake_all$ph, lake_all$alk/1000, T = lake_all$wtr, S = 0, Patm = 1, k1k2 = "w14", kf = "dg", ks = "d")

#Select carb vars and set names
lake_carb_sub <- data.table("pco2" = lake_carb$pCO2)

#Bind to table
lake_all_carb <- cbind(lake_all, lake_carb_sub)

#Filter observations and convert to spatial object
lake_all_carb_sf <- lake_all_carb %>%
  as_tibble() %>% 
  mutate(alk = ifelse(alk > 20, NA, alk),
         pco2 = ifelse(pco2 < 0 | ph < 5.4 | is.na(alk), NA, pco2)) %>% 
  filter(y_coord > 6*10^6) %>% 
  st_as_sf(coords=c("x_coord", "y_coord"), crs = dk_epsg)

#Add gml_id from lake polygons for site aggregation
lakes <- st_read(gis_database, "lakes_grass") %>% 
  rename(basin_id = basin_grass_id) %>% 
  select(gml_id)

lake_all_carb_gml <- lake_all_carb_sf %>% 
  st_join(lakes) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  filter(!is.na(gml_id)) %>% 
  select(-site_id)

#Aggregate by lake polygons
#Calculate monthly median across years
#Interpolate between months to calculate annual mean for all variables
lake_all_carb_agg <- lake_all_carb_gml[1:500,] %>% 
  mutate(month = month(date)) %>% 
  select(-date) %>% 
  group_by(gml_id, month) %>% 
  summarise_all(~median(., na.rm = TRUE)) %>% 
  right_join(expand.grid(gml_id = unique(.$gml_id),month = 1:12)) %>% 
  arrange(gml_id, month) %>% 
  mutate_at(vars(-group_cols()), ~na.approx(., na.rm = FALSE)) %>% 
  mutate_at(vars(-group_cols()), ~na.locf(., na.rm = FALSE)) 
  

#   select(-site_id, -date, -dic, -co2, -elevation) %>% 
#   gather(variable, value, -gml_id, -month) %>%
#   na.omit() %>% 
#   group_by(gml_id, variable, month) %>% 
#   summarise(mean_month = mean(value)) %>% 
#   summarise(mean = mean(mean_month), n = n()) %>% 
#   ungroup() %>% 
#   filter(n >= 3) %>% 
#   select(-n) %>% 
#   spread(variable, mean)
# 
# summary(lake_all_carb_agg)
# 
# #Add polygon features and write data to gis database
# lake_all_carb_poly <- lake_all_carb_agg %>% 
#   left_join(select(dk_lakes, gml_id, elevation)) %>% 
#   mutate(gml_id_num = str_sub(gml_id, start=9)) %>% 
#   st_as_sf() %>% 
#   mutate(habitat = "lake") %>% 
#   select(-gml_id)
# 
# st_write(lake_all_carb_poly, dsn = gis_database, layer = "lake_poly", delete_layer = TRUE)
