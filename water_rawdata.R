#Processing lake water rawdata

source("libs_and_funcs.R")

library(data.table);library(seacarb);library(mgcv)

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
lake_all_carb_agg <- lake_all_carb_gml %>% 
  mutate(month = month(date)) %>% 
  select(-date, -wtr) %>% 
  gather(variable, value, -gml_id, -month) %>% 
  group_by(gml_id, variable,  month) %>% 
  na.omit() %>% 
  summarise(value_med = median(value)) %>% 
  add_tally() %>% 
  ungroup() %>% 
  filter(n >= 4) %>% 
  select(-n) %>% 
  spread(variable, value_med) %>% 
  mutate(gml_id = factor(gml_id))

#Fit cyclic cubic spline models for each variable with "site" as random effect
gam_list <- lapply(response_vars, function(var){
  mod_form <- as.formula(paste0(var, "~s(month, bs='cc') + s(gml_id, bs='re')"))
  mod <- bam(formula=mod_form, data=lake_all_carb_agg, discrete=TRUE)
  return(mod)
})
names(gam_list) <- response_vars
lapply(gam_list, summary)
saveRDS(gam_list, paste0(getwd(), "/data/", "gam_list.rds"))

#Create site by month grid
month_grid <- expand.grid(gml_id = factor(unique(lake_all_carb_agg$gml_id)), month = 1:12)

#Create predictions on grid for each variable
pred_list <- lapply(gam_list, function(mod){
  predict(mod, newdata=month_grid, discrete=FALSE)
})
names(pred_list) <- paste0(response_vars, "_interp")

#Join predictions with grid
month_grid_with_pred <- bind_cols(month_grid, pred_list)

#Add predictions and fill NA values with predicted values
#Replace negative (impossible values) with zeroes
neg_replace <- function(x){ifelse(x < 0, 0, x)}

lake_all_carb_interp <- lake_all_carb_agg %>%
  right_join(month_grid_with_pred) %>%
  arrange(gml_id, month) %>% 
  mutate(alk_response = coalesce(alk, alk_interp),
         chl_a_response = coalesce(chl_a, chl_a_interp),
         color_response = coalesce(color, color_interp),
         pco2_response = coalesce(pco2, pco2_interp),
         ph_response = coalesce(ph, ph_interp),
         secchi_response = coalesce(secchi, secchi_interp),
         tn_response = coalesce(tn, tn_interp),
         tp_response = coalesce(tp, tp_interp)) %>% 
  mutate_at(vars(contains("_response")), ~neg_replace(.))

#Calculate annual mean for each site
lake_all_carb_mean <- lake_all_carb_interp %>% 
  group_by(gml_id) %>% 
  summarise_at(vars(contains("_response")), list(mean))

saveRDS(lake_all_carb_mean, paste0(getwd(), "/data/", "response_df.rds"))
