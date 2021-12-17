#Figures for manuscript

source("libs_and_funcs.R")

library(scales);library(patchwork)

#Figure 1 
#Map showing Denmark and sites

response_df <- readRDS(paste0(getwd(), "/data/", "response_df.rds"))

dk_border <- st_read(dsn = gis_database, layer = "dk_border") 
dk_iceage <- st_read(dsn = gis_database, layer = "dk_iceage") 
lakes <- st_read(dsn = gis_database, layer = "lakes_grass") 

dk_iceage_cut <- dk_iceage %>% 
  st_cast("LINESTRING") %>% 
  st_intersection(dk_border) %>% 
  st_collection_extract("LINESTRING")

lakes_points <- lakes %>% 
  filter(gml_id %in% response_df$gml_id) %>% 
  st_centroid()

#DK plot
figure_1 <- ggplot()+
  geom_sf(data = dk_border, fill = grey(0.8), col = grey(0.8))+
  geom_sf(data = dk_iceage_cut, col = "dodgerblue", linetype = 1, show.legend = FALSE)+
  geom_sf(data = lakes_points, shape=1, size = 0.6)+
  scale_x_continuous(breaks = seq(8, 15, 1), labels = paste0(seq(8, 15, 1),'°E')) +
  scale_y_continuous(breaks = seq(54.5, 57.5, 0.5), labels = paste0(seq(54.5, 57.5, 0.5),'°N'))+
  theme(legend.position = "bottom")+
  guides(fill=guide_colorbar(title.position = "top", barwidth = 8, title.hjust = 0.5))

ggsave(paste0(getwd(), "/manuscript/figures/figure_1.png"), figure_1, units = "mm", width = 129, height = 84)

#Table 1
#Table with response variable distributional characteristics
table_1 <- response_df %>% 
  select(-gml_id) %>% 
  gather(variable, value) %>% 
  na.omit() %>% 
  group_by(variable) %>% 
  summarise(min = min(value), q25 = quantile(value, 0.25), median = median(value), 
            mean = mean(value), q75 = quantile(value, 0.75), max=max(value), n=n())

write_csv(table_1, paste0(getwd(), "/manuscript/figures/table_1.csv"))

#Figure 2
#Lake and catchment area distributions
#A - lake area
#B - catch area
#C - lake and catch area ratio

all_features <- readRDS(paste0(getwd(), "/data/", "all_features.rds"))

df_other <- all_features$df_other

figure_2_a <- df_other %>% 
  ggplot(aes(x = lake_area_m2)) +
  geom_density()+
  #geom_density(aes(y=log10(..count..)), adjust=10)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  ylab("Density")+
  xlab(expression(Lake~area~"("*m^{2}*")"))

figure_2_b <- df_other %>% 
  ggplot(aes(x = catch_area_m2)) +
  geom_density()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  ylab("Density")+
  xlab(expression(Catchment~area~"("*m^{2}*")"))

figure_2_c <- df_other %>% 
  ggplot(aes(x = lake_catch_area_ratio)) +
  geom_vline(xintercept = 1, linetype=3) +
  geom_density()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  ylab("Density")+
  xlab("Lake to catchment area ratio")

figure_2 <- figure_2_a+figure_2_b+figure_2_c+plot_annotation(tag_levels = "A")+plot_layout(ncol = 1)

ggsave(paste0(getwd(), "/manuscript/figures/figure_2.png"), figure_2, units = "mm", width = 84, height = 150)

#Figure 3
#Benchmark of learners

model_bmr <- readRDS(paste0(getwd(), "/data/", "model_bmr.rds"))

model_aggr_df <- lapply(model_bmr, \(x){x$aggr}) %>% 
  bind_rows()

model_aggr_df_clean <- model_aggr_df %>% 
  mutate(response = gsub("_response", "", response),
         model = gsub("regr.", "", learner.id),
         model = gsub(".tuned", "", model)) %>% 
  select(response, model, rmse.test.rmse, rsq.test.mean, mae.test.mean) %>% 
  gather(variable, value, rmse.test.rmse:mae.test.mean) %>% 
  mutate(variable = str_sub(variable, end = -11)) %>% 
  group_by(response, variable) %>% 
  mutate(value_rank = rank(-value)) 

figure_3 <- model_aggr_df_clean %>% 
  ggplot(aes(reorder(model, value_rank), value))+
  geom_point()+
  geom_linerange(aes(ymin=0, ymax=value))+
  coord_flip(ylim=c(0, 0.80))+
  geom_hline(yintercept = 0, linetype=3)+
  facet_grid(response~variable, scales = "free")+
  xlab("Model")+
  ylab("Score")+
  theme(strip.background = element_blank())

ggsave(paste0(getwd(), "/manuscript/figures/figure_3.png"), figure_3, units = "mm", width = 174, height = 200)

#Figure 4 
#Density distributions of predicted values

#Figure 5
#Partial dependence plots of the most important predictors

#Figure 6
#Dimensionality reduction of the response varibles based on variable importance

