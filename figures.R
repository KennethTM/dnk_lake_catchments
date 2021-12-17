#Figures for manuscript

source("libs_and_funcs.R")

library(scales);library(patchwork);library(RColorBrewer)

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
predict_all_df <- read_csv(paste0(getwd(), "/data/", "predict_all.csv"))

figure_4 <- predict_all_df %>% 
  select(-contains("_id")) %>% 
  gather(variable, value) %>% 
  ggplot(aes(value)) +
  geom_density()+
  facet_wrap(variable~., scales="free", ncol=2)+
  ylab("Density")+
  xlab(NULL)+
  theme(strip.background = element_blank())
  
ggsave(paste0(getwd(), "/manuscript/figures/figure_4.png"), figure_4, units = "mm", width = 174, height = 200)

#Figure 5
#Variable importance plot
model_results <- readRDS(paste0(getwd(), "/data/", "model_results.rds"))
importance <- model_results$importance

importance_df <- bind_rows(importance, .id = "response") %>% 
  select(response, feature, importance) %>% 
  group_by(response) %>% 
  mutate(importance_normalized = (importance - min(importance))/(max(importance) - min(importance)))

importance_cleaned <- importance_df %>% 
  mutate(feature = gsub("mean.", "", feature),
         feature = gsub("10m.", "", feature))

figure_5 <- importance_cleaned %>% 
  rename(Importance = importance_normalized) %>% 
  ggplot(aes(x = factor(response), y=factor(reorder(feature, Importance)), fill=Importance))+
  geom_tile()+
  scale_x_discrete(expand = c(0,0))+
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"))+
  theme(legend.position = "top", legend.direction = "horizontal")+
  guides(fill=guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = unit(82, "mm"), ticks=FALSE))+
  ylab("Predictor variable")+
  xlab("Response variable")

ggsave(paste0(getwd(), "/manuscript/figures/figure_5.png"), figure_5, units = "mm", width = 129, height = 180)

#Figure 6
#Ale plots for ?? most important vars
top_4_imp <- importance_df %>% 
  group_by(response) %>% 
  arrange(desc(importance)) %>% 
  slice(1:4) %>% 
  ungroup()

ale <- model_results$ale
ale_df <- bind_rows(lapply(model_results$ale, \(x){bind_rows(x)}), .id = "response")

ale_df_top_4 <- top_4_imp %>% 
  select(response, feature) %>% 
  left_join(ale_df, by = c("response" = "response", "feature" =".feature"))

figure_6 <- ale_df_top_4 %>% 
  ggplot(aes(.borders, .value, col=feature))+
  geom_line()+
  facet_wrap(response~., scales="free", ncol=2)+
  #ylab("Density")+
  #xlab(NULL)+
  theme(strip.background = element_blank())

ggsave(paste0(getwd(), "/manuscript/figures/figure_6.png"), figure_6, units = "mm", width = 174, height = 200)

#Figure 7
#Dimensionality reduction of the response variables based on variable importance
importance_wide <- importance_cleaned %>% 
  ungroup() %>% 
  select(-importance) %>% 
  spread(feature, importance_normalized)

pca_res <- prcomp(importance_wide[,-1], scale. = TRUE)

pca_df <- data.frame(response = importance_wide$response, PC1 = pca_res$x[,1], PC2 = pca_res$x[,2])

figure_7 <- pca_df %>% 
  ggplot(aes(PC1, PC2, label=response))+
  geom_text()+
  xlab("1st principal component (36%)")+
  ylab("2nd principal component (25%)")

ggsave(paste0(getwd(), "/manuscript/figures/figure_7.png"), figure_7, units = "mm", width = 84, height = 84)
