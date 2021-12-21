#Figures for manuscript

source("libs_and_funcs.R")

library(scales);library(patchwork);library(RColorBrewer)

response_df <- readRDS(paste0(getwd(), "/data/", "response_vars.rds"))
model_results <- readRDS(paste0(getwd(), "/data/", "model_results.rds"))

#Table 1
#Table with response variable distributional characteristics
table_1 <- response_df %>% 
  select(-gml_id) %>% 
  gather(variable, value) %>% 
  na.omit() %>% 
  group_by(variable) %>% 
  summarise(min = min(value), q25 = quantile(value, 0.25), median = median(value), 
            mean = mean(value), q75 = quantile(value, 0.75), max=max(value), n=n()) %>% 
  arrange(variable)

table_1

write_csv(table_1, paste0(getwd(), "/manuscript/figures/table_1.csv"))

#CORRELATION BETWEEN RESPONSES

#Table 2
#Table with test set performance characteristics
test_performance <- model_results$performance
test_obs_n <- lapply(model_results$predictions, \(x){nrow(x$data)})
test_performance_n <- mapply(\(x, y){c(x, "n" = y)}, test_performance, test_obs_n, SIMPLIFY = FALSE)

table_2 <- bind_rows(test_performance_n, .id = "variable") %>% 
  arrange(variable)

table_2

write_csv(table_2, paste0(getwd(), "/manuscript/figures/table_2.csv"))

#Figure 1 
#Map showing Denmark and sites
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

#Figure 2
#Lake and catchment area distributions
all_features <- readRDS(paste0(getwd(), "/data/", "all_features.rds"))

df_other <- all_features$df_other

lake_obs_area <- lakes %>% 
  filter(gml_id %in% response_df$gml_id) %>% 
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY))) %>% 
  st_drop_geometry()

figure_2_a <- df_other %>% 
  filter(lake_area_m2 > 100) %>% 
  ggplot(aes(x = lake_area_m2)) +
  geom_histogram(aes(y=..count.., fill="All lakes"), binwidth = 0.5, col="black")+
  geom_histogram(data = lake_obs_area, aes(y=..count.., fill="Lakes used in analysis"), binwidth = 0.5, col="black", alpha=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = expansion(mult = c(0, .1)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  ylab("Count")+
  xlab(expression(Lake~area~"("*m^{2}*")"))+
  scale_fill_manual(values = c("white", "dodgerblue"))+
  theme(legend.title = element_blank(), legend.position = c(0.75, 0.78))

figure_2_b <- df_other %>% 
  filter(catch_area_m2 > 100) %>% 
  ggplot(aes(x = catch_area_m2)) +
  geom_histogram(aes(y=..count..), binwidth = 0.5, fill="white", col="black")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = expansion(mult = c(0, .1)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  ylab("Count")+
  xlab(expression(Catchment~area~"("*m^{2}*")"))
  
figure_2_c <- df_other %>% 
  ggplot(aes(x = lake_catch_area_ratio)) +
  geom_histogram(aes(y=..count..), binwidth = 0.5, fill="white", col="black")+
  geom_vline(xintercept = 1, linetype=3) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = expansion(mult = c(0, .1)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  ylab("Count")+
  xlab("Lake to catchment area ratio")

figure_2 <- figure_2_a+figure_2_b+figure_2_c+plot_annotation(tag_levels = "A")+plot_layout(ncol = 1)

ggsave(paste0(getwd(), "/manuscript/figures/figure_2.png"), figure_2, units = "mm", width = 129, height = 190)

#Figure 3 
#Density distributions of predicted values
predict_all_df <- read_csv(paste0(getwd(), "/data/", "all_predict.csv"))

figure_3 <- predict_all_df %>% 
  select(-contains("_id")) %>% 
  gather(variable, value) %>% 
  ggplot(aes(value)) +
  geom_freqpoly(aes(y=..count..), col="black", bins=50)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
               labels = trans_format("log10", math_format(10^.x)),
               expand = expansion(mult = c(0, .1)))+
  facet_wrap(variable~., scales="free", ncol=2)+
  ylab("Count")+
  xlab(NULL)+
  theme(strip.background = element_blank())

figure_3

ggsave(paste0(getwd(), "/manuscript/figures/figure_3.png"), figure_3, units = "mm", width = 174, height = 200)

#Figure 4
#Variable importance plot
importance <- model_results$importance

importance_df <- bind_rows(importance, .id = "response") %>% 
  select(response, feature, importance) %>% 
  group_by(response) %>% 
  mutate(importance_normalized = (importance - min(importance))/(max(importance) - min(importance)))

importance_cleaned <- importance_df %>% 
  mutate(feature = gsub("mean.", "", feature),
         feature = gsub("10m.", "", feature))

figure_4 <- importance_cleaned %>% 
  rename(Importance = importance_normalized) %>% 
  ggplot(aes(x = factor(response), y=factor(reorder(feature, Importance)), fill=Importance))+
  geom_tile()+
  scale_x_discrete(expand = c(0,0))+
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"))+
  theme(legend.position = "top", legend.direction = "horizontal")+
  guides(fill=guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = unit(82, "mm"), ticks=FALSE))+
  ylab("Predictor variable")+
  xlab("Response variable")

figure_4

ggsave(paste0(getwd(), "/manuscript/figures/figure_4.png"), figure_4, units = "mm", width = 129, height = 180)

#Figure 5
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

ale_df_labels <- ale_df_top_4 %>% 
  group_by(response, feature) %>% 
  summarise(.borders = last(.borders), .value = last(.value))

figure_5 <- ale_df_top_4 %>% 
  ggplot(aes(.borders, .value, col=feature))+
  geom_line()+
  geom_text(data = ale_df_labels, aes(label = feature), hjust=0)+ 
  facet_wrap(response~., scales="free", ncol=2)+
  scale_color_viridis_d()+
  #ylab("Density")+
  #xlab(NULL)+
  theme(strip.background = element_blank())

#COLOR BY IMPORTANCE RANK FOR EACH RESPONSE EXPAND X-AXIS

figure_5

ggsave(paste0(getwd(), "/manuscript/figures/figure_5.png"), figure_5, units = "mm", width = 174, height = 200)

#Figure 6
#Dimensionality reduction of the response variables based on variable importance
importance_wide <- importance_cleaned %>% 
  ungroup() %>% 
  select(-importance) %>% 
  spread(feature, importance_normalized)

pca_res <- prcomp(importance_wide[,-1], scale. = TRUE)

pca_df <- data.frame(response = importance_wide$response, PC1 = pca_res$x[,1], PC2 = pca_res$x[,2])

figure_6 <- pca_df %>% 
  ggplot(aes(PC1, PC2, label=response))+
  geom_text()+
  xlab("1st principal component (36%)")+
  ylab("2nd principal component (25%)")

ggsave(paste0(getwd(), "/manuscript/figures/figure_6.png"), figure_6, units = "mm", width = 84, height = 84)

#Supplementary figure 1
#Benchmark of learners
model_bmr <- readRDS(paste0(getwd(), "/data/model_bmr_211221.rds"))

model_aggr_df <- lapply(model_bmr, \(x){x$aggr}) %>% 
  bind_rows(.id="response")

model_aggr_df_clean <- model_aggr_df %>% 
  mutate(response = gsub("_response", "", response),
         model = gsub("regr.", "", learner.id),
         model = gsub(".tuned", "", model)) %>% 
  select(response, model, rmse.test.rmse, rsq.test.mean, mae.test.mean) %>% 
  gather(variable, value, rmse.test.rmse:mae.test.mean) %>% 
  mutate(variable = str_sub(variable, end = -11)) %>% 
  group_by(response, variable) %>% 
  mutate(value_rank = rank(-value)) 

figure_s1 <- model_aggr_df_clean %>% 
  ggplot(aes(reorder(model, value_rank), value))+
  geom_point()+
  geom_linerange(aes(ymin=0, ymax=value))+
  coord_flip(ylim=c(0, 0.70))+
  geom_hline(yintercept = 0, linetype=3)+
  facet_grid(response~variable, scales = "free")+
  xlab("Model")+
  ylab("Score")+
  theme(strip.background = element_blank())

figure_s1

ggsave(paste0(getwd(), "/manuscript/figures/figure_s1.png"), figure_s1, units = "mm", width = 174, height = 200)
