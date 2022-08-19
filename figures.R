#Figures for manuscript

source("libs_and_funcs.R")

response_df <- readRDS(paste0(getwd(), "/data/", "response_vars.rds"))
model_results <- readRDS(paste0(getwd(), "/data/", "model_results.rds"))
all_features <- readRDS(paste0(getwd(), "/data/", "all_features.rds"))

feature_labels <- read_excel(paste0(getwd(), "/rawdata/", "feature_labels.xlsx"))

#Table 1
#Table with response variable distributional characteristics
table_1 <- response_df %>% 
  select(-gml_id) %>% 
  gather(variable, value) %>% 
  na.omit() %>% 
  group_by(variable) %>% 
  summarise(min = min(value), q25 = quantile(value, 0.25), median = median(value), 
            mean = mean(value), q75 = quantile(value, 0.75), max=max(value), n=n()) %>% 
  left_join(labels_df) %>% 
  select(label_table, -label_unit, -variable, min:n) %>% 
  arrange(label_table)

table_1

write_csv(table_1, paste0(getwd(), "/manuscript/figures/table_1.csv"))

#Table 2
#Table with test set performance characteristics
test_performance <- model_results$performance
test_obs_n <- lapply(model_results$predictions, \(x){nrow(x$data)})
test_performance_n <- mapply(\(x, y){c(x, "n" = y)}, test_performance, test_obs_n, SIMPLIFY = FALSE)

table_2 <- bind_rows(test_performance_n, .id = "variable") %>% 
  left_join(labels_df) %>% 
  select(label_table, -label_unit, -variable, rmse:n) %>% 
  arrange(label_table) %>% 
  rename(RMSE = rmse, MAE = mae, R2 = rsq, N = n)

table_2

write_csv(table_2, paste0(getwd(), "/manuscript/figures/table_2.csv"))

#Table 3
#Table with summary of cross-validation statistics from using water quality predictions as features
preds_as_feats <- readRDS(paste0(getwd(), "/data/", "predictions_as_features_results.rds"))

table_3 <- preds_as_feats %>% 
  bind_rows(.id = "variable") %>% 
  select(-iter, -timeboth) %>% 
  gather(score, value, rmse, rsq, mae) %>% 
  group_by(variable, score) %>% 
  summarise(mean = mean(value), sd = sd(value)) %>% 
  ungroup() %>% 
  mutate(value_format = paste0(round(mean, 3), " (±", round(sd, 3), ")")) %>% 
  left_join(labels_df) %>% 
  select(label_table, score, value_format) %>% 
  spread(score, value_format) %>% 
  arrange(label_table) %>% 
  rename(RMSE = rmse, MAE = mae, R2 = rsq) %>% 
  select(label_table, RMSE, MAE, R2)

table_3

write_csv(table_3, paste0(getwd(), "/manuscript/figures/table_3.csv"))

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
  right_join(response_df) %>% 
  st_centroid() %>% 
  mutate(alk_impute = ifelse(is.na(alk), median(alk, na.rm = TRUE), alk))

#DK plot
figure_1 <- ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "black", size = 0.3)+
  geom_sf(data = lakes_points, aes(col=alk_impute), size = 0.6)+
  geom_sf(data = dk_iceage_cut, col = "black", linetype = 2, show.legend = FALSE)+ #brewer.pal(5, "BrBG")[5]
  scale_color_viridis_c(direction = -1)+
  scale_x_continuous(breaks = seq(8, 15, 1), labels = paste0(seq(8, 15, 1),'°E')) +
  scale_y_continuous(breaks = seq(54.5, 57.5, 0.5), labels = paste0(seq(54.5, 57.5, 0.5),'°N'))+
  theme(legend.position = c(0.8, 0.85), legend.direction = "horizontal")+
  xlab("Longitude")+
  ylab("Latitude")+
  guides(color=guide_colorbar(title = expression("Alkalinity (meq."~L^{-1}*")"), title.position = "top", barwidth = unit(35, "mm"), ticks=FALSE))

figure_1

ggsave(paste0(getwd(), "/manuscript/figures/figure_1.png"), figure_1, units = "mm", width = 129, height = 100)
ggsave(paste0(getwd(), "/manuscript/figures/figure_1.pdf"), figure_1, units = "mm", width = 129, height = 100)

#Figure 2
#Lake and catchment area distributions
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
                expand = expansion(mult = c(0, 0.1)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  ylab("Count")+
  xlab(expression(Lake~area~"("*m^{2}*")"))+
  scale_fill_manual(values = c("white", brewer.pal(5, "BrBG")[5]))+
  theme(legend.title = element_blank(), legend.position = c(0.78, 0.78))

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
  filter(gml_id != "dk.hy-p.1093392866") %>% 
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

figure_2

ggsave(paste0(getwd(), "/manuscript/figures/figure_2.png"), figure_2, units = "mm", width = 129, height = 190)
ggsave(paste0(getwd(), "/manuscript/figures/figure_2.pdf"), figure_2, units = "mm", width = 129, height = 190)

#Figure 3 
#Density distributions of predicted values
predict_all_df <- read_csv(paste0(getwd(), "/data/", "all_predict.csv"))

predict_long <- predict_all_df %>% 
  select(-contains("_id")) %>% 
  gather(variable, value) %>% 
  left_join(labels_df) %>% 
  select(-variable, -label_table)

predict_stats <- predict_long %>% 
  group_by(label_unit) %>% 
  summarise(Mean = mean(value), Median = median(value), `25%` = quantile(value, 0.25), `75%` = quantile(value, 0.75), 
            `5%` = quantile(value, 0.05), `95%` = quantile(value, 0.95)) %>% 
  gather(stat, value, -label_unit) %>% 
  mutate(stat = factor(stat, levels = c("5%", "25%", "Median", "Mean", "75%", "95%")))

figure_3 <- predict_long %>% 
  mutate(value = ifelse(label_long_no_unit == "Secchi~depth" & value > 4, NA, value),
         value = ifelse(label_long_no_unit == "Total~phosphorus" & value > 1, NA, value),
         value = ifelse(label_long_no_unit == "Color" & value > 800, NA, value),
         value = ifelse(label_long_no_unit == "Chlorophyll~italic(a)" & value > 150, NA, value),
         value = ifelse(label_long_no_unit == "Alkalinity" & value > 6, NA, value)) %>% 
  ggplot(aes(value)) +
  geom_freqpoly(aes(y=..count..), col="black", bins=25)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
               labels = trans_format("log10", math_format(10^.x)),
               expand = expansion(mult = c(0, .1)))+
  geom_vline(data = predict_stats, aes(xintercept = value, col = stat))+
  scale_color_brewer(palette = "BrBG")+
  facet_wrap(label_unit~., scales="free", ncol=2, labeller = label_parsed)+
  ylab("Count")+
  xlab(expression("Values"))+
  theme(strip.background = element_blank(), legend.title = element_blank())

figure_3

ggsave(paste0(getwd(), "/manuscript/figures/figure_3.png"), figure_3, units = "mm", width = 174, height = 200)
ggsave(paste0(getwd(), "/manuscript/figures/figure_3.pdf"), figure_3, units = "mm", width = 174, height = 200)

#Figure 4
#Variable importance plot
importance <- model_results$importance

importance_df <- bind_rows(importance, .id = "response") %>% 
  select(response, feature, importance) %>% 
  group_by(response) %>% 
  mutate(importance_normalized = (importance - min(importance))/(max(importance) - min(importance))) %>% 
  ungroup()

importance_cleaned <- importance_df %>%
  left_join(labels_df, by=c("response" = "variable")) %>% 
  select(-response, -label_table, -label_unit) 

importance_cleaned_label <- importance_cleaned %>% 
  left_join(feature_labels, by =c("feature" = "variable")) %>% 
  mutate(level_abbrev = str_sub(Level, end = 1),
         label_join = paste0(Variable, " (", level_abbrev, ifelse(is.na(Distance), "", Distance), ")"))

figure_4 <- importance_cleaned_label %>% 
  rename(Importance = importance_normalized) %>% 
  ggplot(aes(x = label_no_unit, y=factor(reorder(label_join, Importance)), fill=Importance))+
  geom_tile()+
  scale_x_discrete(expand = c(0,0), position = "top", 
                   labels = parse(text = levels(importance_cleaned_label$label_no_unit)))+
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  guides(fill=guide_colorbar(title.position = "bottom", title.hjust = 0.5, barwidth = unit(100, "mm"), ticks=FALSE))+
  ylab("Predictor variables")+
  xlab("Response variables")

figure_4

ggsave(paste0(getwd(), "/manuscript/figures/figure_4.png"), figure_4, units = "mm", width = 174, height = 234)
ggsave(paste0(getwd(), "/manuscript/figures/figure_4.pdf"), figure_4, units = "mm", width = 174, height = 234)

#Figure 5
#Ale plots for four most important vars
top_4_imp <- importance_df %>% 
  group_by(response) %>% 
  arrange(desc(importance)) %>% 
  slice(1:4) %>% 
  mutate(rank = factor(rank(-importance))) %>% 
  ungroup()

ale <- model_results$ale
ale_df <- bind_rows(lapply(model_results$ale, \(x){bind_rows(x)}), .id = "response")

ale_df_top_4 <- top_4_imp %>% 
  select(response, feature, rank) %>% 
  left_join(ale_df, by = c("response" = "response", "feature" =".feature")) %>% 
  left_join(labels_df, by = c("response" = "variable"))

ale_df_labels <- ale_df_top_4 %>% 
  group_by(response, feature, rank) %>% 
  summarise(.borders = last(.borders), .value = last(.value)) %>% 
  left_join(labels_df, by = c("response" = "variable")) %>% 
  left_join(feature_labels, by =c("feature" = "variable")) %>% 
  mutate(level_abbrev = str_sub(Level, end = 1),
         label_join = paste0(Variable, " (", level_abbrev, ifelse(is.na(Distance), "", Distance), ")"))

figure_5 <- ale_df_top_4 %>% 
  ggplot(aes(.borders, .value, col=rank))+
  geom_line()+
  geom_text(data = ale_df_labels, aes(label = label_join), hjust=0.5, show.legend = FALSE, size = 3)+ 
  facet_wrap(label_long_no_unit~., scales="free", ncol=2, labeller = label_parsed)+
  scale_color_manual(values = rev(brewer.pal(9, "YlOrRd"))[c(1, 3, 5, 6)])+
  ylab(expression("Relative response (log"[10]~scale*")"))+
  xlab("Standardized values (unit standard deviation)")+
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.5)))+
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))+
  theme(strip.background = element_blank(), legend.position = "bottom")+
  guides(color = guide_legend(title = "Importance rank"))

figure_5

ggsave(paste0(getwd(), "/manuscript/figures/figure_5_raw.pdf"), figure_5, units = "mm", width = 174, height = 200)

#Figure 6
#Dimensionality reduction of the response variables based on variable importance
importance_wide <- importance_cleaned %>% 
  ungroup() %>% 
  select(-importance_normalized, -label_long_no_unit) %>% 
  spread(feature, importance)

pca_res <- prcomp(importance_wide[,-1], scale. = TRUE, center = TRUE)
summary(pca_res)

pca_df <- data.frame(response = importance_wide$label_no_unit, PC1 = pca_res$x[,1], PC2 = pca_res$x[,2], PC3 = pca_res$x[,3])

library(plot3D)
svg(paste0(getwd(), "/manuscript/figures/figure_6.svg"))
text3D(x=pca_df$PC1, y=pca_df$PC2, z=pca_df$PC3, xlab="1st PC (33.8%)", ylab="2nd PC (26.1%)", zlab="3rd PC (15.8%)",
       labels = c("Alkalinity", expression(Chl.~italic(a)), "Color", expression(pCO[2]), "pH", "Secchi", "TN", "TP"),
       phi=22, theta = 135+45+45, bty="b")
dev.off()

#Supplementary figure S1
#Benchmark of learners
model_bmr <- readRDS(paste0(getwd(), "/data/model_benchmark.rds"))

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
  mutate(value_rank = rank(-value)) %>% 
  left_join(labels_df, by = c("response" = "variable")) %>% 
  mutate(variable_label = case_when(variable == "rmse" ~ "RMSE",
                                    variable == "mae" ~ "MAE",
                                    variable == "rsq" ~ "R^{2}"),
         variable_label = factor(variable_label, levels = c("R^{2}", "RMSE", "MAE"))) %>% 
  left_join(model_labels_df)

model_aggr_df_best <- model_aggr_df_clean %>% 
  group_by(label_no_unit, variable_label) %>% 
  mutate(value = ifelse(variable == "rsq", value*-1, value)) %>% 
  summarise(label = label[which.min(value)], value_rank = value_rank[which.min(value)], value = min(value)) %>% 
  mutate(value = ifelse(variable_label == "R^{2}", value*-1, value))

figure_s1 <- model_aggr_df_clean %>% 
  ggplot(aes(reorder(label, value_rank), value))+
  geom_col(col="black", fill="white")+
  geom_col(data = model_aggr_df_best, fill="grey", color="black")+
  coord_flip(ylim=c(0, 0.70))+
  geom_hline(yintercept = 0, linetype=3)+
  facet_grid(label_no_unit~variable_label, scales = "free", labeller = label_parsed)+
  xlab("Model")+
  ylab("Score")+
  theme(strip.background = element_blank())

figure_s1

ggsave(paste0(getwd(), "/manuscript/figures/figure_s1.png"), figure_s1, units = "mm", width = 174, height = 234)

#Supplementary figure S2
predictions <- model_results$predictions

obs_pred_df <- lapply(predictions, \(x){x$data}) %>% 
  bind_rows(.id = "variable") %>% 
  as_tibble() %>% 
  select(-id) %>% 
  left_join(labels_df)

figure_s2 <- obs_pred_df %>% 
  ggplot(aes(truth, response))+
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point(shape=1, alpha=0.5)+
  facet_wrap(label_long_no_unit~., scales="free", ncol=2, labeller = label_parsed)+
  ylab(expression("Predicted (log"[10]~transformed*")"))+
  xlab(expression("Observed (log"[10]~transformed*")"))+
  theme(strip.background = element_blank())+
  scale_x_continuous(expand = expansion(mult = c(0.3, 0.3)))+
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.3)))

figure_s2

ggsave(paste0(getwd(), "/manuscript/figures/figure_s2.png"), figure_s2, units = "mm", width = 129, height = 234)

#Supplementary figure S3
#Compare chl_a and tp/tn relationships
predict_all_df <- read_csv(paste0(getwd(), "/data/", "all_predict.csv")) |> 
  mutate(tp_log10 = log10(tp*1000), #convert mg/L to ug/L
         tn_log10 = log10(tn*1000),
         chl_a_log10 = log10(chl_a))

figure_s3_tp <- predict_all_df |> 
  #sample_n(1000) |>
  ggplot(aes(x=tp_log10, y=chl_a_log10))+
  geom_hex(bins=50)+
  scale_fill_gradient(low = grey(0.9), high = grey(0), name="Count", limits = c(0, 2500))+
  geom_smooth(method="lm", aes(col = "This study"), se = FALSE)+
  geom_abline(aes(col = "OECD 1982", intercept = -0.55, slope = 0.96), show.legend = FALSE)+
  geom_abline(aes(col = "Prarie et al. 1989", intercept = -0.39, slope = 0.87), show.legend = FALSE)+
  geom_abline(aes(col = "Jones and Bachmann 1976", intercept = -1.09, slope = 1.46), show.legend = FALSE)+
  geom_abline(aes(col = "Dilon and Rigler 1974", intercept = -1.13, slope = 1.58), show.legend = FALSE)+
  geom_abline(aes(col = "Canfield 1983", intercept = -0.15, slope = 0.74), show.legend = FALSE)+
  geom_abline(aes(col = "Quirós 1990", intercept = -1.94, slope = 1.06), show.legend = FALSE)+
  geom_abline(aes(col = "Pridmore 1990", intercept = -1.13, slope = 1.35), show.legend = FALSE)+
  ylab(expression(log[10]*"(Chlorophyll"~italic(a)~"["*mu*g~L^{-1}*"])"))+
  xlab(expression(log[10]*"(Total phosphorus"~"["*mu*g~L^{-1}*"])"))+
  scale_color_manual(values = c("This study" = brewer.pal(8, "Dark2")[1],
                                "OECD 1982" = brewer.pal(8, "Dark2")[2],
                                "Prarie et al. 1989" = brewer.pal(8, "Dark2")[3],
                                "Jones and Bachmann 1976" = brewer.pal(8, "Dark2")[4],
                                "Dilon and Rigler 1974" = brewer.pal(8, "Dark2")[5],
                                "Canfield 1983" = brewer.pal(8, "Dark2")[6],
                                "Quirós 1990" = brewer.pal(8, "Dark2")[7],
                                "Pridmore 1990" = brewer.pal(8, "Dark2")[8]), name="Study")

figure_s3_tn <- predict_all_df |> 
  #sample_n(1000) |>
  ggplot(aes(x=tn_log10, y=chl_a_log10))+
  geom_hex(bins=50)+
  scale_fill_gradient(low = grey(0.9), high = grey(0), name="Count", limits = c(0, 2500))+
  geom_smooth(method="lm", aes(col = "This study"), se=FALSE)+
  geom_abline(aes(col = "Prarie et al. 1989", intercept = -3.13, slope = 144), show.legend = FALSE)+
  geom_abline(aes(col = "Canfield 1983", intercept = -2.99, slope = 1.38), show.legend = FALSE)+
  geom_abline(aes(col = "Pridmore 1990", intercept = -2.56, slope = 1.22), show.legend = FALSE)+
  ylab(expression(log[10]*"(Chlorophyll"~italic(a)~"["*mu*g~L^{-1}*"])"))+
  xlab(expression(log[10]*"(Total nitrogen"~"["*mu*g~L^{-1}*"])"))+
  scale_color_manual(values = c("This study" = brewer.pal(8, "Dark2")[1],
                                "OECD 1982" = brewer.pal(8, "Dark2")[2],
                                "Prarie et al. 1989" = brewer.pal(8, "Dark2")[3],
                                "Jones and Bachmann 1976" = brewer.pal(8, "Dark2")[4],
                                "Dilon and Rigler 1974" = brewer.pal(8, "Dark2")[5],
                                "Canfield 1983" = brewer.pal(8, "Dark2")[6],
                                "Quirós 1990" = brewer.pal(8, "Dark2")[7],
                                "Pridmore 1990" = brewer.pal(8, "Dark2")[8]), name="Study")

figure_s3 <- figure_s3_tp+figure_s3_tn+plot_annotation(tag_levels = "A")+plot_layout(ncol = 1, guides = "collect")

ggsave(paste0(getwd(), "/manuscript/figures/figure_s3.png"), figure_s3, units = "mm", width = 174, height = 200)

#Supplementary table S1
#Overview of predictor variables
table_s1 <- feature_labels %>% 
  mutate(`Aggregation level` = ifelse(is.na(Distance), Level, paste0(Level, " (", Distance, ")")),
         `Included in modeling` = ifelse(variable %in% names(importance_wide)[-1], "Yes", "No")) %>% 
  select(Variable, Unit, `Aggregation level`, `Aggregation summary` = Summary, `Included in modeling`)

table_s1

write_csv(table_s1, paste0(getwd(), "/manuscript/figures/table_s1.csv"))

#Supplementary table S2
#(in manuscript)

#Supplementary table S3
response_df_cor <- response_df %>% 
  select(-gml_id) %>% 
  mutate_at(vars(chl_a, color, ph, tn, tp, secchi, pco2), ~log10(.x)) %>% 
  mutate_at(vars(alk), ~log10(.x + 1))

corr_matrix <- rcorr(as.matrix(response_df_cor), type="pearson")

corr_matrix_mask <- lower.tri(corr_matrix$r, diag = FALSE)

corr_matrix$r[corr_matrix_mask] = corr_matrix$P[corr_matrix_mask]

corr_matrix_round = round(corr_matrix$r, 3)

write.csv(as.data.frame(corr_matrix_round), paste0(getwd(), "/manuscript/figures/table_s3.csv"))
