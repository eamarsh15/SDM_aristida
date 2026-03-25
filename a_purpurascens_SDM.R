library(rgbif)
library(CoordinateCleaner)
library(terra)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(remotes)
library(corrplot)
library(blockCV)
library(SDMtune)
library(patchwork)
library(flexsdm)
library(conflicted)
library(rasterVis)
library(mapview)
library(ggspatial)
library(patchwork)
library(rnaturalearthdata)
library(ggtext)


apu_clean <- read.table(file = "species_data/a_purpurascens_occ_clean.csv", header = TRUE, sep = ",")

apu_no_crs <- st_as_sf(apu_clean, coords = c('lon', 'lat'), remove = FALSE)
apu_crs84 <- apu_no_crs
st_crs(apu_crs84) = 4326


# Setting AOI -------------------------------------------------------------
se_aoi <- read_sf("SE_study_region_data")
st_crs(se_aoi) = 4326
se_vect <- terra::vect(se_aoi)
aoi <- se_vect

us_states <- ne_states(country = 'united states of america', returnclass = 'sf')
states_sel <- c('North Carolina', 'South Carolina', 'Georgia', 'Florida', 'Alabama',
                'Mississippi', 'Virginia', 'West Virginia', 'Tennessee','Missouri',
                'Louisiana', 'Arkansas', 'Kentucky', 'Indiana', 'Ohio', 'Pennsylvania',
                'Maryland', 'Delaware', 'New Jersey', 'Illinois', 'Missouri',
                'Oklahoma', 'Texas')
us_states %>%
  dplyr::filter(name_en %in% states_sel) -> sern
states <- terra::vect(sern)


# Calibration Area --------------------------------------------------------
# calib_apu <- calib_area (data = as.data.frame(apu_crs84),
#                         x = 'lon', y = 'lat',
#                         method = c('buffer', width = 150000),
#                         crs = crs(apu_crs84))

# aoi <- terra::intersect(calib_apu, states)

# plot(calib_apu)
# plot(aoi, col = 'red', add = TRUE)
# plot(states, add = TRUE)
# plot(apu_crs84, add = TRUE)



# Variable Data ----------------------------------------------------------
### Worldclim
wc_rast <- rast("variable_data/worldclim_named_rast.tiff")

# files <- list.files("worldclim_data", pattern = "\\.tif$", full.names = TRUE)
# files <- files[order(as.numeric(sub(".*bio_([0-9]+)\\.tif$", "\\1", files)))]
# wc_rast <- rast(files)
# 
# wc_crop <- resample(x = wc_rast, y = f_rast)
# 
# wc_crop[[c(1:2,5:11)]] <- wc_crop[[c(1:2,5:11)]]/10
# wc_crop[[3:4]] <- wc_crop[[3:4]]/100
# 
# names(wc_crop) <- c("mean_ann_t","mean_diurnal_t_range", "isothermality",
#                        "t_seas", 'max_t_warm_m','min_t_cold_m', "t_ann_range",
#                        'mean_t_wet_q','mean_t_dry_q','mean_t_warm_q', 'mean_t_cold_q',
#                        'ann_p', 'p_wet_m','p_dry_m','p_seas', 'p_wet_q','p_dry_q',
#                        'p_warm_q','p_cold_q')
# terra::writeRaster(x = wc_crop, file = "worldclim_named_rast.tiff", overwrite = TRUE) 


### Fire
f_rast <- rast("variable_data/modisfire_named_rast.tiff")

# fire_rast_na <- rast("modis_totalmean_30s.tif")
# land_mask <- rasterize(se_vect, fire_rast_na[[1]], field = 1, background = NA)
# mask_base <- rep(land_mask, nlyr(fire_rast_na))
# fire_rast <- ifel(is.na(fire_rast_na) & !is.na(mask_base), 0, fire_rast_na)
# f_crop <- mask(crop(fire_rast, aoi), aoi)
# 
# names(f_crop) <- c("f_intensity", "f_freq", "f_seas")
# 
# terra::writeRaster(x = f_crop, file = "modisfire_named_rast.tiff")

### Soil
s_rast <- rast("variable_data/soil_named_rast.tiff")

# s_rast <- rast("SE_soil_data_30s.tiff")
# 
# s_crop <- resample(x = s_rast, y = wc_rast)
# 
# names(s_rast) <- c("clay_0_5cm_mean", "clay_5_15cm", "clay_15_30cm", "nitrogen_0_5cm",
#                    "nitrogen_5_15cm", "nitrogen_15_30cm", "ocd_0_5cm", "ocd_5_15cm",
#                    "ocd_15_30cm", "soc_0_5cm", "soc_5_15cm", "soc_15_30cm",
#                    "phh2o_0_5cm", "phh2o_5_15cm", "phh2o_15_30cm", "sand_0_5cm",
#                    "sand_5_15cm", "sand_15_30cm", "silt_0_5cm", "silt_5_15cm",
#                    "silt_15_30cm")
# 
# terra::writeRaster(x = s_crop, file = "soil_named_rast.tiff", overwrite = TRUE)

covariates <- c(wc_rast, f_rast, s_rast)

names(covariates) <- c("mean_ann_t","mean_diurnal_t_range", "isothermality",
                       "t_seas", 'max_t_warm_m','min_t_cold_m', "t_ann_range",
                       'mean_t_wet_q','mean_t_dry_q','mean_t_warm_q', 'mean_t_cold_q',
                       'ann_p', 'p_wet_m','p_dry_m','p_seas', 'p_wet_q','p_dry_q',
                       'p_warm_q','p_cold_q',
                       "f_intensity","f_freq","f_seas",
                       "clay_0_5cm", "clay_5_15cm", "clay_15_30cm", "nitrogen_0_5cm",
                       "nitrogen_5_15cm", "nitrogen_15_30cm", "ocd_0_5cm", "ocd_5_15cm",
                       "ocd_15_30cm", "soc_0_5cm", "soc_5_15cm", "soc_15_30cm",
                       "phh2o_0_5cm", "phh2o_5_15cm", "phh2o_15_30cm", "sand_0_5cm",
                       "sand_5_15cm", "sand_15_30cm", "silt_0_5cm", "silt_5_15cm",
                       "silt_15_30cm")
# 
# cov_colin <- correct_colinvar(covariates, method = c('pearson', th = "0.7"))
# corrplot(cov_colin$cor_table, tl.cex = 0.6)
# cov_colin$cor_variables

selected_vars <- c("mean_diurnal_t_range", "p_warm_q", "p_cold_q", "mean_t_wet_q", "f_intensity", "nitrogen_0_5cm", "phh2o_5_15cm", "clay_15_30cm")

cov_clean <- covariates[[selected_vars]]


## table with cross-validation
# purpurascens_filt_pres <- read.table(file = "species_data/a_purpurascens_spatial_blocks.csv", header = TRUE, sep = ",")

k = 5
# Spatial Block Cross-Validation ------------------------------------------

purpurascens_df <- as.data.frame(apu_crs84) %>%
  (dplyr::select)(lon, lat)

purpurascens_df$id <- 1:nrow(purpurascens_df)

occ_filt_nbin <- occfilt_env(
  data = purpurascens_df,
  x = 'lon',
  y = 'lat',
  id = 'id',
  env_layer = cov_clean,
  nbins = 10
)

# par(mfrow = c(1,1))
#   plot(cov_clean[[1]])
#   points(purpurascens_df, pch = 19, cex = 0.3)
#   points(occ_filt_nbin[,2:3], pch = 19, cex = 0.3, col = 'red')

purpurascens_filt_pres <- occ_filt_nbin[,2:3]
purpurascens_filt_pres$pr_ab <- 1

spat_range <- cv_spatial_autocor(
  r = cov_clean,
  x = st_as_sf(purpurascens_filt_pres, coords = c('lon', 'lat'), crs = crs(cov_clean)),
  column = 'pr_ab',
  plot = TRUE
)
#recommended block size returned 162522

spat_blocks1 <- cv_spatial(
  x = st_as_sf(purpurascens_filt_pres, coords = c('lon', 'lat'), crs = crs(cov_clean)),
  column = "pr_ab",
  r = cov_clean,
  k = 5,
  hexagon = FALSE,
  size = spat_range$range_table[[2]],
  seed = 101
)

## assigning the folds to species presence data
purpurascens_filt_pres$folds <- spat_blocks1$folds_ids
purpurascens_filt_pres %>%
  group_by(folds) %>%
  count()

grid_env <- rasterize(vect(spat_blocks1$blocks), cov_clean, field = 'folds')
# plot(grid_env)

# write.csv(x = purpurascens_filt_pres, file = "purpurascens_spatial_blocks.csv", sep = ",", quote = FALSE)


# pa <- read.table(file = "species_data/a_purpurascens_pseudo_absence_data.csv", header = TRUE, sep = ",")
# Pseudo-Absence Data -----------------------------------------------------
## using proportional stratification
pa <- lapply(1:k, function(x) {
  sample_pseudoabs(
    data = purpurascens_filt_pres,
    x = 'lon',
    y = 'lat',
    n = sum(purpurascens_filt_pres$folds == x),
    method = c('env_const', env = cov_clean),
    maskval = x,
    rlayer = grid_env,
    calibarea = aoi
  )
}) %>%
  bind_rows()

pa <- sdm_extract(data = pa, x = "lon", y = "lat", env_layer = grid_env)
pa %>%
  group_by(folds) %>%
  count() == purpurascens_filt_pres %>%
  group_by(folds) %>%
  count()

## plotting it out
# ggplot() +
#   geom_sf(data = st_as_sf(aoi), fill = NA) +
#   geom_sf(data = st_as_sf(spat_blocks1$blocks)) +
#   geom_point(data = rbind(purpurascens_filt_pres, pa),
#              aes(
#                x = lon,
#                y = lat,
#                col = as.factor(folds),
#                pch = as.factor(pr_ab))) +
#   labs(colour = 'folds', shape = 'Presence/\nPseudo-absence') +
#   theme_void()

# write.csv(x = pa, file = "a_purpurascens_pseudo_absence_data.csv", quote = FALSE)

### Extracting covariate values for each point
SWDdata <- prepareSWD(
  species = 'Aristida purpurascens',
  p = purpurascens_filt_pres[,1:2],
  a = pa[,1:2],
  env = cov_clean
)


# RandomForest ------------------------------------------------------------
#with random folds
rand_folds <- randomFolds(SWDdata, k = k, seed = 1)
set.seed(1)
rf_randcv <- train(method = 'RF', data = SWDdata, folds = rand_folds)

# evaluation metrics using 'Area under curve' and 'TrueSkill statistics'
paste0('Testing AUC: ', round(SDMtune::auc(rf_randcv, test = TRUE),2))
# returned 0.87
paste0('Testing TSS: ', round(SDMtune::tss(rf_randcv, test = TRUE),2))
# returned 0.67

#with spatial folds
spat_blocks2 <- cv_spatial(
  x = st_as_sf(bind_rows(purpurascens_filt_pres, pa), coords = c('lon', 'lat'), crs = crs(cov_clean)),
  column = 'pr_ab',
  r = cov_clean[[1]],
  k = k,
  hexagon = FALSE,
  size = spat_range$range_table[[2]],
  selection = 'predefined',
  user_blocks = spat_blocks1$blocks,
  folds_column = "folds",
  seed = 101
)

# running model with spatial blocks
set.seed(1)
rf_sbcv <- train(method = 'RF', data = SWDdata, folds = spat_blocks2)


paste0('Testing AUC: ', round(SDMtune::auc(rf_sbcv, test = TRUE),2))
# returned 0.81
paste0('Testing TSS: ', round(SDMtune::tss(rf_sbcv, test = TRUE),2))
# returned 0.52

# Receiver operator characteristics for each curve
source('C:/Users/emars/Desktop/intro to spatial data in R/Intro_to_spatial-main/scripts/functions/extract_roc_vals.R')
spec_sens_val <- extract_spec_sens_vals(rf_sbcv, spat_blocks2, SWDdata)
auc_vals <- extract_auc_vals(rf_sbcv, spat_blocks2, SWDdata)
auc_vals$label <- paste0(auc_vals$model_no, ": ", round(auc_vals$auc,2))


# ggplot(data = spec_sens_val) +
#   geom_abline(aes(slope = 1, intercept = 0), lty = 2) +
#   geom_path(aes(x = 1- specificities, y = sensitivities, group = model_no, col = as.factor(model_no)), alpha = 0.8) +
#   scale_colour_viridis_d(name = 'Model no. + AUC',
#                          labels = auc_vals$label) +
#   labs(x = 'false positive rate', y = 'true positive rate') +
#   geom_text(aes(x = 0.15, y = 0.95), label = paste0('overall testing AUC: ', round(SDMtune::auc(rf_sbcv, test = TRUE),2)), size = 3) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         legend.position = c(0.8, 0.25),
#         legend.title = element_text(size = 8),
#         legend.text = element_text(size = 7))


# Variable Importance -----------------------------------------------------
vi_rf_sbcv <- varImp(rf_sbcv)
plotVarImp(vi_rf_sbcv)

### Response Curves
plotResponse(rf_sbcv, var = "p_warm_q", marginal = TRUE, rug = TRUE) + labs(x = 'precipitation of the warmest quarter') +
  plotResponse(rf_sbcv, var = "mean_diurnal_t_range", marginal = TRUE, rug = TRUE) + labs(x = 'mean diurnal temperature range') +
  plotResponse(rf_sbcv, var = "p_cold_q", marginal = TRUE, rug = TRUE) + labs(x = 'precipitation of the coldest quarter') +
  plotResponse(rf_sbcv, var = "mean_t_wet_q", marginal = TRUE, rug = TRUE) + labs(x = 'mean temperature of the wettest quarter')
  
plotResponse(rf_sbcv, var = "clay_15_30cm", marginal = TRUE, rug = TRUE) + labs(x = 'clay content 15-30cm deep') +
  plotResponse(rf_sbcv, var = "clay_15_30cm", marginal = TRUE, rug = TRUE) + labs(x = 'clay content 15-30cm deep') +
  plotResponse(rf_sbcv, var = "nitrogen_0_5cm", marginal = TRUE, rug = TRUE) + labs(x = 'nitrogen content 0-5cm deep') +
  plotResponse(rf_sbcv, var = "f_intensity", marginal = TRUE, rug = TRUE) + labs(x = 'fire intensity')



### model prediction
# terra::writeRaster(x = pred, file = "a_purpurascens_model_prediction.tiff") 
pred <- predict(rf_sbcv, data = cov_clean)

pred_df <- as.data.frame(pred, xy = TRUE) 


ggplot() +
  geom_sf(data = st_as_sf(states), fill = 'white', col = NA) + 
  geom_sf(data = apu_crs84) +
  geom_tile(data = pred_df, aes(x = x, y = y, fill = mean, col = mean)) +
  scale_colour_viridis_c(na.value = NA, option = 'C', breaks = seq(0,1,0.25),limits = c(0,1)) +
  scale_fill_viridis_c(na.value = NA, option = 'C', breaks = seq(0,1,0.25),limits = c(0,1)) +
  geom_sf(data = st_as_sf(states), fill = NA, col = 'gray80', lwd = 0.25) + 
  geom_sf(data = st_as_sf(purpurascens_filt_pres, coords = c('lon', 'lat'), crs = crs(states)), size = 1, col = 'black', fill = 'white', pch = 21) +
  scale_x_continuous(limits = c(ext(cov_clean)[1], ext(cov_clean)[2]), breaks = seq(26,32,3)) + 
  scale_y_continuous(limits = c(ext(cov_clean)[3],ext(cov_clean)[4]), breaks = seq(-32,-22,5)) +
  
  annotation_scale(location = 'br', width_hint = 0.1, style = 'ticks', tick_height = 0) +
  annotation_north_arrow(location = 'br', pad_y = unit(0.5, 'cm'), style = north_arrow_minimal()) +
  labs(fill = 'Habitat\nsuitability', 
       col = 'Habitat\nsuitability',
       x = 'Longitude', y = 'Latitude') +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "italic"),
    axis.ticks = element_line(linewidth = 0.5)
  )