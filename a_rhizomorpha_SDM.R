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


# Species Data ------------------------------------------------------------
ar_clean <- read.table(file = "Aristida_rhizomophora/a_rhizomophora_occ_clean.csv", header = TRUE, sep = ",")


# Set CRS -----------------------------------------------------------------
ar_no_crs <- st_as_sf(ar_clean, coords = c('lon', 'lat'), remove = FALSE)
ar_crs84 <- ar_no_crs
st_crs(ar_crs84) = 4326

# Setting AOI -------------------------------------------------------------
se_aoi <- read_sf("SE_study_region_data")
st_crs(se_aoi) = 4326
se_vect <- terra::vect(se_aoi)

us_states <- ne_states(country = 'united states of america', returnclass = 'sf')
states_sel <- c('North Carolina', 'South Carolina', 'Georgia', 'Florida', 'Alabama', 'Mississippi')
us_states %>% dplyr::filter(name_en %in% states_sel) -> sern
states <- terra::vect(sern)


# Calibration Area --------------------------------------------------------
calib_as <- calib_area (data = as.data.frame(ar_crs84),
                        x = 'lon', y = 'lat',
                        method = c('buffer', width = 150000),
                        crs = crs(ar_crs84))

aoi <- terra::intersect(calib_as, states)

# plot(calib_as)
# plot(aoi, col = 'red', add = TRUE)
# plot(states, add = TRUE)
# plot(as_crs84, add = TRUE)



# WorldClim Data ----------------------------------------------------------
files <- list.files("worldclim_data", pattern = "\\.tif$", full.names = TRUE)
files <- files[order(as.numeric(sub(".*bio_([0-9]+)\\.tif$", "\\1", files)))]
wc_rast <- rast(files)

fire_rast_na <- rast("modis_totalmean_30s.tif")
land_mask <- rasterize(se_vect, fire_rast_na[[1]], field = 1, background = NA)
mask_base <- rep(land_mask, nlyr(fire_rast_na))
fire_rast <- ifel(is.na(fire_rast_na) & !is.na(mask_base), 0, fire_rast_na)
f_crop <- mask(crop(fire_rast, aoi), aoi)


wc_crop <- resample(x = wc_rast, y = f_crop)

covariates <- c(wc_crop, f_crop)


# renaming the worldclim bands
names(covariates) <- c("mean_ann_t","mean_diurnal_t_range", "isothermality",
                       "t_seas", 'max_t_warm_m','min_t_cold_m', "t_ann_range",
                       'mean_t_wet_q','mean_t_dry_q','mean_t_warm_q',
                       'mean_t_cold_q','ann_p', 'p_wet_m','p_dry_m','p_seas',
                       'p_wet_q','p_dry_q','p_warm_q','p_cold_q', "f_intensity",
                       "f_freq", "f_seas")

# re-scaling the temperature data
covariates[[c(1:2,5:11)]] <- covariates[[c(1:2,5:11)]]/10
covariates[[3:4]] <- covariates[[3:4]]/100

cov_colin <- correct_colinvar(covariates, method = c('pearson', th = "0.7"))
# corrplot(cov_colin$cor_table, tl.cex = 0.6)
# cov_colin$cor_variables

selected_vars <- c('mean_ann_t', 'max_t_warm_m', 'mean_t_wet_q', 'ann_p',
                   'p_cold_q', "f_freq", "f_seas")

cov_clean <- covariates[[selected_vars]]

rhizomophora_df <- as.data.frame(ar_crs84) %>% (dplyr::select)(lon, lat)
rhizomophora_df$id <- 1:nrow(rhizomophora_df)

occ_filt_nbin <- occfilt_env(
  data = rhizomophora_df,
  x = 'lon',
  y = 'lat',
  id = 'id',
  env_layer = cov_clean,
  nbins = 50
)

# par(mfrow = c(1,1))
#   plot(cov_clean[[1]])
#   points(rhizomophora_df, pch = 19, cex = 0.3)
#   points(occ_filt_nbin[,2:3], pch = 19, cex = 0.3, col = 'red')

rhizomophora_filt_pres <- rhizomophora_df[1:2]
rhizomophora_filt_pres$pr_ab <- 1


# Spatial Block Cross-Validation ------------------------------------------
k = 5

spat_range <- cv_spatial_autocor(
  r = cov_clean,
  x = st_as_sf(rhizomophora_filt_pres, coords = c('lon', 'lat'), crs = crs(cov_clean)),
  column = 'pr_ab',
  plot = TRUE
)
#recommended block size, returned 158086

spat_blocks1 <- cv_spatial(
  x = st_as_sf(rhizomophora_filt_pres, coords = c('lon', 'lat'), crs = crs(cov_clean)),
  column = "pr_ab",
  r = cov_clean,
  k = k,
  hexagon = FALSE,
  size = 158086,
  seed = 101
)

# assigning the folds to species presence data
rhizomophora_filt_pres$folds <- spat_blocks1$folds_ids
rhizomophora_filt_pres %>% group_by(folds) %>% count()

grid_env <- rasterize(vect(spat_blocks1$blocks), cov_clean, field = 'folds')
plot(grid_env)


# Pseudo-Absence Data -----------------------------------------------------
# using proportional stratification
pa <- lapply(1:k, function(x) {
  sample_pseudoabs(
    data = rhizomophora_filt_pres,
    x = 'lon',
    y = 'lat',
    n = sum(rhizomophora_filt_pres$folds == x),
    method = c('env_const', env = cov_clean),
    maskval = x,
    rlayer = grid_env,
    calibarea = aoi
  )
}) %>% bind_rows()

pa <- sdm_extract(data = pa, x = "lon", y = "lat", env_layer = grid_env)
pa %>% group_by(folds) %>% count() == rhizomophora_filt_pres %>% group_by(folds) %>% count()

## plotting it out
# ggplot() +
#   geom_sf(data = st_as_sf(aoi), fill = NA) +
#   geom_sf(data = st_as_sf(spat_blocks1$blocks)) +
#   geom_point(data = rbind(rhizomophora_filt_pres, pa),
#              aes(
#                x = lon,
#                y = lat,
#                col = as.factor(folds),
#                pch = as.factor(pr_ab))) +
#   labs(colour = 'folds', shape = 'Presence/\nPseudo-absence') +
#   theme_void()

### Extracting covariate values for each point
SWDdata <- prepareSWD(
  species = 'Aristida rhizomophora',
  p = rhizomophora_filt_pres[,1:2],
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
# returned 0.91
paste0('Testing TSS: ', round(SDMtune::tss(rf_randcv, test = TRUE),2))
# returned 0.72

#with spatial folds
spat_blocks2 <- cv_spatial(
  x = st_as_sf(bind_rows(rhizomophora_filt_pres, pa), coords = c('lon', 'lat'), crs = crs(cov_clean)),
  column = 'pr_ab',
  r = cov_clean[[1]],
  k = k,
  hexagon = FALSE,
  size = 158086,
  selection = 'predefined',
  user_blocks = spat_blocks1$blocks,
  folds_column = "folds",
  seed = 101
)

# running model with spatial blocks
set.seed(1)
rf_sbcv <- train(method = 'RF', data = SWDdata, folds = spat_blocks2)

paste0('Testing AUC: ', round(SDMtune::auc(rf_sbcv, test = TRUE),2))
# returned 0.85
paste0('Testing TSS: ', round(SDMtune::tss(rf_sbcv, test = TRUE),2))
# returned 0.62

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
plotResponse(rf_sbcv, var = "ann_p", marginal = TRUE, rug = TRUE) + labs(x = 'annual precipitation') +
  plotResponse(rf_sbcv, var = "mean_t_wet_q", marginal = TRUE, rug = TRUE) + labs(x = 'mean temperature of the wettest quarter')
#ggsave('C:/Users/emars/Desktop/Intro to spatial data in R/response_curves.pdf')

### model prediction
pred <- predict(rf_sbcv, data = cov_clean)

pred_df <- as.data.frame(pred, xy = TRUE) 

ggplot() +
  geom_sf(data = st_as_sf(states), fill = 'white', col = NA) + 
  geom_sf(data = as_crs84) +
  geom_tile(data = pred_df, aes(x = x, y = y, fill = mean, col = mean)) +
  scale_colour_viridis_c(na.value = NA, option = 'C', breaks = seq(0,1,0.25),limits = c(0,1)) +
  scale_fill_viridis_c(na.value = NA, option = 'C', breaks = seq(0,1,0.25),limits = c(0,1)) +
  geom_sf(data = st_as_sf(states), fill = NA, col = 'black', lwd = 0.25) + 
  geom_sf(data = st_as_sf(rhizomophora_filt_pres, coords = c('lon', 'lat'), crs = crs(states)), size = 1, col = 'black', fill = 'white', pch = 21) +
  scale_x_continuous(limits = c(ext(cov_clean)[1], ext(cov_clean)[2]), breaks = seq(26,32,3)) + 
  scale_y_continuous(limits = c(ext(cov_clean)[3],ext(cov_clean)[4]), breaks = seq(-32,-22,5)) +
  labs(fill = 'Habitat\nsuitability', 
       col = 'Habitat\nsuitability',
       x = 'Longitude', y = 'Latitude') +
  theme_minimal()
# ggsave('C:/Users/emars/Desktop/SDM_aristida/model_prediction_rhizomophora.pdf')
