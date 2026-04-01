setwd("~/NCSU/Students/Ellison_Marshall/")
library(terra)
library(sf)
library(rnaturalearth)

# Setting AOI -------------------------------------------------------------
se_aoi <- read_sf("SE_Study_Region")
st_crs(se_aoi) = 4326
se_vect <- terra::vect(se_aoi)
aoi <- se_vect

a_stricta_raster <- rast("A_stricta_prediction.tif")
a_stricta_suitability <- a_stricta_raster > 0.5
a_beyrichiana_raster <- rast("A_beyrichiana_prediction.tif")
a_beyrichiana_suitability <- a_beyrichiana_raster > 0.5
# 0 = Neither, 1 = Species 1 Only, 2 = Species 2 Only, 3 = Both
overlap <- (a_stricta_suitability * 1) + (a_beyrichiana_suitability * 2)

levels(overlap) <- data.frame(
  id = 0:3,
  legend = c("Unsuitable", "A. stricta", "A. beyrichiana", "Overlap")
)


plot(overlap, col=c("grey90", "blue", "red", "purple"), 
     main="Suitability Overlap")