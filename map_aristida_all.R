#install.packages('rgbif')
#install.packages('tidyverse')
#install.packages('CoordinateCleaner')
#install.packages('rnaturalearth')
#install.packages('sf')
#install.packages('mapview')
#install.packages('ggspatial')
#install.packages('patchwork')
#install.packages('rnaturalearthdata')

library(tidyverse)
library(rgbif)
library(CoordinateCleaner)
library(rnaturalearth)
library(sf)
library(mapview)
library(ggspatial)
library(patchwork)
library(rnaturalearthdata)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)


# A. stricta --------------------------------------------------------------
myspecies1 <- "Aristida stricta"
gbif_download <- occ_data(scientificName = myspecies1, hasCoordinate = TRUE)
gbif_data <- gbif_download$data
# names(gbif_data)[1:10]
    
### filtering the gbif data
gbif_data %>%
  filter(year >= 1900) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  filter(decimalLatitude >= 25) %>%
  cc_cen(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_cap(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_inst(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_sea(lat = 'decimalLatitude', lon = 'decimalLongitude') %>%
  distinct(decimalLongitude,
           decimalLatitude,
           speciesKey,
           datasetKey,
           .keep_all = TRUE) -> a_stricta 
    
### variable selection and renaming
a_stricta %>%
  select(key, year, basisOfRecord, decimalLongitude, decimalLatitude) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) -> as_clean
# write.csv(as_clean, file = "a_stricta_occ_clean_v2.csv")

### setting cleaned gbif data to the correct crs
as_no_crs <- st_as_sf(as_clean, coords = c('lon', 'lat'))
as_WGS_84 <- as_no_crs
st_crs(as_WGS_84) = 4326


# as_clean <- read.table(file = "Aristida_stricta/a_stricta_occ_clean.csv", header = TRUE, sep = ",")


# A. beyrichiana ----------------------------------------------------------
myspecies2 <- "Aristida beyrichiana"
gbif_download <- occ_data(scientificName = myspecies2, hasCoordinate = TRUE)
gbif_data <- gbif_download$data
# names(gbif_data)[1:10]

### filtering the gbif data
gbif_data %>%
  filter(year >= 1900) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  cc_cen(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_cap(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_inst(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_sea(lat = 'decimalLatitude', lon = 'decimalLongitude') %>%
  distinct(decimalLongitude,
           decimalLatitude,
           speciesKey,
           datasetKey,
           .keep_all = TRUE) -> a_beyrichiana
    
### variable selection and renaming
a_beyrichiana %>%
  select(key, year, basisOfRecord, decimalLongitude, decimalLatitude) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) -> ab_clean

# write.csv(ab_clean, file = "a_beyrichiana_occ_clean.csv")
# ab_clean <- read.table(file = "Aristida_beyrichiana/a_beyrichiana_occ_clean.csv", header = TRUE, sep = ",")


### setting cleaned gbif data to the correct crs
ab_no_crs <- st_as_sf(ab_clean, coords = c('lon', 'lat'))
ab_WGS_84 <- ab_no_crs
st_crs(ab_WGS_84) = 4326


# A. rhizomophora ----------------------------------------------------------
myspecies3 <- "Aristida rhizomophora"
gbif_download <- occ_data(scientificName = myspecies3, hasCoordinate = TRUE)
gbif_data <- gbif_download$data
# names(gbif_data)[1:10]
    
### filtering the gbif data
gbif_data %>%
  filter(year >= 1900) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  cc_cen(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_cap(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_inst(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_sea(lat = 'decimalLatitude', lon = 'decimalLongitude') %>%
  distinct(decimalLongitude,
           decimalLatitude,
           speciesKey,
           datasetKey,
           .keep_all = TRUE) -> a_rhizomophora 
    
### variable selection and renaming
a_rhizomophora %>%
  select(key, year, basisOfRecord, decimalLongitude, decimalLatitude) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) -> ar_clean

# write.csv(ar_clean, file = "a_rhizomophora_occ_clean.csv")

### setting cleaned gbif data to the correct crs
ar_no_crs <- st_as_sf(ar_clean, coords = c('lon', 'lat'))
ar_WGS_84 <- ar_no_crs
st_crs(ar_WGS_84) = 4326

# ar_clean <- read.table(file = "Aristida_rhizomophora/a_rhizomophora_occ_clean.csv", header = TRUE, sep = ",")


# A. palustris ------------------------------------------------------------
myspecies4 <- "Aristida palustris"
gbif_download <- occ_data(scientificName = myspecies4, hasCoordinate = TRUE)
gbif_data <- gbif_download$data
# names(gbif_data)[1:10]

### filtering the gbif data
gbif_data %>%
  filter(year >= 1900) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  cc_cen(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_cap(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_inst(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_sea(lat = 'decimalLatitude', lon = 'decimalLongitude') %>%
  distinct(decimalLongitude,
           decimalLatitude,
           speciesKey,
           datasetKey,
           .keep_all = TRUE) -> a_palustris

### variable selection and renaming
a_palustris %>%
  select(key, year, basisOfRecord, decimalLongitude, decimalLatitude) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) -> apa_clean

# write.csv(apa_clean, file = "a_palustris_occ_clean.csv")


### setting cleaned gbif data to the correct crs
apa_no_crs <- st_as_sf(apa_clean, coords = c('lon', 'lat'))
apa_WGS_84 <- apa_no_crs
st_crs(apa_WGS_84) = 4326


# A. purpurascens ---------------------------------------------------------
myspecies5 <- "Aristida purpurascens"
gbif_download <- occ_data(scientificName = myspecies5, hasCoordinate = TRUE)
gbif_data <- gbif_download$data
# names(gbif_data)[1:10]
    
### filtering the gbif data
gbif_data %>%
  filter(year >= 1900) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  cc_cen(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_cap(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_inst(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_sea(lat = 'decimalLatitude', lon = 'decimalLongitude') %>%
  distinct(decimalLongitude,
           decimalLatitude,
           speciesKey,
           datasetKey,
           .keep_all = TRUE) -> a_purpurascens

### variable selection and renaming
a_purpurascens %>%
  dplyr::select(key, year, basisOfRecord, decimalLongitude, decimalLatitude) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) -> apu_clean

# write.csv(apu_clean, file = "a_purpurascens_occ_clean.csv")


### setting cleaned gbif data to the correct crs
apu_no_crs <- st_as_sf(apu_clean, coords = c('lon', 'lat'))
apu_WGS_84 <- apu_no_crs
st_crs(apu_WGS_84) = 4326


# A. gyrans ---------------------------------------------------------------
myspecies6 <- "Aristida gyrans"
gbif_download <- occ_data(scientificName = myspecies6, hasCoordinate = TRUE)
gbif_data <- gbif_download$data
# names(gbif_data)[1:10]
    
### filtering the gbif data
gbif_data %>%
  filter(year >= 1900) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  cc_cen(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_cap(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_inst(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_sea(lat = 'decimalLatitude', lon = 'decimalLongitude') %>%
  distinct(decimalLongitude,
           decimalLatitude,
           speciesKey,
           datasetKey,
           .keep_all = TRUE) -> a_gyrans

### variable selection and renaming
a_gyrans %>%
  select(key, year, basisOfRecord, decimalLongitude, decimalLatitude) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) -> ag_clean

# write.csv(ag_clean, file = "a_gyrans_occ_clean.csv")


### setting cleaned gbif data to the correct crs
ag_no_crs <- st_as_sf(ag_clean, coords = c('lon', 'lat'))
ag_WGS_84 <- ag_no_crs
st_crs(ag_WGS_84) = 4326   


# A. mohrii ---------------------------------------------------------------
myspecies7 <- "Aristida mohrii"
gbif_download <- occ_data(scientificName = myspecies7, hasCoordinate = TRUE)
gbif_data <- gbif_download$data
names(gbif_data)[1:10]
    
### filtering the gbif data
gbif_data %>%
  filter(year >= 1900) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  cc_cen(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_cap(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_inst(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_sea(lat = 'decimalLatitude', lon = 'decimalLongitude') %>%
  distinct(decimalLongitude,
           decimalLatitude,
           speciesKey,
           datasetKey,
           .keep_all = TRUE) -> a_mohrii

### variable selection and renaming
a_mohrii %>%
  select(key, year, basisOfRecord, decimalLongitude, decimalLatitude) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) -> am_clean

# write.csv(am_clean, file = "a_mohrii_occ_clean.csv")


### setting cleaned gbif data to the correct crs
am_no_crs <- st_as_sf(am_clean, coords = c('lon', 'lat'))
am_WGS_84 <- am_no_crs
st_crs(am_WGS_84) = 4326
    

# A. condensata -----------------------------------------------------------
myspecies8 <- "Aristida condensata"
gbif_download <- occ_data(scientificName = myspecies8, hasCoordinate = TRUE)
gbif_data <- gbif_download$data
# names(gbif_data)[1:10]

### filtering the gbif data
gbif_data %>%
  filter(year >= 1900) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999)) %>%
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>%
  cc_cen(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_cap(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_inst(lat = 'decimalLatitude', lon = 'decimalLongitude', buffer = 2000) %>%
  cc_sea(lat = 'decimalLatitude', lon = 'decimalLongitude') %>%
  distinct(decimalLongitude,
           decimalLatitude,
           speciesKey,
           datasetKey,
           .keep_all = TRUE) -> a_condensata

### variable selection and renaming
a_condensata %>%
  select(key, year, basisOfRecord, decimalLongitude, decimalLatitude) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) -> ac_clean

# write.csv(ac_clean, file = "a_condensata_occ_clean.csv")


### setting cleaned gbif data to the correct crs
ac_no_crs <- st_as_sf(ac_clean, coords = c('lon', 'lat'))
ac_WGS_84 <- ac_no_crs
st_crs(ac_WGS_84) = 4326     
    


# Setting the Map -------------------------------------------------------------
us_states <- ne_states(country = 'united states of america', returnclass = 'sf')
# names(us_states)
canada_mexico <- ne_countries(country = c('Canada', 'Mexico'))

### my group of states
states_sel <- c('North Carolina', 'South Carolina', 'Georgia', 'Florida', 'Alabama',
                'Mississippi', 'Virginia', 'West Virginia', 'Tennessee','Missouri',
                'Louisiana', 'Arkansas', 'Kentucky', 'Indiana', 'Ohio', 'Pennsylvania',
                'Maryland', 'Delaware', 'New Jersey', 'Illinois', 'Missouri', 'Texas',
                'Oklahoma', 'Kansas', 'Iowa', 'Nebraska')
    us_states %>% dplyr::filter(name_en %in% states_sel) -> sern_a

states_other <- c('North Dakota', 'South Dakota', 'Minnesota', 'Nebraska', 'Iowa',
                  'Maine', 'New York', 'Vermont', 'New Hampshire')
    us_states %>% dplyr::filter(name_en %in% states_other) -> sern_b
se_aoi <- read_sf("SE_study_region_data")



as_clean$species <- "stricta"
ab_clean$species <- "beyrichiana"
ar_clean$species <- "rhizomophora"
aristida_target <- rbind(as_clean, ab_clean, ar_clean)


ggplot() +
  scale_x_continuous(limits = c(-98, -75)) +
  scale_y_continuous(limits = c(25, 41)) +
  xlab('Longitude') + ylab('Latitude') + ggtitle('Aristida spp') +
  
  geom_sf(data = sern_a, fill = 'gray90', colour = 'gray40', linewidth = 0.6) +
  geom_sf(data = se_aoi, fill = 'gray80', alpha = 0.4, colour = 'black', linewidth = 0.6) +
  geom_sf(data = canada_mexico, fill = 'gray90', colour = 'gray40', linewidth = 0.6) +

  geom_point(data = aristida_target, aes(lon, lat, colour = species), alpha = 0.5) +

  annotation_scale(location = 'br', width_hint = 0.1, style = 'ticks', tick_height = 0) +
  annotation_north_arrow(location = 'br', pad_y = unit(0.5, 'cm'), style = north_arrow_minimal()) +
  
  theme_bw() +
  theme(
    plot.title = element_text(face = "italic"),
    legend.text = element_text(face = "italic"),
    panel.background = element_rect(fill = '#E8F6F8'),
    axis.ticks = element_line(linewidth = 0.5),
    grid.minor = element_line(linewidth = 0.3)
  )

  
##original
# ggplot() +
#   scale_x_continuous(limits = c(-100, -70)) +
#   scale_y_continuous(limits = c(24, 45)) +
#   xlab('Longitude') + ylab('Latitude') + ggtitle('Aristida spp') +
#   
#   geom_sf(data = sern_a, fill = 'gray90') +
#   geom_sf(data = sern_b, fill = 'gray80') +
#   geom_sf(data = canada, fill = 'gray80') +
#   geom_sf(data = mexico, fill = 'gray80') +
#   
#   geom_sf(data = as_WGS_84, color = '#156064') +
#   geom_sf(data = ab_WGS_84, color = '#00C49A') +
#   geom_sf(data = ar_WGS_84, color = '#87a96b') +
#   geom_sf(data = apa_WGS_84, color = '#F8E16C') +
#   #geom_sf(data = apu_WGS_84, color = '#dcd7') +
#   #geom_sf(data = ag_WGS_84, color = '#f8c422') +
#   #geom_sf(data = am_WGS_84, color = '#f1ae45') +
#   geom_sf(data = ac_WGS_84, color = '#FFC2B4') +
#   annotation_scale(location = 'br', width_hint = 0.1, style = 'ticks', tick_height = 0) +
#   annotation_north_arrow(location = 'br', pad_y = unit(0.5, 'cm'), style = north_arrow_minimal()) +
#   
#   theme_bw() +
#   theme(
#     plot.title = element_text(face = "italic"),
#     panel.background = element_rect(fill = '#E8F6F8'),
#     axis.ticks = element_line(linewidth = 0.5),
#   )


