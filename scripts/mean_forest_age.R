# === Mean forest age -------------------------------------
# Julie Turner
# started 14 December 2023

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf',
          'ggplot2', 'viridis', 'tidyterra', 'patchwork')
lapply(libs, Require::Require, character.only = TRUE)

### functions ----
geomean <- function(x){
  exp(mean(log(x)))
}

### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')


### layers -----
fires <- rast(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires_2019.tif'))
harv <- rast(file.path('data', 'raw-data', 'WB_harv_1985-2020.tif'))

herds <- vect(file.path('data', 'raw-data', 'Johnsonetal2020_studyareas', 'Enhanced_MetaHerds_20191029.shp'))
studyArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))

fage <- rast(file.path(canada, 'CA_forest_age_2019', 'CA_forest_age_2019.tif'))
fage <- project(fage, fires)
fage.crop <- crop(fage, fires)

writeRaster(fage.crop, file.path('data', 'raw-data', 'f_age_2019.tif'))

fage.unk.fires <- mask(fage.crop, fires)
writeRaster(fage.unk.fires , file.path('data', 'raw-data', 'f_age-fires_2019.tif'))

