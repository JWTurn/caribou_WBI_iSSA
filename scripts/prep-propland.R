require(terra)
require(sf)
require(data.table)

### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')

crs <- st_crs(3978)$wkt

year = 2010
studyArea <- file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp')
can2010 <- file.path(canada, 'canada_2010', 'CAN_LC_2010_CAL.tif')

land.full <- rast(can2010)
if(st_crs(crs(land.full)) != st_crs(crs)){
  land.proj <- project(land.full, crs, method = 'near')
  land.full <- land.proj
}
sArea <- vect(studyArea)
land <- crop(land.full, sArea)

needleleaf <- land %in% c(1,2)
names(needleleaf) <- "needleleaf"
deciduous <- land == 5
names(deciduous) <- "deciduous"
mixed <- land == 6
names(mixed) <- "mixed"
shrub <- land == 8
names(shrub) <- "shrub"
grassland <- land == 10
names(grassland) <- "grassland"
lichenshrub <- land == 11
names(lichenshrub) <- "lichenshrub"
lichengrass <- land == 12
names(lichengrass) <- "lichengrass"
wet <- land == 14
names(wet) <- "wetland"
cropland <- land == 15
names(cropland) <- "cropland"
barrenland <- land == 16
names(barrenland) <- "barrenland"
urban <- land == 17
names(urban) <- "urban"
water <- land == 18
names(water) <- "water"
snow <- land == 19
names(snow) <- "snow"



needleleaf01 <- classify(needleleaf,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
needleleaf500 <- resample(needleleaf01, lf, method ='average')
writeRaster(needleleaf500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                     paste0('needleleaf_500', '.tif')))

deciduous01 <- classify(deciduous,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
deciduous500 <- resample(deciduous, lf, method ='average')
writeRaster(deciduous500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                    paste0('deciduous_500', '.tif')))

mixed01 <- classify(mixed,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
mixed500 <- resample(mixed01, lf, method ='average')
writeRaster(mixed500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                paste0('mixed_500', '.tif')))

shrub01 <- classify(shrub,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
shrub500 <- resample(shrub01, lf, method ='average')
writeRaster(shrub500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                paste0('shrub_500', '.tif')))

grassland01 <- classify(grassland,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
grassland500 <- resample(grassland01, lf, method ='average')
writeRaster(grassland500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                    paste0('grassland_500', '.tif')))

lichenshrub01 <- classify(lichenshrub,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
lichenshrub500 <- resample(lichenshrub01, lf, method ='average')
writeRaster(lichenshrub500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                      paste0('lichenshrub_500', '.tif')))

lichengrass01 <- classify(lichengrass,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
lichengrass500 <- resample(lichengrass01, lf, method ='average')
writeRaster(lichengrass500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                      paste0('lichengrass_500', '.tif')))

wet01 <- classify(wet,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
wet500 <- resample(wet01, lf, method ='average')
writeRaster(wet500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                              paste0('wet_500', '.tif')))

cropland01 <- classify(cropland,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
cropland500 <- resample(cropland01, lf, method ='average')
writeRaster(cropland500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                   paste0('cropland_500', '.tif')))

barrenland01 <- classify(barrenland,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
barrenland500 <- resample(barrenland01, lf, method ='average')
writeRaster(barrenland500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                     paste0('barrenland_500', '.tif')))

urban01 <- classify(urban,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
urban500 <- resample(urban01, lf, method ='average')
writeRaster(urban500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                paste0('urban_500', '.tif')))

water01 <- classify(water,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
water500 <- resample(water01, lf, method ='average')
writeRaster(water500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                paste0('water_500', '.tif')))

snow01 <- classify(snow,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
snow500 <- resample(snow01, lf, method ='average')
writeRaster(snow500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                               paste0('snow_500', '.tif')))
