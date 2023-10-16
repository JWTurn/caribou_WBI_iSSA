# bigger grid prop land for yearly land forest dataset
require(terra)
require(sf)
require(data.table)

### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'
#canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
canada <- file.path("C:", "users", "julie", 'Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
#C:\Users\julie\Dropbox\ActiveDocs
crs <- st_crs(3978)$wkt

year = 2019
studyArea <- file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp')
can.yr <- c(file.path(canada, 'Landcover_1984-2019', paste0('CA_forest_VLCE2_', year), 
                      paste0('CA_forest_VLCE2_', year, '.tif')))
land.full <- rast(can.yr)
if(st_crs(crs(land.full)) != st_crs(crs)){
  land.proj <- project(land.full, crs, method = 'near')
  land.full <- land.proj
}
sArea <- vect(studyArea)
land <- crop(land.full, sArea)

water <- land == 20
names(water) <- "water"
snow <- land == 31
names(snow) <- "snow"
rock <- land == 32
names(rock) <- "rock"
barrenland <- land == 33
names(barrenland) <- "barrenland" # exposed_barren_land in CA_forest, but keeping for consistency with landsat
bryoids <- land == 40
names(bryoids) <- "bryoids"
shrub <- land == 50
names(shrub) <- "shrub"
wet <- land == 80
names(wet) <- "wetland"
wet_treed <- land == 81
names(wet_treed) <- "wet_treed"
herbs <- land == 100
names(herbs) <- "herbs"
needleleaf <- land == 210 
names(needleleaf) <- "needleleaf" # coniferous in CA_forest, but keeping for consistency with landsat
deciduous <- land == 220
names(deciduous) <- "deciduous" # broadleaf in CA_forest, but keeping for consistency with landsat
mixed <- land == 230
names(mixed) <- "mixed"

lf.full <- rast(file.path('data', 'derived-data', 'distto_roadrail_500.tif'))
lf <- crop(lf.full, sArea)

water01 <- classify(water,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
water500 <- resample(water01, lf, method ='average')
writeRaster(water500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                paste0('water_500', '.tif')))

snow01 <- classify(snow,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
snow500 <- resample(snow01, lf, method ='average')
writeRaster(snow500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                               paste0('snow_500', '.tif')))

rock01 <- classify(rock,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
rock500 <- resample(rock01, lf, method ='average')
writeRaster(rock500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                     paste0('rock_500', '.tif')))

barrenland01 <- classify(barrenland,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
barrenland500 <- resample(barrenland01, lf, method ='average')
writeRaster(barrenland500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                     paste0('barrenland_500', '.tif')))

bryoids01 <- classify(bryoids,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
bryoids500 <- resample(bryoids01, lf, method ='average')
writeRaster(bryoids500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                     paste0('bryoids_500', '.tif')))

shrub01 <- classify(shrub,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
shrub500 <- resample(shrub01, lf, method ='average')
writeRaster(shrub500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                paste0('shrub_500', '.tif')))

wet01 <- classify(wet,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
wet500 <- resample(wet01, lf, method ='average')
writeRaster(wet500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                              paste0('wet_500', '.tif')))

wettreed01 <- classify(wet_treed,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
wettreed500 <- resample(wettreed01, lf, method ='average')
writeRaster(wettreed500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                              paste0('wet_treed_500', '.tif')))

herbs01 <- classify(herbs,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
herbs500 <- resample(herbs01, lf, method ='average')
writeRaster(herbs500, file.path('data', 'raw-data','prop_land', year, '500grid', 
                                     paste0('herbs_500', '.tif')))

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



