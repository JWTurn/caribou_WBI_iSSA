require(terra)
require(sf)
require(data.table)
require(foreach)


lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')

crs <- st_crs(3978)$wkt


dat<- readRDS(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS'))

studyArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))

## LAND ----
make_landforest_prop(studyArea = file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'), crs, buff = 850, startyr = 2019, endyr = 2019)

## ROADS ----

roads <- st_read(file.path('data', 'raw-data', 'wbi_nrn.shp'))
rail <- st_read(file.path('data', 'raw-data', 'wbi_rail.shp'))

paved <- vect(dplyr::filter(roads, PAVSTATUS %in% 'Paved'))
paved.rail <- rbind(paved, vect(rail))
#writeVector(vect(paved), file.path('data', 'raw-data', 'wbi_pave_roads.shp'), overwrite = T)

unpaved <- vect(dplyr::filter(roads, PAVSTATUS %in% 'Unpaved'))

### ECCC disturbance ----
eccc_lines_2010 <- vect(file.path(canada, 'caribou-disturbance', '2010', 
                                  'Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010', 
                                  'EC_borealdisturbance_linear_2008_2010_FINAL_ALBERS.shp'))
eccc_lines_2010 <- project(eccc_lines_2010, studyArea)

#eccc_lines_2010.crop <- crop(eccc_lines_2010, ext(studyArea))

eccc_polys_2010 <- vect(file.path(canada, 'caribou-disturbance', '2010', 
                                  'Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010', 
                                  'EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.shp'))
eccc_polys_2010 <- project(eccc_polys_2010, studyArea)
#eccc_polys_2010.crop <- crop(eccc_polys_2010, ext(studyArea))


eccc_lines_2015 <- vect(file.path(raw, 'ECCC_disturbance', 'WB_dist_2015_line.shp'))
eccc_lines_2015 <- project(eccc_lines_2015, studyArea)
#eccc_lines_2015.crop <- crop(eccc_lines_2015, ext(studyArea))

eccc_polys_2015 <- vect(file.path(raw, 'ECCC_disturbance', 'WB_dist_2015_poly.shp'))
eccc_polys_2015 <- project(eccc_polys_2015, studyArea)


eccc_lines_2020 <- vect(file.path(raw, 'ECCC_disturbance', 'WB_dist_2020_line.shp'))
#eccc_lines_2020 <- project(eccc_lines_2020, studyArea)
#eccc_lines_2020.crop <- crop(eccc_lines_2020, ext(studyArea))

eccc_polys_2020 <- vect(file.path(raw, 'ECCC_disturbance', 'WB_dist_2020_poly.shp'))
#eccc_polys_2020 <- project(eccc_polys_2020, studyArea)

summary(as.factor(eccc_lines_2010$Class))
notroads_2010 <- subset(eccc_lines_2010, !(eccc_lines_2010$Class %in% c('Road', 'Railway')))


summary(as.factor(eccc_lines_2015$Class))
notroads_2015 <- subset(eccc_lines_2015, !(eccc_lines_2015$Class %in% c('Road', 'Railway')))


summary(as.factor(eccc_lines_2020$Class))
notroads_2020 <- subset(eccc_lines_2020, !(eccc_lines_2020$Class %in% c('Road', 'Railway')))

notroads_2010.crop <- crop(notroads_2010, ext(notroads_2015))

notroads.mask.2015 <- mask(notroads_2010.crop, notroads_2015, inverse = T)
notroads_2015_merge <- union(notroads_2015, notroads.mask.2015)

notroads_2020.crop <- crop(notroads_2020, ext(notroads_2015))
notroads.mask.2020 <- mask(notroads_2010.crop, notroads_2020.crop, inverse = T)
notroads_2020_merge <- union(notroads_2020.crop, notroads.mask.2020)


summary(as.factor(eccc_polys_2010$Class))
disturb_2010 <- subset(eccc_polys_2010, !(eccc_polys_2010$Class %in% c('Cutblock')))

summary(as.factor(eccc_polys_2015$Class))
disturb_2015 <- subset(eccc_polys_2015, !(eccc_polys_2015$Class %in% c('Cutblock')))

summary(as.factor(eccc_polys_2020$Class))
disturb_2020 <- subset(eccc_polys_2020, !(eccc_polys_2020$Class %in% c('Cutblock', 'Harvest')))

disturb_2010.crop <- crop(disturb_2010, ext(disturb_2015))

dist.mask.2015 <- mask(disturb_2010.crop, disturb_2015, inverse = T)
disturb_2015_merge <- union(disturb_2015, dist.mask.2015)

disturb_2020.crop <- crop(disturb_2020, ext(disturb_2015))
dist.mask.2020 <- mask(disturb_2010.crop, disturb_2020.crop, inverse = T)
disturb_2020_merge <- union(disturb_2020.crop, dist.mask.2020)

writeVector(notroads_2010.crop, file.path(raw, 'ECCC_disturbance', 'WB_lfother_2010.shp'))
writeVector(notroads_2015_merge, file.path(raw, 'ECCC_disturbance', 'WB_lfother_2015.shp'), overwrite = T)
writeVector(notroads_2020_merge, file.path(raw, 'ECCC_disturbance', 'NEW', 'WB_lfother_2020.shp'), overwrite = T)

writeVector(disturb_2010.crop, file.path(raw, 'ECCC_disturbance', 'WB_disturb_other_2010.shp'))
writeVector(disturb_2015_merge, file.path(raw, 'ECCC_disturbance', 'WB_disturb_other_2015.shp'))
writeVector(disturb_2020_merge, file.path(raw, 'ECCC_disturbance', 'NEW', 'WB_disturb_other_2020.shp'))

# just picking a land layer to be a template for raterization
land <- rast(file.path('data', 'raw-data','prop_land', 2010, 'prop_deciduous.tif'))
disturb_2010_rast <- rasterize((vect(file.path(raw, 'ECCC_disturbance', 'WB_disturb_other_2010.shp'))),
                               land)
disturb_2010_rast[is.na(disturb_2010_rast)]<-0

disturb_2015_rast <- rasterize((vect(file.path(raw, 'ECCC_disturbance', 'WB_disturb_other_2015.shp'))),
                               land)
disturb_2015_rast[is.na(disturb_2015_rast)]<-0

writeRaster(disturb_2010_rast, file.path(raw, 'ECCC_disturbance', 'WB_disturb_other_2010.tif'))
writeRaster(disturb_2015_rast, file.path(raw, 'ECCC_disturbance', 'WB_disturb_other_2015.tif'))

## HARVEST ----
harv <- rast(file.path(canada, 'CA_Forest_Harvest_1985-2020', 'CA_Forest_Harvest_1985-2020.tif'))
harv <- project(harv, crs, method = 'near')
harv.crop <- crop(harv, ext(disturb_2015))
harv.crop[harv.crop==0]<-NA
writeRaster(harv.crop, file.path(raw, 'WB_harv_1985-2020.tif'), overwrite =T)

## FIRES #####
## this has been run already
fires <- vect(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'nbac_1986_to_2020_20210810.shp'))
fires.proj <- project(fires, crs)
writeVector(fires.proj, file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires_1986_2020.shp'))

# make rasters from polygonal fires by year
make_fire_rast(fires, land)
