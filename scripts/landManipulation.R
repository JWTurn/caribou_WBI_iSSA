require(terra)
require(sf)
require(data.table)

### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')

crs <- st_crs(3978)$wkt


dat<- readRDS(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS'))
# dat.clean <- dat[complete.cases(x,y, datetime) & between(x, -1665110, 458200) &between(y, -98940, 2626920),
#            .(x,y, datetime, id)]
# saveRDS(dat.clean, file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS'))
# coords<- dat.clean%>%st_as_sf(coords = c('x','y'))%>%
#   st_set_crs(crs)

#coords <- vect(coords)
#st_write(coords, file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.shp'), append = F)

coords <- st_read(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.shp'))

# stepID[case_==TRUE, quantile(sl_, na.rm = T, 0.95)]
# # max = 51233.12 -- This is probably an error 
# # 95% 4422

### 10 km buffer around points to get an idea of extent of study area
# studyArea <- st_buffer(coords, dist = 10000)
# sa.union <- st_union(studyArea)
# sa.union
# plot(sa.union)
#sa.vect <- vect(sa.union)
#st_write(sa.union, file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'), append = F)

#land <- rast(file.path('data', 'raw-data', 'WB_LC.tif'))

studyArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))

## LAND ----
can2010 <- rast(file.path(canada, 'canada_2010', 'CAN_LC_2010_CAL.tif'))
can2015 <- rast(file.path(canada, 'canada_2015', 'CAN_LC_2015_CAL.tif'))
can2020 <- rast(file.path(canada, 'canada_2020', 'landcover-2020-classification.tif'))
can2020 <- project(can2020, can2015, method = 'near')

wb2020 <- crop(can2020, studyArea)
#plot(wb2020)
writeRaster(wb2020, file.path('data', 'derived-data', 'prepped-data', 'WB_LC_2020.tif'))

gc()
wb2015 <- crop(can2015, studyArea)
#plot(wb2015)
writeRaster(wb2015, file.path('data', 'derived-data', 'prepped-data', 'WB_LC_2015.tif'))

gc()
wb2010 <- crop(can2010, studyArea)
#plot(wb2010)
writeRaster(wb2010, file.path('data', 'derived-data', 'prepped-data', 'WB_LC_2010.tif'))

## PROP LANDCOVER ----
# What to buffer for proportion of landclasses
buff.diam <- 500  ## median step length = 752, I chose something a bit less

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
## This creates an object which can be used to make a layer of specified diameter
# The d value is what determines the buffer size if you want to change it.
## If you're doing multiple landcover classes, you only need to run this line once, as long as each of the habitat variables has the same resolution
Buff <- focalMat(land, d=buff.diam, type = 'circle')
## This generates a new raster where each cell corresponds to the mean wetland within the buffer.
# Since it's all 1s and 0s, this is the same as the proportion of wetland surrounding the focal variable
propneedle <- focal(needleleaf, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propneedle, file.path('data', 'raw-data', 'prop_needleleaf.tif'))

propdecid <- focal(deciduous, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propdecid, file.path('data', 'raw-data', 'prop_deciduous.tif'))

propmixed <- focal(mixed, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propmixed, file.path('data', 'raw-data', 'prop_mixed.tif'))

propshrub <- focal(shrub, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propshrub, file.path('data', 'raw-data', 'prop_shrub.tif'))

propgrass <- focal(grassland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propgrass, file.path('data', 'raw-data', 'prop_grassland.tif'))

proplichshrub <- focal(lichenshrub, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(proplichshrub, file.path('data', 'raw-data', 'prop_lichenshrub.tif'))

proplichgrass <- focal(lichengrass, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(proplichgrass, file.path('data', 'raw-data', 'prop_lichengrass.tif'))

propwet <- focal(wet, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propwet, file.path('data', 'raw-data', 'prop_wetland.tif'))

propcrop <- focal(cropland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propcrop, file.path('data', 'raw-data', 'prop_cropland.tif'))

propbarren <- focal(barrenland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propbarren, file.path('data', 'raw-data', 'prop_barrenland.tif'))

propurban <- focal(urban, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propurban, file.path('data', 'raw-data', 'prop_urban.tif'))

propwater <- focal(water, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propwater, file.path('data', 'raw-data', 'prop_water.tif'))

propsnow <- focal(snow, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
writeRaster(propsnow, file.path('data', 'raw-data', 'prop_snow.tif'))

## ROADS ----

roads <- st_read(file.path('data', 'raw-data', 'wbi_nrn.shp'))
rail <- st_read(file.path('data', 'raw-data', 'wbi_rail.shp'))

paved <- dplyr::filter(roads, PAVSTATUS %in% 'Paved')
#writeVector(vect(paved), file.path('data', 'raw-data', 'wbi_pave_roads.shp'), overwrite = T)

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

summary(as.factor(eccc_lines_2010$Class))
notroads_2010 <- subset(eccc_lines_2010, !(eccc_lines_2010$Class %in% c('Road', 'Railway')))


summary(as.factor(eccc_lines_2015$Class))
notroads_2015 <- subset(eccc_lines_2015, !(eccc_lines_2015$Class %in% c('Road', 'Railway')))

notroads_2010.crop <- crop(notroads_2010, ext(notroads_2015))

summary(as.factor(eccc_polys_2010$Class))
disturb_2010 <- subset(eccc_polys_2010, !(eccc_polys_2010$Class %in% c('Cutblock')))

summary(as.factor(eccc_polys_2015$Class))
disturb_2015 <- subset(eccc_polys_2015, !(eccc_polys_2015$Class %in% c('Cutblock')))

disturb_2010.crop <- crop(disturb_2010, ext(disturb_2015))

dist.mask <- mask(disturb_2010.crop, disturb_2015, inverse = T)
disturb_2015_merge <- union(disturb_2015, dist.mask)


writeVector(notroads_2010.crop, file.path(raw, 'ECCC_disturbance', 'WB_lfother_2010.shp'))
writeVector(notroads_2015, file.path(raw, 'ECCC_disturbance', 'WB_lfother_2015.shp'))

writeVector(disturb_2010.crop, file.path(raw, 'ECCC_disturbance', 'WB_disturb_other_2010.shp'))
writeVector(disturb_2015_merge, file.path(raw, 'ECCC_disturbance', 'WB_disturb_other_2015.shp'))

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
# fires <- vect(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'nbac_1986_to_2020_20210810.shp'))
# fires.proj <- project(fires, crs)
#writeVector(fires.proj, file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires_1986_2020.shp'))

lyr <- vect(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires_1986_2020.shp'))
fires <- readRDS(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires_list.RDS'))
fires<- rast(fires)
DT <- dattab[1:100]
DT[,year:= lubridate::year(t2_)]
lyr.sub <- subset(lyr, lyr$YEAR<= year)
DT[,fires_end:= 
     terra::extract(subset(lyr, lyr$YEAR<= year), cbind(.SD), fun = max)[,-1],
   .SDcols = c(coords)]


remotes::install_github(c("PredictiveEcology/LandR@development")) # may need to install
library(LandR)
library(sp)

# if you have a studyarea, skip this line, but give you study area this name
#studyArea <- randomStudyArea(size = 1e11) #  a LandR function

# Use that study area to crop/mask
sa <- prepInputsStandAgeMap(destinationPath = tempdir(), studyArea = studyArea) # creates the raster template without Fire DataBase

studyAreaExtent <- as(raster::extent(studyArea), "SpatialPolygons")
crs(e) <- crs(studyArea)
rasterToMatch <- rasterize(studyAreaExtent, sa) # creates a raster

# Now can get stand Age ...
saNoFires <- prepInputsStandAgeMap(destinationPath = tempdir(), studyArea = e) # now with fire database cut in
saWithFires <- prepInputsStandAgeMap(destinationPath = tempdir(), rasterToMatch = rasterToMatch) # now with fire database cut in
