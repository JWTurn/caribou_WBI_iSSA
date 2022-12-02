require(terra)
require(sf)
require(data.table)



crs <- st_crs(3978)$wkt


dat<- readRDS(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS'))
dat.clean <- dat[complete.cases(x,y, datetime) & between(x, -1665110, 458200) &between(y, -98940, 2626920),
           .(x,y, datetime, id)]
coords<- dat.clean%>%st_as_sf(coords = c('x','y'))%>%
  st_set_crs(crs)

#coords <- vect(coords)
#st_write(coords, file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.shp'), append = F)


studyArea <- st_buffer(coords, dist = 10000)
land <- rast(file.path('data', 'raw-data', 'WB_LC.tif'))

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

#######S

roads <- st_read(file.path('data', 'raw-data', 'wbi_nrn.shp'))
rail <- st_read(file.path('data', 'raw-data', 'wbi_rail.shp'))

paved <- dplyr::filter(roads, PAVSTATUS %in% 'Paved')
#writeVector(vect(paved), file.path('data', 'raw-data', 'wbi_pave_roads.shp'), overwrite = T)

 #### FIRES #####
## this has been run already
# fires <- vect(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'nbac_1986_to_2020_20210810.shp'))
# fires.proj <- project(fires, crs)
writeVector(fires.proj, file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires_1986_2020.shp'))

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
