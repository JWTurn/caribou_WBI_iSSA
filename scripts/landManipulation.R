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


roads <- st_read(file.path('data', 'raw-data', 'wbi_nrn.shp'))
rail <- st_read(file.path('data', 'raw-data', 'wbi_rail.shp'))

paved <- dplyr::filter(roads, PAVSTATUS %in% 'Paved')
#writeVector(vect(paved), file.path('data', 'raw-data', 'wbi_pave_roads.shp'), overwrite = T)

 #### FIRES #####
remotes::install_github(c("PredictiveEcology/LandR@development")) # may need to install

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
