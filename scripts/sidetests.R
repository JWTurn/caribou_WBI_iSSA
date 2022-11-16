require(targets)
require(raster)
require(sf)
require(rgeos)
require(rgdal)
# tar_load(inputland)
# tar_load(lcvalues)

crs <- CRS(st_crs(4326)$wkt)

sk<- readRDS(file.path('data', 'derived-data', 'prepped-data', 'SKprepDat.RDS'))
sk <- sk[complete.cases(long,lat, datetime),.(long, lat, datetime, id)]
coords<- sk%>%st_as_sf(coords = c('long','lat'))%>%
  st_set_crs(crs)

land <- raster(file.path('data', 'raw-data', 'WB_LCC.tif'))

land <- raster(file.path('data', 'raw-data', 'CanLCC.tif'))
projland <- projectRaster(land, crs = crs)
writeRaster(projland, file.path('data', 'raw-data', 'CanLCC_WGS84.tif'))

merge(inputland[, value := extract(lc, xy)], lcvalues, by = value)


road.shp <- rgdal::readOGR(file.path('data', 'raw-data', 'sk_roads', 'sk_roads.shp'))  
# %>%
#    st_transform(crs)# %>% as_Spatial()
road.shp <- spTransform(road.shp, crs)
#road.poly <- st_cast(road.shp, 'POLYGON')
#st_crs(road.shp)
#dum <- raster(ext = extent(coords))
dist2road <- gDistance(road.shp, hausdorff = T)

#dum2 <- raster(ext = extent(road.shp), resolution = 30, crs = crs)
road.ras <- raster(file.path('data', 'raw-data','sk_roads', 'sk_roads.tif'))
dist2road <- gridDistance(road.ras)
#road.ras<- stars::st_rasterize(road.shp)
#road.ras <- raster(road.ras$FID, ext=extent(road.shp))

#plot(road.shp)
