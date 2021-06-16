require(targets)
require(raster)
require(sf)
tar_load(inputland)
tar_load(lcvalues)

land <- st_read(file.path('data', 'raw-data', 'CanLCC.tif'))
inputland <- projectRaster(inputland, crs = crs)
writeRaster(inputland, file.path('data', 'raw-data', 'CanLCC_WGS84.tif'))

merge(inputland[, value := extract(lc, xy)], lcvalues, by = value)


road.shp <- st_read(file.path('data', 'raw-data', 'sk_roads.shp'))

