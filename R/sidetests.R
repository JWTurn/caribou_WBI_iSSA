require(targets)
require(raster)
tar_load(inputland)
tar_load(lcvalues)

inputland <- projectRaster(inputland, crs = crs)
writeRaster(inputland, file.path('data', 'raw-data', 'CanLCC_WGS84.tif'))

merge(inputland[, value := extract(lc, xy)], lcvalues, by = value)

