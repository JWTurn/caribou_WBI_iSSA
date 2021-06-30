require(targets)
require(raster)
require(ggplot2)
require(rasterVis)
require(viridis)
require(data.table)
require(sf)

tar_load(resamples)
tar_load(lc)
tar_load(randsteps)
tar_load(mergelc)

crs <- CRS(st_crs(4326)$wkt)
coords <- st_as_sf(resamples, coords = c('x1_', 'y1_'),
                   crs = crs)

coordsrand <- st_as_sf(mergelc, coords = c('x2_', 'y2_'),
                   crs = crs)
lcCrop<- crop(lc, coords)
lcCropRand<- crop(lc, coordsrand)
plot(lcCropRand)
gplot(lcCropRand) +
  geom_tile(aes(fill = value), show.legend = F) +
  scale_fill_gradientn(colours = viridis(15),na.value = "white") +
  coo_sf(data = coordsrand$geometry, aes(color = geometry)) +
  coord_equal()
 # 
test <- mergelc
old <- c('becomes')
new <- c('lc_end')
setnames(test, old, new)
