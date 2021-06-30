require(targets)
require(raster)
require(ggplot2)
require(rasterVis)
require(viridis)

tar_load(resamples)
tar_load(lc)
values(lc==0)<-NA
plot(lc)
gplot(lc) +
  geom_tile(aes(fill = value), show.legend = F) +
  scale_fill_gradientn(colours = viridis(15),na.value = "white") +
  geom_point(data = resamples, aes(x1_, y1_)) +
  coord_equal()
 # 

