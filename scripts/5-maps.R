# === Predicted use maps -------------------------------------
# Julie Turner
# started 14 April 2022

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf', 'prioritizr', 
          'glmmTMB', 'ggplot2', 'rasterVis', 'viridis', 'tidyterra', 'patchwork')
lapply(libs, Require::Require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')


## Extract values ----
canPoly <- vect(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))
studyArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))
ab.range <- vect(file.path(canada, 'AB_CaribouSubregionalPlanBoundaries', 'CARIBOU_SUB_REGIONAL_PLAN_BOUNDARIES_2022_07_04.shp'))

# load layers
year = 2015
linfeat_other <- rast(file.path('data', 'raw-data', 'ECCC_disturbance', 
                                paste0('WB_lfother_', year, '_distto.tif')))
disturb <- rast(file.path('data', 'raw-data', 'ECCC_disturbance', 
                          paste0('WB_disturb_other_', year, '.tif')))


yr = 2019

bryoids <- rast(file.path('data', 'raw-data','prop_land', yr, '500grid', 
                                  paste0('bryoids_500', '.tif')))

shrub <- rast(file.path('data', 'raw-data','prop_land', yr, '500grid', 
                                paste0('shrub_500', '.tif')))

wet <- rast(file.path('data', 'raw-data','prop_land', yr, '500grid', 
                              paste0('wet_500', '.tif')))

wettreed <- rast(file.path('data', 'raw-data','prop_land', yr, '500grid', 
                                   paste0('wet_treed_500', '.tif')))

herbs <- rast(file.path('data', 'raw-data','prop_land', yr, '500grid', 
                                paste0('herbs_500', '.tif')))

needleleaf<- rast(file.path('data', 'raw-data','prop_land', yr, '500grid', 
                                     paste0('needleleaf_500', '.tif')))

deciduous <- rast(file.path('data', 'raw-data','prop_land', yr, '500grid', 
                                    paste0('deciduous_500', '.tif')))

mixed <- rast(file.path('data', 'raw-data','prop_land', yr, '500grid', 
                                paste0('mixed_500', '.tif')))


prop_needleleaf <- needleleaf
prop_mixforest <- deciduous + mixed + wettreed
prop_veg <- shrub + bryoids + herbs
prop_wets <- wet

# needleleaf <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                              paste0('needleleaf_500', '.tif')))
# deciduous <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                             paste0('deciduous_500', '.tif')))
# mixed <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                         paste0('mixed_500', '.tif')))
# shrub <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                         paste0('shrub_500', '.tif')))
# grassland <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                             paste0('grassland_500', '.tif')))
# lichenshrub <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                               paste0('lichenshrub_500', '.tif')))
# lichengrass <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                               paste0('lichengrass_500', '.tif')))
# wetland <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                           paste0('wet_500', '.tif')))
# barrenland <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                              paste0('barrenland_500', '.tif')))
# water <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                         paste0('water_500', '.tif')))
# snow <- rast(file.path('data', 'raw-data','prop_land', year, '500grid',
#                        paste0('snow_500', '.tif')))
# 
# prop_forest <- needleleaf + deciduous + mixed
# prop_forage <- shrub + grassland + lichenshrub+ lichengrass
# prop_open <- barrenland + snow
# prop_wets <- wetland + water


fires <- rast(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', paste0('fires_', (year+5), '.tif')))

lf.full <- rast(file.path('data', 'derived-data', 'distto_roadrail_500.tif'))
lf <- crop(lf.full, studyArea)
harv <- rast(file.path('data', 'raw-data', 'WB_harv_1985-2020.tif'))

lf_other <- resample(linfeat_other, lf, method = 'average')
lf_other.ext <- extend(lf_other, ext(lf))
disturb <- resample(disturb, lf, method = 'max')
disturb.ext <- extend(disturb, ext(lf))
harv <- resample(harv, lf, method = 'max')
harv.ext <- extend(harv, ext(lf))
tsh <- (year + 5) - harv.ext
tsh[is.na(tsh)] <- 40


fires.crop <- resample(fires, lf, method = 'max')
#names(land.brick) <- c("lf_dist", "lc")
tsf <- (year + 5) - fires.crop
tsf[is.na(tsf)] <- 40

log_tsf <- log(tsf + 1)
log_tsh <- log(tsh + 1)
log_distlf <- log(lf + 1)
log_distlfother <- log(lf_other.ext + 1)


# removed open
# props <- c(prop_forage, prop_forest, prop_wets, log_tsf)
# props <- crop(props, ext(studyArea))
# 
# log_distlfother <- crop(log_distlfother, ext(studyArea))
# disturb <- crop(disturb, ext(studyArea))

land <- c(prop_veg, prop_needleleaf, prop_mixforest, prop_wets, log_tsf, log_tsh, log_distlf, 
          log_distlfother, disturb.ext)
names(land) <- c('prop_veg', 'prop_needleleaf', 'prop_mixforest', 'prop_wets', 'log_tsf', 'log_tsh', 'log_distlf', 
                 'log_distlfother', 'disturb')



# writeRaster(prop_forest.crop, file.path('data', 'derived-data', 'prop_forest_2015_500.tif'))
# writeRaster(prop_forage.crop, file.path('data', 'derived-data', 'prop_forage_2015_500.tif'))
# writeRaster(prop_open.crop, file.path('data', 'derived-data', 'prop_open_2015_500.tif'))
# writeRaster(prop_wets.crop, file.path('data', 'derived-data', 'prop_wets_2015_500.tif'))
# writeRaster(log_tsf, file.path('data', 'derived-data', 'log_tsf_2020_500.tif'))

ab.range <- project(ab.range, studyArea)
studyArea.ab <- union(studyArea, ab.range)


## Models ----

sel.2015 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2015-2020_HPC_noTA.RDS'))


### PDE ----
View(summary(sel.2015)$coef$cond)

mod2015re.tab <- make_betas_tab(sel.2015)


gc()
### START -----
canPoly <- project(canPoly, lf)

wbi.prov <- subset(canPoly, canPoly$PREABBR %in% c('Alta.', 'B.C.', 'Man.', 'N.W.T.', 'Sask.', 'Y.T.'))



#### full model PDE 2015 ----
gc()
lf.cov.re.2015<- (2*as.double(mod2015re.tab[term %like% 'distlf_end', 
                                            .(estimate)])*land$log_distlf)
lfother.cov.re.2015<- (2*as.double(mod2015re.tab[term %like% 'distlf_other_end', 
                                                 .(estimate)])*land$log_distlfother)
tsf.cov.re.2015<- (2*as.double(mod2015re.tab[term %like% 'ts_fire', 
                                             .(estimate)])*land$log_tsf)
tsh.cov.re.2015<- (2*as.double(mod2015re.tab[term %like% 'ts_harv', 
                                             .(estimate)])*land$log_tsh)
needleleaf.cov.re.2015 <- (2*as.double(mod2015re.tab[term %like% 'needleleaf_end', 
                                                 .(estimate)])*land$prop_needleleaf)
veg.cov.re.2015 <- (2*as.double(mod2015re.tab[term %like% 'veg_end', 
                                                 .(estimate)])*land$prop_veg)
mixforest.cov.re.2015 <- (2*as.double(mod2015re.tab[term %like% 'mixforest_end',
                                                .(estimate)])*land$prop_mixforest)
wets.cov.re.2015 <- (2*as.double(mod2015re.tab[term %like% 'wets_end', 
                                               .(estimate)])*land$prop_wets)
disturb.cov.re.2015 <- (2*as.double(mod2015re.tab[term %like% 'disturb', 
                                                  .(estimate)])*land$disturb)


numerator.re.2015 <- exp(lf.cov.re.2015 + lfother.cov.re.2015 + 
                           tsf.cov.re.2015 + tsh.cov.re.2015 + 
                           needleleaf.cov.re.2015 + veg.cov.re.2015 + 
                           mixforest.cov.re.2015 + 
                           wets.cov.re.2015 +
                           disturb.cov.re.2015)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.re.2015 <- global(numerator.re.2015, sum, na.rm = T)
pde.re.2015 <- numerator.re.2015/C.re.2015[[1]]


pde.re.2015.sa <- crop(pde.re.2015, studyArea, mask = T)
plot(pde.re.2015.sa)

breaks.re.2015 <- global(pde.re.2015.sa, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks.re.2015 <- unname(breaks.re.2015)
t.breaks.re.2015 <- as.vector(t(v.breaks.re.2015))
pde.re.discrete.2015 <- classify(pde.re.2015.sa, t.breaks.re.2015, include.lowest=TRUE, brackets=TRUE)
plot(pde.re.discrete.2015, breaks=0:10)

writeRaster(pde.re.discrete.2015, 
            file.path(derived, 'pde2015_re.tif'), overwrite = T)


## with ab guestimate
pde.re.2015.sa.ab <- crop(pde.re.2015, studyArea.ab, mask = T)

breaks.re.2015.sa.ab <- global(pde.re.2015.sa.ab, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks.re.2015.sa.ab <- unname(breaks.re.2015.sa.ab)
t.breaks.re.2015.sa.ab <- as.vector(t(v.breaks.re.2015.sa.ab))
pde.re.discrete.2015.sa.ab <- classify(pde.re.2015.sa.ab, t.breaks.re.2015.sa.ab, include.lowest=TRUE, brackets=TRUE)
plot(pde.re.discrete.2015.sa.ab, breaks=0:10)

writeRaster(pde.re.discrete.2015.sa.ab, 
            file.path(derived, 'pde2015_re_ab.tif'))






# plots ----
pde.re.discrete.2015 <- rast(file.path(derived, 'pde2015_re.tif'))
pde.re.discrete.2015.sa.ab <- rast(file.path(derived, 'pde2015_re_ab.tif'))

# nwt <- subset(canPoly, canPoly$PREABBR %in% c('N.W.T.'))
# nwt.2010 <- crop(pde.re.discrete.2010, nwt, mask = T)
# nwt.2015 <- crop(pde.re.discrete.2015, nwt, mask = T)
p.re.2015 <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = pde.re.2015.sa, show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA) +
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2015

p.re.2015.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2015), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2015.wbi

p.re.2015.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2015.sa.ab), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2015-2020 model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2015.wbi.ab

p.re.2010.wbi + p.re.2010.wbi.ab


###### nwt ----
nwt.sf <- st_as_sf(nwt)
nwt.2010.rast <- raster::raster(nwt.2010)
nwt.2010_df <- as.data.frame(nwt.2010.rast, xy = TRUE) %>%
  mutate(layer = factor(log_distlf))

# ggplot(nwt) +
#   geom_spatvector(fill = NA) +
#   geom_spatraster(data = nwt.2010, show.legend = T) +
ggplot() +
  geom_tile(data = nwt.2010_df, aes(x = x, y=y, fill = layer), show.legend = F) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = nwt, colour = "black", fill = NA) + 
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.re.2010.nwt<- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(nwt.2010), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2010.nwt

p.re.2015.nwt<- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(nwt.2015), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2015.nwt

##### attempt 2 ----
nwt.sf <- st_as_sf(nwt)
nwt.2010.rast <- raster::raster(nwt.2010)
nwt.2010_df <- as.data.frame(nwt.2010.rast, xy = TRUE) %>%
  mutate(layer = factor(log_distlf))
nwt.2015_df <- as.data.frame(raster::raster(nwt.2015), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))
nwt.diff_df <- as.data.frame(raster::raster(nwt.2015-nwt.2010), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))


p.nwt.2010 <- ggplot() +
  geom_tile(data = nwt.2010_df, aes(x = x, y=y, fill = layer), show.legend = F) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = nwt, colour = "black", fill = NA) + 
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.nwt.2015 <- ggplot() +
  geom_tile(data = nwt.2015_df, aes(x = x, y=y, fill = layer), show.legend = T) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = nwt, colour = "black", fill = NA) + 
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.nwt.diff <- ggplot() +
  geom_tile(data = nwt.diff_df, aes(x = x, y=y, fill = layer), show.legend = T) +
  scale_fill_manual(values = magma(20),na.value = NA, breaks= -10:10) +
  geom_spatvector(data = nwt, colour = "black", fill = NA) + 
  ggtitle('Change over time') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Change in selection') +
  coord_sf(crs = 3978)
p.nwt.diff

p.nwt.2010 + p.nwt.2015 + p.nwt.diff

wbi.sf <- st_as_sf(wbi.prov)
wbi.2010_df <- as.data.frame(raster::raster(pde.re.discrete.2010*1), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))
wbi.2015_df <- as.data.frame(raster::raster(pde.re.discrete.2015*1), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))
wbi.diff_df <- as.data.frame(raster::raster(pde.re.discrete.2015-pde.re.discrete.2010), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))


p.wbi.2010 <- ggplot() +
  geom_tile(data = wbi.2010_df, aes(x = x, y=y, fill = layer), show.legend = F) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = wbi.prov, colour = "black", fill = NA) + 
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.wbi.2015 <- ggplot() +
  geom_tile(data = wbi.2015_df, aes(x = x, y=y, fill = layer), show.legend = T) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = wbi.prov, colour = "black", fill = NA) + 
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.wbi.diff <- ggplot() +
  geom_tile(data = wbi.diff_df, aes(x = x, y=y, fill = layer), show.legend = T) +
  scale_fill_manual(values = magma(20),na.value = NA, breaks= -10:10) +
  geom_spatvector(data = wbi.prov, colour = "black", fill = NA) + 
  ggtitle('Change over time') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Change in selection') +
  coord_sf(crs = 3978)
p.wbi.diff

p.wbi.2010 + p.wbi.2015 + p.wbi.diff

#######

p.2015 <- gplot(pde.discrete.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  #geom_sf(data = st_as_sf(wbi.prov), fill = NA) +
  ggtitle('2015-2020 Jurisdictional model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.2015

p.2015.ab <- gplot(pde.discrete.2015.sa.ab) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  Jurisdictional model extrapolating AB') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.2015.ab


p.simp.2015 <- gplot(pde.simp.discrete.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020 nested jurisdictional model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.simp.2015


p.simp.2015.ab <- gplot(pde.simp.discrete.2015.sa.ab) +
  #geom_spatvector(data = wbi.prov, fill = NA) + 
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020 nested jurisdictional model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of use') +
  # coord_equal() +
  coord_sf(crs = 3978)

p.simp.2015.ab
plot(pde.simp.discrete.2015.sa.ab, breaks=0:10)


p.re.2010 <- gplot(pde.re.discrete.2010) +
  geom_tile(aes(fill = value), show.legend = F) +
  ggtitle('2010-2015  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.re.2010

p.re.2015 <- gplot(pde.re.discrete.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.re.2015

p.re.diff <- gplot(pde.re.discrete.2015 - pde.re.discrete.2010) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('Change over time') +
  scale_fill_gradientn(colours = magma(10),na.value = "white") +
  theme_bw() +
  labs(fill = 'Change in intensity') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.re.diff

p.re.2010 + p.re.2015 + p.re.diff

p.nwt.2010 <- gplot(nwt.2010) +
  geom_tile(aes(fill = value), show.legend = F) +
  ggtitle('2010-2015  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.nwt.2010

p.nwt.2015 <- gplot(nwt.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.nwt.2015

p.nwt.diff <- gplot(nwt.2015 - nwt.2010) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('Change over time') +
  scale_fill_gradientn(colours = magma(10),na.value = "white") +
  theme_bw() +
  labs(fill = 'Change in intensity') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.nwt.diff

p.nwt.2010 + p.nwt.2015 + p.nwt.diff

p.re.2015.ab <- gplot(pde.re.discrete.2015.sa.ab) +
  #geom_spatvector(data = wbi.prov, fill = NA) + 
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of use') +
  # coord_equal() +
  coord_sf(crs = 3978)

p.re.2015.ab


p.2015.ab + p.simp.2015.ab #+ p.re.2015.ab

####
# p.2015.wbi<- ggplot(wbi.prov) +
#   geom_spatvector(fill = NA) +
#   geom_spatraster(data = as.numeric(pde.discrete.2015.sa), show.legend = T) +
#   scale_fill_viridis(na.value = NA) +
#   ggtitle('2015-2020 model') +
#   theme_bw() +
#   theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
#   theme_void() +
#   labs(fill = 'Intensity of use') +
#   coord_sf(crs = 3978)
# p.2015.wbi

p.2015.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.discrete.2015.sa.ab), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  #scale_fill_viridis(na.value = NA) +
  ggtitle('2015-2020 fixed model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.2015.wbi.ab


p.simp.2015.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.simp.discrete.2015), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  #scale_fill_viridis(na.value = NA) +
  ggtitle('2015-2020 jurisdictional model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.simp.2015.wbi

p.simp.2015.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.simp.discrete.2015.sa.ab), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  #scale_fill_viridis(na.value = NA) +
  ggtitle('2015-2020 jurisdictional model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.simp.2015.wbi.ab


p.simp.2015.wbi + p.simp.2015.wbi.ab

