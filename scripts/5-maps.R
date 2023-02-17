# === Predicted use maps -------------------------------------
# Julie Turner
# started 14 April 2022

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf', 'prioritizr', 
          'glmmTMB', 'ggplot2', 'rasterVis', 'viridis', 'tidyterra')
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
needleleaf <- rast(file.path('data', 'raw-data','prop_land', year, 
                        paste0('prop_needleleaf', '.tif')))
deciduous <- rast(file.path('data', 'raw-data', 'prop_land', year,
                       paste0('prop_deciduous', '.tif')))
mixed <- rast(file.path('data', 'raw-data', 'prop_land', year,
                   paste0('prop_mixed', '.tif')))
shrub <- rast(file.path('data', 'raw-data', 'prop_land', year,
                   paste0('prop_shrub', '.tif')))
grassland <- rast(file.path('data', 'raw-data', 'prop_land', year,
                   paste0('prop_grassland', '.tif')))
lichenshrub <- rast(file.path('data', 'raw-data', 'prop_land', year,
                       paste0('prop_lichenshrub', '.tif')))
lichengrass <- rast(file.path('data', 'raw-data', 'prop_land', year,
                       paste0('prop_lichengrass', '.tif')))
wetland <- rast(file.path('data', 'raw-data', 'prop_land', year,
                     paste0('prop_wetland', '.tif')))
barrenland <- rast(file.path('data', 'raw-data', 'prop_land', year,
                    paste0('prop_barrenland', '.tif')))
water <- rast(file.path('data', 'raw-data', 'prop_land', year,
                   paste0('prop_water', '.tif')))
snow <- rast(file.path('data', 'raw-data', 'prop_land', year,
                  paste0('prop_snow', '.tif')))

prop_forest <- needleleaf + deciduous + mixed
prop_forage <- shrub + grassland + lichenshrub+ lichengrass
prop_open <- barrenland + snow
prop_wets <- wetland + water


fires <- rast(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires_2020.tif'))

lf <- rast(file.path('data', 'derived-data', 'distto_roadrail_500.tif'))

#prop_forest.crop <- crop(prop_forest, lf)
prop_forest.crop <- resample(prop_forest, lf, method = 'average')

#prop_forage.crop <- crop(prop_forage, lf)
prop_forage.crop <- resample(prop_forage, lf, method = 'average')

#prop_open.crop <- crop(prop_open, lf)
prop_open.crop <- resample(prop_open, lf, method = 'average')

#prop_wets.crop <- crop(prop_wets, lf)
prop_wets.crop <- resample(prop_wets, lf, method = 'average')

#fires.crop <- crop(fires, lf)
fires.crop <- resample(fires, lf, method = 'max')
#names(land.brick) <- c("lf_dist", "lc")

tsf <- 2020 - fires.crop
tsf[is.na(tsf)] <- 100
log_tsf <- log(tsf + 1)
log_distlf <- log(lf + 1)

land <- c(prop_forage.crop, prop_forest.crop, prop_open.crop, prop_wets.crop, log_tsf, log_distlf)
names(land) <- c('prop_forage', 'prop_forest', 'prop_open', 'prop_wets', 'log_tsf', 'log_distlf')



writeRaster(prop_forest.crop, file.path('data', 'derived-data', 'prop_forest_2015_500.tif'))
writeRaster(prop_forage.crop, file.path('data', 'derived-data', 'prop_forage_2015_500.tif'))
writeRaster(prop_open.crop, file.path('data', 'derived-data', 'prop_open_2015_500.tif'))
writeRaster(prop_wets.crop, file.path('data', 'derived-data', 'prop_wets_2015_500.tif'))
writeRaster(log_tsf, file.path('data', 'derived-data', 'log_tsf_2020_500.tif'))

ab.range <- project(ab.range, studyArea)
studyArea.ab <- union(studyArea, ab.range)


## Models ----
sel.2010 <- readRDS(file.path(derived, 'mod_ssa_2010-2015.RDS'))
sel.2015 <- readRDS(file.path(derived, 'mod_ssa_2015-2020.RDS'))

sel.2010.juris <- readRDS(file.path(derived, 'mod_ssa_juris_2010-2015.RDS'))
sel.2015.juris <- readRDS(file.path(derived, 'mod_ssa_juris_2015-2020.RDS'))


### PDE ----
summary(sel.2010.juris)
#summary(summer)$coef$cond[-1, "Estimate"]
mod2010avg.tab <- make_betas_tab(sel.2010)
mod2015avg.tab <- make_betas_tab(sel.2015)

mod2010.tab <- make_betas_tab(sel.2010.juris)
mod2015.tab <- make_betas_tab(sel.2015.juris)


gc()
### START -----
canPoly <- project(canPoly, lf)
ab <- subset(canPoly, canPoly$PREABBR == 'Alta.')
bc <- subset(canPoly, canPoly$PREABBR == 'B.C.')
mb <- subset(canPoly, canPoly$PREABBR == 'Man.')
nwt <- subset(canPoly, canPoly$PREABBR == 'N.W.T.')
sk <- subset(canPoly, canPoly$PREABBR == 'Sask.')
yt <- subset(canPoly, canPoly$PREABBR == 'Y.T.')

wbi.prov <- subset(canPoly, canPoly$PREABBR %in% c('Alta.', 'B.C.', 'Man.', 'N.W.T.', 'Sask.', 'Y.T.'))

land.ab <- crop(land, ab, mask = T)
land.bc <- crop(land, bc, mask = T)
land.mb <- crop(land, mb, mask = T)
land.nwt <- crop(land, nwt, mask = T)
land.sk <- crop(land, sk, mask = T)
land.yt <- crop(land, yt, mask = T)



#### ab 2010 ----
# this is extrapolated from the overall model not specifying jurisdiction
# numerator raster
lf.cov.2010.ab <- (2*as.double(mod2010.tab[term %like% 'lf' & term %like% 'mb', 
                                           .(estimate)])*land.ab$log_distlf)
tsf.cov.2010.ab <- (2*as.double(mod2010.tab[term %like% 'tsf' & term %like% 'mb', 
                                            .(estimate)])*land.ab$log_tsf)
forest.cov.2010.ab <- (2*as.double(mod2010.tab[term %like% 'forest' & term %like% 'mb', 
                                               .(estimate)])*land.ab$prop_forest)
forage.cov.2010.ab <- (2*as.double(mod2010.tab[term %like% 'forage' & term %like% 'mb', 
                                               .(estimate)])*land.ab$prop_forage)
open.cov.2010.ab <- (2*as.double(mod2010.tab[term %like% 'open' & term %like% 'mb', 
                                             .(estimate)])*land.ab$prop_open)
wets.cov.2010.ab <- (2*as.double(mod2010.tab[term %like% 'wets' & term %like% 'mb', 
                                             .(estimate)])*land.ab$prop_wets)

gc()
numerator.2010.ab <- exp(lf.cov.2010.ab + tsf.cov.2010.ab + forest.cov.2010.ab +
                           forage.cov.2010.ab + open.cov.2010.ab + wets.cov.2010.ab)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2010.ab <- global(numerator.2010.ab, sum, na.rm = T)
pde.2010.ab <- numerator.2010.ab/C.2010.ab[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2010.ab <- global(pde.2010.ab, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2010.ab <- unname(breaks.2010.ab)
# t.breaks.2010.ab <- as.vector(t(v.breaks.2010.ab))
# pde.discrete.2010.ab <- classify(pde.2010.ab, t.breaks.2010.ab, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2010.ab, breaks=0:10)
# writeRaster(pde.discrete.2010.ab, file.path(derived, 'pde_ab_disc10_2010.tif'), overwrite = T)
# 
#### mb 2010 ----
# numerator raster
lf.cov.2010.mb <- (2*as.double(mod2010.tab[term %like% 'lf' & term %like% 'mb', 
                                           .(estimate)])*land.mb$log_distlf)
tsf.cov.2010.mb <- (2*as.double(mod2010.tab[term %like% 'tsf' & term %like% 'mb', 
                                           .(estimate)])*land.mb$log_tsf)
forest.cov.2010.mb <- (2*as.double(mod2010.tab[term %like% 'forest' & term %like% 'mb', 
                                           .(estimate)])*land.mb$prop_forest)
forage.cov.2010.mb <- (2*as.double(mod2010.tab[term %like% 'forage' & term %like% 'mb', 
                                               .(estimate)])*land.mb$prop_forage)
open.cov.2010.mb <- (2*as.double(mod2010.tab[term %like% 'open' & term %like% 'mb', 
                                               .(estimate)])*land.mb$prop_open)
wets.cov.2010.mb <- (2*as.double(mod2010.tab[term %like% 'wets' & term %like% 'mb', 
                                               .(estimate)])*land.mb$prop_wets)

gc()
numerator.2010.mb <- exp(lf.cov.2010.mb + tsf.cov.2010.mb + forest.cov.2010.mb +
                      forage.cov.2010.mb + open.cov.2010.mb + wets.cov.2010.mb)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2010.mb <- global(numerator.2010.mb, sum, na.rm = T)
pde.2010.mb <- numerator.2010.mb/C.2010.mb[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2010.mb <- global(pde.2010.mb, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2010.mb <- unname(breaks.2010.mb)
# t.breaks.2010.mb <- as.vector(t(v.breaks.2010.mb))
# pde.discrete.2010.mb <- classify(pde.2010.mb, t.breaks.2010.mb, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2010.mb, breaks=0:10)
# writeRaster(pde.discrete.2010.mb, file.path(derived, 'pde_mb_disc10_2010.tif'), overwrite = T)
# 


#### sk 2010 ----
# numerator raster
lf.cov.2010.sk <- (2*as.double(mod2010.tab[term %like% 'lf' & term %like% 'sk', 
                                           .(estimate)])*land.sk$log_distlf)
tsf.cov.2010.sk <- (2*as.double(mod2010.tab[term %like% 'tsf' & term %like% 'sk', 
                                            .(estimate)])*land.sk$log_tsf)
forest.cov.2010.sk <- (2*as.double(mod2010.tab[term %like% 'forest' & term %like% 'sk', 
                                               .(estimate)])*land.sk$prop_forest)
forage.cov.2010.sk <- (2*as.double(mod2010.tab[term %like% 'forage' & term %like% 'sk', 
                                               .(estimate)])*land.sk$prop_forage)
open.cov.2010.sk <- (2*as.double(mod2010.tab[term %like% 'open' & term %like% 'sk', 
                                             .(estimate)])*land.sk$prop_open)
wets.cov.2010.sk <- (2*as.double(mod2010.tab[term %like% 'wets' & term %like% 'sk', 
                                             .(estimate)])*land.sk$prop_wets)

gc()
numerator.2010.sk <- exp(lf.cov.2010.sk + tsf.cov.2010.sk + forest.cov.2010.sk +
                           forage.cov.2010.sk + open.cov.2010.sk + wets.cov.2010.sk)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2010.sk <- global(numerator.2010.sk, sum, na.rm = T)
pde.2010.sk <- numerator.2010.sk/C.2010.sk[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2010.sk <- global(pde.2010.sk, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2010.sk <- unname(breaks.2010.sk)
# t.breaks.2010.sk <- as.vector(t(v.breaks.2010.sk))
# pde.discrete.2010.sk <- classify(pde.2010.sk, t.breaks.2010.sk, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2010.sk, breaks=0:10)
# writeRaster(pde.discrete.2010.sk, file.path(derived, 'pde_sk_disc10_2010.tif'), overwrite = T)



#### bc 2010 ----
# numerator raster
lf.cov.2010.bc <- (2*as.double(mod2010.tab[term %like% 'lf' & term %like% 'bc', 
                                           .(estimate)])*land.bc$log_distlf)
tsf.cov.2010.bc <- (2*as.double(mod2010.tab[term %like% 'tsf' & term %like% 'bc', 
                                            .(estimate)])*land.bc$log_tsf)
forest.cov.2010.bc <- (2*as.double(mod2010.tab[term %like% 'forest' & term %like% 'bc', 
                                               .(estimate)])*land.bc$prop_forest)
forage.cov.2010.bc <- (2*as.double(mod2010.tab[term %like% 'forage' & term %like% 'bc', 
                                               .(estimate)])*land.bc$prop_forage)
open.cov.2010.bc <- (2*as.double(mod2010.tab[term %like% 'open' & term %like% 'bc', 
                                             .(estimate)])*land.bc$prop_open)
wets.cov.2010.bc <- (2*as.double(mod2010.tab[term %like% 'wets' & term %like% 'bc', 
                                             .(estimate)])*land.bc$prop_wets)

gc()
numerator.2010.bc <- exp(lf.cov.2010.bc + tsf.cov.2010.bc + forest.cov.2010.bc +
                           forage.cov.2010.bc + open.cov.2010.bc + wets.cov.2010.bc)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2010.bc <- global(numerator.2010.bc, sum, na.rm = T)
pde.2010.bc <- numerator.2010.bc/C.2010.bc[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2010.bc <- global(pde.2010.bc, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2010.bc <- unname(breaks.2010.bc)
# t.breaks.2010.bc <- as.vector(t(v.breaks.2010.bc))
# pde.discrete.2010.bc <- classify(pde.2010.bc, t.breaks.2010.bc, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2010.bc, breaks=0:10)
# writeRaster(pde.discrete.2010.bc, file.path(derived, 'pde_bc_disc10_2010.tif'), overwrite = T)


#### nwt 2010 ----
# numerator raster
lf.cov.2010.nwt <- (2*as.double(mod2010.tab[term %like% 'lf' & term %like% 'nwt', 
                                           .(estimate)])*land.nwt$log_distlf)
tsf.cov.2010.nwt <- (2*as.double(mod2010.tab[term %like% 'tsf' & term %like% 'nwt', 
                                            .(estimate)])*land.nwt$log_tsf)
forest.cov.2010.nwt <- (2*as.double(mod2010.tab[term %like% 'forest' & term %like% 'nwt', 
                                               .(estimate)])*land.nwt$prop_forest)
forage.cov.2010.nwt <- (2*as.double(mod2010.tab[term %like% 'forage' & term %like% 'nwt', 
                                               .(estimate)])*land.nwt$prop_forage)
open.cov.2010.nwt <- (2*as.double(mod2010.tab[term %like% 'open' & term %like% 'nwt', 
                                             .(estimate)])*land.nwt$prop_open)
wets.cov.2010.nwt <- (2*as.double(mod2010.tab[term %like% 'wets' & term %like% 'nwt', 
                                             .(estimate)])*land.nwt$prop_wets)

gc()
numerator.2010.nwt <- exp(lf.cov.2010.nwt + tsf.cov.2010.nwt + forest.cov.2010.nwt +
                           forage.cov.2010.nwt + open.cov.2010.nwt + wets.cov.2010.nwt)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2010.nwt <- global(numerator.2010.nwt, sum, na.rm = T)
pde.2010.nwt <- numerator.2010.nwt/C.2010.nwt[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2010.nwt <- global(pde.2010.nwt, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2010.nwt <- unname(breaks.2010.nwt)
# t.breaks.2010.nwt <- as.vector(t(v.breaks.2010.nwt))
# pde.discrete.2010.nwt <- classify(pde.2010.nwt, t.breaks.2010.nwt, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2010.nwt, breaks=0:10)
# writeRaster(pde.discrete.2010.nwt, file.path(derived, 'pde_nwt_disc10_2010.tif'), overwrite = T)


#### Combine 2010 PDEs ----
pde.2010 <- mosaic(pde.2010.ab, pde.2010.bc, pde.2010.mb, pde.2010.nwt, pde.2010.sk)
plot(pde.2010)

pde.2010.sa <- crop(pde.2010, studyArea, mask = T)

breaks.2010 <- global(pde.2010.sa, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks.2010 <- unname(breaks.2010)
t.breaks.2010 <- as.vector(t(v.breaks.2010))
pde.discrete.2010 <- classify(pde.2010.sa, t.breaks.2010, include.lowest=TRUE, brackets=TRUE)
plot(pde.discrete.2010, breaks=0:10)

## with ab guestimate
pde.2010.sa.ab <- crop(pde.2010, studyArea.ab, mask = T)

breaks.2010.sa.ab <- global(pde.2010.sa.ab, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks.2010.sa.ab <- unname(breaks.2010.sa.ab)
t.breaks.2010.sa.ab <- as.vector(t(v.breaks.2010.sa.ab))
pde.discrete.2010.sa.ab <- classify(pde.2010.sa.ab, t.breaks.2010.sa.ab, include.lowest=TRUE, brackets=TRUE)
plot(pde.discrete.2010.sa.ab, breaks=0:10)


####

#### ab 2015 ----
# this is using the mb coefs for now
# numerator raster
lf.cov.2015.ab <- (2*as.double(mod2015.tab[term %like% 'lf' & term %like% 'mb', 
                                           .(estimate)])*land.ab$log_distlf)
tsf.cov.2015.ab <- (2*as.double(mod2015.tab[term %like% 'tsf' & term %like% 'mb', 
                                            .(estimate)])*land.ab$log_tsf)
forest.cov.2015.ab <- (2*as.double(mod2015.tab[term %like% 'forest' & term %like% 'mb', 
                                               .(estimate)])*land.ab$prop_forest)
forage.cov.2015.ab <- (2*as.double(mod2015.tab[term %like% 'forage' & term %like% 'mb', 
                                               .(estimate)])*land.ab$prop_forage)
open.cov.2015.ab <- (2*as.double(mod2015.tab[term %like% 'open' & term %like% 'mb', 
                                             .(estimate)])*land.ab$prop_open)
wets.cov.2015.ab <- (2*as.double(mod2015.tab[term %like% 'wets' & term %like% 'mb', 
                                             .(estimate)])*land.ab$prop_wets)

gc()
numerator.2015.ab <- exp(lf.cov.2015.ab + tsf.cov.2015.ab + forest.cov.2015.ab +
                           forage.cov.2015.ab + open.cov.2015.ab + wets.cov.2015.ab)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2015.ab <- global(numerator.2015.ab, sum, na.rm = T)
pde.2015.ab <- numerator.2015.ab/C.2015.ab[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2015.ab <- global(pde.2015.ab, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2015.ab <- unname(breaks.2015.ab)
# t.breaks.2015.ab <- as.vector(t(v.breaks.2015.ab))
# pde.discrete.2015.ab <- classify(pde.2015.ab, t.breaks.2015.ab, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2015.ab, breaks=0:10)
# writeRaster(pde.discrete.2015.ab, file.path(derived, 'pde_ab_disc10_2015.tif'), overwrite = T)
# 
#### mb 2015 ----
# numerator raster
lf.cov.2015.mb <- (2*as.double(mod2015.tab[term %like% 'lf' & term %like% 'mb', 
                                           .(estimate)])*land.mb$log_distlf)
tsf.cov.2015.mb <- (2*as.double(mod2015.tab[term %like% 'tsf' & term %like% 'mb', 
                                            .(estimate)])*land.mb$log_tsf)
forest.cov.2015.mb <- (2*as.double(mod2015.tab[term %like% 'forest' & term %like% 'mb', 
                                               .(estimate)])*land.mb$prop_forest)
forage.cov.2015.mb <- (2*as.double(mod2015.tab[term %like% 'forage' & term %like% 'mb', 
                                               .(estimate)])*land.mb$prop_forage)
open.cov.2015.mb <- (2*as.double(mod2015.tab[term %like% 'open' & term %like% 'mb', 
                                             .(estimate)])*land.mb$prop_open)
wets.cov.2015.mb <- (2*as.double(mod2015.tab[term %like% 'wets' & term %like% 'mb', 
                                             .(estimate)])*land.mb$prop_wets)

gc()
numerator.2015.mb <- exp(lf.cov.2015.mb + tsf.cov.2015.mb + forest.cov.2015.mb +
                           forage.cov.2015.mb + open.cov.2015.mb + wets.cov.2015.mb)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2015.mb <- global(numerator.2015.mb, sum, na.rm = T)
pde.2015.mb <- numerator.2015.mb/C.2015.mb[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2015.mb <- global(pde.2015.mb, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2015.mb <- unname(breaks.2015.mb)
# t.breaks.2015.mb <- as.vector(t(v.breaks.2015.mb))
# pde.discrete.2015.mb <- classify(pde.2015.mb, t.breaks.2015.mb, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2015.mb, breaks=0:10)
# writeRaster(pde.discrete.2015.mb, file.path(derived, 'pde_mb_disc10_2015.tif'), overwrite = T)
# 


#### sk 2015 ----
# numerator raster
lf.cov.2015.sk <- (2*as.double(mod2015.tab[term %like% 'lf' & term %like% 'sk', 
                                           .(estimate)])*land.sk$log_distlf)
tsf.cov.2015.sk <- (2*as.double(mod2015.tab[term %like% 'tsf' & term %like% 'sk', 
                                            .(estimate)])*land.sk$log_tsf)
forest.cov.2015.sk <- (2*as.double(mod2015.tab[term %like% 'forest' & term %like% 'sk', 
                                               .(estimate)])*land.sk$prop_forest)
forage.cov.2015.sk <- (2*as.double(mod2015.tab[term %like% 'forage' & term %like% 'sk', 
                                               .(estimate)])*land.sk$prop_forage)
open.cov.2015.sk <- (2*as.double(mod2015.tab[term %like% 'open' & term %like% 'sk', 
                                             .(estimate)])*land.sk$prop_open)
wets.cov.2015.sk <- (2*as.double(mod2015.tab[term %like% 'wets' & term %like% 'sk', 
                                             .(estimate)])*land.sk$prop_wets)

gc()
numerator.2015.sk <- exp(lf.cov.2015.sk + tsf.cov.2015.sk + forest.cov.2015.sk +
                           forage.cov.2015.sk + open.cov.2015.sk + wets.cov.2015.sk)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2015.sk <- global(numerator.2015.sk, sum, na.rm = T)
pde.2015.sk <- numerator.2015.sk/C.2015.sk[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2015.sk <- global(pde.2015.sk, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2015.sk <- unname(breaks.2015.sk)
# t.breaks.2015.sk <- as.vector(t(v.breaks.2015.sk))
# pde.discrete.2015.sk <- classify(pde.2015.sk, t.breaks.2015.sk, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2015.sk, breaks=0:10)
# writeRaster(pde.discrete.2015.sk, file.path(derived, 'pde_sk_disc10_2015.tif'), overwrite = T)



#### bc 2015 ----
# numerator raster
lf.cov.2015.bc <- (2*as.double(mod2015.tab[term %like% 'lf' & term %like% 'bc', 
                                           .(estimate)])*land.bc$log_distlf)
tsf.cov.2015.bc <- (2*as.double(mod2015.tab[term %like% 'tsf' & term %like% 'bc', 
                                            .(estimate)])*land.bc$log_tsf)
forest.cov.2015.bc <- (2*as.double(mod2015.tab[term %like% 'forest' & term %like% 'bc', 
                                               .(estimate)])*land.bc$prop_forest)
forage.cov.2015.bc <- (2*as.double(mod2015.tab[term %like% 'forage' & term %like% 'bc', 
                                               .(estimate)])*land.bc$prop_forage)
open.cov.2015.bc <- (2*as.double(mod2015.tab[term %like% 'open' & term %like% 'bc', 
                                             .(estimate)])*land.bc$prop_open)
wets.cov.2015.bc <- (2*as.double(mod2015.tab[term %like% 'wets' & term %like% 'bc', 
                                             .(estimate)])*land.bc$prop_wets)

gc()
numerator.2015.bc <- exp(lf.cov.2015.bc + tsf.cov.2015.bc + forest.cov.2015.bc +
                           forage.cov.2015.bc + open.cov.2015.bc + wets.cov.2015.bc)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2015.bc <- global(numerator.2015.bc, sum, na.rm = T)
pde.2015.bc <- numerator.2015.bc/C.2015.bc[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2015.bc <- global(pde.2015.bc, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2015.bc <- unname(breaks.2015.bc)
# t.breaks.2015.bc <- as.vector(t(v.breaks.2015.bc))
# pde.discrete.2015.bc <- classify(pde.2015.bc, t.breaks.2015.bc, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2015.bc, breaks=0:10)
# writeRaster(pde.discrete.2015.bc, file.path(derived, 'pde_bc_disc10_2015.tif'), overwrite = T)


#### nwt 2015 ----
# numerator raster
lf.cov.2015.nwt <- (2*as.double(mod2015.tab[term %like% 'lf' & term %like% 'nwt', 
                                            .(estimate)])*land.nwt$log_distlf)
tsf.cov.2015.nwt <- (2*as.double(mod2015.tab[term %like% 'tsf' & term %like% 'nwt', 
                                             .(estimate)])*land.nwt$log_tsf)
forest.cov.2015.nwt <- (2*as.double(mod2015.tab[term %like% 'forest' & term %like% 'nwt', 
                                                .(estimate)])*land.nwt$prop_forest)
forage.cov.2015.nwt <- (2*as.double(mod2015.tab[term %like% 'forage' & term %like% 'nwt', 
                                                .(estimate)])*land.nwt$prop_forage)
open.cov.2015.nwt <- (2*as.double(mod2015.tab[term %like% 'open' & term %like% 'nwt', 
                                              .(estimate)])*land.nwt$prop_open)
wets.cov.2015.nwt <- (2*as.double(mod2015.tab[term %like% 'wets' & term %like% 'nwt', 
                                              .(estimate)])*land.nwt$prop_wets)

gc()
numerator.2015.nwt <- exp(lf.cov.2015.nwt + tsf.cov.2015.nwt + forest.cov.2015.nwt +
                            forage.cov.2015.nwt + open.cov.2015.nwt + wets.cov.2015.nwt)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2015.nwt <- global(numerator.2015.nwt, sum, na.rm = T)
pde.2015.nwt <- numerator.2015.nwt/C.2015.nwt[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)



# breaks.2015.nwt <- global(pde.2015.nwt, quantile, na.rm = T, probs = seq(0,1,.1))
# 
# v.breaks.2015.nwt <- unname(breaks.2015.nwt)
# t.breaks.2015.nwt <- as.vector(t(v.breaks.2015.nwt))
# pde.discrete.2015.nwt <- classify(pde.2015.nwt, t.breaks.2015.nwt, include.lowest=TRUE, brackets=TRUE)
# plot(pde.discrete.2015.nwt, breaks=0:10)
# writeRaster(pde.discrete.2015.nwt, file.path(derived, 'pde_nwt_disc10_2015.tif'), overwrite = T)


#### Combine 2015 PDEs ----
pde.2015 <- mosaic(pde.2015.ab, pde.2015.bc, pde.2015.mb, pde.2015.nwt, pde.2015.sk)
plot(pde.2015)

pde.2015.sa <- crop(pde.2015, studyArea, mask = T)

breaks.2015 <- global(pde.2015.sa, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks.2015 <- unname(breaks.2015)
t.breaks.2015 <- as.vector(t(v.breaks.2015))
pde.discrete.2015 <- classify(pde.2015.sa, t.breaks.2015, include.lowest=TRUE, brackets=TRUE)
plot(pde.discrete.2015, breaks=0:10)

## with ab guestimate
pde.2015.sa.ab <- crop(pde.2015, studyArea.ab, mask = T)

breaks.2015.sa.ab <- global(pde.2015.sa.ab, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks.2015.sa.ab <- unname(breaks.2015.sa.ab)
t.breaks.2015.sa.ab <- as.vector(t(v.breaks.2015.sa.ab))
pde.discrete.2015.sa.ab <- classify(pde.2015.sa.ab, t.breaks.2015.sa.ab, include.lowest=TRUE, brackets=TRUE)
plot(pde.discrete.2015.sa.ab, breaks=0:10)


#### full model PDE 2015 ----

lf.cov.2015<- (2*as.double(mod2015avg.tab[term %like% 'lf', 
                                              .(estimate)])*land$log_distlf)
tsf.cov.2015<- (2*as.double(mod2015avg.tab[term %like% 'tsf', 
                                               .(estimate)])*land$log_tsf)
forest.cov.2015 <- (2*as.double(mod2015avg.tab[term %like% 'forest', 
                                                  .(estimate)])*land$prop_forest)
forage.cov.2015 <- (2*as.double(mod2015avg.tab[term %like% 'forage', 
                                                  .(estimate)])*land$prop_forage)
open.cov.2015 <- (2*as.double(mod2015avg.tab[term %like% 'open', 
                                                .(estimate)])*land$prop_open)
wets.cov.2015 <- (2*as.double(mod2015avg.tab[term %like% 'wets', 
                                                .(estimate)])*land$prop_wets)


numerator.2015 <- exp(lf.cov.2015 + tsf.cov.2015 + forest.cov.2015 +
                            forage.cov.2015 + open.cov.2015 + wets.cov.2015)

#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C.2015 <- global(numerator.2015, sum, na.rm = T)
pde.avg.2015 <- numerator.2015/C.2015[[1]]


pde.avg.2015.sa <- crop(pde.avg.2015, studyArea, mask = T)

breaks.avg.2015 <- global(pde.avg.2015.sa, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks.avg.2015 <- unname(breaks.avg.2015)
t.breaks.avg.2015 <- as.vector(t(v.breaks.avg.2015))
pde.avg.discrete.2015 <- classify(pde.avg.2015.sa, t.breaks.avg.2015, include.lowest=TRUE, brackets=TRUE)
plot(pde.avg.discrete.2015, breaks=0:10)

## with ab guestimate
pde.avg.2015.sa.ab <- crop(pde.avg.2015, studyArea.ab, mask = T)

breaks.avg.2015.sa.ab <- global(pde.avg.2015.sa.ab, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks.avg.2015.sa.ab <- unname(breaks.avg.2015.sa.ab)
t.breaks.avg.2015.sa.ab <- as.vector(t(v.breaks.avg.2015.sa.ab))
pde.avg.discrete.2015.sa.ab <- classify(pde.avg.2015.sa.ab, t.breaks.avg.2015.sa.ab, include.lowest=TRUE, brackets=TRUE)
plot(pde.avg.discrete.2015.sa.ab, breaks=0:10)




#### plots
p.2015 <- gplot(pde.discrete.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  #geom_sf(data = st_as_sf(wbi.prov), fill = NA) +
  ggtitle('2015-2020 Jurisdictional model') +
  scale_fill_gradientn(colours = viridis(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.2015

p.2015.ab <- gplot(pde.discrete.2015.sa.ab) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  Jurisdictional model extrapolating AB') +
  scale_fill_gradientn(colours = viridis(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.2015.ab


p.avg.2015 <- gplot(pde.avg.discrete.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  model') +
  scale_fill_gradientn(colours = viridis(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.avg.2015


p.avg.2015.ab <- gplot(pde.avg.discrete.2015.sa.ab) +
  #geom_spatvector(data = wbi.prov, fill = NA) + 
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  model') +
  scale_fill_gradientn(colours = viridis(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of use') +
 # coord_equal() +
  coord_sf(crs = 3978)

p.avg.2015.ab


####
p.2015.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = pde.2015.sa, show.legend = T) +
  scale_fill_viridis(na.value = NA, breaks = c(0,10)) +
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of use') +
  coord_sf(crs = 3978)
p.2015.wbi

p.2015.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = pde.2015.sa.ab, show.legend = T) +
  scale_fill_viridis(na.value = NA, breaks = c(0,10)) +
  ggtitle('2015-2020 model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_sf(crs = 3978)
p.015.wbi.ab

p.avg.2015.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = pde.avg.2015.sa, show.legend = T) +
  scale_fill_viridis(na.value = NA, breaks = c(0,10)) +
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of use') +
  coord_sf(crs = 3978)
p.avg.2015.wbi

p.avg.2015.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = pde.avg.2015.sa.ab, show.legend = T) +
  scale_fill_viridis(na.value = NA, breaks = c(0,10)) +
  ggtitle('2015-2020 model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_sf(crs = 3978)
p.avg.2015.wbi.ab
