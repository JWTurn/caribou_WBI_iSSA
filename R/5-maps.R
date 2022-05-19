# === Predicted use maps -------------------------------------
# Julie Turner
# started 14 April 2022

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf', 'prioritizr', 'glmmTMB')
lapply(libs, Require::Require, character.only = TRUE)


### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

landclass <- fread(file.path('data', 'raw-data', 'rcl.csv'))

## Extract values ----
# load layers
land <- rast(file.path('data', 'raw-data', 'WB_LC.tif'))
lf <- rast(file.path('data', 'derived-data', 'roads_dist_mb.tif'))

land.crop <- crop(land, lf)
land.crop <- resample(land.crop,lf, method = 'near')

#names(land.brick) <- c("lf_dist", "lc")




## Models ----

mod <- readRDS(file.path('data', 'derived-data', 'mb_ssa.RDS'))


### PDE ----
summary(mod)
#summary(summer)$coef$cond[-1, "Estimate"]
mod.sum <- broom.mixed::tidy(mod, effect = 'fixed')
mod.tab <- as.data.table(mod.sum)[,.(term, estimate)]


## reclassifly land with betas
# 
# dat[lc_end %in% c('barren', 'grassland', 'lichen-grass', 'lichen-shrub', 'shrub', 'urban', 'cropland'),lc_end_adj:= 'open-forage']
# dat[lc_end %in% c('deciduous', 'mixedforest'),lc_end_adj:= 'deciduous']
# dat[lc_end %in% c('water', 'wetland', 'snow'),lc_end_adj:= 'wet']

landclass[becomes %in% c('needleleaf'), new := mod.tab[[3,2]]]
landclass[becomes %in% c('barren', 'grassland', 'lichen-grass', 'lichen-shrub', 'shrub', 'urban', 'cropland'),
          new := mod.tab[[4,2]]] 
landclass[becomes %in% c('water', 'wetland', 'snow'), new := mod.tab[[5,2]]] # open

land_rcl <- classify(land.crop, landclass[,.(value, new)])
#writeRaster(land.brick$lc_rcl_fall, file.path(derived, 'lc_rcl_fall.tif'))

### START -----
# 
# land.brick <- c(rast(file.path(derived, 'lf_boo_range.tif')), 
#                 rast(file.path(derived, 'land_boo_range.tif')),
#                 rast(file.path(derived, 'lc_rcl_fall.tif')))
# names(land.brick) <- c("lf_dist", "lc", 'lc_rcl_fall')

# numerator raster
land.cov <- (2*land_rcl)
lf.cov <- (2*mod.tab[[6,2]]*log(lf))

numerator <- exp(land.cov +lf.cov)
#numerator_fall[is.infinite(numerator_fall)] <- NA
#plot(numerator,  breaks=seq(min(numerator, na.rm = T), max(numerator, na.rm = T), length.out = 10))

# the normalizing constant.
C <- global(numerator, sum, na.rm = T)
pde <- numerator/C[[1]]
#writeRaster(pde, file.path(derived, 'pde.tif'), overwrite = T)


### START -----
#pde <- rast(file.path(derived, 'pde.tif'))
#plot(pde,  breaks=seq(0, 1.0289e-06, length.out = 5))
#plot(pde,  breaks=0:10)


breaks <- global(pde, quantile, na.rm = T, probs = seq(0,1,.1))
breaks.5 <- global(pde, quantile, na.rm = T, probs = seq(0,1, length.out = 5))

v.breaks <- unname(as.vector(breaks))
t.breaks <- as.vector(t(v.breaks))
pde.discrete <- classify(pde, t.breaks, include.lowest=TRUE, brackets=TRUE)
plot(pde.discrete, breaks=0:10)
writeRaster(pde.discrete, file.path(derived, 'pde_mb_disc10.tif'), overwrite = T)

v.breaks.5 <- unname(as.vector(breaks.5))
t.breaks.5 <- as.vector(t(v.breaks.5))
pde.discrete.5 <- classify(pde, t.breaks.5, include.lowest=TRUE, brackets=TRUE)
plot(pde.discrete.5, breaks=0:5)
writeRaster(pde.discrete.5, file.path(derived, 'pde_fall_disc5.tif'), overwrite = T)
#global(land.brick$lc_rcl, sum, na.rm = T)



