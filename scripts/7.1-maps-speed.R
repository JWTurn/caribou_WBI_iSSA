# === Predicted speed maps -------------------------------------
# Julie Turner
# started 4 December 2023

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf', 
          'glmmTMB', 'ggplot2', 'viridis', 'tidyterra', 'patchwork')
lapply(libs, Require::Require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')


dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))

int.yr <- 2015
set.seed(53)

# prepping MB to be a random subset of data because too much to converge
juris <- 'mb'


dat.yr <- dat[int.year==int.yr]
indivs <- sample(unique(dat.yr[jurisdiction == juris]$id), 
                 ceiling(length(unique(dat.yr[jurisdiction == juris]$id))*0.80))
dat.sub<- dat.yr[!(id %in% indivs)]


# distribution parameters from data set up
targets::tar_load(distparams)
dtparams <- setDT(stack(distparams))

## Study area ----
canPoly <- vect(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))
studyArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))
ab.range <- vect(file.path(canada, 'AB_CaribouSubregionalPlanBoundaries', 'CARIBOU_SUB_REGIONAL_PLAN_BOUNDARIES_2022_07_04.shp'))
### study area prep ----
crs <- st_crs(3978)$wkt
ab.range <- project(ab.range, studyArea)
studyArea.ab <- union(studyArea, ab.range)


canPoly <- project(canPoly, crs)

wbi.prov <- subset(canPoly, canPoly$PREABBR %in% c('Alta.', 'B.C.', 'Man.', 'N.W.T.', 'Sask.', 'Y.T.'))


bc <- subset(canPoly, canPoly$PREABBR %in% c('B.C.'))
nwt <- subset(canPoly, canPoly$PREABBR %in% c('N.W.T.', 'Y.T.'))
sk <- subset(canPoly, canPoly$PREABBR %in% c('Sask.'))
mb <- subset(canPoly, canPoly$PREABBR %in% c('Man.'))



## Models ----
sel.2010 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2010-2015_HPC_noTA.RDS'))

sel.2015 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2015-2020_HPC_noTA.RDS'))

sel.bc <- readRDS(file.path(derived, 'mod_selmove_bc.RDS'))
sel.mb <- readRDS(file.path(derived, 'mod_selmove_mb_2015_50.RDS'))
sel.sk <- readRDS(file.path(derived, 'mod_selmove_sk.RDS'))
sel.nwt <- readRDS(file.path(derived, 'mod_selmove_nwt.RDS'))

# tables of model covariates
mod2010re.tab <- make_betas_tab(sel.2010)
mod2015re.tab <- make_betas_tab(sel.2015)

bc.tab <- make_betas_tab(sel.bc)
mb.tab <- make_betas_tab(sel.mb)
sk.tab <- make_betas_tab(sel.sk)
nwt.tab <- make_betas_tab(sel.nwt)


# calc mean distribution parameters from individuals
shape <- mean(dtparams[ind %like% 'shape']$values)
scale <- mean(dtparams[ind %like% 'scale']$values)
kappa <- mean(dtparams[ind %like% 'kappa']$values)


# load layers ----
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


year = 2015
linfeat_other <- rast(file.path('data', 'raw-data', 'ECCC_disturbance', 
                                paste0('WB_lfother_', year, '_distto.tif')))
disturb <- rast(file.path('data', 'raw-data', 'ECCC_disturbance', 
                          paste0('WB_disturb_other_', year, '.tif')))



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


land <- c(prop_veg, prop_needleleaf, prop_mixforest, prop_wets, log_tsf, log_tsh, log_distlf, 
          log_distlfother, disturb.ext)
names(land) <- c('prop_veg', 'prop_needleleaf', 'prop_mixforest', 'prop_wets', 'log_tsf', 'log_tsh', 'log_distlf', 
                 'log_distlfother', 'disturb')
land <- project(land, lf)

## 2015 ----
# speed calc
beta.2015 <- tidyr::pivot_wider(mod2015re.tab, names_from = term, values_from = estimate)
spd.2015 <- (shape + 
               beta.2015$`I(log(sl_ + 1))` + 
               beta.2015$`I(log(sl_ + 1)):I(cos(ta_))`*kappa +
               beta.2015$`I(log(sl_ + 1)):prop_needleleaf_start`*land$prop_needleleaf +
               beta.2015$`I(log(sl_ + 1)):prop_mixforest_start`*land$prop_mixforest +
               beta.2015$`I(log(sl_ + 1)):prop_veg_start`*land$prop_veg +
               beta.2015$`I(log(sl_ + 1)):prop_wets_start`*land$prop_wets)*(scale)
plot(spd.2015)

# speed m per hour
spd.ph.2015 <- spd.2015/13
plot(spd.ph.2015)

spd.ph.2015.wbi <- mask(spd.ph.2015, studyArea)
plot(spd.ph.2015.wbi)

# selection maps ----
pde.full.2015 <- rast(file.path(derived, 'pde2015_re.tif'))
pde.full.2015.sa.ab <- rast(file.path(derived, 'pde2015_re_ab.tif'))


### 2015 plots ----
p.sel.2015.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.full.2015), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('Habitat selection 2015-2020') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.sel.2015.wbi

p.spd.2015.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(spd.ph.2015.wbi), show.legend = T) +
  scale_fill_viridis(option = 'magma',na.value = NA) +
  ggtitle('Mean estimated speed 2015-2020') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Speed (m/hr)') +
  coord_sf(crs = 3978)
p.spd.2015.wbi

p.sel.2015.wbi + p.spd.2015.wbi

r.2015 <- c(pde.full.2015, spd.ph.2015.wbi)
cor.2015 <- focalPairs(r.2015, w=3, 'pearson', use='complete.obs')
plot(cor.2015)
cor(values(r.2015)[,1],
    values(r.2015)[,2],
    use = "na.or.complete")
# [1] 0.6357797


## BC plots ----
land.bc <- crop(land, bc)
# speed calc
beta.bc <- tidyr::pivot_wider(bc.tab, names_from = term, values_from = estimate)
spd.bc <- (shape + 
               beta.bc$`I(log(sl_ + 1))` + 
               beta.bc$`I(log(sl_ + 1)):I(cos(ta_))`*kappa +
               beta.bc$`I(log(sl_ + 1)):prop_needleleaf_start`*land.bc$prop_needleleaf +
               beta.bc$`I(log(sl_ + 1)):prop_mixforest_start`*land.bc$prop_mixforest +
               beta.bc$`I(log(sl_ + 1)):prop_veg_start`*land.bc$prop_veg +
               beta.bc$`I(log(sl_ + 1)):prop_wets_start`*land.bc$prop_wets +
               beta.bc$`I(log(sl_ + 1)):I(log(ts_fires_start + 1))`*land.bc$log_tsf +
               beta.bc$`I(log(sl_ + 1)):I(log(ts_harv_start + 1))`*land.bc$log_tsh +
               beta.bc$`I(log(sl_ + 1)):I(log(distlf_start + 1))`*land.bc$log_distlf +
               beta.bc$`I(log(sl_ + 1)):I(log(distlf_other_start + 1))`*land.bc$log_distlfother)*(scale)
plot(spd.bc)

# speed m per hour
spd.ph.bc <- spd.bc/13
plot(spd.ph.bc)

spd.ph.bc.sa <- mask(mask(spd.ph.bc, studyArea), bc)
plot(spd.ph.bc.sa)
spd.ph.bc.sa[values(spd.ph.bc.sa)<0] <- 0

#### selection maps ----
pde.bc <- rast(file.path(derived, 'pde_bc.tif'))

#### BC plots ----
p.bc <- ggplot(bc) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.bc), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('Habitat selection') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.bc


p.spd.bc <- ggplot(bc) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(spd.ph.bc.sa), show.legend = T) +
  scale_fill_viridis(option = 'magma',na.value = NA) +
  ggtitle('Mean estimated speed BC') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Speed (m/hr)') +
  coord_sf(crs = 3978)
p.spd.bc

p.bc + p.spd.bc

spd.ph.bc.sa.crop <- crop(spd.ph.bc.sa, pde.bc)
r.bc <- c(pde.bc, spd.ph.bc.sa.crop)
cor.bc <- focalPairs(r.bc, w=3, 'pearson', use='complete.obs')
plot(cor.bc)
cor(values(r.bc)[,1],
    values(r.bc)[,2],
    use = "na.or.complete")
# [1] 0.4235212

## MB plots ----
land.mb <- crop(land, mb)
# speed calc
beta.mb <- tidyr::pivot_wider(mb.tab, names_from = term, values_from = estimate)
spd.mb <- (shape + 
             beta.mb$`I(log(sl_ + 1))` + 
             beta.mb$`I(log(sl_ + 1)):I(cos(ta_))`*kappa +
             beta.mb$`I(log(sl_ + 1)):prop_needleleaf_start`*land.mb$prop_needleleaf +
             beta.mb$`I(log(sl_ + 1)):prop_mixforest_start`*land.mb$prop_mixforest +
             beta.mb$`I(log(sl_ + 1)):prop_veg_start`*land.mb$prop_veg +
             beta.mb$`I(log(sl_ + 1)):prop_wets_start`*land.mb$prop_wets +
             beta.mb$`I(log(sl_ + 1)):I(log(ts_fires_start + 1))`*land.mb$log_tsf +
             beta.mb$`I(log(sl_ + 1)):I(log(ts_harv_start + 1))`*land.mb$log_tsh +
             beta.mb$`I(log(sl_ + 1)):I(log(distlf_start + 1))`*land.mb$log_distlf +
             beta.mb$`I(log(sl_ + 1)):I(log(distlf_other_start + 1))`*land.mb$log_distlfother)*(scale)
plot(spd.mb)

# speed m per hour
spd.ph.mb <- spd.mb/13
plot(spd.ph.mb)

spd.ph.mb.sa <- mask(mask(spd.ph.mb, studyArea), mb)
plot(spd.ph.mb.sa)

#### selection maps ----
pde.mb <- rast(file.path(derived, 'pde_mb.tif'))

#### mb plots ----
p.mb <- ggplot(mb) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.mb), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('Habitat selection') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.mb


p.spd.mb <- ggplot(mb) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(spd.ph.mb.sa), show.legend = T) +
  scale_fill_viridis(option = 'magma',na.value = NA) +
  ggtitle('Mean estimated speed MB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Speed (m/hr)') +
  coord_sf(crs = 3978)
p.spd.mb

p.mb + p.spd.mb

spd.ph.mb.sa.crop <- crop(spd.ph.mb.sa, pde.mb)
r.mb <- c(pde.mb, spd.ph.mb.sa.crop)
cor.mb <- focalPairs(r.mb, w=3, 'pearson', use='complete.obs')
plot(cor.mb)
cor(values(r.mb)[,1],
    values(r.mb)[,2],
    use = "na.or.complete")
# [1] 0.4142408


## SK plots ----
land.sk <- crop(land, sk)
# speed calc
beta.sk <- tidyr::pivot_wider(sk.tab, names_from = term, values_from = estimate)
spd.sk <- (shape + 
             beta.sk$`I(log(sl_ + 1))` + 
             beta.sk$`I(log(sl_ + 1)):I(cos(ta_))`*kappa +
             beta.sk$`I(log(sl_ + 1)):prop_needleleaf_start`*land.sk$prop_needleleaf +
             beta.sk$`I(log(sl_ + 1)):prop_mixforest_start`*land.sk$prop_mixforest +
             beta.sk$`I(log(sl_ + 1)):prop_veg_start`*land.sk$prop_veg +
             beta.sk$`I(log(sl_ + 1)):prop_wets_start`*land.sk$prop_wets +
             beta.sk$`I(log(sl_ + 1)):I(log(ts_fires_start + 1))`*land.sk$log_tsf +
             beta.sk$`I(log(sl_ + 1)):I(log(ts_harv_start + 1))`*land.sk$log_tsh +
             beta.sk$`I(log(sl_ + 1)):I(log(distlf_start + 1))`*land.sk$log_distlf +
             beta.sk$`I(log(sl_ + 1)):I(log(distlf_other_start + 1))`*land.sk$log_distlfother)*(scale)
plot(spd.sk)

# speed m per hour
spd.ph.sk <- spd.sk/13
plot(spd.ph.sk)

spd.ph.sk.sa <- mask(mask(spd.ph.sk, studyArea), sk)
plot(spd.ph.sk.sa)

spd.ph.sk.sa[values(spd.ph.sk.sa)<0] <- 0
#### selection maps ----
pde.sk <- rast(file.path(derived, 'pde_sk.tif'))

#### sk plots ----
p.sk <- ggplot(sk) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.sk), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('Habitat selection') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.sk

p.spd.sk <- ggplot(sk) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(spd.ph.sk.sa), show.legend = T) +
  scale_fill_viridis(option = 'magma',na.value = NA) +
  ggtitle('Mean estimated speed sk') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Speed (m/hr)') +
  coord_sf(crs = 3978)
p.spd.sk

p.sk + p.spd.sk

spd.ph.sk.sa.crop <- crop(spd.ph.sk.sa, pde.sk)
r.sk <- c(pde.sk, spd.ph.sk.sa.crop)
cor.sk <- focalPairs(r.sk, w=3, 'pearson', use='complete.obs')
plot(cor.sk)
cor(values(r.sk)[,1],
    values(r.sk)[,2],
    use = "na.or.complete")
# [1] 0.5704895



## NWT plots ----
land.nwt <- crop(land, nwt)
# speed calc
beta.nwt <- tidyr::pivot_wider(nwt.tab, names_from = term, values_from = estimate)
spd.nwt <- (shape + 
             beta.nwt$`I(log(sl_ + 1))` + 
             beta.nwt$`I(log(sl_ + 1)):I(cos(ta_))`*kappa +
             beta.nwt$`I(log(sl_ + 1)):prop_needleleaf_start`*land.nwt$prop_needleleaf +
             beta.nwt$`I(log(sl_ + 1)):prop_mixforest_start`*land.nwt$prop_mixforest +
             beta.nwt$`I(log(sl_ + 1)):prop_veg_start`*land.nwt$prop_veg +
             beta.nwt$`I(log(sl_ + 1)):prop_wets_start`*land.nwt$prop_wets +
             beta.nwt$`I(log(sl_ + 1)):I(log(ts_fires_start + 1))`*land.nwt$log_tsf +
             beta.nwt$`I(log(sl_ + 1)):I(log(ts_harv_start + 1))`*land.nwt$log_tsh +
             beta.nwt$`I(log(sl_ + 1)):I(log(distlf_start + 1))`*land.nwt$log_distlf +
             beta.nwt$`I(log(sl_ + 1)):I(log(distlf_other_start + 1))`*land.nwt$log_distlfother)*(scale)
plot(spd.nwt)

# speed m per hour
spd.ph.nwt <- spd.nwt/13
plot(spd.ph.nwt)

spd.ph.nwt.sa <- mask(mask(spd.ph.nwt, studyArea), nwt)
plot(spd.ph.nwt.sa)

#### selection maps ----
pde.nwt <- rast(file.path(derived, 'pde_nwt.tif'))

#### nwt plots ----
p.nwt <- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.nwt), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('Habitat selection') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.nwt


p.spd.nwt <- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(spd.ph.nwt.sa), show.legend = T) +
  scale_fill_viridis(option = 'magma',na.value = NA) +
  ggtitle('Mean estimated speed nwt') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Speed (m/hr)') +
  coord_sf(crs = 3978)
p.spd.nwt

p.nwt + p.spd.nwt

spd.ph.nwt.sa.crop <- crop(spd.ph.nwt.sa, pde.nwt)
r.nwt <- c(pde.nwt, spd.ph.nwt.sa.crop)
cor.nwt <- focalPairs(r.nwt, w=3, 'pearson', use='complete.obs')
plot(cor.nwt)
cor(values(r.nwt)[,1],
    values(r.nwt)[,2],
    use = "na.or.complete")
# [1] 0.741875


### Juris plot ----
p.juris <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.bc), show.legend = T) +
  geom_spatraster(data = as.numeric(pde.nwt), show.legend = F) +
  geom_spatraster(data = as.numeric(pde.mb), show.legend = F) +
  geom_spatraster(data = as.numeric(pde.sk), show.legend = F) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('Habitat Selection - Jurisdictional models') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.juris

p.spd.juris <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(spd.ph.bc.sa), show.legend = T) +
  geom_spatraster(data = as.numeric(spd.ph.nwt.sa), show.legend = F) +
  geom_spatraster(data = as.numeric(spd.ph.mb.sa), show.legend = F) +
  geom_spatraster(data = as.numeric(spd.ph.sk.sa), show.legend = F) +
  scale_fill_viridis(option = 'magma',na.value = NA) +
  ggtitle('Mean Estimated Speed - Jurisdictional models') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Speed (m/hr)') +
  coord_sf(crs = 3978)
p.spd.juris
