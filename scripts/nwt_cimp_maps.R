# === Predicted use maps NWT -------------------------------------
# Julie Turner
# started 31 March 2023

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf', 
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
issaArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIiSSAdat_10kmBuff.shp'))
ab.range <- vect(file.path(canada, 'AB_CaribouSubregionalPlanBoundaries', 'CARIBOU_SUB_REGIONAL_PLAN_BOUNDARIES_2022_07_04.shp'))
dus <- vect(file.path(raw, 'Johnsonetal2020_studyareas', 'Enhanced_MetaHerds_20191029.shp'))
dus.proj <- project(dus, issaArea)
wbi.dus <- subset(dus.proj, dus.proj$PROV_TERR %in% c('BC', 'MB', 'NWT', 'SK') 
                  & !(dus.proj$HERD %in% c('Smoothstone')))

wbi.sa <- union(wbi.dus, issaArea)
studyArea <- aggregate(wbi.sa)

ext <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))

# load layers
disturbyr = 2015
linfeat_other <- rast(file.path('data', 'raw-data', 'ECCC_disturbance', 
                                paste0('WB_lfother_', disturbyr, '_distto.tif')))
disturb <- rast(file.path('data', 'derived-data', 'NWT_proj', 
                          'bufferedAnthDist_500m_2041.tif'))


landyr = 2019
bryoids <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                          paste0('bryoids_500', '.tif')))

shrub <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                        paste0('shrub_500', '.tif')))

wet <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                      paste0('wet_500', '.tif')))

wettreed <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                           paste0('wet_treed_500', '.tif')))

herbs <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                        paste0('herbs_500', '.tif')))

needleleaf<- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                            paste0('needleleaf_500', '.tif')))

deciduous <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                            paste0('deciduous_500', '.tif')))

mixed <- rast(file.path('data', 'raw-data','prop_land', landyr, '500grid', 
                        paste0('mixed_500', '.tif')))


prop_needleleaf <- needleleaf
prop_mixforest <- deciduous + mixed + wettreed
prop_veg <- shrub + bryoids + herbs
prop_wets <- wet


fires <- rast(file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', paste0('fires_', (disturbyr+5), '.tif')))

lf.full <- rast(file.path('data', 'derived-data', 'distto_roadrail_500.tif'))
lf <- crop(lf.full, ext)
harv <- rast(file.path('data', 'raw-data', 'WB_harv_1985-2020.tif'))

lf_other <- resample(linfeat_other, lf, method = 'average')
lf_other.ext <- extend(lf_other, ext(lf))
disturb.proj <- project(disturb, lf)
disturb.proj <- resample(disturb.proj, lf, method = 'max')
disturb.ext <- extend(disturb.proj, ext(lf))
harv <- resample(harv, lf, method = 'max')
harv.ext <- extend(harv, ext(lf))
tsh <- (disturbyr + 5) - harv.ext
tsh[is.na(tsh)] <- 40


fires.crop <- resample(fires, lf, method = 'max')
#names(land.brick) <- c("lf_dist", "lc")
tsf <- (disturbyr + 5) - fires.crop
tsf[is.na(tsf)] <- 40

log_tsf <- log(tsf + 1)
log_tsh <- log(tsh + 1)
log_distlf <- log(lf + 1)
log_distlfother <- log(lf_other.ext + 1)



land <- c(prop_veg, prop_needleleaf, prop_mixforest, prop_wets, log_tsf, log_tsh, log_distlf, 
          log_distlfother, disturb.ext)
names(land) <- c('prop_veg', 'prop_needleleaf', 'prop_mixforest', 'prop_wets', 'log_tsf', 'log_tsh', 'log_distlf', 
                 'log_distlfother', 'disturb')


## Models ----

sel.2010 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2010-2015_HPC_noTA.RDS'))

sel.2015 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2015-2020_HPC_noTA.RDS'))

sel.nwt <- readRDS(file.path(derived, 'mod_selmove_nwt.RDS'))

### PDE ----
mod2010.tab <- make_betas_tab(sel.2010)

mod2015.tab <- make_betas_tab(sel.2015)

modNWT.tab <- make_betas_tab(sel.nwt)

gc()
### START -----
canPoly <- project(canPoly, lf)

wbi.prov <- subset(canPoly, canPoly$PREABBR %in% c('Alta.', 'B.C.', 'Man.', 'N.W.T.', 'Sask.', 'Y.T.'))

nwt <- subset(canPoly, canPoly$PREABBR %in% c('N.W.T.'))


nwt.land <- crop(land, ext(nwt))

make_pde <- function(mod.tab, land, saveName = NULL){
  lf.cov<- (2*as.double(mod.tab[term %like% 'distlf_end', 
                                .(estimate)])*land$log_distlf)
  lfother.cov<- (2*as.double(mod.tab[term %like% 'distlf_other_end', 
                                     .(estimate)])*land$log_distlfother)
  tsf.cov<- (2*as.double(mod.tab[term %like% 'ts_fires_end', 
                                 .(estimate)])*land$log_tsf)
  tsh.cov<- (2*as.double(mod.tab[term %like% 'ts_harv_end', 
                                 .(estimate)])*land$log_tsh)
  needleleaf.cov <- (2*as.double(mod.tab[term %like% 'needleleaf_end', 
                                         .(estimate)])*land$prop_needleleaf)
  veg.cov <- (2*as.double(mod.tab[term %like% 'veg_end', 
                                  .(estimate)])*land$prop_veg)
  mixforest.cov <- (2*as.double(mod.tab[term %like% 'mixforest_end',
                                        .(estimate)])*land$prop_mixforest)
  wets.cov <- (2*as.double(mod.tab[term %like% 'wets_end', 
                                   .(estimate)])*land$prop_wets)
  disturb.cov <- (2*as.double(mod.tab[term %like% 'disturbance_end', 
                                      .(estimate)])*land$disturb)
  
  numerator <- exp(lf.cov + lfother.cov + 
                     tsf.cov + tsh.cov + 
                     needleleaf.cov + veg.cov + 
                     mixforest.cov + 
                     wets.cov +
                     disturb.cov)
  
  
  # the normalizing constant.
  C <- global(numerator, sum, na.rm = T)
  pde <- numerator/C[[1]]
  
  if(!is.null(saveName)){
    writeRaster(pde, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde)
}



make_pde_map <- function(pde, sArea, saveName = NULL){
  pde.sa <- crop(pde, sArea, mask = T)
  plot(pde.sa)
  
  breaks <- global(pde.sa, quantile, na.rm = T, probs = seq(0,1,.1))
  v.breaks <- unname(breaks)
  t.breaks <- as.vector(t(v.breaks))
  pde.discrete <- classify(pde.sa, t.breaks, include.lowest=TRUE, brackets=TRUE)
  
  if(!is.null(saveName)){
    writeRaster(pde.discrete, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde.discrete)
}


### 2010 ----

pde.2010 <- make_pde(mod2010.tab, land)

pde.2010.sa <- make_pde_map(pde.2010, studyArea, saveName = 'pde2010_re.tif')
plot(pde.2010.sa, breaks=0:10)


## with ab guestimate
pde.2010.sa.ab <- make_pde_map(pde.2010, studyArea.ab, saveName = 'pde2010_re_ab.tif')
plot(pde.2010.sa.ab, breaks=0:10)

### 2015 ----

pde.2015 <- make_pde(mod2015.tab, land)

pde.2015.sa <- make_pde_map(pde.2015, studyArea, saveName = 'pde2015_re.tif')
plot(pde.2015.sa, breaks=0:10)


## with ab guestimate
pde.2015.sa.ab <- make_pde_map(pde.2015, studyArea.ab, saveName = 'pde2015_re_ab.tif')
plot(pde.2015.sa.ab, breaks=0:10)

#### full model PDE 2041 ----

pde.global.2041 <- make_pde(mod2015.tab, nwt.land)

nwt.sa <- crop(studyArea, nwt)
pde.2041.sa <- make_pde_map(pde.global.2041, nwt.sa, saveName = 'pde2041_re_nwtcrop.tif')
plot(pde.2041.sa, breaks=0:10)

## Jurisidiction model pdes ----
### NWT + YT ----
pde.nwt <- make_pde(nwt.tab, land)
#nwt.sa <- crop(studyArea, nwt)
pde.nwt.sa <- make_pde_map(pde.nwt, nwt.sa, saveName = 'pde_nwt.tif')

#### NWT model PDE 2041 ----
pde.nwt.2041 <- make_pde(modNWT.tab, nwt.land)
pde.2041.nwt <- make_pde_map(pde.nwt.2041, nwt.sa, saveName = 'pde_nwt_2041.tif')
plot(pde.2041.nwt, breaks=0:10)

# plots ----
pde.re.discrete.2010 <- rast(file.path(derived, 'pde2010_re.tif'))
pde.re.discrete.2015 <- rast(file.path(derived, 'pde2015_re.tif'))
pde.nwt <- rast(file.path(derived, 'pde_nwt.tif'))

nwt <- subset(canPoly, canPoly$PREABBR %in% c('N.W.T.'))
nwt.2010 <- crop(pde.re.discrete.2010, nwt, mask = T)
nwt.2015 <- crop(pde.re.discrete.2015, nwt, mask = T)
# writeRaster(nwt.2010, file.path(derived, 'nwt2010_re.tif'))
# writeRaster(nwt.2015, file.path(derived, 'nwt2015_re.tif'))


### NWT diff 2010-2015 based on global model ----
p.nwt.2010 <- 
  ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(nwt.2010), show.legend = F) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)


p.nwt.2015 <-  ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(nwt.2015), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

nwt.diff <- nwt.2015 - nwt.2010

p.nwt.diff <- 
  ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(nwt.diff), show.legend = T) +
  scale_fill_gradientn(colours = magma(10),na.value = NA) +
  ggtitle('Change over time') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Change in selection') +
  coord_sf(crs = 3978)
p.nwt.diff

p.nwt.2010 + p.nwt.2015 + p.nwt.diff

### 2041 forecast ----

pde.2041.sa.crop <- extend(crop(pde.2041.sa, nwt.2015), nwt.2015)
glob.nwt.forecast.diff <- nwt.2015 - pde.2041.sa.crop

p.nwt.2041 <- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.2041.sa.crop), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('Projected 2041 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.nwt.2041


p.nwt.forecast.diff <- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(glob.nwt.forecast.diff), show.legend = T) +
  scale_fill_gradientn(colours = magma(10),na.value = NA) +
  ggtitle('Difference 2015 - 2041') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.nwt.forecast.diff

p.nwt.2015 + p.nwt.2041 + p.nwt.forecast.diff

### NWT juris model plot ----
p.nwt <- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.nwt), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('NWT jurisdictional model 2015-2020') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.nwt

### Global WBI plot ----
p.wbi.2010 <- 
  ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2010), show.legend = F) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)


p.wbi.2015 <-  ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2015), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

wbi.diff <- pde.re.discrete.2015 - pde.re.discrete.2010

p.wbi.diff <- 
  ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(wbi.diff), show.legend = T) +
  scale_fill_gradientn(colours = magma(10),na.value = NA) +
  ggtitle('Change over time') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Change in selection') +
  coord_sf(crs = 3978)
p.wbi.diff

p.wbi.2010 + p.wbi.2015 + p.wbi.diff

