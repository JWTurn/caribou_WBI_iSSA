# === Predicted use maps -------------------------------------
# Julie Turner
# started 14 April 2022

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf',
          'glmmTMB', 'ggplot2', 'rasterVis', 'viridis', 'RColorBrewer',
          'tidyterra', 'patchwork')
lapply(libs, Require::Require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
#canada <- file.path('C:','Users', 'julie', 'Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')


## Extract values ----
canPoly <- vect(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))
# # make study area of available points
# datSteps <- readRDS(file.path(derived, 'dat_iSSA.RDS'))
# coords<- datSteps%>%st_as_sf(coords = c('x2_','y2_'))%>%
#   st_set_crs(st_crs(3978))
# ## 10 km buffer around points to get an idea of extent of study area ----
# gc()
# studyArea <- st_buffer(coords, dist = 10000)
# gc()
# sa.union <- st_union(studyArea)
# ## save buffered study area ----
# st_write(sa.union, file.path('data', 'derived-data', 'prepped-data', 'WBIiSSAdat_10kmBuff.shp'), append = F)
ext <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))
issaArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIiSSAdat_10kmBuff.shp'))
ab.range <- vect(file.path(canada, 'AB_CaribouSubregionalPlanBoundaries', 'CARIBOU_SUB_REGIONAL_PLAN_BOUNDARIES_2022_07_04.shp'))
dus <- vect(file.path(raw, 'juris_herds', 'wbi_herds.shp'))
wbi.dus.data <- subset(dus, !(dus$herd %in% c('Sahtu', 'Bear River', 'Cameron Hills')))
plot(wbi.dus.data)

wbi.sa <- union(wbi.dus.data, issaArea)
studyArea <- aggregate(wbi.sa, disolve = T)

## prep study areas for sims
studyArea.buff <- buffer(studyArea, 50000)
#writeVector(studyArea, file.path(derived, 'studyArea_4maps.shp'))
#studyArea <- issaArea

studyArea.sf <- st_as_sf(studyArea.buff)
studyArea.ch <- st_convex_hull(studyArea.sf)
st_write(studyArea.ch, file.path(derived, 'studyArea_4sims.shp'))
studyArea.split <- st_cast(studyArea.sf, 'POLYGON')
mbsk <- studyArea.split$geometry[1]
st_write(mbsk, file.path(derived, 'studyArea_mbsk_4sims.shp'))
bcnwt <- studyArea.split$geometry[2]
st_write(bcnwt, file.path(derived, 'studyArea_bcnwt_4sims.shp'))
nnwt <- studyArea.split$geometry[3]
st_write(nnwt, file.path(derived, 'studyArea_nnwt_4sims.shp'))

### Jurisidictional boundaries -----
gc()

canPoly <- project(canPoly, studyArea)

wbi.prov.full <- subset(canPoly, canPoly$PREABBR %in% c('Alta.', 'B.C.', 'Man.', 'N.W.T.', 'Sask.', 'Y.T.'))
wbi.prov <- crop(wbi.prov.full, ext(ext)) 

wbi.dus <- crop(wbi.dus.data, ext(ext))

bc <- subset(canPoly, canPoly$PREABBR %in% c('B.C.'))
nwt <- subset(canPoly, canPoly$PREABBR %in% c('N.W.T.', 'Y.T.'))
sk <- subset(canPoly, canPoly$PREABBR %in% c('Sask.'))
mb <- subset(canPoly, canPoly$PREABBR %in% c('Man.'))


# load layers
land <- load_map_layers(landyr = 2019, disturbyr = 2015, ts_else = 40)
gc()
land2010 <- load_map_layers(landyr = 2015, disturbyr = 2010, ts_else = 40)
gc()


ab.range <- project(ab.range, studyArea)
studyArea.ab <- union(studyArea, ab.range)


## Models ----
gc()
sel.2010 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2010-2015_HPC_noTA.RDS'))

sel.2015 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2015-2020_HPC_noTA.RDS'))

sel.bc <- readRDS(file.path(derived, 'mod_selmove_bc.RDS'))
sel.mb <- readRDS(file.path(derived, 'mod_selmove_mb_2015_50.RDS'))
sel.sk <- readRDS(file.path(derived, 'mod_selmove_sk.RDS'))
sel.nwt <- readRDS(file.path(derived, 'mod_selmove_nwt.RDS'))


### PDE ----
gc()
mod2010re.tab <- make_betas_tab(sel.2010)

#View(summary(sel.2015)$coef$cond)
mod2015re.tab <- make_betas_tab(sel.2015)

gc()
bc.tab <- make_betas_tab(sel.bc)
mb.tab <- make_betas_tab(sel.mb)
sk.tab <- make_betas_tab(sel.sk)
nwt.tab <- make_betas_tab(sel.nwt)





#### full model PDE functions ----
gc()

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

pde.2010 <- make_pde(mod2010re.tab, land2010)

pde.2010.sa <- make_pde_map(pde.2010, studyArea, saveName = 'pde2010_re.tif')
plot(pde.2010.sa, breaks=0:10)


## with ab guestimate
pde.2010.sa.ab <- make_pde_map(pde.2010, studyArea.ab, saveName = 'pde2010_re_ab.tif')
plot(pde.2010.sa.ab, breaks=0:10)

### 2015 ----

pde.2015 <- make_pde(mod2015re.tab, land)

pde.2015.sa <- make_pde_map(pde.2015, studyArea, saveName = 'pde2015_re.tif')
plot(pde.2015.sa, breaks=0:10)


## with ab guestimate
pde.2015.sa.ab <- make_pde_map(pde.2015, studyArea.ab, saveName = 'pde2015_re_ab.tif')
plot(pde.2015.sa.ab, breaks=0:10)


## Jurisidiction model pdes ----
### BC ----
pde.bc <- make_pde(bc.tab, land)
bc.sa <- crop(studyArea, bc)
pde.bc.sa <- make_pde_map(pde.bc, bc.sa, saveName = 'pde_bc.tif')

### MB ----
pde.mb <- make_pde(mb.tab, land)
mb.sa <- crop(studyArea, mb)
pde.mb.sa <- make_pde_map(pde.mb, mb.sa, saveName = 'pde_mb.tif')

### SK ----
pde.sk <- make_pde(sk.tab, land)
sk.sa <- crop(studyArea, sk)
pde.sk.sa <- make_pde_map(pde.sk, sk.sa, saveName = 'pde_sk.tif')

### NWT + YT ----
pde.nwt <- make_pde(nwt.tab, land)
nwt.sa <- crop(studyArea, nwt)
pde.nwt.sa <- make_pde_map(pde.nwt, nwt.sa, saveName = 'pde_nwt.tif')

# plots ----
pde.re.discrete.2010 <- rast(file.path(derived, 'pde2010_re.tif'))
pde.re.discrete.2010.sa.ab <- rast(file.path(derived, 'pde2010_re_ab.tif'))

pde.re.discrete.2015 <- rast(file.path(derived, 'pde2015_re.tif'))
pde.re.discrete.2015.sa.ab <- rast(file.path(derived, 'pde2015_re_ab.tif'))

pde.bc.sa <- rast(file.path(derived, 'pde_bc.tif'))
pde.mb.sa <- rast(file.path(derived, 'pde_mb.tif'))
pde.nwt.sa <- rast(file.path(derived, 'pde_nwt.tif'))
pde.sk.sa <- rast(file.path(derived, 'pde_sk.tif'))

### 2010 plots ----
p.re.2010.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2010), show.legend = T) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2010.wbi



### 2015 plots ----
p.re.2015.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2015), show.legend = T) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2015.wbi

### Juris plot ----
p.juris <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.bc.sa), show.legend = T) +
  geom_spatraster(data = as.numeric(pde.nwt.sa), show.legend = F) +
  geom_spatraster(data = as.numeric(pde.mb.sa), show.legend = F) +
  geom_spatraster(data = as.numeric(pde.sk.sa), show.legend = F) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('Jurisdictional models') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.juris


### Diff plot ----
pde.juris <- mosaic(pde.bc.sa, pde.mb.sa, pde.nwt.sa, pde.sk.sa)

pde.mod.diff <- pde.re.discrete.2015 - pde.juris
range(values(pde.mod.diff), na.rm = T)

p.mod.diff <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.mod.diff), show.legend = T) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  scale_fill_distiller(type = 'div', palette = 'PRGn', na.value = NA) +
  ggtitle('Global 2015 - Jurisdiction models') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Difference in selection') +
  coord_sf(crs = 3978)
p.mod.diff

p.re.2015.wbi + p.juris + p.mod.diff
#################
### BC plot ----
p.bc <- ggplot(bc) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.bc.sa), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('BC') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.bc

pde.bc.global <- crop(crop(pde.re.discrete.2015, pde.bc.sa), bc)
p.bc.global <- ggplot(bc) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.bc.global), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('BC') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.bc.global

### MB plot ----
p.mb <- ggplot(mb) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.mb.sa), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('mb') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.mb

### SK plot ----
p.sk <- ggplot(sk) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.sk.sa), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('sk') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.sk

### NWT plot ----
p.nwt <- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.nwt.sa), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('nwt') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.nwt


#######
##### extrapolate ab ----
p.re.2010.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2010.sa.ab), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2010.wbi.ab

p.re.2010.wbi + p.re.2010.wbi.ab

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

p.re.2015.wbi + p.re.2015.wbi.ab
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

