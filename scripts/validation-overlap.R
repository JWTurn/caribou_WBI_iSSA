# === Predicted and used overlap -------------------------------------
# Julie Turner
# started 14 February 2024

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf',
          'glmmTMB', 'ggplot2', 'viridis', 'patchwork')
lapply(libs, Require::Require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')
#canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')

# scale raster 0-1
raster01 = function(r){
  
  # get the min max values
  minmax_r = range(values(r), na.rm=TRUE) 
  
  # rescale 
  return( (r-minmax_r[1]) / (diff(minmax_r)))
}

## load data ----
dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))
dat.obs <- dat[case_==TRUE]
issaArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIiSSAdat_10kmBuff.shp'))

int.yr <- 2015
set.seed(53)

# Global models -----
# prepping MB to be a random subset of data because too much to converge
juris <- 'mb'


dat.yr <- dat[int.year==int.yr]
indivs <- sample(unique(dat.yr[jurisdiction == juris]$id), 
                 ceiling(length(unique(dat.yr[jurisdiction == juris]$id))*0.80))
dat.sub<- dat.yr[!(id %in% indivs)]



make_study_area <- function(dat, crs, buff){
  coords<- dat%>%st_as_sf(coords = c('x2_','y2_'))%>%
    st_set_crs(crs)
  ## buffer around points to get an idea of extent of study area 
  print('buffering')
  studyArea <- st_buffer(coords, dist = buff)
  print('union-ing')
  sa.union <- st_union(studyArea)
  return(sa.union)
}

make_prop_use <- function(dat, crs, trast){
  dat.sub <- dat[case_==TRUE]
  dat.coords<- vect(cbind(dat.sub$x1_, dat.sub$y1_), crs =crs, type = 'points')
  res <- res(trast)[[1]]
  rast<- rasterize(dat.coords, trast, fun = 'sum')/(res*res)
  return(rast)
}

make_map_bins <- function(map){
  breaks <- global(map, quantile, na.rm = T, probs = seq(0,1,.1))
  v.breaks <- unname(breaks)
  t.breaks <- as.vector(t(v.breaks))
  binned <- as.numeric(classify(map, t.breaks, include.lowest=TRUE, brackets=TRUE))
  return(binned)
}


pts_in_bins <- function(dat, bin.ud){
  DT <- dat[id!='' & case_ == TRUE]
  DT[, use.bin:= terra::extract(bin.ud, cbind(.SD))[,-1],
     .SDcols = c('x1_','y1_')]
  npts <- DT[,.(npts = .N), by= .(id)]
  mrg <- merge(DT, npts, by = 'id')
  propptsbin <- mrg[,.(prop.pts = .N/npts), by = .(use.bin, id)]
  return(propptsbin)
}


pts_in_bins_tot <- function(dat, bin.ud){
  DT <- dat[id!='' & case_ == TRUE]
  DT[, use.bin:= terra::extract(bin.ud, cbind(.SD))[,-1],
     .SDcols = c('x1_','y1_')]
  npts <- DT[,.(npts = .N)][[1]]
  propptsbin.tot <- DT[,.(prop.pts = .N/npts), by = .(use.bin)]
  return(propptsbin.tot)
}



crs <- st_crs(3978)$wkt
buffer <- 10000

gc()
sArea.2015 <- make_study_area(dat.sub, crs, buff = buffer)



# Test data ----
gc()
## individuals ----
indivs.train <- sample(unique(dat.sub$id), 
                       ceiling(length(unique(dat.sub$id))*0.80))



dat.test<- dat.sub[!(id %in% indivs.train)]
dat.test[,indiv_step_id := as.factor(indiv_step_id)]
dat.test[,jurisdiction := as.factor(jurisdiction)]
dat.test[,year:=as.factor(year)]
dat.test[,pop := as.factor(pop)]
dat.test[,id := as.factor(id)]

gc()
test.sArea.2015 <- make_study_area(dat.test, crs, buff = buffer)

## 2010 ----
dat.2010 <- dat[int.year==2010]
indivs.2010 <- sample(unique(dat.2010[jurisdiction == juris]$id), 
                      ceiling(length(unique(dat.2010[jurisdiction == juris]$id))*0.80))
test.2010 <- dat.2010[!(id %in% indivs.2010)]
test.2010[,indiv_step_id := as.factor(indiv_step_id)]
test.2010[,jurisdiction := as.factor(jurisdiction)]
test.2010[,year:=as.factor(year)]
test.2010[,pop := as.factor(pop)]
test.2010[,id := as.factor(id)]

gc()
test.sArea.2010 <- make_study_area(dat.2010, crs, buff = buffer)

## Jurisdictions -----
### BC ----

bc.test <- dat.sub[jurisdiction=='bc']
bc.test[,indiv_step_id := as.factor(indiv_step_id)]
bc.test[,jurisdiction := as.factor(jurisdiction)]
bc.test[,year:=as.factor(year)]
bc.test[,pop := as.factor(pop)]
bc.test[,id := as.factor(id)]

gc()
test.sArea.bc <- make_study_area(bc.test, crs, buff = buffer)

### MB ----

mb.test <- dat.sub[jurisdiction=='mb']
mb.test[,indiv_step_id := as.factor(indiv_step_id)]
mb.test[,jurisdiction := as.factor(jurisdiction)]
mb.test[,year:=as.factor(year)]
mb.test[,pop := as.factor(pop)]
mb.test[,id := as.factor(id)]

gc()
test.sArea.mb <- make_study_area(mb.test, crs, buff = buffer)
### SK ----

sk.test <- dat.sub[jurisdiction=='sk']
sk.test[,indiv_step_id := as.factor(indiv_step_id)]
sk.test[,jurisdiction := as.factor(jurisdiction)]
sk.test[,year:=as.factor(year)]
sk.test[,pop := as.factor(pop)]
sk.test[,id := as.factor(id)]

gc()
test.sArea.sk <- make_study_area(sk.test, crs, buff = buffer)
### NWT ----

nwt.test <- dat.sub[jurisdiction=='nwt']
nwt.test[,indiv_step_id := as.factor(indiv_step_id)]
nwt.test[,jurisdiction := as.factor(jurisdiction)]
nwt.test[,year:=as.factor(year)]
nwt.test[,pop := as.factor(pop)]
nwt.test[,id := as.factor(id)]

gc()
test.sArea.nwt <- make_study_area(nwt.test, crs, buff = buffer)
# predicted use plots ----
gc()

## Global model ----
land2015 <- load_map_layers(landyr = 2019, disturbyr = 2015, ts_else = 40)

### global 2015 ----
modtrain2015 <- file.path(derived, 'mods_train', 'mod_train_selmove_2015-2020_HPC.RDS')

gc()
test2015.ud <- mod2UD(modpath = modtrain2015, envlayers = land2015, 
                      studyArea = vect(test.sArea.2015))
plot(test2015.ud)


gc()
test2015.prop <- make_prop_use(dat.test, crs, test2015.ud)


gc()
used.bin.2015 <- make_map_bins(test2015.prop)


gc()
propptsbin.2015 <- pts_in_bins(dat.test, test2015.ud)
plot(propptsbin.2015$use.bin, propptsbin.2015$prop.pts)
cor(propptsbin.2015$use.bin, propptsbin.2015$prop.pts, method = 'spearman')

layerCor(c(test2015.ud, used.bin.2015), fun = 'pearson', na.rm = T)

propptsbin.2015.tot <- pts_in_bins_tot(dat.test, test2015.ud)

plot(propptsbin.2015.tot$use.bin, propptsbin.2015.tot$prop.pts)
cor(propptsbin.2015.tot$use.bin, propptsbin.2015.tot$prop.pts, method = 'spearman')

### BC ----
modtrainbc <- file.path(derived, 'mods_train', 'mod_bc_train_selmove_2015-2020_HPC.RDS')

gc()
testbc.ud <- mod2UD(modpath = modtrainbc, envlayers = land2015, 
                      studyArea = vect(test.sArea.bc))
plot(testbc.ud)

gc()
testbc.prop <- make_prop_use(bc.test, crs, testbc.ud)

gc()
used.bin.bc <- make_map_bins(testbc.prop)

gc()
propptsbin.bc <- pts_in_bins(bc.test, testbc.ud)
plot(propptsbin.bc$use.bin, propptsbin.bc$prop.pts)
cor(propptsbin.bc$use.bin, propptsbin.bc$prop.pts, method = 'spearman')

layerCor(c(test2015.ud, used.bin.2015), fun = 'pearson', na.rm = T)

propptsbin.bc.tot <- pts_in_bins_tot(bc.test, testbc.ud)

plot(propptsbin.bc.tot$use.bin, propptsbin.bc.tot$prop.pts)
cor(propptsbin.bc.tot$use.bin, propptsbin.bc.tot$prop.pts, method = 'spearman')


### MB ----
modtrainmb <- file.path(derived, 'mods_train', 'mod_mb_train_selmove_2015-2020_HPC.RDS')

gc()
testmb.ud <- mod2UD(modpath = modtrainmb, envlayers = land2015, 
                    studyArea = vect(test.sArea.mb))
plot(testmb.ud)

gc()
testmb.prop <- make_prop_use(mb.test, crs, testmb.ud)

gc()
used.bin.mb <- make_map_bins(testmb.prop)

gc()
propptsbin.mb <- pts_in_bins(mb.test, testmb.ud)
plot(propptsbin.mb$use.bin, propptsbin.mb$prop.pts)
cor(propptsbin.mb$use.bin, propptsbin.mb$prop.pts, method = 'spearman')

layerCor(c(test2015.ud, used.bin.2015), fun = 'pearson', na.rm = T)

propptsbin.mb.tot <- pts_in_bins_tot(mb.test, testmb.ud)

plot(propptsbin.mb.tot$use.bin, propptsbin.mb.tot$prop.pts)
cor(propptsbin.mb.tot$use.bin, propptsbin.mb.tot$prop.pts, method = 'spearman')


### NWT ----
modtrainnwt <- file.path(derived, 'mods_train', 'mod_nwt_train_selmove_2015-2020_HPC.RDS')

gc()
testnwt.ud <- mod2UD(modpath = modtrainnwt, envlayers = land2015, 
                    studyArea = vect(test.sArea.nwt))
plot(testnwt.ud)

gc()
testnwt.prop <- make_prop_use(nwt.test, crs, testnwt.ud)

gc()
used.bin.nwt <- make_map_bins(testnwt.prop)

gc()
propptsbin.nwt <- pts_in_bins(nwt.test, testnwt.ud)
plot(propptsbin.nwt$use.bin, propptsbin.nwt$prop.pts)
cor(propptsbin.nwt$use.bin, propptsbin.nwt$prop.pts, method = 'spearman')

layerCor(c(test2015.ud, used.bin.2015), fun = 'pearson', na.rm = T)

propptsbin.nwt.tot <- pts_in_bins_tot(nwt.test, testnwt.ud)

plot(propptsbin.nwt.tot$use.bin, propptsbin.nwt.tot$prop.pts)
cor(propptsbin.nwt.tot$use.bin, propptsbin.nwt.tot$prop.pts, method = 'spearman')


### SK ----
modtrainsk <- file.path(derived, 'mods_train', 'mod_sk_train_selmove_2015-2020_HPC.RDS')

gc()
testsk.ud <- mod2UD(modpath = modtrainsk, envlayers = land2015, 
                    studyArea = vect(test.sArea.sk))
plot(testsk.ud)

gc()
testsk.prop <- make_prop_use(sk.test, crs, testsk.ud)

gc()
used.bin.sk <- make_map_bins(testsk.prop)

gc()
propptsbin.sk <- pts_in_bins(sk.test, testsk.ud)
plot(propptsbin.sk$use.bin, propptsbin.sk$prop.pts)
cor(propptsbin.sk$use.bin, propptsbin.sk$prop.pts, method = 'spearman')

layerCor(c(test2015.ud, used.bin.2015), fun = 'pearson', na.rm = T)

propptsbin.sk.tot <- pts_in_bins_tot(sk.test, testsk.ud)

plot(propptsbin.sk.tot$use.bin, propptsbin.sk.tot$prop.pts)
cor(propptsbin.sk.tot$use.bin, propptsbin.sk.tot$prop.pts, method = 'spearman')




### global 2010 ----
land2010 <- load_map_layers(landyr = 2015, disturbyr = 2010, ts_else = 40)

modtrain2010 <- file.path(derived, 'mods_hpc', 'mod_selmove_2010-2015_HPC_noTA.RDS')

gc()
test2010.ud <- mod2UD(modpath = modtrain2010, envlayers = land2010, 
                      studyArea = vect(test.sArea.2010))
plot(test2010.ud)


gc()
test2010.prop <- make_prop_use(test.2010, crs, test2010.ud)


gc()
used.bin.2010 <- make_map_bins(test2010.prop)


gc()
propptsbin.2010 <- pts_in_bins(test.2010, test2010.ud)
plot(propptsbin.2010$use.bin, propptsbin.2010$prop.pts)
cor(propptsbin.2010$use.bin, propptsbin.2010$prop.pts, method = 'spearman')

layerCor(c(test2010.ud, used.bin.2010), fun = 'pearson', na.rm = T)

propptsbin.2010.tot <- pts_in_bins_tot(test.2010, test2010.ud)

plot(propptsbin.2010.tot$use.bin, propptsbin.2010.tot$prop.pts)
cor.test(propptsbin.2010.tot$use.bin, propptsbin.2010.tot$prop.pts, 
         method = 'spearman', na.rm = T)


### global consolidated ----
global.proppts <- rbind(propptsbin.2010.tot[,fold:= 2010], propptsbin.2015.tot[,fold:= 2015], 
                        propptsbin.bc.tot[,fold:= 'bc'], propptsbin.mb.tot[,fold:= 'mb'],
                        propptsbin.nwt.tot[,fold:= 'nwt'], propptsbin.sk.tot[,fold:= 'sk'])
saveRDS(global.proppts, file.path(derived, 'validations', 'global_ud_overlap.RDS'))

ggplot(global.proppts, aes(use.bin, prop.pts, color = fold)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  xlab('Use bin') + ylab('Area adjusted proportion of used points') +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  scale_color_viridis_d()

cor.test(global.proppts$use.bin, global.proppts$prop.pts, 
         method = 'spearman', na.rm = T)

global.cor <- global.proppts[,.(rho = cor.test(use.bin, prop.pts, method = 'spearman', na.rm = T)$estimate[[1]]),
                           by = .(fold)]
global.cor.sum <- global.cor[,.(mean.rho = mean(rho), sd = sd(rho))]

## Jurisdictional models ----

dat.sub.juris <- dat[year>=2014 & year<=2019]

# models
modbc <- file.path(derived, 'mod_selmove_bc.RDS')
modmb <- file.path(derived, 'mod_selmove_mb_2015_50.RDS')
modnwt <- file.path(derived, 'mod_selmove_nwt.RDS')
modsk <- file.path(derived, 'mod_selmove_sk.RDS')

### nwt ----
nwt <- dat.sub.juris[jurisdiction %in% c('nwt', 'yt')]
nwt[,id:=as.factor(id)]
nwt[,indiv_step_id := as.factor(indiv_step_id)]
nwt[,jurisdiction := as.factor(jurisdiction)]
nwt[,year:=as.factor(year)]
nwt[,pop := as.factor(pop)]

gc()
test.sArea.nwt.juris <- make_study_area(nwt, crs, buff = buffer)

# predict NWT based on other juris mods
#### BC mod ----
gc()
nwt.modbc.ud <- mod2UD(modpath = modbc, envlayers = land2015, 
                      studyArea = vect(test.sArea.nwt.juris))
plot(nwt.modbc.ud)


gc()
nwt.modbc.prop <- make_prop_use(nwt, crs, nwt.modbc.ud)


gc()
used.bin.nwt.modbc <- make_map_bins(nwt.modbc.prop)


gc()
propptsbin.nwt.modbc <- pts_in_bins(nwt, nwt.modbc.ud)
plot(propptsbin.nwt.modbc$use.bin, propptsbin.nwt.modbc$prop.pts)
cor.test(propptsbin.nwt.modbc$use.bin, propptsbin.nwt.modbc$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(nwt.modbc.ud, used.bin.nwt.modbc), fun = 'pearson', na.rm = T)

propptsbin.nwt.modbc.tot <- pts_in_bins_tot(nwt, nwt.modbc.ud)

plot(propptsbin.nwt.modbc.tot$use.bin, propptsbin.nwt.modbc.tot$prop.pts)
cor.test(propptsbin.nwt.modbc.tot$use.bin, propptsbin.nwt.modbc.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### MB mod ----
gc()
nwt.modmb.ud <- mod2UD(modpath = modmb, envlayers = land2015, 
                       studyArea = vect(test.sArea.nwt.juris))
plot(nwt.modmb.ud)


gc()
nwt.modmb.prop <- make_prop_use(nwt, crs, nwt.modmb.ud)


gc()
used.bin.nwt.modmb <- make_map_bins(nwt.modmb.prop)


gc()
propptsbin.nwt.modmb <- pts_in_bins(nwt, nwt.modmb.ud)
plot(propptsbin.nwt.modmb$use.bin, propptsbin.nwt.modmb$prop.pts)
cor.test(propptsbin.nwt.modmb$use.bin, propptsbin.nwt.modmb$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(nwt.modmb.ud, used.bin.nwt.modmb), fun = 'pearson', na.rm = T)

propptsbin.nwt.modmb.tot <- pts_in_bins_tot(nwt, nwt.modmb.ud)

plot(propptsbin.nwt.modmb.tot$use.bin, propptsbin.nwt.modmb.tot$prop.pts)
cor.test(propptsbin.nwt.modmb.tot$use.bin, propptsbin.nwt.modmb.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### SK mod ----
gc()
nwt.modsk.ud <- mod2UD(modpath = modsk, envlayers = land2015, 
                       studyArea = vect(test.sArea.nwt.juris))
plot(nwt.modsk.ud)


gc()
nwt.modsk.prop <- make_prop_use(nwt, crs, nwt.modsk.ud)


gc()
used.bin.nwt.modsk <- make_map_bins(nwt.modsk.prop)


gc()
propptsbin.nwt.modsk <- pts_in_bins(nwt, nwt.modsk.ud)
plot(propptsbin.nwt.modsk$use.bin, propptsbin.nwt.modsk$prop.pts)
cor.test(propptsbin.nwt.modsk$use.bin, propptsbin.nwt.modsk$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(nwt.modsk.ud, used.bin.nwt.modsk), fun = 'pearson', na.rm = T)

propptsbin.nwt.modsk.tot <- pts_in_bins_tot(nwt, nwt.modsk.ud)

plot(propptsbin.nwt.modsk.tot$use.bin, propptsbin.nwt.modsk.tot$prop.pts)
cor.test(propptsbin.nwt.modsk.tot$use.bin, propptsbin.nwt.modsk.tot$prop.pts, 
         method = 'spearman', na.rm = T)



### mb ----
mb.2015 <- dat[jurisdiction == 'mb' & int.year ==2015]
length(unique(mb.2015$id))*.5
# worked when sampled 150 indivs
mb.sub.id <- sample(unique(mb.2015$id), floor(length(unique(mb.2015$id))*.50))
mb <- mb.2015[id %in% mb.sub.id]
mb[,id:=as.factor(id)]
mb[,indiv_step_id := as.factor(indiv_step_id)]
mb[,jurisdiction := as.factor(jurisdiction)]
mb[,year:=as.factor(year)]
mb[,pop := as.factor(pop)]


gc()
test.sArea.mb.juris <- make_study_area(mb.2015.sub, crs, buff = buffer)

# predict MB based on other juris mods
#### BC mod ----
gc()
mb.modbc.ud <- mod2UD(modpath = modbc, envlayers = land2015, 
                       studyArea = vect(test.sArea.mb.juris))
plot(mb.modbc.ud)


gc()
mb.modbc.prop <- make_prop_use(mb, crs, mb.modbc.ud)


gc()
used.bin.mb.modbc <- make_map_bins(mb.modbc.prop)


gc()
propptsbin.mb.modbc <- pts_in_bins(mb, mb.modbc.ud)
plot(propptsbin.mb.modbc$use.bin, propptsbin.mb.modbc$prop.pts)
cor.test(propptsbin.mb.modbc$use.bin, propptsbin.mb.modbc$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(mb.modbc.ud, used.bin.mb.modbc), fun = 'pearson', na.rm = T)

propptsbin.mb.modbc.tot <- pts_in_bins_tot(mb, mb.modbc.ud)

plot(propptsbin.mb.modbc.tot$use.bin, propptsbin.mb.modbc.tot$prop.pts)
cor.test(propptsbin.mb.modbc.tot$use.bin, propptsbin.mb.modbc.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### NWT mod ----
gc()
mb.modnwt.ud <- mod2UD(modpath = modnwt, envlayers = land2015, 
                       studyArea = vect(test.sArea.mb.juris))
plot(mb.modnwt.ud)


gc()
mb.modnwt.prop <- make_prop_use(mb, crs, mb.modnwt.ud)


gc()
used.bin.mb.modnwt <- make_map_bins(mb.modnwt.prop)


gc()
propptsbin.mb.modnwt <- pts_in_bins(mb, mb.modnwt.ud)
plot(propptsbin.mb.modnwt$use.bin, propptsbin.mb.modnwt$prop.pts)
cor.test(propptsbin.mb.modnwt$use.bin, propptsbin.mb.modnwt$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(mb.modnwt.ud, used.bin.mb.modnwt), fun = 'pearson', na.rm = T)

propptsbin.mb.modnwt.tot <- pts_in_bins_tot(mb, mb.modnwt.ud)

plot(propptsbin.mb.modnwt.tot$use.bin, propptsbin.mb.modnwt.tot$prop.pts)
cor.test(propptsbin.mb.modnwt.tot$use.bin, propptsbin.mb.modnwt.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### SK mod ----
gc()
mb.modsk.ud <- mod2UD(modpath = modsk, envlayers = land2015, 
                       studyArea = vect(test.sArea.mb.juris))
plot(mb.modsk.ud)


gc()
mb.modsk.prop <- make_prop_use(mb, crs, mb.modsk.ud)


gc()
used.bin.mb.modsk <- make_map_bins(mb.modsk.prop)


gc()
propptsbin.mb.modsk <- pts_in_bins(mb, mb.modsk.ud)
plot(propptsbin.mb.modsk$use.bin, propptsbin.mb.modsk$prop.pts)
cor.test(propptsbin.mb.modsk$use.bin, propptsbin.mb.modsk$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(mb.modsk.ud, used.bin.mb.modsk), fun = 'pearson', na.rm = T)

propptsbin.mb.modsk.tot <- pts_in_bins_tot(mb, mb.modsk.ud)

plot(propptsbin.mb.modsk.tot$use.bin, propptsbin.mb.modsk.tot$prop.pts)
cor.test(propptsbin.mb.modsk.tot$use.bin, propptsbin.mb.modsk.tot$prop.pts, 
         method = 'spearman', na.rm = T)



### sk ----
sk <- dat.sub[jurisdiction == 'sk']
sk[,id:=as.factor(id)]
sk[,indiv_step_id := as.factor(indiv_step_id)]
sk[,jurisdiction := as.factor(jurisdiction)]
sk[,year:=as.factor(year)]
sk[,pop := as.factor(pop)]

gc()
test.sArea.sk.juris <- make_study_area(sk, crs, buff = buffer)

# predict SK based on other juris mods
#### BC mod ----
gc()
sk.modbc.ud <- mod2UD(modpath = modbc, envlayers = land2015, 
                       studyArea = vect(test.sArea.sk.juris))
plot(sk.modbc.ud)


gc()
sk.modbc.prop <- make_prop_use(sk, crs, sk.modbc.ud)


gc()
used.bin.sk.modbc <- make_map_bins(sk.modbc.prop)


gc()
propptsbin.sk.modbc <- pts_in_bins(sk, sk.modbc.ud)
plot(propptsbin.sk.modbc$use.bin, propptsbin.sk.modbc$prop.pts)
cor.test(propptsbin.sk.modbc$use.bin, propptsbin.sk.modbc$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(sk.modbc.ud, used.bin.sk.modbc), fun = 'pearson', na.rm = T)

propptsbin.sk.modbc.tot <- pts_in_bins_tot(sk, sk.modbc.ud)

plot(propptsbin.sk.modbc.tot$use.bin, propptsbin.sk.modbc.tot$prop.pts)
cor.test(propptsbin.sk.modbc.tot$use.bin, propptsbin.sk.modbc.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### MB mod ----
gc()
sk.modmb.ud <- mod2UD(modpath = modmb, envlayers = land2015, 
                       studyArea = vect(test.sArea.sk.juris))
plot(sk.modmb.ud)


gc()
sk.modmb.prop <- make_prop_use(sk, crs, sk.modmb.ud)


gc()
used.bin.sk.modmb <- make_map_bins(sk.modmb.prop)


gc()
propptsbin.sk.modmb <- pts_in_bins(sk, sk.modmb.ud)
plot(propptsbin.sk.modmb$use.bin, propptsbin.sk.modmb$prop.pts)
cor.test(propptsbin.sk.modmb$use.bin, propptsbin.sk.modmb$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(sk.modmb.ud, used.bin.sk.modmb), fun = 'pearson', na.rm = T)

propptsbin.sk.modmb.tot <- pts_in_bins_tot(sk, sk.modmb.ud)

plot(propptsbin.sk.modmb.tot$use.bin, propptsbin.sk.modmb.tot$prop.pts)
cor.test(propptsbin.sk.modmb.tot$use.bin, propptsbin.sk.modmb.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### NWT mod ----
gc()
sk.modnwt.ud <- mod2UD(modpath = modnwt, envlayers = land2015, 
                       studyArea = vect(test.sArea.sk.juris))
plot(sk.modnwt.ud)


gc()
sk.modnwt.prop <- make_prop_use(sk, crs, sk.modnwt.ud)


gc()
used.bin.sk.modnwt <- make_map_bins(sk.modnwt.prop)


gc()
propptsbin.sk.modnwt <- pts_in_bins(sk, sk.modnwt.ud)
plot(propptsbin.sk.modnwt$use.bin, propptsbin.sk.modnwt$prop.pts)
cor.test(propptsbin.sk.modnwt$use.bin, propptsbin.sk.modnwt$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(sk.modnwt.ud, used.bin.sk.modnwt), fun = 'pearson', na.rm = T)

propptsbin.sk.modnwt.tot <- pts_in_bins_tot(sk, sk.modnwt.ud)

plot(propptsbin.sk.modnwt.tot$use.bin, propptsbin.sk.modnwt.tot$prop.pts)
cor.test(propptsbin.sk.modnwt.tot$use.bin, propptsbin.sk.modnwt.tot$prop.pts, 
         method = 'spearman', na.rm = T)


### bc ----
bc <- dat.sub[jurisdiction == 'bc']
bc[,id:=as.factor(id)]
bc[,indiv_step_id := as.factor(indiv_step_id)]
bc[,jurisdiction := as.factor(jurisdiction)]
bc[,year:=as.factor(year)]
bc[,pop := as.factor(pop)]

gc()
test.sArea.bc.juris <- make_study_area(bc, crs, buff = buffer)

#### MB mod ----
gc()
bc.modmb.ud <- mod2UD(modpath = modmb, envlayers = land2015, 
                      studyArea = vect(test.sArea.bc.juris))
plot(bc.modmb.ud)


gc()
bc.modmb.prop <- make_prop_use(bc, crs, bc.modmb.ud)


gc()
used.bin.bc.modmb <- make_map_bins(bc.modmb.prop)


gc()
propptsbin.bc.modmb <- pts_in_bins(bc, bc.modmb.ud)
plot(propptsbin.bc.modmb$use.bin, propptsbin.bc.modmb$prop.pts)
cor.test(propptsbin.bc.modmb$use.bin, propptsbin.bc.modmb$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(bc.modmb.ud, used.bin.bc.modmb), fun = 'pearson', na.rm = T)

propptsbin.bc.modmb.tot <- pts_in_bins_tot(bc, bc.modmb.ud)

plot(propptsbin.bc.modmb.tot$use.bin, propptsbin.bc.modmb.tot$prop.pts)
cor.test(propptsbin.bc.modmb.tot$use.bin, propptsbin.bc.modmb.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### NWT mod ----
gc()
bc.modnwt.ud <- mod2UD(modpath = modnwt, envlayers = land2015, 
                       studyArea = vect(test.sArea.bc.juris))
plot(bc.modnwt.ud)


gc()
bc.modnwt.prop <- make_prop_use(bc, crs, bc.modnwt.ud)


gc()
used.bin.bc.modnwt <- make_map_bins(bc.modnwt.prop)


gc()
propptsbin.bc.modnwt <- pts_in_bins(bc, bc.modnwt.ud)
plot(propptsbin.bc.modnwt$use.bin, propptsbin.bc.modnwt$prop.pts)
cor.test(propptsbin.bc.modnwt$use.bin, propptsbin.bc.modnwt$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(bc.modnwt.ud, used.bin.bc.modnwt), fun = 'pearson', na.rm = T)

propptsbin.bc.modnwt.tot <- pts_in_bins_tot(bc, bc.modnwt.ud)

plot(propptsbin.bc.modnwt.tot$use.bin, propptsbin.bc.modnwt.tot$prop.pts)
cor.test(propptsbin.bc.modnwt.tot$use.bin, propptsbin.bc.modnwt.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### SK mod ----
gc()
bc.modsk.ud <- mod2UD(modpath = modsk, envlayers = land2015, 
                      studyArea = vect(test.sArea.bc.juris))
plot(bc.modsk.ud)


gc()
bc.modsk.prop <- make_prop_use(bc, crs, bc.modsk.ud)


gc()
used.bin.bc.modsk <- make_map_bins(bc.modsk.prop)


gc()
propptsbin.bc.modsk <- pts_in_bins(bc, bc.modsk.ud)
plot(propptsbin.bc.modsk$use.bin, propptsbin.bc.modsk$prop.pts)
cor.test(propptsbin.bc.modsk$use.bin, propptsbin.bc.modsk$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(bc.modsk.ud, used.bin.bc.modsk), fun = 'pearson', na.rm = T)

propptsbin.bc.modsk.tot <- pts_in_bins_tot(bc, bc.modsk.ud)

plot(propptsbin.bc.modsk.tot$use.bin, propptsbin.bc.modsk.tot$prop.pts)
cor.test(propptsbin.bc.modsk.tot$use.bin, propptsbin.bc.modsk.tot$prop.pts, 
         method = 'spearman', na.rm = T)


## Juris consolidated ----
juris.proppts <- rbind(propptsbin.mb.modbc.tot[,`:=`(dat = 'mb', model = 'bc')], 
                       propptsbin.mb.modnwt.tot[,`:=`(dat = 'mb', model = 'nwt')],
                       propptsbin.mb.modsk.tot[,`:=`(dat = 'mb', model = 'sk')],
                       propptsbin.nwt.modbc.tot[,`:=`(dat = 'nwt', model = 'bc')], 
                       propptsbin.nwt.modmb.tot[,`:=`(dat = 'nwt', model = 'mb')],
                       propptsbin.nwt.modsk.tot[,`:=`(dat = 'nwt', model = 'sk')],
                       propptsbin.sk.modbc.tot[,`:=`(dat = 'sk', model = 'bc')], 
                       propptsbin.sk.modnwt.tot[,`:=`(dat = 'sk', model = 'nwt')],
                       propptsbin.sk.modmb.tot[,`:=`(dat = 'sk', model = 'mb')],
                       propptsbin.bc.modmb.tot[,`:=`(dat = 'bc', model = 'mb')], 
                       propptsbin.bc.modnwt.tot[,`:=`(dat = 'bc', model = 'nwt')],
                       propptsbin.bc.modsk.tot[,`:=`(dat = 'bc', model = 'sk')])

saveRDS(juris.proppts, file.path(derived, 'validations', 'juris_ud_overlap.RDS'))

juris.proppts[,mod.labs:= paste0(toupper(model), ' model')]
ggplot(juris.proppts, aes(use.bin, prop.pts, color = toupper(dat))) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  xlab('Use bin') + ylab('Area adjusted proportion of used points') +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  scale_color_viridis_d(name='fold') +
  facet_wrap(~mod.labs) 

cor.test(juris.proppts$use.bin, juris.proppts$prop.pts, 
         method = 'spearman', na.rm = T)

juris.cor <- juris.proppts[,.(rho = cor.test(use.bin, prop.pts, method = 'spearman', na.rm = T)$estimate[[1]]),
                           by = .(model, dat)]
juris.cor.sum <- juris.cor[,.(mean.rho = mean(rho), sd = sd(rho)), by = .(model)]

######

pde.2010 <- rast(file.path(derived, 'pde2010_re.tif'))

pde.2015 <- as.numeric(rast(file.path(derived, 'pde2015_re.tif')))


crs <- st_crs(pde.2015)$wkt

temp <- pde.2015
temp[!is.na(temp)]<-0



# used.2015 <- rast.2015
# used.2015[is.na(used.2015)] <- temp

breaks <- global(rast.2015, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks <- unname(breaks)
t.breaks <- as.vector(t(v.breaks))
used.bin.2015 <- as.numeric(classify(rast.2015, t.breaks, include.lowest=TRUE, brackets=TRUE))
freq(used.bin.2015)
used.bin.0.2015 <- used.bin.2015
used.bin.0.2015[used.bin.0.2015==0]<-1
used.bin.0.2015[is.na(used.bin.0.2015)] <- temp

ptsbin.2015 <- dat.obs[int.year==2015]
ptsbin.2015[, use.bin:= terra::extract(pde.2015, cbind(.SD))[,-1],
            .SDcols = c('x1_','y1_')]
freq(pde.2015)
npts.2015 <- ptsbin.2015[,.(npts = .N), by= .(id)]
propptsbin.2015 <- merge(ptsbin.2015[id!=''], npts.2015, by = 'id')
propptsbin.2015 <- propptsbin.2015[,prop.pts := .N/npts, by = .(use.bin, id)]
plot(propptsbin.2015$use.bin, propptsbin.2015$prop.pts)
cor(propptsbin.2015$use.bin, propptsbin.2015$prop.pts, method = 'spearman')

global((sqrt(raster01(pde.2015))*sqrt(raster01(used.2015))), fun = 'sum', na.rm = T)

layerCor(c(as.numeric(pde.2015), used.bin.0.2015), fun = 'pearson', na.rm = T)
cor.2015 <- focalPairs(c(as.numeric(pde.2015), used.bin.0.2015), fun = 'pearson')

pde.use.2015 <- mask(as.numeric(pde.2015), rast.2015)
layerCor(c(pde.use.2015, rast.2015), fun = 'pearson', na.rm = T)
