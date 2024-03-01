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
  propptsbin <- merge(DT, npts, by = 'id')
  propptsbin <- propptsbin[,prop.pts := .N/npts, by = .(use.bin, id)]
  return(propptsbin)
}


crs <- st_crs(3978)$wkt

gc()
sArea.2015 <- make_study_area(dat.sub, crs, buff = 500)



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

test.sArea.2015 <- make_study_area(dat.test, crs, buff = 500)

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

## Jurisdictions -----
### BC ----

bc.test <- dat.sub[jurisdiction=='bc']
bc.test[,indiv_step_id := as.factor(indiv_step_id)]
bc.test[,jurisdiction := as.factor(jurisdiction)]
bc.test[,year:=as.factor(year)]
bc.test[,pop := as.factor(pop)]
bc.test[,id := as.factor(id)]

test.sArea.bc <- make_study_area(bc.test, crs, buff = 500)

### MB ----

mb.test <- dat.sub[jurisdiction=='mb']
mb.test[,indiv_step_id := as.factor(indiv_step_id)]
mb.test[,jurisdiction := as.factor(jurisdiction)]
mb.test[,year:=as.factor(year)]
mb.test[,pop := as.factor(pop)]
mb.test[,id := as.factor(id)]


### SK ----

sk.test <- dat.sub[jurisdiction=='sk']
sk.test[,indiv_step_id := as.factor(indiv_step_id)]
sk.test[,jurisdiction := as.factor(jurisdiction)]
sk.test[,year:=as.factor(year)]
sk.test[,pop := as.factor(pop)]
sk.test[,id := as.factor(id)]


### NWT ----

nwt.test <- dat.sub[jurisdiction=='nwt']
nwt.test[,indiv_step_id := as.factor(indiv_step_id)]
nwt.test[,jurisdiction := as.factor(jurisdiction)]
nwt.test[,year:=as.factor(year)]
nwt.test[,pop := as.factor(pop)]
nwt.test[,id := as.factor(id)]


# predicted use plots ----
gc()


land2015 <- load_map_layers(landyr = 2019, disturbyr = 2015, ts_else = 40)
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
