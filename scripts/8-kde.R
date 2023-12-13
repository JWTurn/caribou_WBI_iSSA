### KDEs ====
# Julie Turner
# Started: October 31 2022


### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'terra', 'sf', 
          'tidyr', 'ggplot2', 'adehabitatHR', 'sp', 'rgdal')
lapply(libs, require, character.only = TRUE)

### Input data ----

prepped <- 'data/derived-data/prepped-data/'
derived <- 'data/derived-data/'

sk <- readRDS(paste0(prepped, 'SKprepDat.RDS'))
yt <- readRDS(paste0(prepped, 'YTprepDat.RDS'))
mb <- readRDS(paste0(prepped, 'MBprepDat.RDS'))
bc <- readRDS(paste0(prepped, 'BCprepDat.RDS'))
nwt <- readRDS(paste0(prepped, 'NWTprepDat.RDS'))

crs <- CRS(st_crs(3978)$wkt)

sk_track <- sk %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>%
  track_resample(rate = hours(24), tolerance = hours(4))
sk_kde <- hr_kde(sk_track, levels = 0.5)
sk_iso <- vect(hr_isopleths(sk_kde)$geometry)
writeVector(sk_iso, file.path('data','map-output', 'sk_iso50.shp'))

# akde test
# make a list of amt tracks by individual
sk_trk <- lapply(unique(sk$id), function(ii){
  make_track(sk[id == ii], x, y, datetime, crs = st_crs(3978), all_cols = T)
})
# make the raster base amt needs at the size of the whole study area
trast <- make_trast(sk_track, res = 50)
# list of akde by individual
sk_akde <- lapply(unique(sk$id), function(ii){
  hr_akde(sk_track, levels = 0.5, trast = trast)
})




yt_track <- yt %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>%
  track_resample(rate = hours(24), tolerance = hours(4))
yt_kde <- hr_kde(yt_track, levels = 0.5)
yt_iso <- vect(hr_isopleths(yt_kde)$geometry)
writeVector(yt_iso, file.path('data','map-output', 'yt_iso50.shp'))

mb_track <- mb %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>%
  track_resample(rate = hours(24), tolerance = hours(4))
mb_kde <- hr_kde(mb_track, levels = 0.5)
mb_iso <- vect(hr_isopleths(mb_kde)$geometry)
writeVector(mb_iso, file.path('data','map-output', 'mb_iso50.shp'))

bc_track <- bc %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>%
  track_resample(rate = hours(24), tolerance = hours(4))
bc_kde <- hr_kde(bc_track, levels = 0.5)
bc_iso <- vect(hr_isopleths(bc_kde)$geometry)
writeVector(bc_iso, file.path('data','map-output', 'bc_iso50.shp'))

nwt_track <- nwt %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>%
  track_resample(rate = hours(24), tolerance = hours(4))
nwt_kde <- hr_kde(nwt_track, levels = 0.5)
nwt_iso <- vect(hr_isopleths(nwt_kde)$geometry)
writeVector(nwt_iso, file.path('data','map-output', 'nwt_iso50.shp'))


