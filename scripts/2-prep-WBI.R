### Combine Prepared Data ====
# Julie Turner
# Started: November 9 2022

require(Require)
Require('reproducible')
Require('data.table')
Require('sf')
Require('dplyr')
Require('tidyr')
Require('amt')


# Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'
prepped <- ('data/derived-data/prepped-data/')

mb<- readRDS(file.path(prepped, 'MBprepDat.RDS'))
sk<- readRDS(file.path(prepped, 'SKprepDat.RDS'))
bc<- readRDS(file.path(prepped, 'BCprepDat.RDS'))
nwt<- readRDS(file.path(prepped, 'NWTprepDat.RDS'))
yt<- readRDS(file.path(prepped, 'YTprepDat.RDS'))

# mbcoords<- mb%>%st_as_sf(coords = c('x','y'))%>%
#   st_set_crs(st_crs(3978)$wkt)

### check fix rates ####
# check for duplicated time stamps
bc[,any(duplicated(datetime)), by = id]
mb[,any(duplicated(datetime)), by = id]
nwt[,any(duplicated(datetime)), by = id]
sk[,any(duplicated(datetime)), by = id]
yt[,any(duplicated(datetime)), by = id]

# We have some duplicated time stamps, these need to be removed prior to creating a track.
bc <- unique(bc, by = c('id', 'datetime'))
bc[,Npts := .N, by = .(id)]
bc <- bc[Npts>2] # remove those with only 1 point
mb <- unique(mb, by = c('id', 'datetime'))
mb[,Npts := .N, by = .(id)]
mb <- mb[Npts>2] # remove those with only 1  point
nwt <- unique(nwt, by = c('id', 'datetime'))
sk <- unique(sk, by = c('id', 'datetime'))
yt <- unique(yt, by = c('id', 'datetime'))


bc.trk <- bc %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>% 
  nest(data = -"id")
bc.fixrate <- bc.trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
range(bc.fixrate$median)
median(bc.fixrate$median)
# 13 hours

mb.trk <- mb %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>% 
  nest(data = -"id")
mb.fixrate <- mb.trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
range(mb.fixrate$median)
median(mb.fixrate$median)
# 3 hours

nwt.trk <- nwt %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>% 
  nest(data = -"id")
nwt.fixrate <- nwt.trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
range(nwt.fixrate$median)
median(nwt.fixrate$median)
# 8 hours

sk.trk <- sk %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>% 
  nest(data = -"id")
sk.fixrate <- sk.trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
range(sk.fixrate$median)
median(sk.fixrate$median)
# 5 hours

yt.trk <- yt %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>% 
  nest(data = -"id")
yt.fixrate <- yt.trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
range(yt.fixrate$median)
median(yt.fixrate$median)
# 5 hours

### combine all datasets ----
dat.all <- rbind(nwt[,.(id, jurisdiction = 'nwt', pop = area, subpop = habitat, datetime, x, y)],
                mb[,.(id, jurisdiction = 'mb', pop = Range, subpop = NA, datetime, x, y)],
                sk[,.(id, jurisdiction = 'sk', pop = 'SK1', subpop = NA, datetime, x, y)],
                bc[,.(id, jurisdiction = 'bc', pop = Population_Unit, subpop = NA, datetime, x, y)],
                yt[,.(id, jurisdiction = 'yt', pop = "Yukon", subpop = NA, datetime, x, y)]
                )
### remove crazy points in Russia (??) ----
dat.clean <- dat.all[complete.cases(x,y, datetime) & between(x, -1665110, 0) &between(y, -98940, 2626920)]

# Save clean data ----
saveRDS(dat.clean, file.path(prepped, "WBIprepDat.RDS"))

# create study area and buffered study area ----
coords<- dat.clean%>%st_as_sf(coords = c('x','y'))%>%
  st_set_crs(st_crs(3978))

## save study area ----
st_write(coords, file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.shp'), append = F)

## 10 km buffer around points to get an idea of extent of study area ----
studyArea <- st_buffer(coords, dist = 10000)
sa.union <- st_union(studyArea)

## save buffered study area ----
st_write(sa.union, file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'), append = F)


