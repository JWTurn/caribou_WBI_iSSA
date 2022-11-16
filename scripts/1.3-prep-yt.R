### Prepare Data ====
# Julie Turner
# Started: July 14 2021

require(Require)
Require('reproducible')
Require('data.table')
Require('sf')
Require('amt')

### Input data ----
raw <- 'data/raw-data/'
raw.yt <- 'data/raw-data/YT_data/'
derived <- 'data/derived-data/'

dat <- read.csv(file.path(raw.yt, 'YT_data.csv'))

### Prep data ----
dat <- setDT(dat)
dat[, datetime:= trimws(Date...Time..GMT., 'l')]
#dat[, datetime := gsub(' ', '', datetime)]
dat[, datetime := as.POSIXct(datetime, tz ='GMT', 
                             format = '%m-%d-%Y %H:%M:%S')]
dat_cleaner <- dat[complete.cases(Longitude,Latitude, datetime)]


### convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
crs <- st_crs(4326)$wkt
outcrs <- st_crs(3978)

sfboo <- st_as_sf(dat_cleaner, coords = c('Longitude', 'Latitude'),
                  crs = crs)
outboo <- st_transform(sfboo, outcrs)
boo <- setDT(sfheaders::sf_to_df(outboo, fill = T))

### standarize names and columns ----
boo_clean <- boo[, .(id = Animal.ID, datetime, x, y, Altitude, Fix.Status)]

### EXPLORE ----
# check if all observations are complete
all(complete.cases(boo_clean[,.(x,y, datetime)]))


# check for duplicated time stamps
boo_clean[,any(duplicated(datetime)), by = id]

# We have some duplicated time stamps, these need to be removed prior to creating a track.
DT <- unique(boo_clean, by = c('id', 'datetime'))


### track ####
trk <- DT %>% make_track(x,y, datetime, crs = st_crs(3978), id = id)
trk <- trk %>% nest(data = -"id")

fixrate <- trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
# ~ 5 hour fix rates

# save 'clean' data 
saveRDS(boo_clean, paste0(derived, 'prepped-data/YTprepDat.RDS'))
