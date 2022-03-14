### Prepare Data ====
# Julie Turner
# Started: June 14 2021


### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'terra', 'sf', 'tidyr', 'ggplot2')
lapply(libs, require, character.only = TRUE)

### Input data ----

prepped <- 'data/derived-data/prepped-data/'
derived <- 'data/derived-data/'

#dat <- readRDS(paste0(prepped, 'SKprepDat.RDS'))

dat <- readRDS(paste0(prepped, 'NWTprepDat.RDS'))


### EXPLORE ----
# check if all observations are complete
all(complete.cases(dat[,.(x,y, datetime)]))
# remove incomplete observations
dat <- dat[complete.cases(x,y, datetime)]
all(complete.cases(dat[,.(x,y, datetime)]))

# check for duplicated time stamps
dat[,any(duplicated(datetime)), by = id]

# We have some duplicated time stamps, these need to be removed prior to creating a track.
DT <- unique(dat, by = c('id', 'datetime'))


### track ####
trk <- DT %>% make_track(x,y, datetime, crs = st_crs(3978), id = id)
trk <- trk %>% nest(data = -"id")

fixrate <- trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
# setnames(trk, c('id','data'))
# trk[, trk:= amt::make_track(data, long, lat, datetime, crs = sp::CRS("+init=epsg:4326")), by = id]


# trk<- rbindlist(lapply(unique(DT$id), function(i){
#   DT[id == i, .(amt::make_track(.SD, long, lat, datetime, crs = sp::CRS("+init=epsg:4326"), id = id))]
# }))
### standardize fix rate ####

#trk2 <-trk %>% nest(data = -"id") 
# safely gets rid of the individuals causing errors 
#TODO look into errors later
trk2 <-trk %>%
  mutate(steps = map(data, purrr::safely(function(x) 
    x %>% amt::track_resample(rate = hours(5), tolerance = minutes(30)) %>% 
      amt::filter_min_n_burst() %>% amt::steps_by_burst(., lonlat = T))))

steps_by_burst(trk2$steps[[1]], lonlat = T)


trk2 <-trk %>%
  mutate(steps = map(data, function(x)
    x %>% amt::track_resample(rate = hours(5), tolerance = minutes(30)) %>%
      amt::filter_min_n_burst() %>% amt::steps_by_burst(., lonlat = T)))



############
sl<- trk %>%mutate(sl = map(data, function(x) 
  x %>% amt::step_lengths(., lonlat = T)))
sum <- sl %>%
  mutate(sum = lapply(sl, summary)) %>% 
  dplyr::select(id, sum) 

### look at movement ####

trk3 <- trk2 %>% dplyr::select(id, steps) %>% unnest(cols = steps) 
test <- data.table(id = trk3$id, trk3$steps$result)
ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4)

DT %>% ggplot(aes(long,lat, color= factor(id))) +
  geom_point()
