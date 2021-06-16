### Prepare Data ====
# Julie Turner
# Started: June 14 2021


### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'raster', 'tidyr', 'ggplot2')
lapply(libs, require, character.only = TRUE)

### Input data ----

sk <- 'data/derived-data/prepped-data/'
derived <- 'data/derived-data/'

dat_sk <- readRDS(paste0(sk, 'SKprepDat.RDS'))


### EXPLORE ----
# check if all observations are complete
all(complete.cases(dat_sk[,.(long,lat, datetime)]))
# remove incomplete observations
dat_sk <- dat_sk[complete.cases(long,lat, datetime)]
all(complete.cases(dat_sk[,.(long,lat, datetime)]))

# check for duplicated time stamps
dat_sk[,any(duplicated(datetime)), by = id]

# We have some duplicated time stamps, these need to be removed prior to creating a track.
DT_sk <- unique(dat_sk, by = c('id', 'datetime'))


### track ####
trk_sk <- DT_sk %>% make_track(long, lat, datetime, crs = sp::CRS("+init=epsg:4326"), id = id)
trk_sk <- trk_sk %>% nest(data = -"id")

fixrate <- trk_sk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
# setnames(trk_sk, c('id','data'))
# trk_sk[, trk:= amt::make_track(data, long, lat, datetime, crs = sp::CRS("+init=epsg:4326")), by = id]


# trk_sk<- rbindlist(lapply(unique(DT_sk$id), function(i){
#   DT_sk[id == i, .(amt::make_track(.SD, long, lat, datetime, crs = sp::CRS("+init=epsg:4326"), id = id))]
# }))
### standardize fix rate ####

#trk_sk2 <-trk_sk %>% nest(data = -"id") 
# safely gets rid of the individuals causing errors 
#TODO look into errors later
trk_sk2 <-trk_sk %>%
  mutate(steps = map(data, purrr::safely(function(x) 
    x %>% amt::track_resample(rate = hours(5), tolerance = minutes(30)) %>% 
      amt::filter_min_n_burst() %>% amt::steps_by_burst(., lonlat = T))))

steps_by_burst(trk_sk2$steps[[1]], lonlat = T)


trk_sk2 <-trk_sk %>%
  mutate(steps = map(data, function(x)
    x %>% amt::track_resample(rate = hours(5), tolerance = minutes(30)) %>%
      amt::filter_min_n_burst() %>% amt::steps_by_burst(., lonlat = T)))



############
sl<- trk_sk %>%mutate(sl = map(data, function(x) 
  x %>% amt::step_lengths(., lonlat = T)))
sum <- sl %>%
  mutate(sum = lapply(sl, summary)) %>% 
  dplyr::select(id, sum) 

### look at movement ####

trk_sk3 <- trk_sk2 %>% dplyr::select(id, steps) %>% unnest(cols = steps) 
test <- data.table(id = trk_sk3$id, trk_sk3$steps$result)
ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4)

DT_sk %>% ggplot(aes(long,lat, color= factor(id))) +
  geom_point()
