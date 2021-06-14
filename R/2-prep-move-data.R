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
### track ####
dat_sk <- dat_sk %>% group_by(id) %>% nest()

dat_sk <- dat_sk %>%
  mutate(trk = map(data, function(d) {
    amt::make_track(d, long, lat, datetime) 
  }))  



fixrate <- dat_sk %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))


