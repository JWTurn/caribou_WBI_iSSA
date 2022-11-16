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
raw.bc <- 'data/raw-data/BC_data/'
derived <- 'data/derived-data/'

rgdal::ogrListLayers(file.path(raw.bc, 'telem_data_request_20211026.gdb'))

kmb <- st_read(file.path(raw.bc, 'telem_data_request_20211026.gdb'), "KMB_Local_Telemetry_20211026")
regional <- st_read(file.path(raw.bc, 'telem_data_request_20211026.gdb'), "Regional_Telemetry_20211026")


### Prep data ----
# checking for right formats and grabbing what need
kmb.dt <- as.data.table(kmb)
kmb.dt <- kmb.dt[!(is.na(Animal_ID)) & Animal_ID != 'None',.(id = Animal_ID, Region, Population_Unit, 
                    datetime = GMT_FixDateTime, Longitude, Latitude)]
regional.dt <- as.data.table(regional)
regional.dt[, datetime := FixDateTime + hours(8)]
regional.dt <- regional.dt[!(is.na(Animal_ID)) & Animal_ID != 'None',.(id = Animal_ID, Region, Population_Unit, 
                                       datetime, Longitude, Latitude)]
dat<- rbind(kmb.dt, regional.dt)

dat_cleaner <- dat[complete.cases(Longitude,Latitude, datetime)]


### convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
crs <- st_crs(4326)$wkt
outcrs <- st_crs(3978)

sfboo <- st_as_sf(dat_cleaner, coords = c('Longitude', 'Latitude'),
                  crs = crs)
outboo <- st_transform(sfboo, outcrs)
boo <- setDT(sfheaders::sf_to_df(outboo, fill = T))



### EXPLORE ----
# check if all observations are complete
all(complete.cases(boo[,.(x,y, datetime)]))


# check for duplicated time stamps
boo[,any(duplicated(datetime)), by = id]

# We have some duplicated time stamps, these need to be removed prior to creating a track.
DT <- unique(boo, by = c('id', 'datetime'))


### track ####
trk <- DT %>% make_track(x,y, datetime, crs = st_crs(3978), all_cols = T)
trk2 <- trk %>% nest(data = -"Population_Unit") %>%
  mutate(steps = map(data, function(x) 
    x %>% track_resample(rate = hours(12), tolerance = hours(2)) %>% 
      #filter_min_n_burst() %>%
      steps_by_burst(.,keep_cols = 'start') %>%
      random_steps(., n =10)
    )
    )
trk3 <- trk2 %>% unnest(c(Population_Unit, steps))
View(summarize_sampling_rate(trk))
fixrate <- trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(sr) #%>% unnest(cols = c(sr))
# This is actually all over the place, so I'm going to do based on the 12 hrs discussed
# we know they have shorter fixes during calving
# ~ 12 hour fix rates

# save 'clean' data 
saveRDS(boo, paste0(derived, 'prepped-data/BCprepDat.RDS'))
dat <- readRDS(file.path(derived, 'prepped-data/BCprepDat.RDS'))
