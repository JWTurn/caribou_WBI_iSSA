## iSSA individual jurisdictions ----

#require(Require)
require(data.table)
require(glmmTMB)
require(broom.mixed)
require(performance)
#require(ggplot2)

### Input data ----
raw <- file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

# prepped <- readRDS(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS'))
# meta <- unique(prepped[,.(id, jurisdiction, pop=tolower(gsub(' ', '.',pop)), subpop)])

dat.derive <- targets::tar_read(stepID)
# dat <- merge(dat, meta, by = 'id', all.x = T)


# remove tracks that couldn't estimate movement parameters at this fix rate
dat.steps <- dat.derive[!is.na(x2_) & !is.na(y2_),.(n = .N), by = .(indiv_step_id)]

dat <- dat.derive[indiv_step_id %in% dat.steps[n ==11, .(indiv_step_id)]$indiv_step_id]

# Check availability
dat[,.(water = mean(prop_water_end, na.rm = T), snow = mean(prop_snow_end, na.rm = T),
       rock = mean(prop_rock_end, na.rm = T), barren = mean(prop_barren_end, na.rm = T),
       bryoids = mean(prop_bryoids_end, na.rm = T), shrub = mean(prop_shrub_end, na.rm = T),
       wetland = mean(prop_wetland_end, na.rm = T), wettreed = mean(prop_wettreed_end, na.rm = T), 
       herbs = mean(prop_herbs_end, na.rm = T), needle =mean(prop_needleleaf_end, na.rm = T),
       decid = mean(prop_deciduous_end, na.rm = T),mixed = mean(prop_mixed_end, na.rm = T)), 
    by = .(int.year, jurisdiction)]
# snow, rock, barren, bryoids, generally below 1% availability
# dominant: needle, wetland

# aggregate habitat types
dat[, prop_mixforest_start := prop_deciduous_start + prop_mixed_start + prop_wettreed_start]
dat[, prop_veg_start := prop_shrub_start + prop_bryoids_start + prop_herbs_start]
dat[, prop_wets_start := prop_wetland_start]
dat[, prop_openbarren_start := prop_rock_start + prop_barren_start + prop_water_start]
dat[, prop_open_start := prop_rock_start + prop_barren_start + prop_bryoids_start + prop_herbs_start + prop_water_start]
dat[, prop_otherveg_start := prop_deciduous_start + prop_mixed_start + prop_wettreed_start +
      prop_shrub_start + prop_bryoids_start + prop_herbs_start]

dat[, prop_mixforest_end := prop_deciduous_end + prop_mixed_end + prop_wettreed_end]
dat[, prop_veg_end := prop_shrub_end + prop_bryoids_end + prop_herbs_end]
dat[, prop_wets_end := prop_wetland_end] 
dat[, prop_openbarren_end := prop_rock_end + prop_barren_end + prop_water_end]
dat[, prop_open_end := prop_rock_end + prop_barren_end + prop_bryoids_end + prop_herbs_end + prop_water_end]
dat[, prop_otherveg_end := prop_deciduous_end + prop_mixed_end + prop_wettreed_end +
      prop_shrub_end + prop_bryoids_end + prop_herbs_end]

dat[,.(needle =mean(prop_needleleaf_end, na.rm = T), mixforest =mean(prop_mixforest_end, na.rm = T), veg = mean(prop_veg_end, na.rm = T),
       open = mean(prop_open_end, na.rm = T), wets = mean(prop_wets_end, na.rm = T),
       openbarren = mean(prop_openbarren_end, na.rm = T), otherveg = mean(prop_otherveg_end, na.rm = T)), 
    by = .(int.year, jurisdiction)]

dat[,.(distlf_end =mean(distlf_end, na.rm = T), distlf_other_end = mean(distlf_other_end, na.rm = T),
       disturbance = mean(disturbance_end, na.rm = T), tsf = mean(ts_fires_end, na.rm = T),
       tsh = mean(ts_harv_end, na.rm = T)), 
    by = .(int.year, jurisdiction)]
## prop_open too low availability for most


quantile(dat$ts_harv_end)
dat[ts_harv_start == 100, ts_harv_start:= 40]
dat[ts_harv_end == 100, ts_harv_end:= 40]
range(dat$ts_harv_end)
quantile(dat$ts_harv_end)

quantile(dat$ts_fires_end)
dat[ts_fires_start == 100, ts_fires_start:= 40]
dat[ts_fires_end == 100, ts_fires_end:= 40]
range(dat$ts_fires_end)
quantile(dat$ts_fires_end)


dat[,range(year), by = .(jurisdiction)]


### cleaning to just what we need -- trying to save memory space
dat.clean <- dat[,.(x1_ = as.integer(x1_), y1_ = as.integer(y1_), x2_ = as.integer(x2_), y2_ = as.integer(y2_),
                    sl_ = as.integer(sl_), ta_, id, jurisdiction, t1_, t2_, case_, step_id_ = as.integer(step_id_),
                    year = as.integer(year), int.year = as.integer(int.year), 
                    prop_wets_start, prop_veg_start, prop_needleleaf_start, prop_mixforest_start, 
                    prop_wets_end, prop_veg_end, prop_needleleaf_end, prop_mixforest_end, 
                    ts_harv_start = as.integer(ts_harv_start), ts_harv_end = as.integer(ts_harv_end),
                    ts_fires_start = as.integer(ts_fires_start), ts_fires_end = as.integer(ts_fires_end),
                    distlf_start = as.integer(distlf_start), distlf_end = as.integer(distlf_end),
                    distlf_other_start = as.integer(distlf_other_start), distlf_other_end = as.integer(distlf_other_end),
                    disturbance_start = as.integer(disturbance_start), disturbance_end = as.integer(disturbance_end),
                    indiv_step_id)]
saveRDS(dat.clean, file.path(derived, 'dat_iSSA.RDS'))

## START ----
dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))

#setindex(dat, NULL)
nwt <- dat[jurisdiction %in% c('nwt', 'yt')]
nwt[,id:=as.factor(id)]
nwt[,indiv_step_id := as.factor(indiv_step_id)]

mb.2010 <- dat[jurisdiction == 'mb' & int.year ==2010]
mb.2010[,id:=as.factor(id)]
mb.2010[,indiv_step_id := as.factor(indiv_step_id)]
mb.2010[, year := as.factor(year)]

mb.2015 <- dat[jurisdiction == 'mb' & int.year ==2015]
mb.2015[,id:=as.factor(id)]
mb.2015[,indiv_step_id := as.factor(indiv_step_id)]

sk <- dat[jurisdiction == 'sk']
sk[,id:=as.factor(id)]
sk[,indiv_step_id := as.factor(indiv_step_id)]

bc <- dat[jurisdiction == 'bc']
bc[,id:=as.factor(id)]
bc[,indiv_step_id := as.factor(indiv_step_id)]

no.mb <- dat[jurisdiction != 'mb']
no.mb[,id:=as.factor(id)]
no.mb[,indiv_step_id := as.factor(indiv_step_id)]
no.mb[,year := as.factor(year)]



### MODELS ----
#### all but mb ----
gc()
m.no.mb <- glmmTMB(case_ ~ -1 +
                   I(log(sl_+1)) +
                   I(cos(ta_)) +
                   I(log(sl_+1)):I(cos(ta_)) +
                   prop_needleleaf_start:I(log(sl_+1)) + 
                   prop_mixforest_start:I(log(sl_+1)) + 
                   prop_veg_start:I(log(sl_+1)) + 
                   prop_wets_start:I(log(sl_+1)) +
                   prop_needleleaf_end +
                   prop_mixforest_end +
                   prop_veg_end +
                   prop_wets_end +
                   I(log(ts_fires_end+1)) + 
                   I(log(sl_+1)):I(log(ts_fires_start+1)) +
                   I(log(ts_harv_end+1)) + 
                   I(log(sl_+1)):I(log(ts_harv_start+1)) +
                   I(log(distlf_end+1)) + 
                   I(log(sl_+1)):I(log(distlf_start+1)) +
                   I(log(distlf_other_end+1)) + 
                   I(log(sl_+1)):I(log(distlf_other_start+1)) +
                   disturbance_end +
                  # I(log(sl_+1)):disturbance_start +
                   (1|indiv_step_id) +
                   (0 + I(log(sl_ +1))|id) +
                   (0 + I(cos(ta_))|id) +
                   (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                   (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                   (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
                   (0 + prop_veg_start:I(log(sl_+1))|id) + 
                   (0 + prop_wets_start:I(log(sl_+1))|id) +
                   (0 + prop_needleleaf_end|id) +
                   (0 + prop_mixforest_end|id) +
                   (0 + prop_veg_end|id) +
                   (0 + prop_wets_end|id) +
                   (0 + (I(log(ts_fires_end+1)))|id) +
                   (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                   (0 + (I(log(ts_harv_end+1)))|id) + 
                   (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                   (0 + I(log(distlf_end+1))|id) + 
                   (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
                   (0 + I(log(distlf_other_end+1))|id) + 
                   (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
                   (0 + disturbance_end|id) +
                   #(0 + I(log(sl_+1)):disturbance_start|id) +
                   (1|year),
                 family = poisson(), data = no.mb,
                 map= list(theta = factor(c(NA,1:21))),
                 start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
)



summary(m.no.mb)
saveRDS(m.no.mb, file.path(derived, 'mod_selmove_no_mb.RDS'))



#### nwt ----
gc()
m.nwt <- glmmTMB(case_ ~ -1 +
                  I(log(sl_+1)) +
                  I(cos(ta_)) +
                  I(log(sl_+1)):I(cos(ta_)) +
                  prop_needleleaf_start:I(log(sl_+1)) + 
                  prop_mixforest_start:I(log(sl_+1)) + 
                  prop_veg_start:I(log(sl_+1)) + 
                  prop_wets_start:I(log(sl_+1)) +
                  prop_needleleaf_end +
                  prop_mixforest_end +
                  prop_veg_end +
                  prop_wets_end +
                  I(log(ts_fires_end+1)) + 
                  I(log(sl_+1)):I(log(ts_fires_start+1)) +
                  I(log(ts_harv_end+1)) + 
                  I(log(sl_+1)):I(log(ts_harv_start+1)) +
                  I(log(distlf_end+1)) + 
                  I(log(sl_+1)):I(log(distlf_start+1)) +
                  I(log(distlf_other_end+1)) + 
                  I(log(sl_+1)):I(log(distlf_other_start+1)) +
                  disturbance_end +
                  I(log(sl_+1)):disturbance_start +
                  (1|indiv_step_id) +
                  (0 + I(log(sl_ +1))|id) +
                  (0 + I(cos(ta_))|id) +
                  (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                  (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                  (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
                  (0 + prop_veg_start:I(log(sl_+1))|id) + 
                  (0 + prop_wets_start:I(log(sl_+1))|id) +
                  (0 + prop_needleleaf_end|id) +
                  (0 + prop_mixforest_end|id) +
                  (0 + prop_veg_end|id) +
                  (0 + prop_wets_end|id) +
                  (0 + (I(log(ts_fires_end+1)))|id) +
                  (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                  (0 + (I(log(ts_harv_end+1)))|id) + 
                  (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                  (0 + I(log(distlf_end+1))|id) + 
                  (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
                  (0 + I(log(distlf_other_end+1))|id) + 
                  (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
                  (0 + disturbance_end|id) +
                  (0 + I(log(sl_+1)):disturbance_start|id) +
                  (1|year),
                family = poisson(), data = nwt,
                map= list(theta = factor(c(NA,1:22))),
                start = list(theta =c(log(1000), seq(0,0, length.out = 22)))
)



summary(m.nwt)
saveRDS(m.nwt, file.path(derived, 'mod_selmove_nwt.RDS'))


#### bc ----
gc()
m.bc <- glmmTMB(case_ ~ -1 +
                   I(log(sl_+1)) +
                   I(cos(ta_)) +
                   I(log(sl_+1)):I(cos(ta_)) +
                   prop_needleleaf_start:I(log(sl_+1)) + 
                   prop_mixforest_start:I(log(sl_+1)) + 
                   prop_veg_start:I(log(sl_+1)) + 
                   prop_wets_start:I(log(sl_+1)) +
                   prop_needleleaf_end +
                   prop_mixforest_end +
                   prop_veg_end +
                   prop_wets_end +
                   I(log(ts_fires_end+1)) + 
                   I(log(sl_+1)):I(log(ts_fires_start+1)) +
                   I(log(ts_harv_end+1)) + 
                   I(log(sl_+1)):I(log(ts_harv_start+1)) +
                   I(log(distlf_end+1)) + 
                   I(log(sl_+1)):I(log(distlf_start+1)) +
                   I(log(distlf_other_end+1)) + 
                   I(log(sl_+1)):I(log(distlf_other_start+1)) +
                   disturbance_end +
                   I(log(sl_+1)):disturbance_start +
                   (1|indiv_step_id) +
                   (0 + I(log(sl_ +1))|id) +
                   (0 + I(cos(ta_))|id) +
                   (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                   (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                   (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
                   (0 + prop_veg_start:I(log(sl_+1))|id) + 
                   (0 + prop_wets_start:I(log(sl_+1))|id) +
                   (0 + prop_needleleaf_end|id) +
                   (0 + prop_mixforest_end|id) +
                   (0 + prop_veg_end|id) +
                   (0 + prop_wets_end|id) +
                   (0 + (I(log(ts_fires_end+1)))|id) +
                   (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                   (0 + (I(log(ts_harv_end+1)))|id) + 
                   (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                   (0 + I(log(distlf_end+1))|id) + 
                   (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
                   (0 + I(log(distlf_other_end+1))|id) + 
                   (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
                   (0 + disturbance_end|id) +
                   (0 + I(log(sl_+1)):disturbance_start|id) +
                   (1|year),
                 family = poisson(), data = bc,
                 map= list(theta = factor(c(NA,1:22))),
                 start = list(theta =c(log(1000), seq(0,0, length.out = 22)))
)



summary(m.bc)
saveRDS(m.bc, file.path(derived, 'mod_selmove_bc.RDS'))


#### sk ----
gc()
m.sk <- glmmTMB(case_ ~ -1 +
                   I(log(sl_+1)) +
                   I(cos(ta_)) +
                   I(log(sl_+1)):I(cos(ta_)) +
                   prop_needleleaf_start:I(log(sl_+1)) + 
                   prop_mixforest_start:I(log(sl_+1)) + 
                   prop_veg_start:I(log(sl_+1)) + 
                   prop_wets_start:I(log(sl_+1)) +
                   prop_needleleaf_end +
                   prop_mixforest_end +
                   prop_veg_end +
                   prop_wets_end +
                   I(log(ts_fires_end+1)) + 
                   I(log(sl_+1)):I(log(ts_fires_start+1)) +
                   I(log(ts_harv_end+1)) + 
                   I(log(sl_+1)):I(log(ts_harv_start+1)) +
                   I(log(distlf_end+1)) + 
                   I(log(sl_+1)):I(log(distlf_start+1)) +
                   I(log(distlf_other_end+1)) + 
                   I(log(sl_+1)):I(log(distlf_other_start+1)) +
                   disturbance_end +
                   I(log(sl_+1)):disturbance_start +
                   (1|indiv_step_id) +
                   (0 + I(log(sl_ +1))|id) +
                   (0 + I(cos(ta_))|id) +
                   (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                   (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                   (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
                   (0 + prop_veg_start:I(log(sl_+1))|id) + 
                   (0 + prop_wets_start:I(log(sl_+1))|id) +
                   (0 + prop_needleleaf_end|id) +
                   (0 + prop_mixforest_end|id) +
                   (0 + prop_veg_end|id) +
                   (0 + prop_wets_end|id) +
                   (0 + (I(log(ts_fires_end+1)))|id) +
                   (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                   (0 + (I(log(ts_harv_end+1)))|id) + 
                   (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                   (0 + I(log(distlf_end+1))|id) + 
                   (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
                   (0 + I(log(distlf_other_end+1))|id) + 
                   (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
                   (0 + disturbance_end|id) +
                   (0 + I(log(sl_+1)):disturbance_start|id) +
                   (1|year),
                 family = poisson(), data = sk,
                 map= list(theta = factor(c(NA,1:22))),
                 start = list(theta =c(log(1000), seq(0,0, length.out = 22)))
)



summary(m.sk)
saveRDS(m.sk, file.path(derived, 'mod_selmove_sk.RDS'))

#### mb ----
gc()

mb[,.(needle =mean(prop_needleleaf_end, na.rm = T), mixforest =mean(prop_mixforest_end, na.rm = T), veg = mean(prop_veg_end, na.rm = T),
       open = mean(prop_open_end, na.rm = T), wets = mean(prop_wets_end, na.rm = T),
       openbarren = mean(prop_openbarren_end, na.rm = T), otherveg = mean(prop_otherveg_end, na.rm = T))]

mb[,.(sl = mean(sl_, na.rm =T), distlf_end =mean(distlf_end, na.rm = T), distlf_other_end = mean(distlf_other_end, na.rm = T),
       disturbance = mean(disturbance_end, na.rm = T), tsf = mean(ts_fires_end, na.rm = T),
       tsh = mean(ts_harv_end, na.rm = T))]


mb.sub <- mb[sl_<=10000]
mb.sub[, year:= as.factor(year)]
mb.sub[,.(needle =mean(prop_needleleaf_end, na.rm = T), mixforest =mean(prop_mixforest_end, na.rm = T), veg = mean(prop_veg_end, na.rm = T),
      open = mean(prop_open_end, na.rm = T), wets = mean(prop_wets_end, na.rm = T),
      openbarren = mean(prop_openbarren_end, na.rm = T), otherveg = mean(prop_otherveg_end, na.rm = T))]

mb.sub[,.(sl = mean(sl_, na.rm =T), distlf_end =mean(distlf_end, na.rm = T), distlf_other_end = mean(distlf_other_end, na.rm = T),
      disturbance = mean(disturbance_end, na.rm = T), tsf = mean(ts_fires_end, na.rm = T),
      tsh = mean(ts_harv_end, na.rm = T))]

mb.sub[,.(sl = mean(sl_), distlf_end =mean(distlf_end), distlf_other_end = mean(distlf_other_end),
          disturbance = mean(disturbance_end), tsf = mean(ts_fires_end),
          tsh = mean(ts_harv_end))]

mb.sub[, prop_needlewet_end := prop_needleleaf_end + prop_wettreed_end]
mb.sub[, prop_needlewet_start := prop_needleleaf_start + prop_wettreed_start]

mb.2010 <- dat[jurisdiction == 'mb' & int.year ==2010]
mb.2010[,id:=as.factor(id)]
mb.2010[,indiv_step_id := as.factor(indiv_step_id)]
mb.2010[, year:= as.factor(year)]

mb.2015 <- dat[jurisdiction == 'mb' & int.year ==2015]
mb.2015[,id:=as.factor(id)]
mb.2015[,indiv_step_id := as.factor(indiv_step_id)]

mb.201719 <- dat[jurisdiction == 'mb' & year >=2017 & year<2019]
mb.201719[,id:=as.factor(id)]
mb.201719[,indiv_step_id := as.factor(indiv_step_id)]

mb.201720 <- dat[jurisdiction == 'mb' & year >=2017 & year<=2020]
mb.201720[,id:=as.factor(id)]
mb.201720[,indiv_step_id := as.factor(indiv_step_id)]


mb.2mil <- dat[jurisdiction == 'mb'][1:2000000]
mb.2mil[,id:=as.factor(id)]
mb.2mil[,indiv_step_id := as.factor(indiv_step_id)]
mb.2mil[, year:= as.factor(year)]

require(peakRAM)
gc()
p1 <- peakRAM(m.mb.2mil <- glmmTMB(case_ ~ -1 +
                  I(log(sl_+1)) +
                  I(cos(ta_)) +
                  I(log(sl_+1)):I(cos(ta_)) +
                  prop_needleleaf_start:I(log(sl_+1)) + 
                  prop_mixforest_start:I(log(sl_+1)) + 
                  prop_veg_start:I(log(sl_+1)) + 
                  prop_wets_start:I(log(sl_+1)) +
                  prop_needleleaf_end +
                  prop_mixforest_end +
                  prop_veg_end +
                  prop_wets_end +
                  I(log(ts_fires_end+1)) + 
                  I(log(sl_+1)):I(log(ts_fires_start+1)) +
                  I(log(ts_harv_end+1)) + 
                  I(log(sl_+1)):I(log(ts_harv_start+1)) +
                  I(log(distlf_end+1)) + 
                  I(log(sl_+1)):I(log(distlf_start+1)) +
                  I(log(distlf_other_end+1)) + 
                  I(log(sl_+1)):I(log(distlf_other_start+1)) +
                  disturbance_end +
                  I(log(sl_+1)):disturbance_start +
                  (1|indiv_step_id) +
                  (0 + I(log(sl_ +1))|id) +
                  (0 + I(cos(ta_))|id) +
                  (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                  (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                  (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
                  (0 + prop_veg_start:I(log(sl_+1))|id) + 
                  (0 + prop_wets_start:I(log(sl_+1))|id) +
                  (0 + prop_needleleaf_end|id) +
                  (0 + prop_mixforest_end|id) +
                  (0 + prop_veg_end|id) +
                  (0 + prop_wets_end|id) +
                  (0 + (I(log(ts_fires_end+1)))|id) +
                  (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                  (0 + (I(log(ts_harv_end+1)))|id) + 
                  (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                  (0 + I(log(distlf_end+1))|id) + 
                  (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
                  (0 + I(log(distlf_other_end+1))|id) + 
                  (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
                  (0 + disturbance_end|id) +
                  (0 + I(log(sl_+1)):disturbance_start|id) #+
                 # (1|year)
                  ,
                family = poisson(), data = mb.2mil,
                map= list(theta = factor(c(NA,1:21))),
                start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
)
)


summary(m.mb.2010)
saveRDS(m.mb.2010, file.path(derived, 'mod_selmove_mb_2010.RDS'))

require(disk.frame)
mb.2010.disk <- as.disk.frame(mb.2010, outdir = "tmp_mb", 
                               overwrite = TRUE)

p3 <- peakRAM(m.mb.2010<- glmmTMB(case_ ~ -1 +
                                      I(log(sl_+1)) +
                                      I(cos(ta_)) +
                                      I(log(sl_+1)):I(cos(ta_)) +
                                      prop_needleleaf_start:I(log(sl_+1)) + 
                                      prop_mixforest_start:I(log(sl_+1)) + 
                                      prop_veg_start:I(log(sl_+1)) + 
                                      prop_wets_start:I(log(sl_+1)) +
                                      prop_needleleaf_end +
                                      prop_mixforest_end +
                                      prop_veg_end +
                                      prop_wets_end +
                                      I(log(ts_fires_end+1)) + 
                                      I(log(sl_+1)):I(log(ts_fires_start+1)) +
                                      I(log(ts_harv_end+1)) + 
                                      I(log(sl_+1)):I(log(ts_harv_start+1)) +
                                      I(log(distlf_end+1)) + 
                                      I(log(sl_+1)):I(log(distlf_start+1)) +
                                      I(log(distlf_other_end+1)) + 
                                      I(log(sl_+1)):I(log(distlf_other_start+1)) +
                                      disturbance_end +
                                      I(log(sl_+1)):disturbance_start +
                                      (1|indiv_step_id) +
                                      (0 + I(log(sl_ +1))|id) +
                                      (0 + I(cos(ta_))|id) +
                                      (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                                      (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                                      (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
                                      (0 + prop_veg_start:I(log(sl_+1))|id) + 
                                      (0 + prop_wets_start:I(log(sl_+1))|id) +
                                      (0 + prop_needleleaf_end|id) +
                                      (0 + prop_mixforest_end|id) +
                                      (0 + prop_veg_end|id) +
                                      (0 + prop_wets_end|id) +
                                      (0 + (I(log(ts_fires_end+1)))|id) +
                                      (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                                      (0 + (I(log(ts_harv_end+1)))|id) + 
                                      (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                                      (0 + I(log(distlf_end+1))|id) + 
                                      (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
                                      (0 + I(log(distlf_other_end+1))|id) + 
                                      (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
                                      (0 + disturbance_end|id) +
                                      (0 + I(log(sl_+1)):disturbance_start|id) #+
                                    #(1|year)
                                    ,
                                    family = poisson(), data = mb.2010.disk,
                                    map= list(theta = factor(c(NA,1:21))),
                                    start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
)
)


require(disk.frame)
mb.15mil.disk <- as.disk.frame(mb.15mil, outdir = "tmp_mb", 
                          overwrite = TRUE)

p2 <- peakRAM(m.mb.15mil.disk <- glmmTMB(case_ ~ -1 +
                                      I(log(sl_+1)) +
                                      I(cos(ta_)) +
                                      I(log(sl_+1)):I(cos(ta_)) +
                                      prop_needleleaf_start:I(log(sl_+1)) + 
                                      prop_mixforest_start:I(log(sl_+1)) + 
                                      prop_veg_start:I(log(sl_+1)) + 
                                      prop_wets_start:I(log(sl_+1)) +
                                      prop_needleleaf_end +
                                      prop_mixforest_end +
                                      prop_veg_end +
                                      prop_wets_end +
                                      I(log(ts_fires_end+1)) + 
                                      I(log(sl_+1)):I(log(ts_fires_start+1)) +
                                      I(log(ts_harv_end+1)) + 
                                      I(log(sl_+1)):I(log(ts_harv_start+1)) +
                                      I(log(distlf_end+1)) + 
                                      I(log(sl_+1)):I(log(distlf_start+1)) +
                                      I(log(distlf_other_end+1)) + 
                                      I(log(sl_+1)):I(log(distlf_other_start+1)) +
                                      disturbance_end +
                                      I(log(sl_+1)):disturbance_start +
                                      (1|indiv_step_id) +
                                      (0 + I(log(sl_ +1))|id) +
                                      (0 + I(cos(ta_))|id) +
                                      (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                                      (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                                      (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
                                      (0 + prop_veg_start:I(log(sl_+1))|id) + 
                                      (0 + prop_wets_start:I(log(sl_+1))|id) +
                                      (0 + prop_needleleaf_end|id) +
                                      (0 + prop_mixforest_end|id) +
                                      (0 + prop_veg_end|id) +
                                      (0 + prop_wets_end|id) +
                                      (0 + (I(log(ts_fires_end+1)))|id) +
                                      (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                                      (0 + (I(log(ts_harv_end+1)))|id) + 
                                      (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                                      (0 + I(log(distlf_end+1))|id) + 
                                      (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
                                      (0 + I(log(distlf_other_end+1))|id) + 
                                      (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
                                      (0 + disturbance_end|id) +
                                      (0 + I(log(sl_+1)):disturbance_start|id) #+
                                    #(1|year)
                                    ,
                                    family = poisson(), data = mb.15mil.disk,
                                    map= list(theta = factor(c(NA,1:21))),
                                    start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
)
)





m.mb <- glmmTMB(case_ ~ -1 +
                   I(log(sl_+1)) +
                   #I(cos(ta_)) +
                   I(log(sl_+1)):I(cos(ta_)) +
                   #prop_needleleaf_start:I(log(sl_+1)) +
                   prop_needlewet_start:I(log(sl_+1)) +
                   prop_mixforest_start:I(log(sl_+1)) +
                   prop_veg_start:I(log(sl_+1)) +
                   #prop_wets_start:I(log(sl_+1)) +
                  # prop_needleleaf_end +
                  prop_needlewet_end +
                   prop_mixforest_end +
                   prop_veg_end +
                  # prop_wets_end +
                   I(log(ts_fires_end+1)) + 
                   I(log(sl_+1)):I(log(ts_fires_start+1)) +
                   I(log(ts_harv_end+1)) + 
                   I(log(sl_+1)):I(log(ts_harv_start+1)) +
                   I(log(distlf_end+1)) + 
                   I(log(sl_+1)):I(log(distlf_start+1)) +
                   I(log(distlf_other_end+1)) + 
                 #  I(log(sl_+1)):I(log(distlf_other_start+1)) +
                   disturbance_end +
                   I(log(sl_+1)):disturbance_start +
                   (1|indiv_step_id) +
                   (0 + I(log(sl_ +1))|id) +
                   #(0 + I(cos(ta_))|id) +
                   (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                   #(0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                  (0 + prop_needlewet_start:I(log(sl_+1))|id) + 
                  (0 + prop_mixforest_start:I(log(sl_+1))|id) +
                   (0 + prop_veg_start:I(log(sl_+1))|id) +
                  # (0 + prop_wets_start:I(log(sl_+1))|id) +
                   # (0 + prop_needleleaf_end|id) +
                  (0 + prop_needlewet_end|id) +
                   (0 + prop_mixforest_end|id) +
                   (0 + prop_veg_end|id) +
                  # (0 + prop_wets_end|id) +
                   (0 + (I(log(ts_fires_end+1)))|id) +
                   (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                   (0 + (I(log(ts_harv_end+1)))|id) + 
                   (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                   (0 + I(log(distlf_end+1))|id) + 
                   (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
                   (0 + I(log(distlf_other_end+1))|id) + 
                   (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
                   (0 + disturbance_end|id) +
                   #(0 + I(log(sl_+1)):disturbance_start|id) +
                   (1|year),
                 family = poisson(), data = mb.sub,
                 map= list(theta = factor(c(NA,1:19))),
                 start = list(theta =c(log(1000), seq(0,0, length.out = 19)))
)



summary(m.mb)
saveRDS(m.mb, file.path(derived, 'mod_selmove_mb.RDS'))
