## explore availability ----

require(data.table)

### Input data ----
raw <- file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

dat.derive <- targets::tar_read(stepID)

dat.derive[case_==TRUE,.(quantile(sl_)), by = .(jurisdiction)]

dat.derive[, dt_num := as.numeric(dt_)]
dat.derive[case_==TRUE,.(mean(sl_/dt_num)), by = .(jurisdiction, int.year)]

test <- dat.derive[sl_>12000, .N, by = .(indiv_step_id)]

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
                    sl_ = as.integer(sl_), ta_, id, jurisdiction, pop, t1_, t2_, case_, step_id_ = as.integer(step_id_),
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