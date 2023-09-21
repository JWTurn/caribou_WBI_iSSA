## iSSA ----

require(targets)

#require(Require)
require(data.table)
require(glmmTMB)
require(broom.mixed)
require(performance)
require(ggplot2)

### Input data ----
raw <- file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

# prepped <- readRDS(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS'))
# meta <- unique(prepped[,.(id, jurisdiction, pop=tolower(gsub(' ', '.',pop)), subpop)])

dat.derive <- tar_read(stepID)
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

# TODO should wettreed be in wets or mixforest?
# not include snow

## this was for Canada LCC (landsat)
# dat[,.(needle =mean(prop_needleleaf_end, na.rm = T), decid = mean(prop_deciduous_end, na.rm = T),
#           mixed = mean(prop_mixed_end, na.rm = T), shrub = mean(prop_shrub_end, na.rm = T),
#           grass = mean(prop_grassland_end, na.rm = T), lichshrub = mean(prop_lichenshrub_end, na.rm = T),
#           lichgrass = mean(prop_lichengrass_end, na.rm = T), crop = mean(prop_cropland_end, na.rm = T),
#           wetland = mean(prop_wetland_end, na.rm = T), barren = mean(prop_barrenland_end, na.rm = T),
#           urban = mean(prop_urban_end, na.rm = T), water = mean(prop_water_end, na.rm = T),
#        snow = mean(prop_snow_end, na.rm = T)), by = .(int.year, jurisdiction)]
# not include urban or cropland
# dat[, prop_forest_start := prop_needleleaf_start + prop_deciduous_start + prop_mixed_start]
# dat[, prop_forage_start := prop_shrub_start + prop_grassland_start + prop_lichenshrub_start + prop_lichengrass_start]
# dat[, prop_open_start := prop_barrenland_start + prop_snow_start]
# dat[, prop_wets_start := prop_wetland_start + prop_water_start]
# 
# dat[, prop_forest_end := prop_needleleaf_end + prop_deciduous_end + prop_mixed_end]
# dat[, prop_forage_end := prop_shrub_end + prop_grassland_end + prop_lichenshrub_end + prop_lichengrass_end]
# dat[, prop_open_end := prop_barrenland_end + prop_snow_end]
# dat[, prop_wets_end := prop_wetland_end + prop_water_end]

dat[,.(needle =mean(prop_needleleaf_end, na.rm = T), mixforest =mean(prop_mixforest_end, na.rm = T), veg = mean(prop_veg_end, na.rm = T),
       open = mean(prop_open_end, na.rm = T), wets = mean(prop_wets_end, na.rm = T),
       openbarren = mean(prop_openbarren_end, na.rm = T), otherveg = mean(prop_otherveg_end, na.rm = T)), 
    by = .(int.year, jurisdiction)]

dat[,.(distlf_end =mean(distlf_end, na.rm = T), distlf_other_end = mean(distlf_other_end, na.rm = T),
       disturbance = mean(disturbance_end, na.rm = T), tsf = mean(ts_fires_end, na.rm = T),
       tsh = mean(ts_harv_end, na.rm = T)), 
    by = .(int.year, jurisdiction)]
## prop_open too low availability for most


# change arbitrary time since variable
hist(dat[case_==TRUE]$ts_harv_end)
hist(dat[case_==FALSE]$ts_harv_end)
hist(dat[case_==TRUE & jurisdiction == 'mb']$ts_harv_end)
hist(dat[case_==FALSE & jurisdiction == 'mb']$ts_harv_end)
ggplot(data = dat, aes(case_, ts_harv_end, color = jurisdiction)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~int.year)

hist(dat[case_==TRUE]$ts_fires_end)
hist(dat[case_==FALSE]$ts_fires_end)
hist(dat[case_==TRUE & jurisdiction == 'mb']$ts_fires_end)
hist(dat[case_==FALSE & jurisdiction == 'mb']$ts_fires_end)
ggplot(data = dat, aes(case_, ts_fires_end, color = jurisdiction)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~int.year)

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


range(dat$year)
setindex(dat, NULL)
# yr <- dat[case_==TRUE, .(year)]
# hist(yr$year)
dat.2010 <- dat[int.year==2010]
dat.2015 <- dat[int.year==2015]
#dat.2020 <- dat[int.year==2020]

dat.2010[,id:=as.factor(id)]
dat.2010[,indiv_step_id := as.factor(indiv_step_id)]
dat.2010[,jurisdiction := as.factor(jurisdiction)]

dat.2015[,id:=as.factor(id)]
dat.2015[,indiv_step_id := as.factor(indiv_step_id)]
dat.2015[,jurisdiction := as.factor(jurisdiction)]

# dat.2020[,id:=as.factor(id)]
# dat.2020[,indiv_step_id := as.factor(indiv_step_id)]
# dat.2020[,jurisdiction := as.factor(jurisdiction)]


##TODO incorporate season?

## models ----

gc()
### selection 2010  ----
m1 <- glmmTMB(case_ ~ -1 +
                I(log(sl_+1)) +
                prop_needleleaf_end +
                prop_mixforest_end +
                prop_veg_end + 
                prop_wets_end +
                I(log(ts_fires_end+1)) +
                I(log(ts_harv_end+1)) +
                I(log(distlf_end+1)) + I(log(distlf_other_end+1)) + disturbance_end +
                (1|indiv_step_id) +
                (0 + I(log(sl_ +1))|id) +
                (0 + prop_needleleaf_end|id) +
                (0 + prop_mixforest_end|id) +
                (0 + prop_veg_end|id) +
                (0 + prop_wets_end|id) +
                (0 + I(log(ts_fires_end+1))|id) +
                (0 + I(log(ts_harv_end+1))|id) +
                (0 + I(log(distlf_end+1))|id) + (0 + I(log(distlf_other_end+1))|id) + (0 + disturbance_end|id) +
                (1|jurisdiction),
              family = poisson(), data = dat.2010,
              map= list(theta = factor(c(NA,1:11))),
              start = list(theta =c(log(1000), seq(0,0, length.out = 11)))
)


## This runs for 2010, but not 2015...
# m1 <- glmmTMB(case_ ~ -1 +
#                 I(log(sl_+1)) +
#                 prop_needleleaf_end + prop_mixforest_end + 
#                 prop_veg_end + prop_wets_end +
#                 I(log(ts_fires_end+1)) + I(log(sl_+1)):I(log(ts_fires_end+1)) +
#                 I(log(ts_harv_end+1)) + I(log(sl_+1)):I(log(ts_harv_end+1)) +
#                 I(log(distlf_end+1)) + I(log(distlf_other_end+1)) + disturbance_end +
#                 (1|indiv_step_id) +
#                 (0 + I(log(sl_ +1))|id) +
#                 (0 + prop_needleleaf_end|id) + (0 + prop_mixforest_end|id) + 
#                 (0 + prop_veg_end|id) + (0 + prop_wets_end|id) +
#                 (0 + I(log(ts_fires_end+1))|id) + (0 + I(log(sl_+1)):I(log(ts_fires_end+1))|id) +
#                 (0 + I(log(ts_harv_end+1))|id) + (0 + I(log(sl_+1)):I(log(ts_harv_end+1))|id) + 
#                 (0 + I(log(distlf_end+1))|id) + (0 + I(log(distlf_other_end+1))|id) + (0 + disturbance_end|id) +
#                 (1|jurisdiction),
#               family = poisson(), data = dat.2010,
#               map= list(theta = factor(c(NA,1:13))),
#               start = list(theta =c(log(1000), seq(0,0, length.out = 13)))
# )



summary(m1)
saveRDS(m1, file.path(derived, 'mod_sel_2010-2015.RDS'))
check_model(m1)

gc()


m1.2 <- glmmTMB(case_ ~ -1 +
                I(log(sl_+1)) +
                prop_needleleaf_end + prop_mixforest_end + 
                prop_veg_end + prop_wets_end +
                I(log(ts_fires_end+1)) + I(log(sl_+1)):I(log(ts_fires_start+1)) +
                I(log(ts_harv_end+1)) + I(log(sl_+1)):I(log(ts_harv_start+1)) +
                I(log(distlf_end+1)) + I(log(distlf_other_end+1)) + disturbance_end +
                (1|indiv_step_id) +
                (0 + I(log(sl_ +1))|id) +
                (0 + prop_needleleaf_end|id) + (0 + prop_mixforest_end|id) + 
                (0 + prop_veg_end|id) + (0 + prop_wets_end|id) +
                (0 + I(log(ts_fires_end+1))|id) + (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                (0 + I(log(ts_harv_end+1))|id) + (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                (0 + I(log(distlf_end+1))|id) + (0 + I(log(distlf_other_end+1))|id) + (0 + disturbance_end|id) +
                (1|jurisdiction),
              family = poisson(), data = dat.2010,
              map= list(theta = factor(c(NA,1:13))),
              start = list(theta =c(log(1000), seq(0,0, length.out = 13)))
)



summary(m1.2)
saveRDS(m1.2, file.path(derived, 'mod_selmove_2010-2015.RDS'))


gc()

### selection 2015 ----
dat.2015[,.(needle =mean(prop_needleleaf_end, na.rm = T), mixforest =mean(prop_mixforest_end, na.rm = T), veg = mean(prop_veg_end, na.rm = T),
       open = mean(prop_open_end, na.rm = T), wets = mean(prop_wets_end, na.rm = T),
       openbarren = mean(prop_openbarren_end, na.rm = T), otherveg = mean(prop_otherveg_end, na.rm = T)), 
    by = .(int.year, jurisdiction)]

dat.2015[,.(log_sl = mean(log(sl_+1), na.rm = T), distlf_end =mean(log(distlf_end+1), na.rm = T), distlf_other_end = mean(log(distlf_other_end+1), na.rm = T),
       disturbance = mean(disturbance_end, na.rm = T), tsf = mean(log(ts_fires_end+1), na.rm = T),
       tsh = mean(log(ts_harv_end+1), na.rm = T)), 
    by = .(int.year, jurisdiction)]

quantile(dat.2015$sl_)
quantile(dat.2015$sl_, probs = c(0.01, 0.05, 0.95, 0.99))

quantile(dat.2015$ts_fires_end)
quantile(dat.2015$ts_harv_end)

dat.2015[, quantile(ts_fires_end), by = .(jurisdiction)]

quantile(dat.2010$sl_)
quantile(dat.2010$sl_, probs = c(0.01, 0.05, 0.95, 0.99))

# filter out steps outside 99% quantile
# dat.2015<- dat.2015[sl_ <= 7060]
dat.2015[case_==TRUE, median(sl_)]

gc()

m2 <- glmmTMB(case_ ~ -1 +
                I(log(sl_+1)) +
                prop_needleleaf_end +
                prop_mixforest_end +
                prop_veg_end + 
                prop_wets_end +
                I(log(ts_fires_end+1)) +
                I(log(ts_harv_end+1)) +
                I(log(distlf_end+1)) + I(log(distlf_other_end+1)) + disturbance_end +
                (1|indiv_step_id) +
                (0 + I(log(sl_ +1))|id) +
                (0 + prop_needleleaf_end|id) +
                (0 + prop_mixforest_end|id) +
                (0 + prop_veg_end|id) +
                (0 + prop_wets_end|id) +
                (0 + I(log(ts_fires_end+1))|id) +
                (0 + I(log(ts_harv_end+1))|id) +
                (0 + I(log(distlf_end+1))|id) + (0 + I(log(distlf_other_end+1))|id) + (0 + disturbance_end|id) +
                (1|jurisdiction),
              family = poisson(), data = dat.2015,
              map= list(theta = factor(c(NA,1:11))),
              start = list(theta =c(log(1000), seq(0,0, length.out = 11)))
)


summary(m2)
check_model(m2)
saveRDS(m2, file.path(derived, 'mod_sel_2015-2020.RDS'))

gc()
m2.scale <- glmmTMB(case_ ~ -1 +
                scale(I(log(sl_+1))) +
                scale(prop_needleleaf_end) + 
                scale(prop_mixforest_end) +
                #scale(prop_otherveg_end) +
                scale(prop_veg_end) + 
                scale(prop_wets_end) +
                scale(I(log(ts_fires_end+1))) + scale(I(log(sl_+1))):scale(I(log(ts_fires_end+1))) +
                scale(I(log(ts_harv_end+1))) + scale(I(log(sl_+1))):scale(I(log(ts_harv_end+1))) +
                scale(I(log(distlf_end+1))) + scale(I(log(distlf_other_end+1))) + disturbance_end +
               # scale(I(log(sl_+1))):(scale(I(log(distlf_end+1))) + scale(I(log(distlf_other_end+1))) + disturbance_end) +
                (1|indiv_step_id) +
                (0 + scale(I(log(sl_ +1)))|id) +
                (0 + scale(prop_needleleaf_end)|id) + 
                (0 + scale(prop_mixforest_end)|id) +
                #(0 + scale(prop_otherveg_end)|id) +
                (0 + scale(prop_veg_end)|id) +
                (0 + scale(prop_wets_end)|id) +
                (0 + scale(I(log(ts_fires_end+1)))|id) + (0 + scale(I(log(sl_+1))):scale(I(log(ts_fires_end+1)))|id) +
                (0 + scale(I(log(ts_harv_end+1)))|id) + (0 + scale(I(log(sl_+1))):scale(I(log(ts_harv_end+1)))|id) +
                (0 + scale(I(log(distlf_end+1)))|id) + (0 + scale(I(log(distlf_other_end+1)))|id) + (0 + disturbance_end|id) +
              #(0 + scale(I(log(sl_+1))):(scale(I(log(distlf_end+1))) + scale(I(log(distlf_other_end+1))) + disturbance_end)) +
                (1|jurisdiction),
              family = poisson(), data = dat.2015,
              map= list(theta = factor(c(NA,1:13))),
              start = list(theta =c(log(1000), seq(0,0, length.out = 13)))
)



summary(m2.scale)
saveRDS(m2.scale, file.path(derived, 'mod_sel_TESTscale_2015-2020.RDS'))
# 
# m2 <- glmmTMB(case_ ~ -1 +
#                 scale(I(log(sl_+1))) +
#                 (prop_needleleaf_end) + (prop_mixforest_end) + 
#                 (prop_veg_end) + (prop_wets_end) +
#                 scale(I(log(ts_fires_end+1))) + #scale(I(log(sl_+1))):scale(I(log(ts_fires_end+1))) +
#                 scale(I(log(ts_harv_end+1))) + #scale(I(log(sl_+1))):scale(I(log(ts_harv_end+1))) +
#                 scale(I(log(distlf_end+1))) + scale(I(log(distlf_other_end+1))) + disturbance_end +
#                # scale(I(log(sl_+1))):(scale(I(log(distlf_end+1))) + scale(I(log(distlf_other_end+1))) + disturbance_end) +
#                 (1|indiv_step_id) +
#                 (0 + scale(I(log(sl_ +1)))|id) +
#                 (0 + (prop_needleleaf_end)|id) + (0 + (prop_mixforest_end)|id) + 
#                 (0 + (prop_veg_end)|id) + (0 + (prop_wets_end)|id) +
#                 (0 + scale(I(log(ts_fires_end+1)))|id) + #(0 + scale(I(log(sl_+1))):scale(I(log(ts_fires_end+1)))|id) +
#                 (0 + scale(I(log(ts_harv_end+1)))|id) + #(0 + scale(I(log(sl_+1))):scale(I(log(ts_harv_end+1)))|id) + 
#                 (0 + scale(I(log(distlf_end+1)))|id) + (0 + scale(I(log(distlf_other_end+1)))|id) + (0 + disturbance_end|id) +
#                 #(0 + scale(I(log(sl_+1))):(scale(I(log(distlf_end+1))) + scale(I(log(distlf_other_end+1))) + disturbance_end)) +
#                 (1|jurisdiction),
#               family = poisson(), data = dat.2015,
#               map= list(theta = factor(c(NA,1:11))), 
#               start = list(theta =c(log(1000), seq(0,0, length.out = 11)))
# )
# 
# 
# 
# summary(m2)
# saveRDS(m2, file.path(derived, 'mod_sel_TESTsimp_2015-2020.RDS'))


gc()


m2.2 <- glmmTMB(case_ ~ -1 +
                  I(log(sl_+1)) +
                 # I(cos(ta_)) +
                  I(log(sl_+1)):I(cos(ta_)) +
                  prop_needleleaf_start:I(log(sl_+1)) + 
                  prop_mixforest_start:I(log(sl_+1)) + 
                  prop_veg_start:I(log(sl_+1)) + 
                  prop_wets_start:I(log(sl_+1)) +
                 # prop_needleleaf_end +
                  #prop_mixforest_end +
                 # prop_veg_end +
                 # prop_wets_end +
                  I(log(ts_fires_end+1)) + scale(I(ts_fires_end^2)) +
                  #I(log(sl_+1)):I(log(ts_fires_start+1)) +
                  I(log(ts_harv_end+1)) + 
                  #I(log(sl_+1)):I(log(ts_harv_start+1)) +
                  I(log(distlf_end+1)) + 
                  #I(log(sl_+1)):I(log(distlf_start+1)) +
                  I(log(distlf_other_end+1)) + 
                 # I(log(sl_+1)):I(log(distlf_other_start+1)) +
                  disturbance_end +
                #  I(log(sl_+1)):disturbance_start +
                  (1|indiv_step_id) +
                  (0 + I(log(sl_ +1))|jurisdiction/id) +
                 # (0 + I(cos(ta_))|jurisdiction/id) +
                  (0 + I(log(sl_+1)):I(cos(ta_))|jurisdiction/id) +
                  (0 + prop_needleleaf_start:I(log(sl_+1))|jurisdiction/id) + 
                  (0 + prop_mixforest_start:I(log(sl_+1))|jurisdiction/id) + 
                  (0 + prop_veg_start:I(log(sl_+1))|jurisdiction/id) + 
                  (0 + prop_wets_start:I(log(sl_+1))|jurisdiction/id) +
                  #(0 + prop_needleleaf_end|jurisdiction/id) +
                  #(0 + prop_mixforest_end|jurisdiction/id) +
                 # (0 + prop_veg_end|jurisdiction/id) +
                  #(0 + prop_wets_end|jurisdiction/id) +
                  (0 + (I(log(ts_fires_end+1)) + scale(I(ts_fires_end^2)))|jurisdiction/id) +
                  #(0 + I(log(sl_+1)):I(log(ts_fires_start+1))|jurisdiction/id) +
                  (0 + (I(log(ts_harv_end+1)))|jurisdiction/id) + 
                  #(0 + I(log(sl_+1)):I(log(ts_harv_start+1))|jurisdiction/id) + 
                  (0 + I(log(distlf_end+1))|jurisdiction/id) + 
                  #(0 + I(log(sl_+1)):I(log(distlf_start+1))|jurisdiction/id) +
                  (0 + I(log(distlf_other_end+1))|jurisdiction/id) + 
                 # (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|jurisdiction/id) +
                  (0 + disturbance_end|jurisdiction/id) +
                 # (0 + I(log(sl_+1)):disturbance_start|jurisdiction/id) +
                  (1|jurisdiction),
                family = poisson(), data = dat.2015,
                map= list(theta = factor(c(NA,1:27))),
                start = list(theta =c(log(1000), seq(0,0, length.out = 27)))
)



summary(m2.2)
saveRDS(m2.2, file.path(derived, 'mod_selmove_2015-2020.RDS'))


### selection 2010 jurisdictional w/squared terms ----
m3 <- glmmTMB(case_ ~ -1 +
                I(log(sl_+1)) +
                prop_forest_end + prop_forage_end + prop_wets_end +
                # prop_open_end + 
                I(log(ts_fires_end+1)) +
                scale(I(ts_fires_end^2)) +
                I(log(ts_harv_end+1)) +
                scale(I(ts_harv_end^2)) +
                I(log(distlf_end+1)) +
                I(log(distlf_other_end+1)) +
                disturbance_end +
                (1|indiv_step_id) +
                (0 + I(log(sl_ +1))|id) +
                (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                #(0 + prop_open_end|id) + 
                (0 + prop_wets_end|id) +
                (0 + I(log(ts_fires_end+1))|id) +
                (0 + scale(I(ts_fires_end^2))|id) +
                (0 + I(log(ts_harv_end+1))|id) +
                (0 + scale(I(ts_harv_end)^2)|id) +
                (0 + I(log(distlf_end+1))|id) +
                (0 + I(log(distlf_other_end+1))|id) +
                (0 + disturbance_end|id) +
                (1|jurisdiction),
              family = poisson(), data = dat.2010,
              map= list(theta = factor(c(NA,1:12))), 
              start = list(theta =c(log(1000), seq(0,0, length.out = 12)))
)



summary(m3)
saveRDS(m3, file.path(derived, 'mod_sel_jurisREsqd_2010-2015.RDS'))


gc()

### selection 2015 jurisdictional w/squared terms ----
### DOESN'T CONVERGE
m4 <- glmmTMB(case_ ~ -1 +
                I(log(sl_+1)) +
                prop_forest_end + prop_forage_end + prop_wets_end +
                # prop_open_end + 
                scale(ts_fires_end) +
                scale(I(ts_fires_end^2)) +
                scale(ts_harv_end) +
                scale(I(ts_harv_end^2)) +
                I(log(distlf_end+1)) +
                I(log(distlf_other_end+1)) +
                disturbance_end +
                (1|indiv_step_id) +
                (0 + I(log(sl_ +1))|id) +
                (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                #(0 + prop_open_end|id) + 
                (0 + prop_wets_end|id) +
                (0 + scale(ts_fires_end)|id) +
                (0 + scale(I(ts_fires_end^2))|id) +
                (0 + scale(ts_harv_end)|id) +
                (0 + scale(I(ts_harv_end^2))|id) +
                (0 + I(log(distlf_end+1))|id) +
                (0 + I(log(distlf_other_end+1))|id) +
                (0 + disturbance_end|id) +
                (1|jurisdiction),
              family = poisson(), data = dat.2015,
              map= list(theta = factor(c(NA,1:12))), 
              start = list(theta =c(log(1000), seq(0,0, length.out = 12)))
)



summary(m4)
saveRDS(m4, file.path(derived, 'mod_sel_jurisREsqd_2015-2020.RDS'))


#######
m1 <- readRDS(file.path(derived, 'mod_sel_jurisRE_2010-2015.RDS'))
bbmle::AICtab(m1, m3)

### selection 2015 jurisdictional simplified ----
sel.2015.juris.simp <- glmmTMB(case_ ~ -1 +
                            I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_wets_end +
                             # prop_open_end + 
                               I(log(ts_fires_end+1)) +
                               I(log(distlf_end+1)) +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                            #(0 + prop_open_end|id) + 
                            (0 + prop_wets_end|id) +
                            (0 + I(log(ts_fires_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id) +
                            (1|jurisdiction),
                          family = poisson(), data = dat.2015,
                          map= list(theta = factor(c(NA,1:7))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 7)))
)



summary(sel.2015.juris.simp)
saveRDS(sel.2015.juris.simp, file.path(derived, 'mod_ssa_juris_simp_2015-2020.RDS'))



gc()
### selection 2010 jurisdictional FIXED effect only ----
sel.2010.juris.fixed <- glmmTMB(case_ ~ -1 +
                            (I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_wets_end +
                               #prop_open_end + 
                               I(log(ts_fires_end+1)) +
                               I(log(distlf_end+1))):jurisdiction +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                            #(0 + prop_open_end|id) + 
                            (0 + prop_wets_end|id) +
                            (0 + I(log(ts_fires_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id),
                          family = poisson(), data = dat.2010,
                          map= list(theta = factor(c(NA,1:6))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 6)))
)



summary(sel.2010.juris.fixed)
saveRDS(sel.2010.juris.fixed, file.path(derived, 'mod_ssa_juris_fixed_2010-2015.RDS'))

gc()
### selection 2015 jurisdictional FIXED effect only ----
sel.2015.juris.fixed <- glmmTMB(case_ ~ -1 +
                            (I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_wets_end +
                               #prop_open_end + 
                               I(log(ts_fires_end+1)) +
                               I(log(distlf_end+1))):jurisdiction +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                            #(0 + prop_open_end|id) + 
                            (0 + prop_wets_end|id) +
                            (0 + I(log(ts_fires_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id),
                          family = poisson(), data = dat.2015,
                          map= list(theta = factor(c(NA,1:6))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 6)))
)



summary(sel.2015.juris.fixed)
saveRDS(sel.2015.juris.fixed, file.path(derived, 'mod_ssa_juris_fixed_2015-2020.RDS'))




gc()
### selection 2010 ----
sel.2010 <- glmmTMB(case_ ~ -1 +
                            I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_wets_end +
                            #prop_open_end + 
                               I(log(ts_fires_end+1)) +
                               I(log(distlf_end+1)) +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                            #(0 + prop_open_end|id) + 
                            (0 + prop_wets_end|id) +
                            (0 + I(log(ts_fires_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id),
                          family = poisson(), data = dat.2010,
                          map= list(theta = factor(c(NA,1:6))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 6)))
)


summary(sel.2010)
saveRDS(sel.2010, file.path(derived, 'mod_ssa_2010-2015.RDS'))


gc()
### selection 2015 ----
sel.2015 <- glmmTMB(case_ ~ -1 +
                            I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_wets_end +
                      #prop_open_end +         
                      I(log(ts_fires_end+1)) +
                               I(log(distlf_end+1)) +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                           # (0 + prop_open_end|id) + 
                      (0 + prop_wets_end|id) +
                            (0 + I(log(ts_fires_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id),
                          family = poisson(), data = dat.2015,
                          map= list(theta = factor(c(NA,1:6))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 6)))
)



summary(sel.2015)
saveRDS(sel.2015, file.path(derived, 'mod_ssa_2015-2020.RDS'))

####
sel.2010 <- readRDS(file.path(derived, 'mod_ssa_2010-2015.RDS'))
sel.2015 <- readRDS(file.path(derived, 'mod_ssa_2015-2020.RDS'))

sel.2010.juris <- readRDS(file.path(derived, 'mod_ssa_juris_2010-2015.RDS'))
sel.2015.juris <- readRDS(file.path(derived, 'mod_ssa_juris_2015-2020.RDS'))

sel.2010.juris.simp <- readRDS(file.path(derived, 'mod_ssa_juris_simp_2010-2015.RDS'))
sel.2015.juris.simp <- readRDS(file.path(derived, 'mod_ssa_juris_simp_2015-2020.RDS'))

sel.2010.juris.fixed <- readRDS(file.path(derived, 'mod_ssa_juris_fixed_2010-2015.RDS'))
sel.2015.juris.fixed <- readRDS(file.path(derived, 'mod_ssa_juris_fixed_2015-2020.RDS'))

bbmle::AICtab(sel.2010, sel.2010.juris, sel.2010.juris.simp, sel.2010.juris.fixed)
bbmle::AICtab(sel.2015, sel.2015.juris, sel.2015.juris.simp, sel.2015.juris.fixed)

#####
gc()
### selection 2010 jurisdictional ----
sel.2010.juris <- glmmTMB(case_ ~ -1 +
                            (I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_wets_end +
                               #prop_open_end + 
                               I(log(ts_fires_end+1)) +
                               I(log(distlf_end+1))):jurisdiction +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                            #(0 + prop_open_end|id) + 
                            (0 + prop_wets_end|id) +
                            (0 + I(log(ts_fires_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id) +
                            (1|jurisdiction),
                          family = poisson(), data = dat.2010,
                          map= list(theta = factor(c(NA,1:7))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 7)))
)

#11
#22

summary(sel.2010.juris)
saveRDS(sel.2010.juris, file.path(derived, 'mod_ssa_juris_2010-2015.RDS'))

gc()
### selection 2015 jurisdictional ----
sel.2015.juris <- glmmTMB(case_ ~ -1 +
                            (I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_wets_end +
                               #prop_open_end + 
                               I(log(ts_fires_end+1)) +
                               I(log(distlf_end+1))):jurisdiction +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                            #(0 + prop_open_end|id) + 
                            (0 + prop_wets_end|id) +
                            (0 + I(log(ts_fires_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id) +
                            (1|jurisdiction),
                          family = poisson(), data = dat.2015,
                          map= list(theta = factor(c(NA,1:7))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 7)))
)

#11
#22

summary(sel.2015.juris)
saveRDS(sel.2015.juris, file.path(derived, 'mod_ssa_juris_2015-2020.RDS'))
