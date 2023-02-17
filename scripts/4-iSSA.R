## iSSA ----

require(targets)

#require(Require)
require(data.table)
require(glmmTMB)
require(broom.mixed)

### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

# prepped <- readRDS(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS'))
# meta <- unique(prepped[,.(id, jurisdiction, pop=tolower(gsub(' ', '.',pop)), subpop)])

dat <- tar_read(stepID)
# dat <- merge(dat, meta, by = 'id', all.x = T)
#saveRDS(stepID, file.path(derived, 'mb_derived.RDS'))


#dat <- stepID

#dat <- readRDS(file.path(derived, 'mb_derived.RDS'))

dat[,.(needle =mean(prop_needleleaf_end, na.rm = T), decid = mean(prop_deciduous_end, na.rm = T),
          mixed = mean(prop_mixed_end, na.rm = T), shrub = mean(prop_shrub_end, na.rm = T),
          grass = mean(prop_grassland_end, na.rm = T), lichshrub = mean(prop_lichenshrub_end, na.rm = T),
          lichgrass = mean(prop_lichengrass_end, na.rm = T), crop = mean(prop_cropland_end, na.rm = T),
          wetland = mean(prop_wetland_end, na.rm = T), barren = mean(prop_barrenland_end, na.rm = T),
          urban = mean(prop_urban_end, na.rm = T), water = mean(prop_water_end, na.rm = T),
       snow = mean(prop_snow_end, na.rm = T))]

# not include urban or cropland
dat[, prop_forest_start := prop_needleleaf_start + prop_deciduous_start + prop_mixed_start]
dat[, prop_forage_start := prop_shrub_start + prop_grassland_start + prop_lichenshrub_start + prop_lichengrass_start]
dat[, prop_open_start := prop_barrenland_start + prop_snow_start]
dat[, prop_wets_start := prop_wetland_start + prop_water_start]

dat[, prop_forest_end := prop_needleleaf_end + prop_deciduous_end + prop_mixed_end]
dat[, prop_forage_end := prop_shrub_end + prop_grassland_end + prop_lichenshrub_end + prop_lichengrass_end]
dat[, prop_open_end := prop_barrenland_end + prop_snow_end]
dat[, prop_wets_end := prop_wetland_end + prop_water_end]

# dat[,lc_end_adj := lc_end]
# dat[lc_end %in% c('barren', 'grassland', 'lichen-grass', 'lichen-shrub', 'shrub', 'urban', 'cropland'),lc_end_adj:= 'open-forage']
# dat[lc_end %in% c('deciduous', 'mixedforest'),lc_end_adj:= 'deciduous']
# dat[lc_end %in% c('water', 'wetland', 'snow'),lc_end_adj:= 'wet']

# dat[, lc_end_adj := factor(lc_end_adj)]
# 
# summary(dat$lc_end_adj)



# MB is too big for my computer to run all at once, so subset
# quantile(year(dat$t1_))
# 
# 

range(dat$year)
setindex(dat, NULL)
# yr <- dat[case_==TRUE, .(year)]
# hist(yr$year)
dat.2010 <- dat[int.year==2010]
dat.2015 <- dat[int.year==2015]
dat.2020 <- dat[int.year==2020]

dat.2010[,id:=as.factor(id)]
dat.2010[,indiv_step_id := as.factor(indiv_step_id)]
dat.2010[,jurisdiction := as.factor(jurisdiction)]

dat.2015[,id:=as.factor(id)]
dat.2015[,indiv_step_id := as.factor(indiv_step_id)]
dat.2015[,jurisdiction := as.factor(jurisdiction)]

dat.2020[,id:=as.factor(id)]
dat.2020[,indiv_step_id := as.factor(indiv_step_id)]
dat.2020[,jurisdiction := as.factor(jurisdiction)]


##TODO incorporate season?

## model ----
gc()
### selection 2010 jurisdictional ----
sel.2010.juris <- glmmTMB(case_ ~ -1 +
                            (I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_open_end + prop_wets_end +
                               I(log(tsf_end+1)) +
                               I(log(distlf_end+1))):jurisdiction +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                            (0 + prop_open_end|id) + (0 + prop_wets_end|id) +
                            (0 + I(log(tsf_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id) +
                            (1|jurisdiction),
                          family = poisson(), data = dat.2010,
                          map= list(theta = factor(c(NA,1:8))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 8)))
)

#11
#22

summary(sel.2010.juris)
saveRDS(sel.2010.juris, file.path(derived, 'mod_ssa_juris_2010-2015.RDS'))


### selection 2015 jurisdictional ----
sel.2015.juris <- glmmTMB(case_ ~ -1 +
             (I(log(sl_+1)) +
             prop_forest_end + prop_forage_end + prop_open_end + prop_wets_end +
             I(log(tsf_end+1)) +
             I(log(distlf_end+1))):jurisdiction +
             (1|indiv_step_id) +
             (0 + I(log(sl_ +1))|id) +
             (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
             (0 + prop_open_end|id) + (0 + prop_wets_end|id) +
             (0 + I(log(tsf_end+1))|id) +
             (0 + I(log(distlf_end+1))|id) +
             (1|jurisdiction),
             family = poisson(), data = dat.2015,
             map= list(theta = factor(c(NA,1:8))), 
             start = list(theta =c(log(1000), seq(0,0, length.out = 8)))
)

#11
#22

summary(sel.2015.juris)
saveRDS(sel.2015.juris, file.path(derived, 'mod_ssa_juris_2015-2020.RDS'))



gc()
### selection 2010 ----
sel.2010 <- glmmTMB(case_ ~ -1 +
                            I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_open_end + prop_wets_end +
                               I(log(tsf_end+1)) +
                               I(log(distlf_end+1)) +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                            (0 + prop_open_end|id) + (0 + prop_wets_end|id) +
                            (0 + I(log(tsf_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id),
                          family = poisson(), data = dat.2010,
                          map= list(theta = factor(c(NA,1:7))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 7)))
)


summary(sel.2010)
saveRDS(sel.2010, file.path(derived, 'mod_ssa_2010-2015.RDS'))


### selection 2015 ----
sel.2015 <- glmmTMB(case_ ~ -1 +
                            I(log(sl_+1)) +
                               prop_forest_end + prop_forage_end + prop_open_end + prop_wets_end +
                               I(log(tsf_end+1)) +
                               I(log(distlf_end+1)) +
                            (1|indiv_step_id) +
                            (0 + I(log(sl_ +1))|id) +
                            (0 + prop_forest_end|id) + (0 + prop_forage_end|id) + 
                            (0 + prop_open_end|id) + (0 + prop_wets_end|id) +
                            (0 + I(log(tsf_end+1))|id) +
                            (0 + I(log(distlf_end+1))|id),
                          family = poisson(), data = dat.2015,
                          map= list(theta = factor(c(NA,1:7))), 
                          start = list(theta =c(log(1000), seq(0,0, length.out = 7)))
)



summary(sel.2015)
saveRDS(sel.2015, file.path(derived, 'mod_ssa_2015-2020.RDS'))

####
sel.2010 <- readRDS(file.path(derived, 'mod_ssa_2010-2015.RDS'))
sel.2015 <- readRDS(file.path(derived, 'mod_ssa_2015-2020.RDS'))

sel.2010.juris <- readRDS(file.path(derived, 'mod_ssa_juris_2010-2015.RDS'))
sel.2015.juris <- readRDS(file.path(derived, 'mod_ssa_juris_2015-2020.RDS'))

bbmle::AICtab(sel.2010, sel.2010.juris)
bbmle::AICtab(sel.2015, sel.2015.juris)
