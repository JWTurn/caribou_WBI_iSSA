## iSSA HPC run----

require(data.table)
require(glmmTMB)
require(broom.mixed)

#### set up ---
parallelly::availableCores(constraints = "connections")
options(mc.cores = 16)
setDTthreads(100)

### Input data ----
raw <- file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))


range(dat$year)
#setindex(dat, NULL)
# yr <- dat[case_==TRUE, .(year)]
# hist(yr$year)
dat.2010 <- dat[int.year==2010]
#dat.2015 <- dat[int.year==2015]
#dat.2020 <- dat[int.year==2020]

dat.2010[,id:=as.factor(id)]
dat.2010[,indiv_step_id := as.factor(indiv_step_id)]
dat.2010[,jurisdiction := as.factor(jurisdiction)]
dat.2010[,year:=as.factor(year)]

# dat.2015[,id:=as.factor(id)]
# dat.2015[,indiv_step_id := as.factor(indiv_step_id)]
# dat.2015[,jurisdiction := as.factor(jurisdiction)]
# dat.2015[,year:=as.factor(year)]

m2 <- glmmTMB(case_ ~ -1 +
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
                     (1|jurisdiction) + (1|year),
                   family = poisson(), data = dat.2010,
                   map= list(theta = factor(c(NA,1:23))),
                   start = list(theta =c(log(1000), seq(0,0, length.out = 23)))
)

summary(m2)
saveRDS(m2, file.path(derived, 'mod_sel_2010-2015.RDS'))
