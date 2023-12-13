## iSSA individual jurisdictions ----

require(data.table)
require(glmmTMB)
require(broom.mixed)
require(performance)
#require(ggplot2)

### Input data ----
raw <- file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

set.seed(53)
#########################################################################################

dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))

dat[,range(year), by = .(jurisdiction)]

dat.sub <- dat[year>=2014 & year<=2019]
dat.sub[,.N, by=.(jurisdiction)]
#setindex(dat, NULL)

### nwt ----
nwt <- dat[jurisdiction %in% c('nwt', 'yt')]
nwt[,id:=as.factor(id)]
nwt[,indiv_step_id := as.factor(indiv_step_id)]

### mb ----
mb.2015 <- dat[jurisdiction == 'mb' & int.year ==2015]
length(unique(mb.2015$id))*.5
# worked when sampled 150 indivs
mb.sub.id <- sample(unique(mb.2015$id), floor(length(unique(mb.2015$id))*.50))
mb.2015.sub <- mb.2015[id %in% mb.sub.id]
mb.2015.sub[,id:=as.factor(id)]
mb.2015.sub[,indiv_step_id := as.factor(indiv_step_id)]

### sk ----
sk <- dat[jurisdiction == 'sk']
sk[,id:=as.factor(id)]
sk[,indiv_step_id := as.factor(indiv_step_id)]

### bc ----
bc <- dat[jurisdiction == 'bc']
bc[,id:=as.factor(id)]
bc[,indiv_step_id := as.factor(indiv_step_id)]



### MODELS ----

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
                  (1|year),
                family = poisson(), data = nwt,
                map= list(theta = factor(c(NA,1:21))),
                start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
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
                   (1|year),
                 family = poisson(), data = bc,
                 map= list(theta = factor(c(NA,1:21))),
                 start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
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
                   (1|year),
                 family = poisson(), data = sk,
                 map= list(theta = factor(c(NA,1:21))),
                 start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
)



summary(m.sk)
saveRDS(m.sk, file.path(derived, 'mod_selmove_sk.RDS'))

#### mb ----
gc()
p1 <- m.mb.2015.sub <- glmmTMB(case_ ~ -1 +
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
                  (0 + disturbance_end|id) 
                  ,
                family = poisson(), data = mb.2015.sub,
                map= list(theta = factor(c(NA,1:20))),
                start = list(theta =c(log(1000), seq(0,0, length.out = 20))),
                verbose = TRUE
)



summary(m.mb.2015.sub)
saveRDS(m.mb.2015.sub, file.path(derived, 'mod_selmove_mb_2015_50.RDS'))


