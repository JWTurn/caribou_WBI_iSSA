## iSSA HPC run----

require(data.table)
require(glmmTMB)
require(broom.mixed)

#### set up ---
#parallelly::availableCores(constraints = "connections")
options(mc.cores = 24)
setDTthreads(100)

### Input data ----
raw <- file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))

#range(dat$year)
#setindex(dat, NULL)
# yr <- dat[case_==TRUE, .(year)]
# hist(yr$year)
int.yr <- 2015
juris <- 'mb'

dat.yr<- dat[int.year==int.yr & jurisdiction == juris]
#dat.yr<- dat[int.year==int.yr]
indivs <- sample(unique(dat.yr[jurisdiction == juris]$id), 
                 ceiling(length(unique(dat.yr[jurisdiction == juris]$id))*.30))
dat.sub<- dat.yr[!(id %in% indivs)]

dat.sub[,indiv_step_id := as.factor(indiv_step_id)]
#dat.sub[,jurisdiction := as.factor(jurisdiction)]
dat.sub[,year:=as.factor(year)]

print('prepped')

# m <- glmmTMB(case_ ~ -1 +
#                       scale(I(log(sl_+1))) +
#                scale(I(log(sl_+1))):scale(I(cos(ta_))) +
#                scale(prop_needleleaf_start):scale(I(log(sl_+1))) + 
#                scale(prop_mixforest_start):scale(I(log(sl_+1))) + 
#                scale(prop_veg_start):scale(I(log(sl_+1))) + 
#                scale(prop_wets_start):scale(I(log(sl_+1))) +
#                       scale(prop_needleleaf_end) + 
#                       scale(prop_mixforest_end) +
#                       scale(prop_veg_end) + 
#                       scale(prop_wets_end) +
#                       scale(I(log(ts_fires_end+1))) + 
#                scale(I(log(sl_+1))):scale(I(log(ts_fires_start+1))) +
#                       scale(I(log(ts_harv_end+1))) + 
#                scale(I(log(sl_+1))):scale(I(log(ts_harv_start+1))) +
#                       scale(I(log(distlf_end+1))) +
#                scale(I(log(distlf_start+1))):scale(I(log(sl_+1))) +
#                scale(I(log(distlf_other_end+1))) + 
#                scale(I(log(distlf_other_start+1))):scale(I(log(sl_+1))) +
#                disturbance_end +
#                       (1|indiv_step_id) +
#                       (0 + scale(I(log(sl_ +1)))|id) +
#                (0 + scale(I(log(sl_+1))):scale(I(cos(ta_)))|id) +
#               (0 + scale(prop_needleleaf_start):scale(I(log(sl_+1)))|id) + 
#                (0+ scale(prop_mixforest_start):scale(I(log(sl_+1)))|id) + 
#                (0+ scale(prop_veg_start):scale(I(log(sl_+1)))|id) + 
#                (0+ scale(prop_wets_start):scale(I(log(sl_+1)))|id) +
#                       (0 + scale(prop_needleleaf_end)|id) + 
#                       (0 + scale(prop_mixforest_end)|id) +
#                       (0 + scale(prop_veg_end)|id) +
#                       (0 + scale(prop_wets_end)|id) +
#                       (0 + scale(I(log(ts_fires_end+1)))|id) + 
#                (0 + scale(I(log(sl_+1))):scale(I(log(ts_fires_start+1)))|id) +
#                       (0 + scale(I(log(ts_harv_end+1)))|id) + 
#                (0 + scale(I(log(sl_+1))):scale(I(log(ts_harv_start+1)))|id) +
#                       (0 + scale(I(log(distlf_end+1)))|id) + 
#                (0+  scale(I(log(distlf_start+1))):scale(I(log(sl_+1)))|id) +
#                (0 + scale(I(log(distlf_other_end+1)))|id) + 
#                (0+  scale(I(log(distlf_other_start+1))):scale(I(log(sl_+1)))|id) +
#                (0 + disturbance_end|id) #+
#                       #(1|year)
#              ,
#                     family = poisson(), data = dat.sub,
#                     map= list(theta = factor(c(NA,1:19))),
#                     start = list(theta =c(log(1000), seq(0,0, length.out = 19))),
#              verbose=TRUE, control = glmmTMBControl(rank_check = "adjust")
# )
# 

m <- glmmTMB(case_ ~ -1 +
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
                     (0 + disturbance_end|id) #+
                    # (0 + I(log(sl_+1)):disturbance_start|id) +
                    # (1|jurisdiction) # +
                    #(1|year)
             ,
                   family = poisson(), data = dat.sub,
                   map= list(theta = factor(c(NA,1:20))),
                   start = list(theta =c(log(1000), seq(0,0, length.out = 20))),
               verbose = TRUE
    )

summary(m)

saveRDS(m, file.path(derived, paste0('mod_selmove_', juris, '_', int.yr, '-', int.yr+5,'_HPC.RDS')))




