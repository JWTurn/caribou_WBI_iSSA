## iSSA ----

require(targets)

require(Require)
require(data.table)
require(glmmTMB)
require(broom.mixed)

### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

tar_load(stepID)
saveRDS(stepID, file.path(derived, 'yt_derived.RDS'))

dat <- stepID
dat[,id:=as.factor(id)]
dat[,lc_end_adj := lc_end]
dat[lc_end %in% c('barren', 'grassland', 'lichen-grass', 'lichen-shrub', 'shrub', 'urban', 'snow'),lc_end_adj:= 'open-forage']
dat[lc_end %in% c('deciduous', 'mixedforest'),lc_end_adj:= 'deciduous']
dat[lc_end %in% c('water', 'wetland'),lc_end_adj:= 'wet']

dat[, lc_end_adj := factor(lc_end_adj)]

summary(dat$lc_end_adj)

##TODO incorporate season?

## model ----

mod <- glmmTMB(case_ ~
                 I(log(sl_)) +
                 I(log(sl_)):lc_end_adj +
                 lc_end_adj +
                 I(log(dist_lf_end)) +
                 (1|indiv_step_id) +
                 (0 + I(log(sl_))|id) +
                 (0 + I(log(sl_)):lc_end_adj|id) +
                 (0 + lc_end_adj|id) +
                 (0 + I(log(dist_lf_end))|id),
               family = poisson(), data = dat,
               map= list(theta = factor(c(NA,1:22))), 
               start = list(theta =c(log(1000), seq(0,0, length.out = 22)))
)

summary(mod)
saveRDS(mod, file.path(derived, 'yt_ssa.RDS'))
