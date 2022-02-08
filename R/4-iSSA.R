## iSSA ----

require(targets)

require(data.table)
require(glmmTMB)
require(broom.mixed)


tar_load(stepID)

dat <- stepID
dat[,id:=as.factor(id)]
dat[,lc_end:=as.factor(lc_end)]

summary(dat$lc_end)
##TODO incorporate season?

## toy model ----

mod <- glmmTMB(case_ ~
                 I(log(sl_)) +
                 I(log(sl_)):lc_end +
                 lc_end +
                 (1|indiv_step_id) +
                 (0 + I(log(sl_))|id) +
                 (0 + I(log(sl_)):lc_end|id) +
                 (0 + lc_end|id),
               family = poisson(), data = dat,
               map= list(theta = factor(c(NA,1:111))), 
               start = list(theta =c(log(1000), seq(0,0, length.out = 111)))
)

summary(mod)
