# Validation ----
# Julie Turner
# Started: November 13 2023

require(data.table)
require(glmmTMB)
require(amt)
# library(terra)
# library(sf)
library(dplyr)
library(ggplot2)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <- file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))

int.yr <- 2015
set.seed(53)

# Global models -----
# prepping MB to be a random subset of data because too much to converge
juris <- 'mb'


dat.yr <- dat[int.year==int.yr]
indivs <- sample(unique(dat.yr[jurisdiction == juris]$id), 
                 ceiling(length(unique(dat.yr[jurisdiction == juris]$id))*0.80))
dat.sub<- dat.yr[!(id %in% indivs)]





# Split into train (80%) and test (20%) ----
## individuals ----
indivs.train <- sample(unique(dat.sub$id), 
                 ceiling(length(unique(dat.sub$id))*0.80))


dat.train<- dat.sub[(id %in% indivs.train)]
dat.train[,indiv_step_id := as.factor(indiv_step_id)]
dat.train[,jurisdiction := as.factor(jurisdiction)]
dat.train[,year:=as.factor(year)]
dat.train[,pop := as.factor(pop)]
dat.train[,id := as.factor(id)]


dat.test<- dat.sub[!(id %in% indivs.train)]
dat.test[,indiv_step_id := as.factor(indiv_step_id)]
dat.test[,jurisdiction := as.factor(jurisdiction)]
dat.test[,year:=as.factor(year)]
dat.test[,pop := as.factor(pop)]
dat.test[,id := as.factor(id)]

## 2010 ----
dat.2010 <- dat[int.year==2010]
indivs.2010 <- sample(unique(dat.2010[jurisdiction == juris]$id), 
                 ceiling(length(unique(dat.2010[jurisdiction == juris]$id))*0.80))
test.2010 <- dat.2010[!(id %in% indivs.2010)]
test.2010[,indiv_step_id := as.factor(indiv_step_id)]
test.2010[,jurisdiction := as.factor(jurisdiction)]
test.2010[,year:=as.factor(year)]
test.2010[,pop := as.factor(pop)]
test.2010[,id := as.factor(id)]

## Jurisdictions -----
### BC ----
bc.train<- dat.sub[jurisdiction!='bc']
bc.train[,indiv_step_id := as.factor(indiv_step_id)]
bc.train[,jurisdiction := as.factor(jurisdiction)]
bc.train[,year:=as.factor(year)]
bc.train[,pop := as.factor(pop)]
bc.train[,id := as.factor(id)]


bc.test <- dat.sub[jurisdiction=='bc']
bc.test[,indiv_step_id := as.factor(indiv_step_id)]
bc.test[,jurisdiction := as.factor(jurisdiction)]
bc.test[,year:=as.factor(year)]
bc.test[,pop := as.factor(pop)]
bc.test[,id := as.factor(id)]

### MB ----
mb.train<- dat.sub[jurisdiction!='mb']
mb.train[,indiv_step_id := as.factor(indiv_step_id)]
mb.train[,jurisdiction := as.factor(jurisdiction)]
mb.train[,year:=as.factor(year)]
mb.train[,pop := as.factor(pop)]
mb.train[,id := as.factor(id)]


mb.test <- dat.sub[jurisdiction=='mb']
mb.test[,indiv_step_id := as.factor(indiv_step_id)]
mb.test[,jurisdiction := as.factor(jurisdiction)]
mb.test[,year:=as.factor(year)]
mb.test[,pop := as.factor(pop)]
mb.test[,id := as.factor(id)]


### SK ----
sk.train<- dat.sub[jurisdiction!='sk']
sk.train[,indiv_step_id := as.factor(indiv_step_id)]
sk.train[,jurisdiction := as.factor(jurisdiction)]
sk.train[,year:=as.factor(year)]
sk.train[,pop := as.factor(pop)]
sk.train[,id := as.factor(id)]


sk.test <- dat.sub[jurisdiction=='sk']
sk.test[,indiv_step_id := as.factor(indiv_step_id)]
sk.test[,jurisdiction := as.factor(jurisdiction)]
sk.test[,year:=as.factor(year)]
sk.test[,pop := as.factor(pop)]
sk.test[,id := as.factor(id)]



### NWT ----
nwt.train<- dat.sub[jurisdiction!='nwt']
nwt.train[,indiv_step_id := as.factor(indiv_step_id)]
nwt.train[,jurisdiction := as.factor(jurisdiction)]
nwt.train[,year:=as.factor(year)]
nwt.train[,pop := as.factor(pop)]
nwt.train[,id := as.factor(id)]


nwt.test <- dat.sub[jurisdiction=='nwt']
nwt.test[,indiv_step_id := as.factor(indiv_step_id)]
nwt.test[,jurisdiction := as.factor(jurisdiction)]
nwt.test[,year:=as.factor(year)]
nwt.test[,pop := as.factor(pop)]
nwt.test[,id := as.factor(id)]


# jurisdictional models
dat.sub <- dat[year>=2014 & year<=2019]

### nwt ----
nwt <- dat.sub[jurisdiction %in% c('nwt', 'yt')]
nwt[,id:=as.factor(id)]
nwt[,indiv_step_id := as.factor(indiv_step_id)]
nwt[,jurisdiction := as.factor(jurisdiction)]
nwt[,year:=as.factor(year)]
nwt[,pop := as.factor(pop)]


### mb ----
mb.2015 <- dat[jurisdiction == 'mb' & int.year ==2015]
length(unique(mb.2015$id))*.5
# worked when sampled 150 indivs
mb.sub.id <- sample(unique(mb.2015$id), floor(length(unique(mb.2015$id))*.20))
mb.2015.sub <- mb.2015[id %in% mb.sub.id]
mb.2015.sub[,id:=as.factor(id)]
mb.2015.sub[,indiv_step_id := as.factor(indiv_step_id)]
mb.2015.sub[,jurisdiction := as.factor(jurisdiction)]
mb.2015.sub[,year:=as.factor(year)]
mb.2015.sub[,pop := as.factor(pop)]

### sk ----
sk <- dat.sub[jurisdiction == 'sk']
sk[,id:=as.factor(id)]
sk[,indiv_step_id := as.factor(indiv_step_id)]
sk[,jurisdiction := as.factor(jurisdiction)]
sk[,year:=as.factor(year)]
sk[,pop := as.factor(pop)]

### bc ----
bc <- dat.sub[jurisdiction == 'bc']
bc[,id:=as.factor(id)]
bc[,indiv_step_id := as.factor(indiv_step_id)]
bc[,jurisdiction := as.factor(jurisdiction)]
bc[,year:=as.factor(year)]
bc[,pop := as.factor(pop)]

# fit model ----
gc()
en <- new.env(parent = asNamespace('glmmTMB'))
form <- as.formula(case_ ~ -1 +
                     I(log(sl_+1)) +
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
                     I(log(ts_harv_end+1)) +
                     I(log(distlf_end+1)) +
                     I(log(distlf_other_end+1)) +
                     disturbance_end +
                     (1|indiv_step_id) +
                     (0 + I(log(sl_ +1))|id) +
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
                     (0 + (I(log(ts_harv_end+1)))|id) +
                     (0 + I(log(distlf_end+1))|id) +
                     (0 + I(log(distlf_other_end+1))|id) +
                     (0 + disturbance_end|id), env = en)
mapls <- list(theta = factor(c(NA,1:15)))
startls <- list(theta =c(log(1000), seq(0,0, length.out = 15)))
verbose = TRUE
control <- glmmTMBControl(rank_check = "adjust")
family = poisson()

ls <- list(nwt.train, form, mapls, startls, verbose, control, family)

list2env(list(dat.train = nwt.train, form = form, mapls= mapls, 
              startls = startls, verbose = verbose, control = control, 
              family = family), envir=en)
gc()

###
m <- local(envir = en,
           glmmTMB(form,
                   data = dat.train, 
                   family = family,
                   map = mapls,
                   start = startls,
                   verbose = verbose, 
                   control = control))

saveRDS(m, file.path(derived, paste0('mod_nwt_train_selmove_', 
                                        int.yr, '-', int.yr+5,
                                        '_HPC.RDS')))
#######

m.indivs <- glmmTMB(form,
             family = poisson(), data = dat.train,
             map= list(theta = factor(c(NA,1:15))),
             start = list(theta =c(log(1000), seq(0,0, length.out = 15))),
             verbose = TRUE, control = glmmTMBControl(rank_check = "adjust")
)

summary(m.indivs)
m.indivs$frame <- m.indivs$frame[0, ]
saveRDS(m.indivs, file.path(derived, paste0('mod_train_selmove_', 
                                     int.yr, '-', int.yr+5,
                                     '_HPC.RDS')))

## bc -----
m.bc <- glmmTMB(form,
             family = poisson(), data = bc.train,
             map= list(theta = factor(c(NA,1:15))),
             start = list(theta =c(log(1000), seq(0,0, length.out = 15))),
             verbose = TRUE, control = glmmTMBControl(rank_check = "adjust")
)

summary(m.bc)
m.bc$frame <- m.bc$frame[0, ]
saveRDS(m.bc, file.path(derived, paste0('mod_bc_train_selmove_', 
                                            int.yr, '-', int.yr+5,
                                            '_HPC.RDS')))

# ... 4. prepare UHC plots ----
m <- readRDS(file.path(derived, paste0('mod_train_selmove_', 
                                       int.yr, '-', int.yr+5,
                                       '_HPC.RDS')))


# Look for other places that are large
lobstr::obj_size(m)
object.size(m)
sapply(ls(m), function(x) lobstr::obj_size(m[[x]]))
sapply(ls(m$obj), function(x) lobstr::obj_size(m$obj[[x]]))
sapply(ls(m$obj), function(x) object.size(m$obj[[x]]))

coefs <- insight::find_predictors(m)[[1]]
# targets::tar_load(distparams)
# dtparams <- setDT(stack(distparams))
# 
# coefs <- summary(m)$coef$cond[, 1]
# shape <- mean(dtparams[ind %like% 'shape']$values)
# scale <- mean(dtparams[ind %like% 'scale']$values)
# kappa <- mean(dtparams[ind %like% 'kappa']$values)
# 
# m.amt <- make_issf_model(coefs = coefs,
#                          sl = make_gamma_distr(shape = shape, scale = scale),
#                          ta = make_vonmises_distr(kappa = kappa))
gc()

## indivs ----

test_dat <- na.omit(dat.test)
gc()
uhc<- prep_uhc(object = m, test_dat = test_dat,
                             n_samp = 100, verbose = TRUE)

saveRDS(uhc, file.path(derived, "uhc_FE.RDS"))

## 2010 ----
m <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2015-2020_HPC_noTA.RDS'))
test_dat <- na.omit(test.2010)
gc()
uhc<- prep_uhc(object = m, test_dat = test_dat,
               n_samp = 100, verbose = TRUE)

saveRDS(uhc, file.path(derived, "uhc_global_2010.RDS"))

## bc ----
m.bc <- readRDS(file.path(derived, paste0('mod_bc_train_selmove_', 
                                          int.yr, '-', int.yr+5,
                                          '_HPC.RDS')))

test_bc <- na.omit(bc.test)

gc()
uhc.bc <- prep_uhc(object = m.bc, test_dat = test_bc,
               n_samp = 100, verbose = TRUE)

saveRDS(uhc.bc, file.path(derived, "uhc_global_bc.RDS"))


## mb ----
m.mb <- readRDS(file.path(derived, paste0('mod_mb_train_selmove_', 
                                          int.yr, '-', int.yr+5,
                                          '_HPC.RDS')))
# Look for other places that are large
lobstr::obj_size(m.mb)
object.size(m.mb)
sapply(ls(m.mb), function(x) lobstr::obj_size(m.mb[[x]]))

sapply(ls(m.mb$obj), function(x) lobstr::obj_size(m.mb$obj[[x]]))

sapply(ls(m.mb$obj), function(x) object.size(m.mb$obj[[x]]))

test_mb <- na.omit(mb.test)

# pred <- predict(m.mb, newdata = test_mb,
#                 #se.fit = TRUE, 
#                 re.form = NULL, allow.new.levels = T)


gc()
uhc.mb <- prep_uhc(object = m.mb, test_dat = test_mb,
                   n_samp = 100, verbose = TRUE)

saveRDS(uhc.mb, file.path(derived, "uhc_global_mb.RDS"))


## sk ----
m.sk <- readRDS(file.path(derived, paste0('mod_sk_train_selmove_', 
                                          int.yr, '-', int.yr+5,
                                          '_HPC.RDS')))
# Look for other places that are large
lobstr::obj_size(m.sk)
object.size(m.sk)
sapply(ls(m.sk), function(x) lobstr::obj_size(m.sk[[x]]))

sapply(ls(m.sk$obj), function(x) lobstr::obj_size(m.sk$obj[[x]]))

sapply(ls(m.sk$obj), function(x) object.size(m.sk$obj[[x]]))

test_sk <- na.omit(sk.test)

# pred <- predict(m.sk, newdata = test_sk,
#                 #se.fit = TRUE, 
#                 re.form = NULL, allow.new.levels = T)


gc()
uhc.sk <- prep_uhc(object = m.sk, test_dat = test_sk,
                   n_samp = 100, verbose = TRUE)

saveRDS(uhc.sk, file.path(derived, "uhc_global_sk.RDS"))




## nwt ----
m.nwt <- readRDS(file.path(derived, paste0('mod_nwt_train_selmove_', 
                                          int.yr, '-', int.yr+5,
                                          '_HPC.RDS')))
# Look for other places that are large
lobstr::obj_size(m.nwt)
object.size(m.nwt)
sapply(ls(m.nwt), function(x) lobstr::obj_size(m.nwt[[x]]))

sapply(ls(m.nwt$obj), function(x) lobstr::obj_size(m.nwt$obj[[x]]))

sapply(ls(m.nwt$obj), function(x) object.size(m.nwt$obj[[x]]))

test_nwt <- na.omit(nwt.test)

# pred <- predict(m.nwt, newdata = test_nwt,
#                 #se.fit = TRUE, 
#                 re.form = NULL, allow.new.levels = T)


gc()
uhc.nwt <- prep_uhc(object = m.nwt, test_dat = test_nwt,
                   n_samp = 100, verbose = TRUE)

saveRDS(uhc.nwt, file.path(derived, "uhc_global_nwt.RDS"))

# jurisdiction ----
## BC model NWT data
m <- readRDS(file.path(derived, 'mod_selmove_bc.RDS'))

test <- na.omit(sk)
gc()
uhc <- prep_uhc(object = m, test_dat = test,
                    n_samp = 100, verbose = TRUE)

saveRDS(uhc, file.path(derived, "uhc_juris_modbc_sk.RDS"))


## MB model NWT data
m <- readRDS(file.path(derived, 'mod_selmove_mb_2015_45.RDS'))

test <- na.omit(bc)
gc()
uhc <- prep_uhc(object = m, test_dat = test,
                n_samp = 100, verbose = TRUE)

saveRDS(uhc, file.path(derived, "uhc_juris_modmb_bc.RDS"))
#uhc <- readRDS(file.path(derived, "uhc_juris_modmb_sk.RDS"))


## SK model NWT data
m <- readRDS(file.path(derived, 'mod_selmove_sk.RDS'))

test <- na.omit(bc)
gc()
uhc <- prep_uhc(object = m, test_dat = test,
                n_samp = 100, verbose = TRUE)

saveRDS(uhc, file.path(derived, "uhc_juris_modsk_bc.RDS"))


# ... 5. plot ----

plot(uhc)

coefs <- insight::find_predictors(m)[[1]]

# Working with 'uhc_data' objects ----


# Coerce to data.frame
uhc.df <- as.data.frame(uhc)
saveRDS(uhc.df, file.path(derived, "uhc_juris_modsk_mb_df.RDS"))

uhc.df <- readRDS(file.path(derived, "uhc_FE_df.RDS"))
# This gives you the benefit of making custom plots, for example, with
# ggplot2
coefs.end <- coefs[coefs %like% "_end"]
uhc.df %>% 
  filter(var %in% coefs.end) %>% 
  mutate(dist_sort = factor(dist, levels = c("S", "U", "A"))) %>%
  ggplot(aes(x = x, y = y, color = dist_sort, linetype = dist_sort)) +
  geom_line() +
  theme_bw() +
  scale_color_manual(name = "Distribution",
                     breaks = c("S", "U", "A"),
                     labels = c("Sampled", "Used", "Avail"),
                     values = c("gray70", "black", "red")) +
  scale_linetype_manual(name = "Distribution",
                        breaks = c("S", "U", "A"),
                        labels = c("Sampled", "Used", "Avail"),
                        values = c("solid", "solid", "dashed")
  ) +
  # xlab("Time since Fire") +
  ylab("Density") +
  facet_wrap(~var, scales = 'free')


uhc.df %>% 
  filter(var == "prop_needleleaf_end") %>% 
  mutate(dist_sort = factor(dist, levels = c("S", "U", "A"))) %>%
  ggplot(aes(x = x, y = y, color = dist_sort, linetype = dist_sort)) +
  geom_line() +
  scale_color_manual(name = "Distribution",
                     breaks = c("S", "U", "A"),
                     labels = c("Sampled", "Used", "Avail"),
                     values = c("gray70", "black", "red")) +
  scale_linetype_manual(name = "Distribution",
                        breaks = c("S", "U", "A"),
                        labels = c("Sampled", "Used", "Avail"),
                        values = c("solid", "solid", "dashed")
  ) +
  xlab("Proportion of needleleaf") +
  ylab("Density")

uhc.df %>% 
  filter(var == "ts_fires_end") %>% 
  mutate(dist_sort = factor(dist, levels = c("S", "U", "A"))) %>%
  ggplot(aes(x = x, y = y, color = dist_sort, linetype = dist_sort)) +
  geom_line() +
  scale_color_manual(name = "Distribution",
                     breaks = c("S", "U", "A"),
                     labels = c("Sampled", "Used", "Avail"),
                     values = c("gray70", "black", "red")) +
  scale_linetype_manual(name = "Distribution",
                        breaks = c("S", "U", "A"),
                        labels = c("Sampled", "Used", "Avail"),
                        values = c("solid", "solid", "dashed")
  ) +
  xlab("Time since Fire") +
  ylab("Density")

