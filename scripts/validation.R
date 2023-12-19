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

ls <- list(mb.train, form, mapls, startls, verbose, control, family)

list2env(list(mb.train = mb.train, form = form, mapls= mapls, 
              startls = startls, verbose = verbose, control = control, 
              family = family), envir=en)
gc()

###
m <- local(envir = en,
           glmmTMB(form,
                   data = mb.train, 
                   family = family,
                   map = mapls,
                   start = startls,
                   verbose = verbose, 
                   control = control))

saveRDS(m, file.path(derived, paste0('mod_mb_train_selmove_', 
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
#dat.test.nona <- na.omit(dat.test)
test_dat <- na.omit(dat.test[, disturbance_end := as.factor(disturbance_end)])
test_dat <- na.omit(dat.test)[1:100]
gc()
uhc<- prep_uhc(object = m, test_dat = test_dat,
                             n_samp = 1000, verbose = TRUE)

saveRDS(uhc, file.path(derived, "uhc_FE.RDS"))

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

# ... 5. plot ----

plot(uhc.bc)

coefs <- insight::find_predictors(m.bc)[[1]]

# Working with 'uhc_data' objects ----


# Coerce to data.frame
uhc.df <- as.data.frame(uhc.bc)
saveRDS(uhc.df, file.path(derived, "uhc_global_bc_df.RDS"))

uhc.df <- readRDS(file.path(derived, "uhc_FE_df.RDS"))
# This gives you the benefit of making custom plots, for example, with
# ggplot2
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
