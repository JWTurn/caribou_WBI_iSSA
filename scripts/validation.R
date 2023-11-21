# Validation ----
# Julie Turner
# Started: November 13 2023

require(data.table)
require(glmmTMB)
require(amt)
# library(terra)
# library(sf)
# library(dplyr)
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



# fit model ----
gc()
m <- glmmTMB(case_ ~ -1 +
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
               (0 + disturbance_end|id) 
             ,
             family = poisson(), data = dat.train,
             map= list(theta = factor(c(NA,1:15))),
             start = list(theta =c(log(1000), seq(0,0, length.out = 15))),
             verbose = TRUE, control = glmmTMBControl(rank_check = "adjust")
)

summary(m)

saveRDS(m, file.path(derived, paste0('mod_train_selmove_', 
                                     int.yr, '-', int.yr+5,
                                     '_HPC.RDS')))

# ... 4. prepare UHC plots ----
m <- readRDS(file.path(derived, paste0('mod_train_selmove_', 
                                       int.yr, '-', int.yr+5,
                                       '_HPC.RDS')))

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
#dat.test.nona <- na.omit(dat.test)
test_dat <- na.omit(dat.test[, disturbance_end := as.factor(disturbance_end)])
gc()
uhc<- prep_uhc(object = m, test_dat = test_dat,
                             n_samp = 1000, verbose = TRUE)



# ... 5. plot ----

plot(uhc)



# Working with 'uhc_data' objects ----


# Coerce to data.frame
uhc.df <- as.data.frame(uhc)

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
