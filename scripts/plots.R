# === Plots -------------------------------------
# Julie Turner
# started 13 December 2023

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 
          'glmmTMB', 'ggplot2', 'ggthemes', 'viridis', 'patchwork')
lapply(libs, Require::Require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)
se <- function(x, na.rm){
  sd(x, na.rm = na.rm)/ sqrt(length(na.omit(x)))
}

### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))

set.seed(53)

# prepping MB to be a random subset of data because too much to converge
prepGlobalDat <- function(dat, int.yr){
  juris <- 'mb'
  
  
  dat.yr <- dat[int.year==int.yr]
  indivs <- sample(unique(dat.yr[jurisdiction == juris]$id), 
                   ceiling(length(unique(dat.yr[jurisdiction == juris]$id))*0.80))
  dat.sub<- dat.yr[!(id %in% indivs)]
  return(dat.sub)
}

dat.sub.2015 <- prepGlobalDat(dat, int.yr = 2015)
dat.sub.2010 <- prepGlobalDat(dat, int.yr = 2010)

## summaries
indivs <- sample(unique(dat[year>=2014 & year<=2019 & jurisdiction == 'mb']$id), 
                 ceiling(length(unique(dat[year>=2014 & year<=2019 & jurisdiction == 'mb']$id))*0.80))

juris.sum <- dat[!(id %in% indivs),.(prop_wets = mean(prop_wets_end, na.rm = T), prop_veg = mean(prop_veg_end, na.rm = T),
                                     prop_needleleaf = mean(prop_needleleaf_end, na.rm = T), prop_mixforest = mean(prop_mixforest_end, na.rm = T),
                                     harv_mean = mean(ts_harv_end, na.rm = T), harv_median = median(ts_harv_end, na.rm = T),
                                     fires_mean = mean(ts_fires_end, na.rm = T), fires_median = median(ts_fires_end, na.rm = T),
                                     distlf = median(distlf_end, na.rm=T), distlf_other = median(distlf_other_end, na.rm=T),
                                     disturbance = min(disturbance_end, na.rm = T)), by = .(jurisdiction)]

global2015.sum <- dat.sub.2015[,.(prop_wets = mean(prop_wets_end, na.rm = T), prop_veg = mean(prop_veg_end, na.rm = T),
                                     prop_needleleaf = mean(prop_needleleaf_end, na.rm = T), prop_mixforest = mean(prop_mixforest_end, na.rm = T),
                                     harv_mean = mean(ts_harv_end, na.rm = T), harv_median = median(ts_harv_end, na.rm = T),
                                     fires_mean = mean(ts_fires_end, na.rm = T), fires_median = median(ts_fires_end, na.rm = T),
                                     distlf = median(distlf_end, na.rm=T), distlf_other = median(distlf_other_end, na.rm=T),
                                     disturbance = min(disturbance_end, na.rm = T))]
global2010.sum <- dat.sub.2010[,.(prop_wets = mean(prop_wets_end, na.rm = T), prop_veg = mean(prop_veg_end, na.rm = T),
                                  prop_needleleaf = mean(prop_needleleaf_end, na.rm = T), prop_mixforest = mean(prop_mixforest_end, na.rm = T),
                                  harv_mean = mean(ts_harv_end, na.rm = T), harv_median = median(ts_harv_end, na.rm = T),
                                  fires_mean = mean(ts_fires_end, na.rm = T), fires_median = median(ts_fires_end, na.rm = T),
                                  distlf = median(distlf_end, na.rm=T), distlf_other = median(distlf_other_end, na.rm=T),
                                  disturbance = min(disturbance_end, na.rm = T))]

# availablity
juris.hab <- melt(dat[!(id %in% indivs),.(id, jurisdiction, sl_, ta_, prop_wets_end,
                                          prop_veg_end, prop_needleleaf_end, prop_mixforest_end, 
                                          ts_harv_end, ts_fires_end, distlf_end, distlf_other_end, 
                                          disturbance_end)], id.vars = c('id', 'jurisdiction'), 
                  variable.name = 'variable', value.name = 'value')
juris.avail <- juris.hab[,.(mean = mean(value, na.rm = T), se = se(value, na.rm = T)), by = .(jurisdiction, variable)]
juris.avail[,`:=`(lower = mean-se*1.96, upper = mean+se*1.96)]

global2010.hab <- melt(dat.sub.2010[!(id %in% indivs),.(id, jurisdiction, sl_, ta_, prop_wets_end,
                                          prop_veg_end, prop_needleleaf_end, prop_mixforest_end, 
                                          ts_harv_end, ts_fires_end, distlf_end, distlf_other_end, 
                                          disturbance_end)], id.vars = c('id', 'jurisdiction'), 
                  variable.name = 'variable', value.name = 'value')
global2010.avail <- global2010.hab[,.(mean = mean(value, na.rm = T), se = se(value, na.rm = T)), by = .(variable)]
global2010.avail[,`:=`(lower = mean-se*1.96, upper = mean+se*1.96, jurisdiction = '2010')]


global2015.hab <- melt(dat.sub.2015[!(id %in% indivs),.(id, jurisdiction, sl_, ta_, prop_wets_end,
                                          prop_veg_end, prop_needleleaf_end, prop_mixforest_end, 
                                          ts_harv_end, ts_fires_end, distlf_end, distlf_other_end, 
                                          disturbance_end)], id.vars = c('id', 'jurisdiction'), 
                  variable.name = 'variable', value.name = 'value')
global2015.avail <- global2015.hab[,.(mean = mean(value, na.rm = T), se = se(value, na.rm = T)), by = .(variable)]
global2015.avail[,`:=`(lower = mean-se*1.96, upper = mean+se*1.96, jurisdiction = '2015')]

sum.avail <- rbind(juris.avail, global2010.avail, global2015.avail)

hab.avail <- rbind(juris.hab[,mod:=jurisdiction], global2010.hab[,mod:='2010'], global2015.hab[,mod:='2015'])

## Models ----
sel.2010 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2010-2015_HPC_noTA.RDS'))

sel.2015 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2015-2020_HPC_noTA.RDS'))

sel.bc <- readRDS(file.path(derived, 'mod_selmove_bc.RDS'))
sel.mb <- readRDS(file.path(derived, 'mod_selmove_mb_2015_50.RDS'))
sel.sk <- readRDS(file.path(derived, 'mod_selmove_sk.RDS'))
sel.nwt <- readRDS(file.path(derived, 'mod_selmove_nwt.RDS'))


### Results tables ----

mod2010re.tab <- setDT(broom.mixed::tidy(sel.2010, effect = 'fixed'))[,.(term, estimate, std.error, mod = 'global2010')]

mod2015re.tab <- setDT(broom.mixed::tidy(sel.2015, effect = 'fixed'))[,.(term, estimate, std.error, mod = 'global2015')]

bc.tab <- setDT(broom.mixed::tidy(sel.bc, effect = 'fixed'))[,.(term, estimate, std.error, mod = 'bc')]
mb.tab <- setDT(broom.mixed::tidy(sel.mb, effect = 'fixed'))[,.(term, estimate, std.error, mod = 'mb')]
sk.tab <- setDT(broom.mixed::tidy(sel.sk, effect = 'fixed'))[,.(term, estimate, std.error, mod = 'sk')]
nwt.tab <- setDT(broom.mixed::tidy(sel.nwt, effect = 'fixed'))[,.(term, estimate, std.error, mod = 'nwt')]

all.res <- rbind(mod2010re.tab, mod2015re.tab, bc.tab, mb.tab, sk.tab, nwt.tab)
saveRDS(all.res, file.path(derived, 'allMod_results.RDS'))
gc()


# prelim plots ----
ggplot(all.res[term %like% 'sl_'| term %like% 'ta_'], aes(term, estimate, color = mod)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = 0) +
  geom_hline(yintercept = 0)+
  coord_flip()

ggplot(all.res[!((term %like% 'sl_')| (term %like% 'ta_'))], aes(term, estimate, color = mod)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width=0) +
  geom_hline(yintercept = 0)+
  coord_flip()



### availability ----
sum.avail[variable == 'distlf_end', var.name := 'Paved linear feature']
sum.avail[variable %like% 'distlf_other', var.name := 'Unpaved linear feature']
sum.avail[variable %like% 'disturbance', var.name := 'Polygonal disturbance']
sum.avail[variable %like% 'fires', var.name := 'Fire']
sum.avail[variable %like% 'harv', var.name := 'Harvest']
sum.avail[variable %like% 'prop_mixforest', var.name := 'Mixed forest']
sum.avail[variable %like% 'prop_needleleaf', var.name := 'Needleleaf']
sum.avail[variable %like% 'prop_veg', var.name := 'Other vegetation']
sum.avail[variable %like% 'prop_wets', var.name := 'Wetlands']

ggplot(sum.avail[jurisdiction!= 'yt' & ((variable %like% 'prop') | variable %like%  'disturbance')], aes(toupper(jurisdiction), mean, color = toupper(jurisdiction))) +
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  facet_wrap(~var.name) + 
  ylab('Mean proportion available') +
  labs(color = 'Jurisdiction', x = NULL) +
  theme_bw() + 
  scale_color_colorblind() +
  scale_fill_colorblind()

ggplot(hab.avail[jurisdiction!= 'yt' & ((variable %like% 'prop') | variable %like%  'disturbance')], aes(jurisdiction, mean, color = jurisdiction)) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.5) +
  facet_wrap(~variable) + 
  xlab('model') +
  ylab('Proportion available') +
  theme_bw() + 
  scale_color_colorblind() +
  scale_fill_colorblind()

ggplot(sum.avail[jurisdiction!= 'yt' &(variable %like% 'distlf')], aes(toupper(jurisdiction), mean, color = toupper(jurisdiction))) +
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  facet_wrap(~var.name) + 
  labs(color = 'Jurisdiction', x = NULL) +
  ylab('Mean distance to feature available (m)') +
  theme_bw() + 
  scale_color_colorblind() +
  scale_fill_colorblind()

ggplot(sum.avail[jurisdiction!= 'yt' &(variable %in% c('ts_harv_end', 'ts_fires_end'))], aes(toupper(jurisdiction), mean, color = toupper(jurisdiction))) +
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  facet_wrap(~var.name) + 
  labs(color = 'Jurisdiction', x = NULL) +
  ylab('Mean time since disturbance available (years)') +
  theme_bw() + 
  scale_color_colorblind() +
  scale_fill_colorblind()

#boxplots 
ggplot(hab.avail[jurisdiction!= 'yt' & ((variable %like% 'prop') | variable %like%  'disturbance')], aes(mod, value, color = mod)) +
  geom_boxplot() + 
  #geom_jitter(alpha = 0.5) +
  facet_wrap(~variable) + 
  xlab('model') +
  ylab('Proportion available') +
  theme_bw() + 
  scale_color_colorblind() +
  scale_fill_colorblind()

ggplot(hab.avail[jurisdiction!= 'yt' &(variable %like% 'distlf')], aes(mod, value, color = mod)) +
  geom_boxplot() + 
  #geom_jitter(alpha = 0.5) +
  facet_wrap(~variable) + 
  xlab('model') +
  ylab('Distance to feature available (m)') +
  theme_bw() + 
  scale_color_colorblind() +
  scale_fill_colorblind()

ggplot(hab.avail[jurisdiction!= 'yt' &(variable %in% c('ts_harv_end', 'ts_fires_end'))], aes(mod, value, color = mod)) +
  geom_boxplot() + 
  #geom_jitter(alpha = 0.5) +
  facet_wrap(~variable) + 
  xlab('model') +
  ylab('Distance to feature available (m)') +
  theme_bw() + 
  scale_color_colorblind() +
  scale_fill_colorblind()


ggplot(sum.avail[jurisdiction!= 'yt' &!(variable %in% c('sl_', 'ta_'))], aes(jurisdiction, mean, color = jurisdiction)) +
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  facet_wrap(~variable, scales = 'free') + 
  theme_bw() + 
  scale_color_colorblind() +
  scale_fill_colorblind()



# RSS ----
# from Avgar et al. 2017
# simple variable βi∙Δhi + βj∙Δhj, etc
rss <- function(beta, x1, x2){
  beta*(x1-x2)
}
# log transformed variable: ln [hi(x1)/ (hi (x1 )−Δhi)] ^β
rss.ln <- function(beta, x1, x2){
  log(1 + (x1/(x1 -(x1-x2))))^beta
}
#beta <- mod2015re.tab[term %like% 'distlf_other_end']$estimate


x.prop = seq(0,1, length.out = 100)
x.dist = seq(0, 10000, length.out = 100) 
x.ts = seq(0, 40, length.out = 100)

gc()
### global 2015 ----
logrss.2015 <- data.table(var = 'prop_needleleaf', x = x.prop)

logrss.2015[,`:=`(rss= rss(beta = mod2015re.tab[term == 'prop_needleleaf_end']$estimate,
                              x1 = x, x2 = global2015.sum$prop_needleleaf),
            lower =  rss(beta = mod2015re.tab[term == 'prop_needleleaf_end']$estimate - mod2015re.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                         x1 = x, x2 = global2015.sum$prop_needleleaf),
            upper =  rss(beta = mod2015re.tab[term == 'prop_needleleaf_end']$estimate + mod2015re.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                         x1 = x, x2 = global2015.sum$prop_needleleaf))]

logrss.2015 <- rbind(logrss.2015, data.table(var = 'prop_wets', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.2015[var == 'prop_wets',`:=`(rss= rss(beta = mod2015re.tab[term == 'prop_wets_end']$estimate,
                      x1 = x, x2 = global2015.sum$prop_wets),
             lower =  rss(beta = mod2015re.tab[term == 'prop_wets_end']$estimate - mod2015re.tab[term == 'prop_wets_end']$std.error*1.96,
                          x1 = x, x2 = global2015.sum$prop_wets),
             upper =  rss(beta = mod2015re.tab[term == 'prop_wets_end']$estimate + mod2015re.tab[term == 'prop_wets_end']$std.error*1.96,
                          x1 = x, x2 = global2015.sum$prop_wets))]

logrss.2015 <- rbind(logrss.2015, data.table(var = 'prop_mixforest', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.2015[var == 'prop_mixforest',`:=`(rss= rss(beta = mod2015re.tab[term == 'prop_mixforest_end']$estimate,
                                        x1 = x, x2 = global2015.sum$prop_mixforest),
                               lower =  rss(beta = mod2015re.tab[term == 'prop_mixforest_end']$estimate - mod2015re.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                            x1 = x, x2 = global2015.sum$prop_mixforest),
                               upper =  rss(beta = mod2015re.tab[term == 'prop_mixforest_end']$estimate + mod2015re.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                            x1 = x, x2 = global2015.sum$prop_mixforest))]

logrss.2015 <- rbind(logrss.2015, data.table(var = 'prop_veg', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.2015[var == 'prop_veg',`:=`(rss= rss(beta = mod2015re.tab[term == 'prop_veg_end']$estimate,
                                        x1 = x, x2 = global2015.sum$prop_veg),
                               lower =  rss(beta = mod2015re.tab[term == 'prop_veg_end']$estimate - mod2015re.tab[term == 'prop_veg_end']$std.error*1.96,
                                            x1 = x, x2 = global2015.sum$prop_veg),
                               upper =  rss(beta = mod2015re.tab[term == 'prop_veg_end']$estimate + mod2015re.tab[term == 'prop_veg_end']$std.error*1.96,
                                            x1 = x, x2 = global2015.sum$prop_veg))]

logrss.2015 <- rbind(logrss.2015, data.table(var = 'harv', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.2015[var == 'harv', `:=`(rss= rss.ln(beta = mod2015re.tab[term %like% 'harv_end']$estimate,
                                        x1 = x, x2 = 1),
                               lower =  rss.ln(beta = mod2015re.tab[term %like% 'harv_end']$estimate - mod2015re.tab[term %like% 'harv_end']$std.error*1.96,
                                            x1 = x, x2 = 1),
                               upper =  rss.ln(beta = mod2015re.tab[term %like% 'harv_end']$estimate + mod2015re.tab[term %like% 'harv_end']$std.error*1.96,
                                            x1 = x, x2 = 1))]

logrss.2015 <- rbind(logrss.2015, data.table(var = 'fires', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.2015[var == 'fires', `:=`(rss= rss.ln(beta = mod2015re.tab[term %like% 'fires_end']$estimate,
                                       x1 = x, x2 = 1),
                           lower =  rss.ln(beta = mod2015re.tab[term %like% 'fires_end']$estimate - mod2015re.tab[term %like% 'fires_end']$std.error*1.96,
                                           x1 = x, x2 = 1),
                           upper =  rss.ln(beta = mod2015re.tab[term %like% 'fires_end']$estimate + mod2015re.tab[term %like% 'fires_end']$std.error*1.96,
                                           x1 = x, x2 = 1))]

logrss.2015 <- rbind(logrss.2015, data.table(var = 'distlf', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.2015[var == 'distlf', `:=`(rss= rss.ln(beta = mod2015re.tab[term %like% 'distlf_end']$estimate,
                                       x1 = x, x2 = 1),
                           lower =  rss.ln(beta = mod2015re.tab[term %like% 'distlf_end']$estimate - mod2015re.tab[term %like% 'distlf_end']$std.error*1.96,
                                           x1 = x, x2 = 1),
                           upper =  rss.ln(beta = mod2015re.tab[term %like% 'distlf_end']$estimate + mod2015re.tab[term %like% 'distlf_end']$std.error*1.96,
                                           x1 = x, x2 = 1))]

logrss.2015 <- rbind(logrss.2015, data.table(var = 'distlf_other', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.2015[var == 'distlf_other', `:=`(rss= rss.ln(beta = mod2015re.tab[term %like% 'distlf_other_end']$estimate,
                                       x1 = x, x2 = 1),
                           lower =  rss.ln(beta = mod2015re.tab[term %like% 'distlf_other_end']$estimate - mod2015re.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                           x1 = x, x2 = 1),
                           upper =  rss.ln(beta = mod2015re.tab[term %like% 'distlf_other_end']$estimate + mod2015re.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                           x1 = x, x2 = 1))]

logrss.2015 <- rbind(logrss.2015, data.table(var = 'disturbance', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.2015[var == 'disturbance',`:=`(rss= rss(beta = mod2015re.tab[term == 'disturbance_end']$estimate,
                                       x1 = x, x2 = 0),
                              lower =  rss(beta = mod2015re.tab[term == 'disturbance_end']$estimate - mod2015re.tab[term == 'disturbance_end']$std.error*1.96,
                                           x1 = x, x2 = 0),
                              upper =  rss(beta = mod2015re.tab[term == 'disturbance_end']$estimate + mod2015re.tab[term == 'disturbance_end']$std.error*1.96,
                                           x1 = x, x2 = 0))]


logrss.2015[,mod:='global2015']


### global 2010 -----
logrss.2010 <- data.table(var = 'prop_needleleaf', x = x.prop)

logrss.2010[,`:=`(rss= rss(beta = mod2010re.tab[term == 'prop_needleleaf_end']$estimate,
                      x1 = x, x2 = global2010.sum$prop_needleleaf),
             lower =  rss(beta = mod2010re.tab[term == 'prop_needleleaf_end']$estimate - mod2010re.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                          x1 = x, x2 = global2010.sum$prop_needleleaf),
             upper =  rss(beta = mod2010re.tab[term == 'prop_needleleaf_end']$estimate + mod2010re.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                          x1 = x, x2 = global2010.sum$prop_needleleaf))]

logrss.2010 <- rbind(logrss.2010, data.table(var = 'prop_wets', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.2010[var == 'prop_wets',`:=`(rss= rss(beta = mod2010re.tab[term == 'prop_wets_end']$estimate,
                                        x1 = x, x2 = global2010.sum$prop_wets),
                               lower =  rss(beta = mod2010re.tab[term == 'prop_wets_end']$estimate - mod2010re.tab[term == 'prop_wets_end']$std.error*1.96,
                                            x1 = x, x2 = global2010.sum$prop_wets),
                               upper =  rss(beta = mod2010re.tab[term == 'prop_wets_end']$estimate + mod2010re.tab[term == 'prop_wets_end']$std.error*1.96,
                                            x1 = x, x2 = global2010.sum$prop_wets))]

logrss.2010 <- rbind(logrss.2010, data.table(var = 'prop_mixforest', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.2010[var == 'prop_mixforest',`:=`(rss= rss(beta = mod2010re.tab[term == 'prop_mixforest_end']$estimate,
                                             x1 = x, x2 = global2010.sum$prop_mixforest),
                                    lower =  rss(beta = mod2010re.tab[term == 'prop_mixforest_end']$estimate - mod2010re.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                 x1 = x, x2 = global2010.sum$prop_mixforest),
                                    upper =  rss(beta = mod2010re.tab[term == 'prop_mixforest_end']$estimate + mod2010re.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                 x1 = x, x2 = global2010.sum$prop_mixforest))]

logrss.2010 <- rbind(logrss.2010, data.table(var = 'prop_veg', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.2010[var == 'prop_veg',`:=`(rss= rss(beta = mod2010re.tab[term == 'prop_veg_end']$estimate,
                                       x1 = x, x2 = global2010.sum$prop_veg),
                              lower =  rss(beta = mod2010re.tab[term == 'prop_veg_end']$estimate - mod2010re.tab[term == 'prop_veg_end']$std.error*1.96,
                                           x1 = x, x2 = global2010.sum$prop_veg),
                              upper =  rss(beta = mod2010re.tab[term == 'prop_veg_end']$estimate + mod2010re.tab[term == 'prop_veg_end']$std.error*1.96,
                                           x1 = x, x2 = global2010.sum$prop_veg))]

logrss.2010 <- rbind(logrss.2010, data.table(var = 'harv', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.2010[var == 'harv', `:=`(rss= rss.ln(beta = mod2010re.tab[term %like% 'harv_end']$estimate,
                                       x1 = x, x2 = 1),
                           lower =  rss.ln(beta = mod2010re.tab[term %like% 'harv_end']$estimate - mod2010re.tab[term %like% 'harv_end']$std.error*1.96,
                                           x1 = x, x2 = 1),
                           upper =  rss.ln(beta = mod2010re.tab[term %like% 'harv_end']$estimate + mod2010re.tab[term %like% 'harv_end']$std.error*1.96,
                                           x1 = x, x2 = 1))]

logrss.2010 <- rbind(logrss.2010, data.table(var = 'fires', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.2010[var == 'fires', `:=`(rss= rss.ln(beta = mod2010re.tab[term %like% 'fires_end']$estimate,
                                        x1 = x, x2 = 1),
                            lower =  rss.ln(beta = mod2010re.tab[term %like% 'fires_end']$estimate - mod2010re.tab[term %like% 'fires_end']$std.error*1.96,
                                            x1 = x, x2 = 1),
                            upper =  rss.ln(beta = mod2010re.tab[term %like% 'fires_end']$estimate + mod2010re.tab[term %like% 'fires_end']$std.error*1.96,
                                            x1 = x, x2 = 1))]

logrss.2010 <- rbind(logrss.2010, data.table(var = 'distlf', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.2010[var == 'distlf', `:=`(rss= rss.ln(beta = mod2010re.tab[term %like% 'distlf_end']$estimate,
                                         x1 = x, x2 = 1),
                             lower =  rss.ln(beta = mod2010re.tab[term %like% 'distlf_end']$estimate - mod2010re.tab[term %like% 'distlf_end']$std.error*1.96,
                                             x1 = x, x2 = 1),
                             upper =  rss.ln(beta = mod2010re.tab[term %like% 'distlf_end']$estimate + mod2010re.tab[term %like% 'distlf_end']$std.error*1.96,
                                             x1 = x, x2 = 1))]

logrss.2010 <- rbind(logrss.2010, data.table(var = 'distlf_other', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.2010[var == 'distlf_other', `:=`(rss= rss.ln(beta = mod2010re.tab[term %like% 'distlf_other_end']$estimate,
                                               x1 = x, x2 = 1),
                                   lower =  rss.ln(beta = mod2010re.tab[term %like% 'distlf_other_end']$estimate - mod2010re.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                   x1 = x, x2 = 1),
                                   upper =  rss.ln(beta = mod2010re.tab[term %like% 'distlf_other_end']$estimate + mod2010re.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                   x1 = x, x2 = 1))]

logrss.2010 <- rbind(logrss.2010, data.table(var = 'disturbance', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.2010[var == 'disturbance',`:=`(rss= rss(beta = mod2010re.tab[term == 'disturbance_end']$estimate,
                                          x1 = x, x2 = 0),
                                 lower =  rss(beta = mod2010re.tab[term == 'disturbance_end']$estimate - mod2010re.tab[term == 'disturbance_end']$std.error*1.96,
                                              x1 = x, x2 = 0),
                                 upper =  rss(beta = mod2010re.tab[term == 'disturbance_end']$estimate + mod2010re.tab[term == 'disturbance_end']$std.error*1.96,
                                              x1 = x, x2 = 0))]


logrss.2010[,mod:='global2010']


### bc ----
logrss.bc <- data.table(var = 'prop_needleleaf', x = x.prop)

logrss.bc[,`:=`(rss= rss(beta = bc.tab[term == 'prop_needleleaf_end']$estimate,
                           x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_needleleaf),
                  lower =  rss(beta = bc.tab[term == 'prop_needleleaf_end']$estimate - bc.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                               x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_needleleaf),
                  upper =  rss(beta = bc.tab[term == 'prop_needleleaf_end']$estimate + bc.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                               x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_needleleaf))]

logrss.bc <- rbind(logrss.bc, data.table(var = 'prop_wets', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.bc[var == 'prop_wets',`:=`(rss= rss(beta = bc.tab[term == 'prop_wets_end']$estimate,
                                             x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_wets),
                                    lower =  rss(beta = bc.tab[term == 'prop_wets_end']$estimate - bc.tab[term == 'prop_wets_end']$std.error*1.96,
                                                 x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_wets),
                                    upper =  rss(beta = bc.tab[term == 'prop_wets_end']$estimate + bc.tab[term == 'prop_wets_end']$std.error*1.96,
                                                 x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_wets))]

logrss.bc <- rbind(logrss.bc, data.table(var = 'prop_mixforest', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.bc[var == 'prop_mixforest',`:=`(rss= rss(beta = bc.tab[term == 'prop_mixforest_end']$estimate,
                                                  x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_mixforest),
                                         lower =  rss(beta = bc.tab[term == 'prop_mixforest_end']$estimate - bc.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                      x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_mixforest),
                                         upper =  rss(beta = bc.tab[term == 'prop_mixforest_end']$estimate + bc.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                      x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_mixforest))]

logrss.bc <- rbind(logrss.bc, data.table(var = 'prop_veg', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.bc[var == 'prop_veg',`:=`(rss= rss(beta = bc.tab[term == 'prop_veg_end']$estimate,
                                            x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_veg),
                                   lower =  rss(beta = bc.tab[term == 'prop_veg_end']$estimate - bc.tab[term == 'prop_veg_end']$std.error*1.96,
                                                x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_veg),
                                   upper =  rss(beta = bc.tab[term == 'prop_veg_end']$estimate + bc.tab[term == 'prop_veg_end']$std.error*1.96,
                                                x1 = x, x2 = juris.sum[jurisdiction == 'bc']$prop_veg))]

logrss.bc <- rbind(logrss.bc, data.table(var = 'harv', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.bc[var == 'harv', `:=`(rss= rss.ln(beta = bc.tab[term %like% 'harv_end']$estimate,
                                            x1 = x, x2 = 1),
                                lower =  rss.ln(beta = bc.tab[term %like% 'harv_end']$estimate - bc.tab[term %like% 'harv_end']$std.error*1.96,
                                                x1 = x, x2 = 1),
                                upper =  rss.ln(beta = bc.tab[term %like% 'harv_end']$estimate + bc.tab[term %like% 'harv_end']$std.error*1.96,
                                                x1 = x, x2 = 1))]

logrss.bc <- rbind(logrss.bc, data.table(var = 'fires', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.bc[var == 'fires', `:=`(rss= rss.ln(beta = bc.tab[term %like% 'fires_end']$estimate,
                                             x1 = x, x2 = 1),
                                 lower =  rss.ln(beta = bc.tab[term %like% 'fires_end']$estimate - bc.tab[term %like% 'fires_end']$std.error*1.96,
                                                 x1 = x, x2 = 1),
                                 upper =  rss.ln(beta = bc.tab[term %like% 'fires_end']$estimate + bc.tab[term %like% 'fires_end']$std.error*1.96,
                                                 x1 = x, x2 = 1))]

logrss.bc <- rbind(logrss.bc, data.table(var = 'distlf', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.bc[var == 'distlf', `:=`(rss= rss.ln(beta = bc.tab[term %like% 'distlf_end']$estimate,
                                              x1 = x, x2 = 1),
                                  lower =  rss.ln(beta = bc.tab[term %like% 'distlf_end']$estimate - bc.tab[term %like% 'distlf_end']$std.error*1.96,
                                                  x1 = x, x2 = 1),
                                  upper =  rss.ln(beta = bc.tab[term %like% 'distlf_end']$estimate + bc.tab[term %like% 'distlf_end']$std.error*1.96,
                                                  x1 = x, x2 = 1))]

logrss.bc <- rbind(logrss.bc, data.table(var = 'distlf_other', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.bc[var == 'distlf_other', `:=`(rss= rss.ln(beta = bc.tab[term %like% 'distlf_other_end']$estimate,
                                                    x1 = x, x2 = 1),
                                        lower =  rss.ln(beta = bc.tab[term %like% 'distlf_other_end']$estimate - bc.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                        x1 = x, x2 = 1),
                                        upper =  rss.ln(beta = bc.tab[term %like% 'distlf_other_end']$estimate + bc.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                        x1 = x, x2 = 1))]

logrss.bc <- rbind(logrss.bc, data.table(var = 'disturbance', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.bc[var == 'disturbance',`:=`(rss= rss(beta = bc.tab[term == 'disturbance_end']$estimate,
                                               x1 = x, x2 = 0),
                                      lower =  rss(beta = bc.tab[term == 'disturbance_end']$estimate - bc.tab[term == 'disturbance_end']$std.error*1.96,
                                                   x1 = x, x2 = 0),
                                      upper =  rss(beta = bc.tab[term == 'disturbance_end']$estimate + bc.tab[term == 'disturbance_end']$std.error*1.96,
                                                   x1 = x, x2 = 0))]


logrss.bc[,mod:='bc']


### mb ----
logrss.mb <- data.table(var = 'prop_needleleaf', x = x.prop)

logrss.mb[,`:=`(rss= rss(beta = mb.tab[term == 'prop_needleleaf_end']$estimate,
                         x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_needleleaf),
                lower =  rss(beta = mb.tab[term == 'prop_needleleaf_end']$estimate - mb.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                             x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_needleleaf),
                upper =  rss(beta = mb.tab[term == 'prop_needleleaf_end']$estimate + mb.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                             x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_needleleaf))]

logrss.mb <- rbind(logrss.mb, data.table(var = 'prop_wets', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.mb[var == 'prop_wets',`:=`(rss= rss(beta = mb.tab[term == 'prop_wets_end']$estimate,
                                           x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_wets),
                                  lower =  rss(beta = mb.tab[term == 'prop_wets_end']$estimate - mb.tab[term == 'prop_wets_end']$std.error*1.96,
                                               x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_wets),
                                  upper =  rss(beta = mb.tab[term == 'prop_wets_end']$estimate + mb.tab[term == 'prop_wets_end']$std.error*1.96,
                                               x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_wets))]

logrss.mb <- rbind(logrss.mb, data.table(var = 'prop_mixforest', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.mb[var == 'prop_mixforest',`:=`(rss= rss(beta = mb.tab[term == 'prop_mixforest_end']$estimate,
                                                x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_mixforest),
                                       lower =  rss(beta = mb.tab[term == 'prop_mixforest_end']$estimate - mb.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                    x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_mixforest),
                                       upper =  rss(beta = mb.tab[term == 'prop_mixforest_end']$estimate + mb.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                    x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_mixforest))]

logrss.mb <- rbind(logrss.mb, data.table(var = 'prop_veg', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.mb[var == 'prop_veg',`:=`(rss= rss(beta = mb.tab[term == 'prop_veg_end']$estimate,
                                          x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_veg),
                                 lower =  rss(beta = mb.tab[term == 'prop_veg_end']$estimate - mb.tab[term == 'prop_veg_end']$std.error*1.96,
                                              x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_veg),
                                 upper =  rss(beta = mb.tab[term == 'prop_veg_end']$estimate + mb.tab[term == 'prop_veg_end']$std.error*1.96,
                                              x1 = x, x2 = juris.sum[jurisdiction == 'mb']$prop_veg))]

logrss.mb <- rbind(logrss.mb, data.table(var = 'harv', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.mb[var == 'harv', `:=`(rss= rss.ln(beta = mb.tab[term %like% 'harv_end']$estimate,
                                          x1 = x, x2 = 1),
                              lower =  rss.ln(beta = mb.tab[term %like% 'harv_end']$estimate - mb.tab[term %like% 'harv_end']$std.error*1.96,
                                              x1 = x, x2 = 1),
                              upper =  rss.ln(beta = mb.tab[term %like% 'harv_end']$estimate + mb.tab[term %like% 'harv_end']$std.error*1.96,
                                              x1 = x, x2 = 1))]

logrss.mb <- rbind(logrss.mb, data.table(var = 'fires', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.mb[var == 'fires', `:=`(rss= rss.ln(beta = mb.tab[term %like% 'fires_end']$estimate,
                                           x1 = x, x2 = 1),
                               lower =  rss.ln(beta = mb.tab[term %like% 'fires_end']$estimate - mb.tab[term %like% 'fires_end']$std.error*1.96,
                                               x1 = x, x2 = 1),
                               upper =  rss.ln(beta = mb.tab[term %like% 'fires_end']$estimate + mb.tab[term %like% 'fires_end']$std.error*1.96,
                                               x1 = x, x2 = 1))]

logrss.mb <- rbind(logrss.mb, data.table(var = 'distlf', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.mb[var == 'distlf', `:=`(rss= rss.ln(beta = mb.tab[term %like% 'distlf_end']$estimate,
                                            x1 = x, x2 = 1),
                                lower =  rss.ln(beta = mb.tab[term %like% 'distlf_end']$estimate - mb.tab[term %like% 'distlf_end']$std.error*1.96,
                                                x1 = x, x2 = 1),
                                upper =  rss.ln(beta = mb.tab[term %like% 'distlf_end']$estimate + mb.tab[term %like% 'distlf_end']$std.error*1.96,
                                                x1 = x, x2 = 1))]

logrss.mb <- rbind(logrss.mb, data.table(var = 'distlf_other', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.mb[var == 'distlf_other', `:=`(rss= rss.ln(beta = mb.tab[term %like% 'distlf_other_end']$estimate,
                                                  x1 = x, x2 = 1),
                                      lower =  rss.ln(beta = mb.tab[term %like% 'distlf_other_end']$estimate - mb.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                      x1 = x, x2 = 1),
                                      upper =  rss.ln(beta = mb.tab[term %like% 'distlf_other_end']$estimate + mb.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                      x1 = x, x2 = 1))]

logrss.mb <- rbind(logrss.mb, data.table(var = 'disturbance', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.mb[var == 'disturbance',`:=`(rss= rss(beta = mb.tab[term == 'disturbance_end']$estimate,
                                             x1 = x, x2 = 0),
                                    lower =  rss(beta = mb.tab[term == 'disturbance_end']$estimate - mb.tab[term == 'disturbance_end']$std.error*1.96,
                                                 x1 = x, x2 = 0),
                                    upper =  rss(beta = mb.tab[term == 'disturbance_end']$estimate + mb.tab[term == 'disturbance_end']$std.error*1.96,
                                                 x1 = x, x2 = 0))]


logrss.mb[,mod:='mb']



### sk ----
logrss.sk <- data.table(var = 'prop_needleleaf', x = x.prop)

logrss.sk[,`:=`(rss= rss(beta = sk.tab[term == 'prop_needleleaf_end']$estimate,
                         x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_needleleaf),
                lower =  rss(beta = sk.tab[term == 'prop_needleleaf_end']$estimate - sk.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                             x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_needleleaf),
                upper =  rss(beta = sk.tab[term == 'prop_needleleaf_end']$estimate + sk.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                             x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_needleleaf))]

logrss.sk <- rbind(logrss.sk, data.table(var = 'prop_wets', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.sk[var == 'prop_wets',`:=`(rss= rss(beta = sk.tab[term == 'prop_wets_end']$estimate,
                                           x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_wets),
                                  lower =  rss(beta = sk.tab[term == 'prop_wets_end']$estimate - sk.tab[term == 'prop_wets_end']$std.error*1.96,
                                               x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_wets),
                                  upper =  rss(beta = sk.tab[term == 'prop_wets_end']$estimate + sk.tab[term == 'prop_wets_end']$std.error*1.96,
                                               x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_wets))]

logrss.sk <- rbind(logrss.sk, data.table(var = 'prop_mixforest', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.sk[var == 'prop_mixforest',`:=`(rss= rss(beta = sk.tab[term == 'prop_mixforest_end']$estimate,
                                                x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_mixforest),
                                       lower =  rss(beta = sk.tab[term == 'prop_mixforest_end']$estimate - sk.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                    x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_mixforest),
                                       upper =  rss(beta = sk.tab[term == 'prop_mixforest_end']$estimate + sk.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                    x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_mixforest))]

logrss.sk <- rbind(logrss.sk, data.table(var = 'prop_veg', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.sk[var == 'prop_veg',`:=`(rss= rss(beta = sk.tab[term == 'prop_veg_end']$estimate,
                                          x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_veg),
                                 lower =  rss(beta = sk.tab[term == 'prop_veg_end']$estimate - sk.tab[term == 'prop_veg_end']$std.error*1.96,
                                              x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_veg),
                                 upper =  rss(beta = sk.tab[term == 'prop_veg_end']$estimate + sk.tab[term == 'prop_veg_end']$std.error*1.96,
                                              x1 = x, x2 = juris.sum[jurisdiction == 'sk']$prop_veg))]

logrss.sk <- rbind(logrss.sk, data.table(var = 'harv', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.sk[var == 'harv', `:=`(rss= rss.ln(beta = sk.tab[term %like% 'harv_end']$estimate,
                                          x1 = x, x2 = 1),
                              lower =  rss.ln(beta = sk.tab[term %like% 'harv_end']$estimate - sk.tab[term %like% 'harv_end']$std.error*1.96,
                                              x1 = x, x2 = 1),
                              upper =  rss.ln(beta = sk.tab[term %like% 'harv_end']$estimate + sk.tab[term %like% 'harv_end']$std.error*1.96,
                                              x1 = x, x2 = 1))]

logrss.sk <- rbind(logrss.sk, data.table(var = 'fires', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.sk[var == 'fires', `:=`(rss= rss.ln(beta = sk.tab[term %like% 'fires_end']$estimate,
                                           x1 = x, x2 = 1),
                               lower =  rss.ln(beta = sk.tab[term %like% 'fires_end']$estimate - sk.tab[term %like% 'fires_end']$std.error*1.96,
                                               x1 = x, x2 = 1),
                               upper =  rss.ln(beta = sk.tab[term %like% 'fires_end']$estimate + sk.tab[term %like% 'fires_end']$std.error*1.96,
                                               x1 = x, x2 = 1))]

logrss.sk <- rbind(logrss.sk, data.table(var = 'distlf', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.sk[var == 'distlf', `:=`(rss= rss.ln(beta = sk.tab[term %like% 'distlf_end']$estimate,
                                            x1 = x, x2 = 1),
                                lower =  rss.ln(beta = sk.tab[term %like% 'distlf_end']$estimate - sk.tab[term %like% 'distlf_end']$std.error*1.96,
                                                x1 = x, x2 = 1),
                                upper =  rss.ln(beta = sk.tab[term %like% 'distlf_end']$estimate + sk.tab[term %like% 'distlf_end']$std.error*1.96,
                                                x1 = x, x2 = 1))]

logrss.sk <- rbind(logrss.sk, data.table(var = 'distlf_other', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.sk[var == 'distlf_other', `:=`(rss= rss.ln(beta = sk.tab[term %like% 'distlf_other_end']$estimate,
                                                  x1 = x, x2 = 1),
                                      lower =  rss.ln(beta = sk.tab[term %like% 'distlf_other_end']$estimate - sk.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                      x1 = x, x2 = 1),
                                      upper =  rss.ln(beta = sk.tab[term %like% 'distlf_other_end']$estimate + sk.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                      x1 = x, x2 = 1))]

logrss.sk <- rbind(logrss.sk, data.table(var = 'disturbance', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.sk[var == 'disturbance',`:=`(rss= rss(beta = sk.tab[term == 'disturbance_end']$estimate,
                                             x1 = x, x2 = 0),
                                    lower =  rss(beta = sk.tab[term == 'disturbance_end']$estimate - sk.tab[term == 'disturbance_end']$std.error*1.96,
                                                 x1 = x, x2 = 0),
                                    upper =  rss(beta = sk.tab[term == 'disturbance_end']$estimate + sk.tab[term == 'disturbance_end']$std.error*1.96,
                                                 x1 = x, x2 = 0))]


logrss.sk[,mod:='sk']



### nwt ----
logrss.nwt <- data.table(var = 'prop_needleleaf', x = x.prop)

logrss.nwt[,`:=`(rss= rss(beta = nwt.tab[term == 'prop_needleleaf_end']$estimate,
                         x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_needleleaf),
                lower =  rss(beta = nwt.tab[term == 'prop_needleleaf_end']$estimate - nwt.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                             x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_needleleaf),
                upper =  rss(beta = nwt.tab[term == 'prop_needleleaf_end']$estimate + nwt.tab[term == 'prop_needleleaf_end']$std.error*1.96,
                             x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_needleleaf))]

logrss.nwt <- rbind(logrss.nwt, data.table(var = 'prop_wets', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.nwt[var == 'prop_wets',`:=`(rss= rss(beta = nwt.tab[term == 'prop_wets_end']$estimate,
                                           x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_wets),
                                  lower =  rss(beta = nwt.tab[term == 'prop_wets_end']$estimate - nwt.tab[term == 'prop_wets_end']$std.error*1.96,
                                               x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_wets),
                                  upper =  rss(beta = nwt.tab[term == 'prop_wets_end']$estimate + nwt.tab[term == 'prop_wets_end']$std.error*1.96,
                                               x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_wets))]

logrss.nwt <- rbind(logrss.nwt, data.table(var = 'prop_mixforest', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.nwt[var == 'prop_mixforest',`:=`(rss= rss(beta = nwt.tab[term == 'prop_mixforest_end']$estimate,
                                                x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_mixforest),
                                       lower =  rss(beta = nwt.tab[term == 'prop_mixforest_end']$estimate - nwt.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                    x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_mixforest),
                                       upper =  rss(beta = nwt.tab[term == 'prop_mixforest_end']$estimate + nwt.tab[term == 'prop_mixforest_end']$std.error*1.96,
                                                    x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_mixforest))]

logrss.nwt <- rbind(logrss.nwt, data.table(var = 'prop_veg', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.nwt[var == 'prop_veg',`:=`(rss= rss(beta = nwt.tab[term == 'prop_veg_end']$estimate,
                                          x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_veg),
                                 lower =  rss(beta = nwt.tab[term == 'prop_veg_end']$estimate - nwt.tab[term == 'prop_veg_end']$std.error*1.96,
                                              x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_veg),
                                 upper =  rss(beta = nwt.tab[term == 'prop_veg_end']$estimate + nwt.tab[term == 'prop_veg_end']$std.error*1.96,
                                              x1 = x, x2 = juris.sum[jurisdiction == 'nwt']$prop_veg))]

logrss.nwt <- rbind(logrss.nwt, data.table(var = 'harv', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.nwt[var == 'harv', `:=`(rss= rss.ln(beta = nwt.tab[term %like% 'harv_end']$estimate,
                                          x1 = x, x2 = 1),
                              lower =  rss.ln(beta = nwt.tab[term %like% 'harv_end']$estimate - nwt.tab[term %like% 'harv_end']$std.error*1.96,
                                              x1 = x, x2 = 1),
                              upper =  rss.ln(beta = nwt.tab[term %like% 'harv_end']$estimate + nwt.tab[term %like% 'harv_end']$std.error*1.96,
                                              x1 = x, x2 = 1))]

logrss.nwt <- rbind(logrss.nwt, data.table(var = 'fires', x = x.ts, rss = NA, lower = NA, upper=NA))
logrss.nwt[var == 'fires', `:=`(rss= rss.ln(beta = nwt.tab[term %like% 'fires_end']$estimate,
                                           x1 = x, x2 = 1),
                               lower =  rss.ln(beta = nwt.tab[term %like% 'fires_end']$estimate - nwt.tab[term %like% 'fires_end']$std.error*1.96,
                                               x1 = x, x2 = 1),
                               upper =  rss.ln(beta = nwt.tab[term %like% 'fires_end']$estimate + nwt.tab[term %like% 'fires_end']$std.error*1.96,
                                               x1 = x, x2 = 1))]

logrss.nwt <- rbind(logrss.nwt, data.table(var = 'distlf', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.nwt[var == 'distlf', `:=`(rss= rss.ln(beta = nwt.tab[term %like% 'distlf_end']$estimate,
                                            x1 = x, x2 = 1),
                                lower =  rss.ln(beta = nwt.tab[term %like% 'distlf_end']$estimate - nwt.tab[term %like% 'distlf_end']$std.error*1.96,
                                                x1 = x, x2 = 1),
                                upper =  rss.ln(beta = nwt.tab[term %like% 'distlf_end']$estimate + nwt.tab[term %like% 'distlf_end']$std.error*1.96,
                                                x1 = x, x2 = 1))]

logrss.nwt <- rbind(logrss.nwt, data.table(var = 'distlf_other', x = x.dist, rss = NA, lower = NA, upper=NA))
logrss.nwt[var == 'distlf_other', `:=`(rss= rss.ln(beta = nwt.tab[term %like% 'distlf_other_end']$estimate,
                                                  x1 = x, x2 = 1),
                                      lower =  rss.ln(beta = nwt.tab[term %like% 'distlf_other_end']$estimate - nwt.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                      x1 = x, x2 = 1),
                                      upper =  rss.ln(beta = nwt.tab[term %like% 'distlf_other_end']$estimate + nwt.tab[term %like% 'distlf_other_end']$std.error*1.96,
                                                      x1 = x, x2 = 1))]

logrss.nwt <- rbind(logrss.nwt, data.table(var = 'disturbance', x = x.prop, rss = NA, lower = NA, upper=NA))
logrss.nwt[var == 'disturbance',`:=`(rss= rss(beta = nwt.tab[term == 'disturbance_end']$estimate,
                                             x1 = x, x2 = 0),
                                    lower =  rss(beta = nwt.tab[term == 'disturbance_end']$estimate - nwt.tab[term == 'disturbance_end']$std.error*1.96,
                                                 x1 = x, x2 = 0),
                                    upper =  rss(beta = nwt.tab[term == 'disturbance_end']$estimate + nwt.tab[term == 'disturbance_end']$std.error*1.96,
                                                 x1 = x, x2 = 0))]


logrss.nwt[,mod:='nwt']

## logRSS all ----
logrss <- rbind(logrss.2010, logrss.2015, logrss.bc, logrss.mb, logrss.nwt, logrss.sk)
saveRDS(logrss, file.path(derived, 'logRSS.RDS'))
# correcting the lower values that went infinite for the ln calc
logrss[is.infinite(lower), lower:= rss - (upper-rss)]
logrss[is.infinite(rss), `:=`(lower = 0, rss = 0)]
logrss[var %like% 'distlf' |var == 'harv' | var == 'fires', `:=`(lower = lower -1, rss = rss - 1, upper = upper -1)]
saveRDS(logrss, file.path(derived, 'logRSS_corrected.RDS'))

gc()
logrss <- readRDS(file.path(derived, 'logRSS_corrected.RDS'))
# PLOTS -----
logrss[mod=='global2010', mod := 2010]
logrss[mod=='global2015', mod := 2015]

logrss[var == 'distlf', var.name := 'Distance to paved LF (m)']
logrss[var == 'distlf_other', var.name := 'Distance to unpaved LF (m)']
logrss[var == 'disturbance', var.name := 'Polygonal disturbance']
logrss[var == 'fires', var.name := 'Time since fire (years)']
logrss[var == 'harv', var.name := 'Time since harvest (years)']
logrss[var == 'prop_mixforest', var.name := 'Proportion of mixed forest']
logrss[var == 'prop_needleleaf', var.name := 'Proportion of needleleaf']
logrss[var == 'prop_veg', var.name := 'Proportion of other vegetation']
logrss[var == 'prop_wets', var.name := 'Proportion of wetlands']




ggplot(logrss, aes(x, rss, color = toupper(mod), fill = toupper(mod))) +
  geom_line(show.legend = F) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  facet_wrap(~var.name, scales = 'free_x', strip.position = 'bottom') + 
  theme_bw() + 
  theme(
    strip.placement = "outside",   # format to look like title
    strip.background = element_blank()) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  labs(fill = 'Model', color = 'Model', x = NULL, y = 'logRSS')



ggplot(logrss[mod=='global2015'], aes(x, rss)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, alpha = 0.5), show.legend = F) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  facet_wrap(~var, scales = 'free_x')



ggplot(logrss[var=='distlf'], aes(x, rss)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = 'dashed')

ggplot(logrss, aes(ts, fires)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = 'dashed')
