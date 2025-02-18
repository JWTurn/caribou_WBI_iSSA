# === Habitat vs pop status -------------------------------------
# Julie Turner
# started 17 May 2024

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf',
          'glmmTMB', 'ggplot2', 'rasterVis', 'viridis', 'RColorBrewer',
          'tidyterra', 'patchwork', 'ggthemes')
lapply(libs, Require::Require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

### data ----
#### use plots ----
pde.re.discrete.2010 <- rast(file.path(derived, 'pde2010_re.tif'))
#pde.re.discrete.2010.sa.ab <- rast(file.path(derived, 'pde2010_re_ab.tif'))

pde.re.discrete.2015 <- rast(file.path(derived, 'pde2015_re.tif'))
#pde.re.discrete.2015.sa.ab <- rast(file.path(derived, 'pde2015_re_ab.tif'))

pde.bc.sa <- rast(file.path(derived, 'pde_bc.tif'))
pde.mb.sa <- rast(file.path(derived, 'pde_mb.tif'))
pde.nwt.sa <- rast(file.path(derived, 'pde_nwt.tif'))
pde.sk.sa <- rast(file.path(derived, 'pde_sk.tif'))


pde.juris <- mosaic(pde.bc.sa, pde.mb.sa, pde.nwt.sa, pde.sk.sa)

pde.mod.diff <- pde.re.discrete.2015 - pde.juris
# pde.mod.diff.index <- (pde.re.discrete.2015 - pde.juris)/pde.re.discrete.2015
# p.mod.diff <- ggplot() +
#   #geom_spatvector(fill = NA) +
#   geom_spatraster(data = as.numeric(pde.mod.diff.index), show.legend = T) +
#   geom_spatvector(data = wbi.herds, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
#   scale_fill_distiller(type = 'div', palette = 'PRGn', na.value = NA) +
#   ggtitle('Global 2015 - Jurisdiction models') +
#   theme_bw() +
#   theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
#   theme_void() +
#   labs(fill = 'Difference in selection') +
#   coord_sf(crs = 3978)
# p.mod.diff

#### ECCC status data ----
wbi.herds <- vect(file.path(raw, 'juris_herds', 'wbi_herds.shp'))

status <- fread(file.path(raw, 'pop_status_juris_2024.csv'))

### extract means by herd
mean.diff <- extract(pde.mod.diff, wbi.herds, fun = mean, na.rm = T, bind = T)
mean.intensity <- extract(pde.re.discrete.2015, wbi.herds, fun = mean, na.rm = T, bind = T)
mean.juris.intensity <- extract(pde.juris, wbi.herds, fun = mean, na.rm = T, bind = T)

means <- data.table(cbind(herd = mean.diff$herd, 
                          diff = as.numeric(mean.diff$log_distlf), intensity.2015 = as.numeric(mean.intensity$log_distlf),
                          intensity.juris = as.numeric(mean.juris.intensity$log_distlf)))

status2 <- merge(status, means, by = 'herd')
status2[,`:=`(diff=as.numeric(diff), intensity.2015 = as.numeric(intensity.2015), 
              intensity.juris = as.numeric(intensity.juris))]
status2[,size:= factor(size, levels = c('<100', '100-300', '>300'))]
status2[,poptrend:= factor(trend, levels = c(-1, 1, 0), 
                           labels = c('decreasing', 'increasing', 'stable'))]
status2[, diff.index := ifelse(diff>=0, 'positive', 'negative')]
status2[, popstatus := as.factor(ifelse(trend == -1, 'decreasing', 'stable/increasing'))]


#### pop trend ----

##### box plots ----
ggplot(na.omit(status2), aes(popstatus, diff)) +
  geom_boxplot(aes(fill = popstatus), alpha = 0.45, outliers = F, show.legend = F) +
  geom_jitter(aes(color = juris)) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  scale_fill_colorblind() +
  scale_color_viridis_d(option = 'rocket', name = 'Jurisdiction') +
  theme_bw() + 
  labs(x = 'Population trend', y = 'Selection difference index') +
  coord_flip()


ggplot(na.omit(status2), aes(poptrend, intensity.2015)) +
  geom_boxplot(outliers = F) +
  geom_jitter(aes(color = juris)) +
  coord_flip()

ggplot(na.omit(status2), aes(poptrend, intensity.juris)) +
  geom_boxplot(outliers = F) +
  geom_jitter(aes(color = juris)) +
  coord_flip()


#### pop size ----
ggplot(na.omit(status2), aes(size, diff)) +
  geom_boxplot(outliers = F) +
  geom_jitter(aes(color = juris)) +
  coord_flip()

ggplot(na.omit(status2), aes(size, intensity.2015)) +
  geom_boxplot(outliers = F) +
  geom_jitter(aes(color = juris)) +
  coord_flip()

ggplot(na.omit(status2), aes(size, intensity.juris)) +
  geom_boxplot(outliers = F) +
  geom_jitter(aes(color = juris)) +
  coord_flip()


#### stats ----

trend.diff <- glm(popstatus ~ diff, data = status2, family = 'binomial')
summary(trend.diff)

## predict probabilities of poptrend for different diffs 
p.diffs <- data.table(diff = seq(-3, 3, length.out = 50))
pp.diffs<- cbind(p.diffs, as.data.table(predict(trend.diff, newdata = p.diffs, type = "response", se = TRUE)))

# add prob of decreasing for visual
inc <- pp.diffs
inc$probability <-  inc$fit 
inc$trend <- 'stable/increasing'
dec <- pp.diffs
dec$probability <-  1 - dec$fit 
dec$trend <- 'decreasing'
lpp.diffs <- rbind(inc, dec)

## plot predicted prop ----
ggplot(lpp.diffs, aes(x = diff, y = probability, color = trend, fill = trend)) + 
  geom_line() +
  geom_ribbon(aes(ymin = probability - 1.96*se.fit, ymax = probability + 1.96*se.fit), alpha = 0.5) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() + 
  theme(
    strip.placement = "outside",   # format to look like title
    strip.background = element_blank()) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  labs(y = 'Probability', x = 'Selection difference index')

#####
trend.diff.multi <- nnet::multinom(poptrend ~ diff, data = status2, Hess = T)
summary(trend.diff.multi)
z <- summary(trend.diff.multi)$coefficients/summary(trend.diff.multi)$standard.errors
z
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
## extract the coefficients from the model and exponentiate
exp(coef(trend.diff.multi))

## predict probabilities of poptrend for different diffs 
p.diffs <- data.table(diff = seq(-3, 3, length.out = 50))
pp <- cbind(p.diffs, predict(trend.diff.multi, newdata = p.diffs, type = "probs", se = TRUE))
## melt data set to long for ggplot2
lpp <- melt(pp, id.vars = c("diff"), value.name = "probability")
## plot
ggplot(lpp, aes(x = diff, y = probability, colour = variable)) + 
  geom_line()



trend.intensity2015 <- glm(poptrend ~ intensity.2015, data = status2, family = 'binomial')
summary(trend.intensity2015)

trend.intensityjuris <- glm(poptrend ~ intensity.juris, data = status2, family = 'binomial')
summary(trend.intensityjuris)





### categorical look ----
trend.diff.tab <- table(status2$diff.index, status2$poptrend)
trend.diff.tab
mosaicplot(trend.diff.tab, xlab = 'Selection difference', ylab = 'Population trend')

trend.diff.fisher <- fisher.test(trend.diff.tab)
summary(trend.diff.fisher)
trend.diff.fisher$estimate
trend.diff.fisher$conf.int
chisq.test(trend.diff.tab)$expected
chisq.test(trend.diff.tab, correct=FALSE)


#####
ggplot(status2, aes(diff, (trend+1))) +
  geom_point(aes(color = juris)) +
  geom_smooth(method = 'gam') +
  # stat_smooth(formula = "y ~ x", method = "glm", 
  #             method.args = list(family="binomial"), se = T) +
  geom_vline(xintercept = -0.5, linetype = 'dashed')


ggplot(status2, aes(intensity.2015, trend)) +
  geom_point(aes(color = juris)) +
  geom_smooth(method = 'glm')

ggplot(status2, aes(intensity.juris, trend)) +
  geom_point(aes(color = juris)) +
  geom_smooth(method = 'glm')


