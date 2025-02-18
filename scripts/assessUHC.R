# UHC assessment ----
# Julie Turner
# Started: May 2024

require(data.table)
require(dplyr)


# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

# function for min precision
log2 <- function(x, minPrecision = 1e-320) {
  ifelse(x < minPrecision, log(minPrecision), log(x))
}

### Input data ----
raw <- file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')

uhc.global.2010 <- readRDS(file.path(derived, "uhc_global_2010_2015_df.RDS"))
uhc.global.2015 <- readRDS(file.path(derived, "uhc_FE_df.RDS"))
uhc.global.bc <- readRDS(file.path(derived, "uhc_global_bc_df.RDS"))
uhc.global.mb <- readRDS(file.path(derived, "uhc_global_mb_df.RDS"))
uhc.global.nwt <- readRDS(file.path(derived, "uhc_global_nwt_df.RDS"))
uhc.global.sk <- readRDS(file.path(derived, "uhc_global_sk_df.RDS"))

uhc.juris.modbc.mb <- readRDS(file.path(derived, "uhc_juris_modbc_mb_df.RDS"))
uhc.juris.modbc.nwt <- readRDS(file.path(derived, "uhc_juris_modbc_nwt_df.RDS"))
uhc.juris.modbc.sk <- readRDS(file.path(derived, "uhc_juris_modbc_sk_df.RDS"))

uhc.juris.modmb.bc <- readRDS(file.path(derived, "uhc_juris_modmb_bc_df.RDS"))
uhc.juris.modmb.nwt <- readRDS(file.path(derived, "uhc_juris_modmb_nwt_df.RDS"))
uhc.juris.modmb.sk <- readRDS(file.path(derived, "uhc_juris_modmb_sk_df.RDS"))

uhc.juris.modnwt.bc <- readRDS(file.path(derived, "uhc_juris_modnwt_bc_df.RDS"))
uhc.juris.modnwt.mb <- readRDS(file.path(derived, "uhc_juris_modnwt_mb_df.RDS"))
uhc.juris.modnwt.sk <- readRDS(file.path(derived, "uhc_juris_modnwt_sk_df.RDS"))

uhc.juris.modsk.bc <- readRDS(file.path(derived, "uhc_juris_modsk_bc_df.RDS"))
uhc.juris.modsk.mb <- readRDS(file.path(derived, "uhc_juris_modsk_mb_df.RDS"))
uhc.juris.modsk.nwt <- readRDS(file.path(derived, "uhc_juris_modsk_nwt_df.RDS"))




coefs <- unique(uhc.2010_15$var[uhc.2010_15$var %like% '_end'])


est_calibration <- function(coefs_ls, dat){
  name <- deparse(substitute(dat))
   dat2 <- dat
   rbindlist(lapply(coefs_ls, function(cc, dat = dat2){
  dt <- setDT(dat)
  var = cc
 snll.used = mean(sapply(dt[var ==cc & dist=='S',.(y= list(y)), by = .(iter)]$y, function(g) sum(-log2(EnvStats::demp(g, dt[var ==cc & dist == 'U']$y)))))
 snll.avail = mean(sapply(dt[var ==cc & dist=='S',.(y= list(y)), by = .(iter)]$y, function(g) sum(-log2(EnvStats::demp(g, dt[var ==cc & dist == 'A']$y)))))
 return(list(var = var, snll.used = snll.used, snll.avail = snll.avail))
 }))[, mod := gsub('.*uhc.', '', name)]
}
 

snll.global.2010 <- est_calibration(coefs_ls = coefs, dat = uhc.global.2010)

snll.global.2015 <- est_calibration(coefs_ls = coefs, dat = uhc.global.2015)
snll.global.2015 <- est_calibration(coefs_ls = coefs, dat = uhc.global.2015)
snll.global.bc <- est_calibration(coefs_ls = coefs, dat = uhc.global.bc)
snll.global.mb <- est_calibration(coefs_ls = coefs, dat = uhc.global.mb)
snll.global.nwt <- est_calibration(coefs_ls = coefs, dat = uhc.global.nwt)
snll.global.sk <- est_calibration(coefs_ls = coefs, dat = uhc.global.sk)


snll.juris.modbc.mb <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modbc.mb)
snll.juris.modbc.nwt <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modbc.nwt)
snll.juris.modbc.sk <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modbc.sk)

snll.juris.modmb.bc <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modmb.bc)
snll.juris.modmb.nwt <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modmb.nwt)
snll.juris.modmb.sk <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modmb.sk)

snll.juris.modnwt.bc <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modnwt.bc)
snll.juris.modnwt.mb <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modnwt.mb)
snll.juris.modnwt.sk <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modnwt.sk)

snll.juris.modsk.bc <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modsk.bc)
snll.juris.modsk.mb <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modsk.mb)
snll.juris.modsk.nwt <- est_calibration(coefs_ls = coefs, dat = uhc.juris.modsk.nwt)

snll.all <- rbind(snll.global.2010, snll.global.2015, 
                      snll.global.bc, snll.global.mb, snll.global.nwt, snll.global.sk, 
                      snll.juris.modbc.mb, snll.juris.modbc.nwt, snll.juris.modbc.sk,
                      snll.juris.modmb.bc, snll.juris.modmb.nwt, snll.juris.modmb.sk,
                      snll.juris.modnwt.bc, snll.juris.modnwt.mb, snll.juris.modnwt.sk,
                      snll.juris.modsk.bc, snll.juris.modsk.mb, snll.juris.modsk.nwt)
saveRDS(snll.all, file.path(derived, 'uhc_snll_all.RDS'))

snll.all[,diff:=snll.used-snll.avail]
snll.all[,affinity:=(1-(snll.used/snll.avail))]
mod.snll.mean <- snll.all[,.(snll.used = mean(snll.used), snll.avail = mean(snll.avail), mean.diff = mean(diff)), by = mod]
mod.snll.mean <- mod.snll.mean[,diff:= snll.used-snll.avail]
snll.all[,.(min(snll.used)), by = var]

prop.good <- snll.all[, .(mod.type = ifelse(mod %like% 'global', 'global', 'juris'), 
                          mod.start = ifelse(mod %like% 'global.2010', mod, sub('.[^.]*$', '', mod)), 
                          n.good =.SD[diff<0, .N], prop.good =.SD[diff<0, .N]/.N, total_var = .N), by = mod]

prop.good[,.(round(mean(n.good))), by = mod.start]
prop.good[,.(min = min(n.good), mean= round(mean(n.good)), max = max(n.good), total_var=max(total_var)), by = mod.start]

snll.all[,good:=ifelse(diff<0, 1, 0)]
snll.diff.wide <- dcast(snll.all[,diff:=round(diff)], mod ~ var, value.var = 'diff')
snll.all[,.(tot_vals_good = length(which(good==1))), by = var]

good.tots <- dcast(snll.all[,diff:=round(diff)], mod ~ var, value.var = 'good') %>%
  janitor::adorn_totals(where = c('row', 'col'))

snll.diff.wide.tot <- cbind(snll.diff.wide, Total = good.tots$Total[1:(nrow(good.tots)-1)])
snll.diff.wide.tot <- rbind(snll.diff.wide.tot, good.tots[mod=='Total',1:(ncol(good.tots)-1)], fill=T)


snll.used.wide <- dcast(snll.all, var ~ mod, value.var = c('snll.used'))
snll.used.wide$best.used <- names(snll.used.wide)[apply(snll.used.wide, MARGIN = 1, FUN = which.min)]

snll.diff.wide <- dcast(snll.all, var ~ mod, value.var = c('diff'))
snll.diff.wide$best.diff <- names(snll.diff.wide)[apply(snll.diff.wide, MARGIN = 1, FUN = which.min)]

