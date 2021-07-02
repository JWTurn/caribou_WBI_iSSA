# === Functions -----------------------------------------------------------
# Julie W. Turner
# Alec L. Robitaille




# Make unique and complete ------------------------------------------------
make_unique_complete <- function(DT, id, datetime, long, lat) {
  na.omit(unique(DT, by = c(id, datetime)),
          cols = c(long, lat, datetime))
}



# Extract land cover ------------------------------------------------------
extract_lc <- function(DT, lc, x, y, lcvalues) {
  merge(
    DT[, value := raster::extract(lc, do.call(cbind, .SD)),
           .SDcols = c(x, y)],
    lcvalues,
    by = 'value',
    all.x = TRUE)
}




# Check resamples ---------------------------------------------------------
resample_tracks <- function(tracks, rate, tolerance) {
  t <- track_resample(tracks, rate = rate, tolerance = tolerance) %>%
    filter_min_n_burst()
  
  # Cancel if there are not at least 20 observed steps after resample
  # this is semi-arbitrary, but this should be enough for robust estimates in the model 
  # (Street et al preprint 2021)
  if (nrow(t) < 20) return()
  # filter out steps that are outside the 95%: there were unnaturally long steps
  # 3000m was the 95%, but I made it a bit higher as a buffer
  t %>% steps_by_burst(., lonlat = longlat, keep_cols = 'start') %>%
    dplyr::filter(sl_<=3500)
}



# Make random steps ------------------------------------------------------
make_random_steps <- function(DT, lc) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()

  random_steps(DT, n = 10, lonlat = longlat) %>%
    extract_covariates(lc, where = "end") %>%
    time_of_day(where = 'start')
}

# Merge landcover values with descriptions -----------------------------------
make_mergelc <- function(DT, meta) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
   merge(
    DT,
    meta,
    by.x = 'WB_LCC',
    by.y = 'value',
    all.x = TRUE
  )
}

# Make unique step ID across individuals -----------------------------------
make_step_id <- function(DT) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  DT[,indiv_step_id := paste(id, step_id_, sep = '_')]
}

# Calculate distribution parameters ---------------------------------------
calc_distribution_parameters <- function(steps) {
  if (is.null(steps)) return()
  c(ta_distr_params(steps), sl_distr_params(steps))
}

# rename mergelc column
make_good_names <- function(DT, old, new){
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  setnames(DT, old, new)
}

# iSSA ------------------------------------------------------
make_iSSA <- function(DT,resp, expl) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  mod.tmp <- glmmTMB(reformulate(expl, resp), data = DT, family = Poisson(), 
          doFit=FALSE)
  
  mod.tmp$parameters$theta[1] <- log(1e3)
  nvarparm<-length(mod.tmp$parameters$theta)
  mod.tmp$mapArg <- list(theta=factor(c(NA,1:(nvarparm-1))))
  glmmTMB:::fitTMB(mod.tmp)
  
}
