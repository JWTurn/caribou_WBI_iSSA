# === Functions -----------------------------------------------------------
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
  
  # Cancel if there are not at least three rows after resample
  tar_cancel(nrow(t) < 3)
  
  t %>% steps_by_burst(., lonlat = TRUE)  
}



# Make random steps ------------------------------------------------------
make_random_tracks <- function(DT, lc) {
  tar_cancel(nrow(DT) == 0)
  random_steps(DT, n = 10) %>%
    extract_covariates(lc, where = "end") %>%
    time_of_day(where = 'start')
}

