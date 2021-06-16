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





