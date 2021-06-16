# === Functions -----------------------------------------------------------
# Alec L. Robitaille




# Make unique and complete ------------------------------------------------
make_unique_complete <- function(DT, id, datetime, long, lat) {
  na.omit(unique(DT, by = c(id, datetime)),
          cols = c(long, lat, datetime))
}



# Extract land cover ------------------------------------------------------
extract_lc <- function(DT, lc, x, y, lcvalues) {
  merge(DT[, value := raster::extract(lc, matrix(do.call(cbind, .SD), ncol = 2)),
           .SDcols = c(x, y)],
        lcvalues, by = value)
}





