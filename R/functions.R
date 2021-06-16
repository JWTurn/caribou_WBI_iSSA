# === Functions -----------------------------------------------------------
# Alec L. Robitaille




# Make unique and complete ------------------------------------------------
make_unique_complete <- function(DT, id, datetime, long, lat) {
  na.omit(unique(input, by = c(id, datetime)),
          cols = c(long, lat, datetime))
}



# Extract land cover ------------------------------------------------------
extract_lc <- function(DT, lc, lcvalues) {
  merge(DT[, value := extract(lc, xy)], lcvalues, by = value)
}





