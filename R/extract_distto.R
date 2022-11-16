#' @title extract distance to
#' @export
#' @author Julie W. Turner
#' 
extract_distto <- function(DT, feature, featurename, x, y, crs) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()
  
  DT[, paste0('dist_', featurename) := distance_to(st_as_sf(.SD, coords = c(x, y),
                                                            crs = crs), feature)]
}
