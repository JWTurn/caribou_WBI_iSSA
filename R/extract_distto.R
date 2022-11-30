#' @title extract distance to
#' @export
#' @author Julie W. Turner
#' 
extract_distto <- function(DT, feature, where = 'end', crs) {
  object_name <- deparse(substitute(feature))
  
  coords_start  <-  c('x1_', 'y1_')
  coords_end  <-  c('x2_', 'y2_')
  
  if(where == 'start'){
    DT[, paste0('dist', oject_name, '_start') := distance_to(st_as_sf(.SD, coords = coords_start,
                                                              crs = crs), feature)]
  }
  
  if(where == 'end'){
    DT[, paste0('dist', oject_name, '_end') := distance_to(st_as_sf(.SD, coords = coords_end,
                                                                      crs = crs), feature)]
  }
  
  if(where == 'both'){
    DT[, paste0('dist', oject_name, '_start') := distance_to(st_as_sf(.SD, coords = coords_start,
                                                                      crs = crs), feature)]
    DT[, paste0('dist', oject_name, '_end') := distance_to(st_as_sf(.SD, coords = coords_end,
                                                                   crs = crs), feature)]
  }
  
}
