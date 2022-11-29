#' @title extract from from yearly rasters
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
#' 
extract_by_year <- function(DT, var, startyr, endyr, step.end  = T){
  #lyr <- vect(layer)
  object_name <- deparse(substitute(var))
  yrs <- startyr:endyr
  ls_rast <-c(paste0(var, '_', yrs, '.tif'))
  names(ls_rast) <- yrs
  
  if (isTRUE(step.end)) {
    coords <-  c('x2_', 'y2_')
    object_end <- 'end'
  }
  
  if(isFALSE(step.end)){
    coords <-  c('x1_', 'y1_') 
    object_end <- 'start'
  }
  
  DT[, paste(object_name, object_end, sep = "_") := extract(
    rast(ls_rast[as.integer(names(ls_rast)) == .BY[[1]]]),
    .SD,
    cells = FALSE,
    xy = FALSE,
    ID = FALSE
  ),
  by = year,
  .SDcols = c(coords)]

  
}