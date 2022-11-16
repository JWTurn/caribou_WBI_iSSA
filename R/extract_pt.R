#' @title extract data points
#' @export
#' @author Julie W. Turner
#' 
extract_pt <- function(DT, layer, step.end  = T){
  lyr <- rast(layer)
  object_name <- deparse(substitute(layer))
  
  if (isTRUE(step.end)) {
    coords <-  c('x2_', 'y2_')
    object_end <- 'end'
  }
  
  if(isFALSE(step.end)){
    coords <-  c('x1_', 'y1_') 
    object_end <- 'start'
  }
  DT[,(paste(object_name, object_end, sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
     .SDcols = c(coords)]
  
}