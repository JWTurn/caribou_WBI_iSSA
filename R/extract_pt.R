#' @title extract data points
#' @export
#' @author Julie W. Turner
#' 
extract_pt <- function(DT, layer, where  = 'end'){
  lyr <- rast(layer)
  object_name <- deparse(substitute(layer))
  
  coords_start  <-  c('x1_', 'y1_')
  coords_end  <-  c('x2_', 'y2_')
  
  if (where == 'end') {
    DT[,(paste(object_name, 'end', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_end)]
  }
  
  if (where == 'start') {
    DT[,(paste(object_name, 'start', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_start)]
  }
  
  if (where == 'both') {
    DT[,(paste(object_name, 'start', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_start)]
    DT[,(paste(object_name, 'end', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_end)]
  }
  
}