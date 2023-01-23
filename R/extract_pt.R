#' @title extract data points
#' @export
#' @author Julie W. Turner
#' 
extract_pt <- function(DT, layer, name, where, out = 'all'){
  lyr <- rast(layer)
  object_name <- name
  #object_name <- deparse(substitute(name))
  
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
  
  if (out == 'all'){
    return(DT)
  }
  if (out == 'new'){
    return(DT %>% dplyr::select(last_col()))
  }
}