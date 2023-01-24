#' @title extract data points
#' @export
#' @author Julie W. Turner
#' 
extract_pt <- function(DT, layer, name, where, out){
  lyr <- rast(layer)
  object_name <- name
  #object_name <- deparse(substitute(name))
  
  coords_start  <-  c('x1_', 'y1_')
  coords_end  <-  c('x2_', 'y2_')
  
  if (where == 'end') {
    DT[,(paste(object_name, 'end', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_end)]
    if (is.null(out)){
      return(DT)
    }
    if (out == 'new'){
      return(DT %>% dplyr::select(last_col()))
    }
  }
  
  if (where == 'start') {
    DT[,(paste(object_name, 'start', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_start)]
    if (is.null(out)){
      return(DT)
    }
    if (out == 'new'){
      return(DT %>% dplyr::select(last_col()))
    }
  }
  
  if (where == 'both') {
    DT[,(paste(object_name, 'start', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_start)]
    DT[,(paste(object_name, 'end', sep = "_")):= terra::extract(lyr, cbind(.SD))[,-1],
       .SDcols = c(coords_end)]
    if (is.null(out)){
      return(DT)
    }
    if (out == 'new'){
      return(DT %>% dplyr::select(last_col(1), last_col()))
    }
  }
  
}