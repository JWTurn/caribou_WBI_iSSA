#' @title extract from from all proportion layers
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
#' 
extract_prop_layers <- function(DT, ... , where = 'end'){
  lyrs <- list(...)
  object_names <- list(deparse(substitute(...)))
  names(lyrs) <- object_names
  
  lapply(object_names, function(nn){
    extract_pt(DT, lyrs[nn], where = where)
  })
  
  
}