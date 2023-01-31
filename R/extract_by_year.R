#' @title extract from from yearly rasters
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
#' 
extract_by_year <- function(DT, var, startyr, endyr, interval, where = 'end', out){
  #lyr <- vect(layer)
  object_name <- deparse(substitute(var))
  yrs <- seq(startyr, endyr, by = interval)
  ls_rast <-c(paste0(var, '_', yrs, '.tif'))
  names(ls_rast) <- as.character(yrs)
  # one year with gps data but no yearly rasters
  ls_rast['2021'] <- c(paste0(var, '_', '2020', '.tif'))
  
  coords_start  <-  c('x1_', 'y1_')
  coords_end  <-  c('x2_', 'y2_')
  
  if(interval > 1){
    # if this doesn't work, try hard coding new var as "interval.year" or something
    DT[,paste0(interval, 'year'):= plyr::round_any(year, interval, f = 'floor')]
  }
  
  if (where == 'end') {
    DT[, paste(object_name, 'end', sep = "_") := terra::extract(
      rast(ls_rast[as.integer(names(ls_rast)) == .BY[[1]]]),
      .SD,
      cells = FALSE,
      xy = FALSE,
      ID = FALSE
    ),
    by = ifelse(interval ==1, year, paste0(interval, 'year')),
    .SDcols = c(coords_end)]
    
    if (is.null(out)){
      return(DT)
    }
    
    if (out == 'new'){
      return(DT %>% 
               dplyr::select(paste(object_name, 'start', sep = "_"))
      )
    }
  }
  
  if(where == 'start'){
    DT[, paste(object_name, 'start', sep = "_") := terra::extract(
      rast(ls_rast[as.integer(names(ls_rast)) == .BY[[1]]]),
      .SD,
      cells = FALSE,
      xy = FALSE,
      ID = FALSE
    ),
    by = ifelse(interval ==1, year, paste0(interval, 'year')),
    .SDcols = c(coords_start)]
    
    if (is.null(out)){
      return(DT)
    }
    
    if (out == 'new'){
      return(DT %>% 
               dplyr::select(paste(object_name, 'end', sep = "_"))
      )
    }
  }
  
  if(where == 'both'){
    DT[, paste(object_name, 'start', sep = "_") := terra::extract(
      rast(ls_rast[as.integer(names(ls_rast)) == .BY[[1]]]),
      .SD,
      cells = FALSE,
      xy = FALSE,
      ID = FALSE
    ),
    by = ifelse(interval ==1, year, paste0(interval, 'year')),
    .SDcols = c(coords_start)]
    
    DT[, paste(object_name, 'end', sep = "_") := terra::extract(
      rast(ls_rast[as.integer(names(ls_rast)) == .BY[[1]]]),
      .SD,
      cells = FALSE,
      xy = FALSE,
      ID = FALSE
    ),
    by = ifelse(interval ==1, year, paste0(interval, 'year')),
    .SDcols = c(coords_end)]
    
    if (is.null(out)){
      return(DT)
    }
    
    if (out == 'new'){
      return(DT %>% 
               dplyr::select(paste(object_name, 'start', sep = "_"), paste(object_name, 'end', sep = "_"))
      )
    }
  }
  
}