#' @title make proportion layers from landsat
#' @export
#' @author Julie W. Turner
#' 
make_landsat_prop <- function(layer, studyArea, crs, buff, year){
  land.full <- rast(layer)
  if(st_crs(crs(land.full)) != st_crs(crs)){
    land.proj <- project(land.full, crs, method = 'near')
    land.full <- land.proj
    }
  sArea <- vect(studyArea)
  land <- crop(land.full, sArea)
  
  # What to buffer for proportion of landclasses
  buff.diam <- buff  ## median step length = 752, I chose something a bit less
  
  needleleaf <- land %in% c(1,2)
  names(needleleaf) <- "needleleaf"
  deciduous <- land == 5
  names(deciduous) <- "deciduous"
  mixed <- land == 6
  names(mixed) <- "mixed"
  shrub <- land == 8
  names(shrub) <- "shrub"
  grassland <- land == 10
  names(grassland) <- "grassland"
  lichenshrub <- land == 11
  names(lichenshrub) <- "lichenshrub"
  lichengrass <- land == 12
  names(lichengrass) <- "lichengrass"
  wet <- land == 14
  names(wet) <- "wetland"
  cropland <- land == 15
  names(cropland) <- "cropland"
  barrenland <- land == 16
  names(barrenland) <- "barrenland"
  urban <- land == 17
  names(urban) <- "urban"
  water <- land == 18
  names(water) <- "water"
  snow <- land == 19
  names(snow) <- "snow"
  
  ## This creates an object which can be used to make a layer of specified diameter
  # The d value is what determines the buffer size if you want to change it.
  ## If you're doing multiple landcover classes, you only need to run this line once, as long as each of the habitat variables has the same resolution
  Buff <- focalMat(land, d=buff.diam, type = 'circle')
  ## This generates a new raster where each cell corresponds to the mean wetland within the buffer.
  # Since it's all 1s and 0s, this is the same as the proportion of wetland surrounding the focal variable
  propneedle <- focal(needleleaf, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  needleleaf <- file.path('data', 'raw-data','prop_land', 
                           paste0('prop_needleleaf_', year, '.tif'))
  writeRaster(propneedle, needleleaf)
  
  propdecid <- focal(deciduous, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  deciduous <- file.path('data', 'raw-data', 'prop_land',
                          paste0('prop_deciduous_', year, '.tif'))
  writeRaster(propdecid, deciduous)
  
  propmixed <- focal(mixed, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  mixed <- file.path('data', 'raw-data', 'prop_land',
                          paste0('prop_mixed_', year, '.tif'))
  writeRaster(propmixed, mixed)
  
  propshrub <- focal(shrub, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  shrub <- file.path('data', 'raw-data', 'prop_land',
                          paste0('prop_shrub_', year, '.tif'))
  writeRaster(propshrub, shrub)
  
  propgrass <- focal(grassland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  grass <- file.path('data', 'raw-data', 'prop_land',
                          paste0('prop_grassland_', year, '.tif'))
  writeRaster(propgrass, grass)
  
  proplichshrub <- focal(lichenshrub, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  lichshrub <- file.path('data', 'raw-data', 'prop_land',
                              paste0('prop_lichenshrub_', year, '.tif'))
  writeRaster(proplichshrub, lichshrub)
  
  proplichgrass <- focal(lichengrass, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  lichgrass <- file.path('data', 'raw-data', 'prop_land',
                              paste0('prop_lichengrass_', year, '.tif'))
  writeRaster(proplichgrass, lichgrass)
  
  propwet <- focal(wet, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  wet <- file.path('data', 'raw-data', 'prop_land',
                        paste0('prop_wetland_', year, '.tif'))
  writeRaster(propwet, wet)
  
  propcrop <- focal(cropland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  crop <- file.path('data', 'raw-data', 'prop_land',
                         paste0('prop_cropland_', year, '.tif'))
  writeRaster(propcrop, crop)
  
  propbarren <- focal(barrenland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  barren <- file.path('data', 'raw-data', 'prop_land',
                           paste0('prop_barrenland_', year, '.tif'))
  writeRaster(propbarren, barren)
  
  propurban <- focal(urban, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  urban <- file.path('data', 'raw-data', 'prop_land',
                          paste0('prop_urban_', year, '.tif'))
  writeRaster(propurban, urban)
  
  propwater <- focal(water, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  water <- file.path('data', 'raw-data', 'prop_land',
                          paste0('prop_water_', year, '.tif'))
  writeRaster(propwater, water)
  
  propsnow <- focal(snow, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  snow <- file.path('data', 'raw-data', 'prop_land',
                         paste0('prop_snow_', year, '.tif'))
  writeRaster(propsnow, snow)
  
  values <- list(
    r_path = c(
      needleleaf, deciduous, mixed,
      shrub, grass, lichshrub, lichgrass, wetland,
      crop, barren, urban, water, snow
    )
  )
  values$raster_name <- basename(xfun::sans_ext(values$r_path))
  values$r_name <- gsub("_*[0-9]+", "", values$raster_name)
  values$raster_name_sym <- lapply(values$raster_name, as.symbol)
  # Extraction
  values$extract_name_sym <- lapply(paste0('extract_', values$raster_name), as.symbol)
  
}

