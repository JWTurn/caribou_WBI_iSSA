# === Targets workflow: iSSA with amt -------------------------------------
# Julie Turner
# adapted from code by Alec L. Robitaille


# Packages ----------------------------------------------------------------
library(targets)

library(tarchetypes)
library(amt)
library(data.table)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(glmmTMB)
library(distanceto)
# library(dtplyr)
# library(dplyr, warn.conflicts = FALSE)

#library(landscapemetrics)

# Functions ---------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)
#source('R/functions.R')


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               error = 'workspace')


# Variables ---------------------------------------------------------------
set.seed(53)
path <- file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
studyArea <- file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp')
can2010 <- file.path(canada, 'canada_2010', 'CAN_LC_2010_CAL.tif')
can2015 <- file.path(canada, 'canada_2015', 'CAN_LC_2015_CAL.tif')
can2020 <- file.path(canada, 'canada_2020', 'landcover-2020-classification.tif')
#land <- file.path('data', 'raw-data', 'WB_LC.tif')

# land <- file.path('data', 'derived-data', 'prepped-data', 'land')
# landclass <- fread(file.path('data', 'raw-data', 'rcl.csv'))
# needleleaf <- file.path('data', 'raw-data', 'prop_needleleaf.tif')
# deciduous <- file.path('data', 'raw-data', 'prop_deciduous.tif')
# mixed <- file.path('data', 'raw-data', 'prop_mixed.tif')
# shrub <- file.path('data', 'raw-data', 'prop_shrub.tif')
# grass <- file.path('data', 'raw-data', 'prop_grassland.tif')
# lichshrub <- file.path('data', 'raw-data', 'prop_lichenshrub.tif')
# lichgrass <- file.path('data', 'raw-data', 'prop_lichengrass.tif')
# wetland <- file.path('data', 'raw-data', 'prop_wetland.tif')
# crop <- file.path('data', 'raw-data', 'prop_cropland.tif')
# barren <- file.path('data', 'raw-data', 'prop_barrenland.tif')
# urban <- file.path('data', 'raw-data', 'prop_urban.tif')
# water <- file.path('data', 'raw-data', 'prop_water.tif')
# snow <- file.path('data', 'raw-data', 'prop_snow.tif')
# 
# landvals <- c(
#   needleleaf, deciduous, mixed,
#   shrub, grass, lichshrub, lichgrass, wetland,
#   crop, barren, urban, water, snow
# )
# values <- list(
#   r_path = landvals
# )
# values$raster_name <- basename(xfun::sans_ext(values$r_path))
# saveRDS(values, file.path('data', 'raw-data', 'prop_land', year,'propvalues.RDS'))
# values$raster_name_sym <- lapply(values$raster_name, as.symbol)
# # Extraction
# values$extract_name_sym <- lapply(paste0('extract_', values$raster_name), as.symbol)

## this is a short term crutch to get it to work
values2010 <- file.path('data', 'raw-data', 'prop_land', '2010','propvalues.RDS')
values2015 <- file.path('data', 'raw-data', 'prop_land', '2015','propvalues.RDS')
values2020 <- file.path('data', 'raw-data', 'prop_land', '2020','propvalues.RDS')

linfeat <- file.path('data', 'raw-data', 'wbi_road_rail.shp')
fires <- file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires')

id <- 'id'
datetime <- 'datetime'
longlat = FALSE
#not actually longitude and latitude, just don't want to change code
long <- 'x'
lat <- 'y'
crs <- st_crs(3978)$wkt

# minimum year we want to pull data for
minyr <- 2010


# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
splitBy <- id
interval <- 5


# Resampling rate 
rate <- hours(12)

# Tolerance
tolerance <- hours(2)



# Targets: prep -----------------------------------------------------------------
targets_prep <- c(
  # Read input data
  tar_target(
    input,
    readRDS(path)
  ),
  
  # Remove duplicated and incomplete observations
  tar_target(
    mkunique,
    make_unique_complete(input, id, datetime, long, lat)
  ),
  
  
  # load linear features
  tar_target(
    lf,
    load_sf(linfeat, crs)
  ),
  
  # subsample data to that greater than minimum year
  tar_target(
    subdt,
    mkunique[lubridate::year(datetime)>= minyr]
    ),
  
  # Set up split -- these are our iteration units
  tar_target(
    splits,
    subdt[, tar_group := .GRP, by = splitBy],
    iteration = 'group'
  ),
  
  tar_target(
    splitsnames,
    unique(subdt[, .(path = path), by = splitBy])
  )
  )
  

# Targets: tracks -----------------------------------------------------------------------
# Make tracks. Note from here on, when we want to iterate use pattern = map(x)
#  where x is the upstream target name
targets_tracks <- c(
  tar_target(
    tracks,
    make_track(splits, long, lat, datetime, crs = crs, all_cols = T),
    pattern = map(splits)
  ),
  
  # Resample sampling rate
  tar_target(
    resamples,
    resample_tracks(tracks, rate, tolerance),
    pattern = map(tracks)
  ),
  
  # Check step distributions
  #  iteration = 'list' used for returning a list of ggplots,
  #  instead of the usual combination with vctrs::vec_c()
  tar_target(
    distributions,
    ggplot(resamples, aes(sl_)) + geom_density(alpha = 0.4),
    pattern = map(resamples),
    iteration = 'list'
  ),
  
  # create random steps and extract covariates
  tar_target(
    randsteps,
    make_random_steps(resamples),
    pattern = map(resamples)
  ),
  
  
  # Distribution parameters
  tar_target(
    distparams,
    calc_distribution_parameters(randsteps),
    pattern = map(randsteps)
  ), 
  
  # make a data.table so easier to manipulate
  tar_target(
    dattab,
    make_data_table(randsteps)
  ),
  
  # add a year column
  tar_target(
    addyear,
    dattab[,`:=`(year=lubridate::year(t2_), 
                 int.year=plyr::round_any(lubridate::year(t2_), interval, floor))]
  )
)


# Targets: extract ------------------------------------------------------------------
# Targets: extract proportion of land
targets_propland2010 <- c(
  tar_map(
    readRDS(values2010),
    tar_target(extract2010,
               extract_pt(addyear, r_path, raster_name, where = 'both', int.yr = 2010)),
    unlist = FALSE)
  )
  
targets_propland2015 <- c(  
  tar_map(
    readRDS(values2015),
    tar_target(extract2015,
               extract_pt(addyear, r_path, raster_name, where = 'both', int.yr = 2015)),
    unlist = FALSE)
  )
  
targets_propland2020 <- c(  
  tar_map(
    readRDS(values2020),
    tar_target(extract2020,
               extract_pt(addyear, r_path, raster_name, where = 'both', int.yr = 2020)),
    unlist = FALSE)
)


targets_proplandcombo_year<- c(
  tar_combine(
    propland2010,
    targets_propland2010,
    command =  list(!!!.x) %>% purrr::reduce(dplyr::full_join, by = names(addyear))
  ),
  

    tar_combine(
    propland2015,
    targets_propland2015,
    command = list(!!!.x) %>% purrr::reduce(dplyr::full_join, by = names(addyear))
  ),
  

    tar_combine(
    propland2020,
    targets_propland2020,
    command = list(!!!.x) %>% purrr::reduce(dplyr::full_join, by = names(addyear))
  )
)
  
  
targets_proplandcombo <- c(  
  tar_combine(
    extractprop,
    list(targets_proplandcombo_year),
    command = dplyr::bind_rows(!!!.x)
  )
)

targets_fires <- c(    
  
  # Extract fires
  tar_target(
    extrfires,
    extract_by_year(extractprop, fires, startyr = 1986, endyr =2020, where = 'both')
  ),
  
  # calculate time since fire
  tar_target(
    tsfire,
    calc_tsf(extrfires, where = 'both', nofire=100) 
    )
)
 
targets_distto <- c(     
    # Calculate distance to linear features
    tar_target(
      disttolf,
      extract_distto(tsfire, lf, where = 'both', crs)
    ),
    
    # create step ID across individuals
    tar_target(
      stepID,
      setDT(disttolf)[,indiv_step_id := paste(id, step_id_, sep = '_')]
    )
  
)

# targets_proportions <- c(
#   # calculate buffer
#   tar_target(
#     buffer,
#     plyr::round_any(median(dattab$sl_, na.rm = T), 50, floor)
#   ),
#   
#   # make proportion rasters
#   tar_target(
#     prop2010,
#     make_landsat_prop(layer = can2010, studyArea, crs, buff = buffer, year = 2010)
#   ),
#   
#   tar_target(
#     prop2015,
#     make_landsat_prop(layer = can2015, studyArea, crs, buff = buffer, year = 2015)
#   ),
#   
#   tar_target(
#     prop2020,
#     make_landsat_prop(layer = can2020, studyArea, crs, buff = buffer, year = 2020)
#   )
#   
# )



# Targets: combine ------------------------------------------------------------------
# 
# targets_combine <- c(  
#   tar_combine(
#     extrland,
#     targets_distto, targets_proplandcombo,
#     command = list(!!!.x) %>% purrr::reduce(dplyr::full_join, by = names(addyear))
#   ),
# 
# 
#   # create step ID across individuals
#   tar_target(
#     stepID,
#     setDT(extrland)[,indiv_step_id := paste(id, step_id_, sep = '_')]
#   )
# )


# Targets: all ------------------------------------------------------------------
# Automatically grab and combine all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)

