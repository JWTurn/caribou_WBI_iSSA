# === Targets workflow: iSSA with amt -------------------------------------
# Julie Turner
# adapted from code by Alec L. Robitaille


# Packages ----------------------------------------------------------------
library(targets)

library(amt)
library(data.table)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(glmmTMB)
library(distanceto)
library(landscapemetrics)

# Functions ---------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)
#source('R/functions.R')


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               error = 'workspace')


# Variables ---------------------------------------------------------------
set.seed(53)
path <- file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS')
land <- file.path('data', 'raw-data', 'WB_LC.tif')
landclass <- fread(file.path('data', 'raw-data', 'rcl.csv'))
needleleaf <- file.path('data', 'raw-data', 'prop_needleleaf.tif')
deciduous <- file.path('data', 'raw-data', 'prop_deciduous.tif')
mixed <- file.path('data', 'raw-data', 'prop_mixed.tif')
shrub <- file.path('data', 'raw-data', 'prop_shrub.tif')
grass <- file.path('data', 'raw-data', 'prop_grassland.tif')
lichshrub <- file.path('data', 'raw-data', 'prop_lichenshrub.tif')
lichgrass <- file.path('data', 'raw-data', 'prop_lichengrass.tif')
wetland <- file.path('data', 'raw-data', 'prop_wetland.tif')
crop <- file.path('data', 'raw-data', 'prop_cropland.tif')
barren <- file.path('data', 'raw-data', 'prop_barrenland.tif')
urban <- file.path('data', 'raw-data', 'prop_urban.tif')
water <- file.path('data', 'raw-data', 'prop_water.tif')
snow <- file.path('data', 'raw-data', 'prop_snow.tif')

linfeat <- file.path('data', 'raw-data', 'wbi_roads.shp')
fires <- file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires')

id <- 'id'
datetime <- 'datetime'
longlat = FALSE
#not actually longitude and latitude, just don't want to change code
long <- 'x'
lat <- 'y'
crs <- CRS(st_crs(3978)$wkt)


# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
splitBy <- id


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
  
  
  # Set up split -- these are our iteration units
  tar_target(
    splits,
    mkunique[, tar_group := .GRP, by = splitBy],
    iteration = 'group'
  ),
  
  tar_target(
    splitsnames,
    unique(mkunique[, .(path = path), by = splitBy])
  ))
  

# Targets: tracks -----------------------------------------------------------------------
# Make tracks. Note from here on, when we want to iterate use pattern = map(x)
#  where x is the upstream target name
targets_tracks <- c(
  tar_target(
    tracks,
    make_track(splits, long, lat, datetime, crs = crs, id = id),
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
  )
)

# Targets: extract ------------------------------------------------------------------
targets_extract <- c(    
  # make a data.table for future manipulations
  tar_target(
    dattab,
    make_data_table(randsteps)
  ),
  
  # add a year column
  tar_target(
    addyear,
    dattab[,year:=lubridate::year(t2_)]
  ),
  
  # calculate median step length for buffer
  tar_target(
    buff,
    plyr::round_any(median(addyear$sl_, na.rm = T), 50)
  ),
  
  # Extract land cover
  tar_target(
    extract_lc,
    extract_proportion(addyear, feature = land, landclass, buff, crs, where = 'both')
  ),
  
  
  # Extract fires
  tar_target(
    extract_fires,
    extract_by_year(extract_lc, fires, startyr = 1986, endyr =2020, where = 'both')
  ),
  
  # calculate time since fire
  tar_target(
    tsfire,
    calc_tsf(extract_fires, where = 'both', nofire=100)
  ),
  
  # Calculate distance to linear features
  tar_target(
    distto,
    extract_distto(tsfire, lf, where = 'both', crs)
  ),
  
  
  # create step ID across individuals
  tar_target(
    stepID,
    setDT(distto)[,indiv_step_id := paste(id, step_id_, sep = '_')]
  )
  
)

# Targets: all ------------------------------------------------------------------
# Automatically grab and combine all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)

