# === Targets workflow: iSSA with amt -------------------------------------
# Julie Turner
# adapted from code by Alec L. Robitaille


# Packages ----------------------------------------------------------------
library(targets)

library(amt)
library(data.table)
library(sf)
library(sp)
library(ggplot2)
library(glmmTMB)

# Functions ---------------------------------------------------------------
source('R/functions.R')


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               error = 'workspace')


# Variables ---------------------------------------------------------------
path <- file.path('data', 'derived-data', 'prepped-data', 'SKprepDat.RDS')
land <- file.path('data', 'raw-data', 'WB_LCC.tif')
landclass <- fread(file.path('data', 'raw-data', 'rcl.csv'))

id <- 'id'
datetime <- 'datetime'
long <- 'long'
lat <- 'lat'
crs <- CRS(st_crs(4326)$wkt)


# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
splitBy <- id


# Resampling rate 
rate <- hours(5)

# Tolerance
tolerance <- minutes(30)


# Targets -----------------------------------------------------------------
list(
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
  
  # Remove impossible GPS points in a different hemisphere
  tar_target(
    realpts,
    mkunique[long<0&lat>0]
  ),
  
  # load land raster
  tar_target(
    lc,
    raster(land)
  ),
  
  # Extract land cover
  tar_target(
    extracts,
    extract_lc(realpts, lc, long, lat, landclass)
  ),
  
  # Set up split -- these are our iteration units
  tar_target(
    splits,
    extracts[, tar_group := .GRP, by = splitBy],
    iteration = 'group'
  ),
  
  tar_target(
    splitsnames,
    unique(extracts[, .(path = path), by = splitBy])
  ),
  

  # Make tracks. Note from here on, when we want to iterate use pattern = map(x)
  #  where x is the upstream target name
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
    make_random_steps(resamples, lc),
    pattern = map(resamples)
  ),
  
  
  # Distribution parameters
  tar_target(
    distparams,
    calc_distribution_parameters(randsteps),
    pattern = map(randsteps)
  ),
  
  # Merge covariate legend
  tar_target(
    mergelc,
    merge(
      randsteps,
      landclass,
      by.x = 'lc',
      by.y = 'value',
      all.x = TRUE
    ),
    pattern = map(randsteps)
  ),
  
  # create step ID across individuals
  tar_target(
    stepID,
    make_step_id(mergelc)
  )
  
)
