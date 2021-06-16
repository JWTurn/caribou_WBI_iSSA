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

# Functions ---------------------------------------------------------------
source('R/functions.R')


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               error = 'workspace')


# Variables ---------------------------------------------------------------
path <- file.path('data', 'derived-data', 'prepped-data', 'SKprepDat.RDS')
land <- file.path('data', 'raw-data', 'CanLCC.tif')
landclass <- fread(file.path('data', 'raw-data', 'rcl.csv'))

id <- 'id'
datetime <- 'datetime'
long <- 'long'
lat <- 'lat'
crs <- CRS(st_crs(4326)$wkt)


# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
splitBy <- id


# Resampling rate (hours)
rate <- 5

# Tolerance (minutes)
tolerance <- 30


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
  
  tar_target(
    lc,
    raster(land, resolution = c(30, 30))
  ),
  
  # Extract land cover
  tar_target(
    extracts,
    extract_lc(mkunique, lc, long, lat, landclass)
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
    random_steps(resamples, n=10) %>%
      extract_covariates(lc, where = "end") %>%
      time_of_day(where = 'start'),
    pattern = map(resamples)
  )
  
)
