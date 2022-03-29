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

# Functions ---------------------------------------------------------------
source('R/functions.R')


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               error = 'workspace')


# Variables ---------------------------------------------------------------
set.seed(53)
path <- file.path('data', 'derived-data', 'prepped-data', 'MBprepDat.RDS')
land <- file.path('data', 'raw-data', 'WB_LC.tif')
landclass <- fread(file.path('data', 'raw-data', 'rcl.csv'))
linfeat <- file.path('data', 'raw-data', 'wbi_roads.shp')

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
rate <- hours(3)

# Tolerance
tolerance <- minutes(60)

# columns to rename
oldname <- c('becomes')
newname <- c('lc_end')

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
  

  # # load land raster
  # tar_target(
  #   lc,
  #   rast(land)
  # ),
  
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
  
  # make a data.table for future manipulations
  tar_target(
    dattab,
    make_data_table(randsteps),
    pattern = map(randsteps)
  ),
  
  # Extract land cover
  tar_target(
    extracts,
    extract_lc(dattab, land, 'x2_', 'y2_', landclass),
    pattern = map(dattab)
  ),
  
  # Calculate distance to linear features
  tar_target(
    distto,
    calc_distto(extracts, lf, 'lf_end', 'x2_', 'y2_', crs),
    pattern = map(extracts)
  ),
  
  # Merge covariate legend
  # tar_target(
  #   mergelc,
  #   make_mergelc(
  #     distto,
  #     landclass
  #   ),
  #   pattern = map(distto)
  # ),
  
  # Rename weird name
  tar_target(
    named,
    make_good_names(distto, oldname, newname)
  ),
  
  # create step ID across individuals
  tar_target(
    stepID,
    setDT(named)[,indiv_step_id := paste(id, step_id_, sep = '_')]
  )
  
)
