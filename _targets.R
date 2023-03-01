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
#library(glmmTMB)
library(distanceto)
# library(dtplyr)
# library(dplyr, warn.conflicts = FALSE)

#library(landscapemetrics)

# Functions ---------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)
#source('R/functions.R')


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               workspace_on_error = T)


# Variables ---------------------------------------------------------------
set.seed(53)
path <- file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
studyArea <- file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp')
can2010 <- file.path(canada, 'canada_2010', 'CAN_LC_2010_CAL.tif')
can2015 <- file.path(canada, 'canada_2015', 'CAN_LC_2015_CAL.tif')
can2020 <- file.path(canada, 'canada_2020', 'landcover-2020-classification.tif')

## this is a short term crutch to get it to work
values2010 <- file.path('data', 'raw-data', 'prop_land', '2010','propvalues.RDS')
values2015 <- file.path('data', 'raw-data', 'prop_land', '2015','propvalues.RDS')
values2020 <- file.path('data', 'raw-data', 'prop_land', '2020','propvalues.RDS')

linfeat <- file.path('data', 'raw-data', 'wbi_road_rail.shp')
fires <- file.path('data', 'raw-data', 'fire_nbac_1986_to_2020', 'fires')

linfeat_other_2010 <- file.path('data', 'raw-data', 'ECCC_disturbance', 'WB_lfother_2010.shp')
linfeat_other_2015 <- file.path('data', 'raw-data', 'ECCC_disturbance', 'WB_lfother_2015.shp')

disturb_2010 <- file.path('data', 'raw-data', 'ECCC_disturbance', 'WB_disturb_other_2010.tif')
disturb_2015 <- file.path('data', 'raw-data', 'ECCC_disturbance', 'WB_disturb_other_2015.tif')

harv <- file.path('data', 'raw-data', 'WB_harv_1985-2020.tif')


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
  
  # load other linear features
  tar_target(
    lf_other_2010,
    load_sf(linfeat_other_2010, crs)
  ),
  
  # load other linear features
  tar_target(
    lf_other_2015,
    load_sf(linfeat_other_2015, crs)
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
## Proportion of land ----
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

## time since ----
targets_timesince <- c(    
  # Extract harvest
  tar_target(
    extrharv,
    extract_pt(extractprop, harv, 'harv', where = 'both')
  ),
  
  # calculate time since harvest
  tar_target(
    tsharv,
    calc_ts(extrharv, var = 'harv', where = 'both', no.data=100) 
  ),
  
  # Extract fires
  tar_target(
    extrfires,
    extract_by_year(tsharv, fires, startyr = 1986, endyr =2020, where = 'both')
  ),
  
  # calculate time since fire
  tar_target(
    tsfire,
    calc_ts(extrfires, var = 'fires', where = 'both', no.data=100) 
  )
)

## distance to linear features ---- 
### permanent roads and rail ----
targets_disttolf <- c(     
    # Calculate distance to linear features
    tar_target(
      disttolf_2010,
      extract_distto(tsfire, lf, 'lf', where = 'both', crs, int.yr = 2010)
    ),
    
    tar_target(
      disttolf_2015,
      extract_distto(tsfire, lf, 'lf', where = 'both', crs, int.yr = 2015)
    ),
    
    tar_target(
      disttolf_2020,
      extract_distto(tsfire, lf, 'lf', where = 'both', crs, int.yr = 2020)
    )
    
)

targets_disttolf_combo <- c(  
  tar_combine(
    extrdisttolf,
    list(targets_disttolf),
    command = dplyr::bind_rows(!!!.x)
  )
)

### other linear features ----
targets_disttolfother <- c(     

  # Calculate distance to linear features other
  tar_target(
    disttolfother_2010,
    extract_distto(extrdisttolf, lf_other_2010, 'lf_other', where = 'both', crs, int.yr = 2010)
  ),
  
  tar_target(
    disttolfother_2015,
    extract_distto(extrdisttolf, lf_other_2015, 'lf_other', where = 'both', crs, int.yr = 2015)
  ),
  
  tar_target(
    disttolfother_2020,
    extract_distto(extrdisttolf, lf_other_2015, 'lf_other', where = 'both', crs, int.yr = 2020)
  )
  
)

targets_disttolfother_combo <- c(  
  tar_combine(
    extrdisttolfother,
    list(targets_disttolfother),
    command = dplyr::bind_rows(!!!.x)
  )
)

### other disturbances ----
targets_disturb_other <- c(     
  # extract other disturbance
  tar_target(
    distother_2010,
    extract_pt(extrdisttolfother, disturb_2010, 'disturbance', where = 'both', int.yr = 2010)
  ),
  
  tar_target(
    distother_2015,
    extract_pt(extrdisttolfother, disturb_2015, 'disturbance', where = 'both', int.yr = 2015)
  ),
  
  tar_target(
    distother_2020,
    extract_pt(extrdisttolfother, disturb_2015, 'disturbance', where = 'both', int.yr = 2020)
  )
  
)

targets_disturb_other_combo <- c(  
  tar_combine(
    extrdisturbother,
    list(targets_disturb_other),
    command = dplyr::bind_rows(!!!.x)
  )
)

## Step ID ----
targets_stepID <- c(
    # create step ID across individuals
    tar_target(
      stepID,
      setDT(extrdisturbother)[,indiv_step_id := paste(id, step_id_, sep = '_')]
    )
)
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

