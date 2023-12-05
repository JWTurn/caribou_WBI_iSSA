# === Testing move maps -------------------------------------
# Julie Turner


#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf', 'amt',
          'glmmTMB', 'ggplot2', 'viridis', 'tidyterra', 'patchwork')
lapply(libs, Require::Require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')


dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))

int.yr <- 2015
set.seed(53)

# prepping MB to be a random subset of data because too much to converge
juris <- 'mb'


dat.yr <- dat[int.year==int.yr]
indivs <- sample(unique(dat.yr[jurisdiction == juris]$id), 
                 ceiling(length(unique(dat.yr[jurisdiction == juris]$id))*0.80))
dat.sub<- dat.yr[!(id %in% indivs)]


# distribution parameters from data set up
targets::tar_load(distparams)
dtparams <- setDT(stack(distparams))

## Study area ----
canPoly <- vect(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))
studyArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))
ab.range <- vect(file.path(canada, 'AB_CaribouSubregionalPlanBoundaries', 'CARIBOU_SUB_REGIONAL_PLAN_BOUNDARIES_2022_07_04.shp'))
### study area prep ----
crs <- st_crs(3978)$wkt
ab.range <- project(ab.range, studyArea)
studyArea.ab <- union(studyArea, ab.range)


canPoly <- project(canPoly, crs)

wbi.prov <- subset(canPoly, canPoly$PREABBR %in% c('Alta.', 'B.C.', 'Man.', 'N.W.T.', 'Sask.', 'Y.T.'))


bc <- subset(canPoly, canPoly$PREABBR %in% c('B.C.'))
nwt <- subset(canPoly, canPoly$PREABBR %in% c('N.W.T.', 'Y.T.'))
sk <- subset(canPoly, canPoly$PREABBR %in% c('Sask.'))
mb <- subset(canPoly, canPoly$PREABBR %in% c('Man.'))


## prelim move plots
dat.real <- dat.sub[case_==TRUE]

ggplot(dat.real, aes(x = jurisdiction, y=sl_/13)) + 
  geom_boxplot()

ggplot(dat.real, aes(x = jurisdiction, y = sl_/13)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) 

### TODO requires more memory than I have right now, must be better way
ggplot(dat.real, aes(x = jurisdiction, y = sl_/13)) +
  
  # add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  ggdist::stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.25
  )


ggplot()+
  # basemap
  geom_sf(data = wbi.prov)+
  
  # lines and points
  geom_path(data = dat.real, 
            aes(x=x2_,y=y2_,group=id,color=sl_), 
            alpha = 0.3)+
  geom_point(data = dat.real, 
             aes(x=x2_,y=y2_,group=id,fill=sl_),
             alpha = 0.7, shape=21, size = 2)+
  
  # formatting
  scale_fill_viridis_c(option = "inferno")+
  scale_color_viridis_c(option = "inferno")+
  scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL, 
       fill = 'Speed (m/13hour)', 
       color = 'Speed (m/13hour)')+
  theme_dark()+
  theme(panel.grid = element_blank())

### lines ----
trk <- dat.real |>
  make_track(x1_, y1_, t1_, all_cols = T, crs = crs) |> 
  nest(data = -"id") |>
  mutate(lns = map(data, function(x)
    x |> as_sf_lines()))

lns <- trk |>
  select(id,lns) #|>
#unnest(cols = lns) 
all.lns <- st_as_sf(rbindlist(lns$lns))
