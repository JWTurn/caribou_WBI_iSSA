### Prepare Data ====
# Julie Turner
# Started: July 14 2021

require(Require)
Require('reproducible')
Require('data.table')
Require('sf')
Require('amt')

### Input data ----
raw <- 'data/raw-data/'
raw.mb <- 'data/raw-data/MB_data/'
derived <- 'data/derived-data/'

#rgdal::ogrListLayers(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb'))

MI_Berens <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb'), "MI_Berens_Caribou_2020APR02_CLEAN_NAD83_Z14")
BERENS_RND <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb'), "BERENS_RND_GPS_2001_to_2004_complete_NAD83Z14")
BLDVN <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb'), "BLDVN_2000_to_2014_complete_NAD83Z14")
ATIKO <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb'), "ATIKO_2000_to_2014_complete_NAD83Z14")

#rgdal::ogrListLayers(file.path(raw.mb, 'CaribouGPSData_Interlake.gdb'))

interlake <- st_read(file.path(raw.mb, 'CaribouGPSData_Interlake.gdb'), "Interlake_Total_Jan2021")

#rgdal::ogrListLayers(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb'))

MI_NWH <- st_read(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb'), "MI_NWH_Caribou_Telemetry2020APR02_Clean_NAD83")
charron1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb'), "CharronLK_GPS_MBHYDRO_Q2_2010_072020")
charron2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb'), "CharronLK_GPS_MBHYDRO_Q4_2010_022021")

#rgdal::ogrListLayers(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'))
# imperial doesn't have enough data for an SSA, it's just VHF
MBhydro <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "MBHYDRO_Q2_2010_072020")
naosap1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "Naosap_Reed_Total_NAD83_July2018")
naosap2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "gps_naosap_2002_06")
kississing <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "Kississing_Total_NAD83_July2018")
#imperial <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "Imperial_NAD83_2011")
naosap3 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "NaosapReed_MBHYDRO_Q4_2010_022021")

#rgdal::ogrListLayers(file.path(raw.mb, 'CaribouGPSdata_Owl_Flintstone.gdb'))

flintstone <- st_read(file.path(raw.mb, 'CaribouGPSdata_Owl_Flintstone.gdb'), "Owl_Flintstone_1995_2018_complete_WGS84")

#rgdal::ogrListLayers(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb'))

wimwap1<- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb'), "Wim_Wap_GPS_MBHydro_Q2_2010_072020")
wimwap2 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb'), "Wim_Wap_GPS_MBHYDRO_Q4_2010_022021")
harding1 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb'), "Harding_GPS_MBHydro_Q2_2010_072020")
harding2 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb'), "Harding_GPS_MBHYDRO_Q4_2010_022021")
wheadon1 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb'), "Wheadon_GPS_MBHydro_Q2_2010_072020")
wheadon2 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb'), "Wheadon_Total_NAD83_Nov2018_Final")
wheadon3 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb'), "Wheadon_GPS_MBHYDRO_Q4_2010_022021")

#rgdal::ogrListLayers(file.path(raw.mb, 'CaribouGPSdata_TheBog.gdb'))

bog1<- st_read(file.path(raw.mb, 'CaribouGPSdata_TheBog.gdb'), "TheBog_MH_2010_072020")
bog2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_TheBog.gdb'), "TheBog_MBHYDRO_Q4_2010_022021")

#rgdal::ogrListLayers(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb'))

william <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb'), "William_Lake_Total_NAD83_Dec2019")
wabowden1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb'), "Wabowden_GPS_MH_Q2_2010_072020")
wabowden2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb'), "Wabowden_UHF_relocations_2009to2012_NAD83")
wabowden3 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb'), "Wabowden_GPS_MBHYDRO_Q4_2010_022021")

### Prep data ----
### convert to NAD83/Canada Atlas Lambert (need units to be m and consistent across)
outcrs <- st_crs(3978)

out_MI_Berens <- st_transform(MI_Berens, outcrs)
out_BERENS_RND <- st_transform(BERENS_RND, outcrs)
out_BLDVN <- st_transform(BLDVN, outcrs)
out_ATIKO <- st_transform(ATIKO, outcrs)

out_interlake <- st_transform(interlake, outcrs)

out_MI_NWH <- st_transform(MI_NWH, outcrs)
out_charron1 <- st_transform(charron1, outcrs)
out_charron2 <- st_transform(charron2, outcrs)

out_MBhydro <- st_transform(MBhydro, outcrs)
out_naosap1 <- st_transform(naosap1, outcrs)
out_naosap2 <- st_transform(naosap2, outcrs)
out_naosap3 <- st_transform(naosap3, outcrs)
out_kississing <- st_transform(kississing, outcrs)
#out_imperial <- st_transform(imperial, outcrs)

out_flintstone <- st_transform(flintstone, outcrs)

out_wimwap1 <- st_transform(wimwap1, outcrs)
out_wimwap2 <- st_transform(wimwap2, outcrs)
out_harding1 <- st_transform(harding1, outcrs)
out_harding2 <- st_transform(harding2, outcrs)
out_wheadon1 <- st_transform(wheadon1, outcrs)
out_wheadon2 <- st_transform(wheadon2, outcrs)
out_wheadon3 <- st_transform(wheadon3, outcrs)

out_bog1 <- st_transform(bog1, outcrs)
out_bog2 <- st_transform(bog2, outcrs)

out_william <- st_transform(william, outcrs)
out_wabowden1 <- st_transform(wabowden1, outcrs)
out_wabowden2 <- st_transform(wabowden2, outcrs)
out_wabowden3 <- st_transform(wabowden3, outcrs)


# checking for right formats and grabbing what need
DT_MI_Berens <- setDT(sfheaders::sf_to_df(out_MI_Berens, fill = T))
DT_MI_Berens <- DT_MI_Berens[,.(id = Animal_ID, Fix_Status, Range,
                                datetime = as.POSIXct(paste(paste(GMT_Year, GMT_Month, GMT_Day, sep = '-'),
                                                 paste(GMT_Hour, GMT_Minute, sep = ':'), sep = ' '),
                                                 tz = 'gmt', format = '%Y-%m-%d %H:%M'),
                                x, y)]
# TODO what is time tz for BERENS_RND, BLDVN?
DT_BERENS_RND <- setDT(sfheaders::sf_to_df(out_BERENS_RND, fill = T))
DT_BERENS_RND <- DT_BERENS_RND[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                                 datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'), 
                                                             TIME, sep = ' '), 
                                                       format = '%Y-%m-%d %H:%M:%OS'),
                                 x, y)]
DT_BLDVN <- setDT(sfheaders::sf_to_df(out_BLDVN, fill = T))
DT_BLDVN <-DT_BLDVN[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                            datetime = as.POSIXct(paste(gsub( " .*$", "", DATE ), 
                                                        TIME, sep = ' '), 
                                                  format = '%Y-%m-%d %H:%M:%OS'),
                            x, y)]
DT_ATIKO <- setDT(sfheaders::sf_to_df(out_ATIKO, fill = T))
DT_ATIKO <- DT_ATIKO[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                        datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'), 
                                                    TIME, sep = ' '), 
                                              format = '%Y-%m-%d %H:%M:%OS'),
                        x, y)]


DT_interlake <- setDT(sfheaders::sf_to_df(out_interlake, fill = T))
DT_interlake <- DT_interlake[,.(id = Animal_ID, Fix_Status, Range,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                            paste(Hour, Minute, Second, sep = ':'), sep = ' '), 
                                              format = '%Y-%m-%d %H:%M:%OS', tz = 'America/Chicago'),
                        x, y)]

DT_MI_NWH <- setDT(sfheaders::sf_to_df(out_MI_NWH, fill = T))
DT_MI_NWH <- DT_MI_NWH[,.(id = Animal_ID, Fix_Status, Range = RANGE,
                                datetime = as.POSIXct(paste(paste(GMT_Year, GMT_Month, GMT_Day, sep = '-'),
                                                            paste(GMT_Hour, GMT_Minute, sep = ':'), sep = ' '),
                                                      tz = 'gmt', format = '%Y-%m-%d %H:%M'),
                                x, y)]
DT_charron1 <- setDT(sfheaders::sf_to_df(out_charron1, fill = T))
# what is the tz for Charron 1 and 2?
DT_charron1 <- DT_charron1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
DT_charron2 <- setDT(sfheaders::sf_to_df(out_charron2, fill = T))
DT_charron2 <- DT_charron2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]

DT_MBhydro <- setDT(sfheaders::sf_to_df(out_MBhydro, fill = T))
DT_MBhydro <- DT_MBhydro[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
DT_naosap1 <- setDT(sfheaders::sf_to_df(out_naosap1, fill = T))
DT_naosap1 <- DT_naosap1[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'), 
                                                            paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
DT_naosap2 <- setDT(sfheaders::sf_to_df(out_naosap2, fill = T))
DT_naosap2 <- DT_naosap2[,.(id = UNIQUE_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                        datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'), 
                                                    paste(HOUR, MINUTE, SECOND, sep = ':'), sep = ' '), 
                                              format = '%Y-%m-%d %H:%M:%OS'),
                        x, y)]
DT_naosap3 <- setDT(sfheaders::sf_to_df(out_naosap3, fill = T))
DT_naosap3 <- DT_naosap3[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                            datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                        paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                  format = '%Y-%m-%d %H:%M:%OS'),
                            x, y)]
DT_kississing <- setDT(sfheaders::sf_to_df(kississing, fill = T))
DT_kississing <- DT_kississing[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                            datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'), 
                                                        paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '), 
                                                  format = '%Y-%m-%d %H:%M:%OS'),
                            x=-1*x, y)]
# Don't use imperial, not enough info for SSA
#DT_imperial <- setDT(sfheaders::sf_to_df(out_imperial, fill = T))

DT_flintstone <- setDT(sfheaders::sf_to_df(out_flintstone, fill = T))
DT_flintstone <- DT_flintstone[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range,
                        datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'), 
                                                    TIME, sep = ' '), 
                                              format = '%Y-%m-%d %H:%M:%OS'),
                        x, y)]

DT_wimwap1 <- setDT(sfheaders::sf_to_df(out_wimwap1, fill = T))
DT_wimwap1 <- DT_wimwap1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                            datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                        paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                  format = '%Y-%m-%d %H:%M:%OS'),
                            x, y)]
DT_wimwap2 <- setDT(sfheaders::sf_to_df(out_wimwap2, fill = T))
DT_wimwap2 <- DT_wimwap2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                            datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                        paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                  format = '%Y-%m-%d %H:%M:%OS'),
                            x, y)]
DT_harding1 <- setDT(sfheaders::sf_to_df(out_harding1, fill = T))
DT_harding1 <- DT_harding1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                            datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                        paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                  format = '%Y-%m-%d %H:%M:%OS'),
                            x, y)]
DT_harding2 <- setDT(sfheaders::sf_to_df(harding2, fill = T))
DT_harding2 <- DT_harding2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                            datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                        paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                  format = '%Y-%m-%d %H:%M:%OS'),
                            x, y)]
DT_wheadon1 <- setDT(sfheaders::sf_to_df(out_wheadon1, fill = T))
DT_wheadon1 <- DT_wheadon1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                            datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                        paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                  format = '%Y-%m-%d %H:%M:%OS'),
                            x, y)]
DT_wheadon2 <- setDT(sfheaders::sf_to_df(out_wheadon2, fill = T))
DT_wheadon2 <- DT_wheadon2[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                  datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'), 
                                                              paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '), 
                                                        format = '%Y-%m-%d %H:%M:%OS'),
                                  x=x, y)]
DT_wheadon3 <- setDT(sfheaders::sf_to_df(out_wheadon3, fill = T))
DT_wheadon3 <- DT_wheadon3[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]

DT_bog1 <- setDT(sfheaders::sf_to_df(out_bog1, fill = T))
DT_bog1 <- DT_bog1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
DT_bog2 <- setDT(sfheaders::sf_to_df(out_bog2, fill = T))
DT_bog2 <- DT_bog2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]

DT_william <- setDT(sfheaders::sf_to_df(william, fill = T))
DT_william <- DT_william[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                              datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'), 
                                                          paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x=-1 *x, y)]
DT_wabowden1 <- setDT(sfheaders::sf_to_df(out_wabowden1, fill = T))
DT_wabowden1 <- DT_wabowden1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                      datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                  paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                            format = '%Y-%m-%d %H:%M:%OS'),
                      x, y)]
DT_wabowden2 <- setDT(sfheaders::sf_to_df(out_wabowden2, fill = T))
DT_wabowden2 <- DT_wabowden2[,.(id = Unique_id, Fix_Status = NAV, Range = Population,
                                datetime = as.POSIXct(paste(paste(Year, sprintf("%02d", Month), sprintf("%02d", Day), sep = '-'), 
                                                            paste(sprintf("%02d", Hour), sprintf("%02d", Minute), sprintf("%02d", Second), sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                            x, y)]
DT_wabowden3 <- setDT(sfheaders::sf_to_df(out_wabowden3, fill = T))
DT_wabowden3 <- DT_wabowden3[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, sprintf("%02d", Month), sprintf("%02d", Day), sep = '-'), 
                                                            paste(sprintf("%02d", Hour), 00, 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]

mb.dt <- rbind(DT_ATIKO, DT_BERENS_RND, DT_BLDVN, DT_bog1, DT_bog2, DT_charron1, DT_charron2,
               DT_flintstone, DT_harding1, DT_harding2, DT_interlake, DT_kississing, DT_MBhydro,
               DT_MBhydro, DT_MI_Berens, DT_MI_NWH, DT_naosap1, DT_naosap2, DT_naosap3,
               DT_wabowden1, DT_wabowden2, DT_wabowden3, DT_wheadon1, DT_wheadon2, DT_wheadon3,
               DT_william, DT_wimwap1, DT_wimwap2)

# right now quick and dirty way to deal with outliers
dat_cleaner <- mb.dt[complete.cases(x,y, datetime) & x < 0]



### EXPLORE ----
# check if all observations are complete
all(complete.cases(dat_cleaner[,.(x,y, datetime)]))


# check for duplicated time stamps
dat_cleaner[,any(duplicated(datetime)), by = id]

# We have some duplicated time stamps, these need to be removed prior to creating a track.
DT <- unique(dat_cleaner, by = c('id', 'datetime'))


### track ####
trk <- DT %>% make_track(x,y, datetime, crs = st_crs(3978))
#trk <- trk %>% nest(data = -"id")

View(summarize_sampling_rate(trk))
fixrate <- trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(sr) #%>% unnest(cols = c(sr))
# This is actually all over the place, so I'm going to do based on the 3 hrs discussed


# save 'clean' data 
saveRDS(DT, paste0(derived, 'prepped-data/MBprepDat.RDS'))
