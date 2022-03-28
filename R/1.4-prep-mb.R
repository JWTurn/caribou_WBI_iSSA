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

MBhydro <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "MBHYDRO_Q2_2010_072020")
naosap1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "Naosap_Reed_Total_NAD83_July2018")
naosap2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "gps_naosap_2002_06")
kississing <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "Kississing_Total_NAD83_July2018")
imperial <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb'), "Imperial_NAD83_2011")
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
DT_MI_Berens <- setDT(sfheaders::sf_to_df(out_MI_Berens, fill = T))
out_BERENS_RND <- st_transform(BERENS_RND, outcrs)
DT_BERENS_RND <- setDT(sfheaders::sf_to_df(out_BERENS_RND, fill = T))
out_BLDVN <- st_transform(BLDVN, outcrs)
DT_BLDVN <- setDT(sfheaders::sf_to_df(out_BLDVN, fill = T))
out_ATIKO <- st_transform(ATIKO, outcrs)
DT_ATIKO <- setDT(sfheaders::sf_to_df(out_ATIKO, fill = T))

out_interlake <- st_transform(interlake, outcrs)
DT_interlake <- setDT(sfheaders::sf_to_df(out_interlake, fill = T))

out_MI_NWH <- st_transform(MI_NWH, outcrs)
DT_MI_NWH <- setDT(sfheaders::sf_to_df(out_MI_NWH, fill = T))
out_charron1 <- st_transform(charron1, outcrs)
DT_charron1 <- setDT(sfheaders::sf_to_df(out_charron1, fill = T))
out_charron2 <- st_transform(charron2, outcrs)
DT_charron2 <- setDT(sfheaders::sf_to_df(out_charron2, fill = T))

out_MBhydro <- st_transform(MBhydro, outcrs)
DT_MBhydro <- setDT(sfheaders::sf_to_df(out_MBhydro, fill = T))
out_naosap1 <- st_transform(naosap1, outcrs)
DT_naosap1 <- setDT(sfheaders::sf_to_df(out_naosap1, fill = T))
out_naosap2 <- st_transform(naosap2, outcrs)
DT_naosap2 <- setDT(sfheaders::sf_to_df(out_naosap2, fill = T))
out_naosap3 <- st_transform(naosap3, outcrs)
DT_naosap3 <- setDT(sfheaders::sf_to_df(out_naosap3, fill = T))
out_kississing <- st_transform(kississing, outcrs)
DT_kississing <- setDT(sfheaders::sf_to_df(kississing, fill = T))
out_imperial <- st_transform(imperial, outcrs)
DT_imperial <- setDT(sfheaders::sf_to_df(out_imperial, fill = T))

out_flintstone <- st_transform(flintstone, outcrs)
DT_flintstone <- setDT(sfheaders::sf_to_df(out_flintstone, fill = T))

out_wimwap1 <- st_transform(wimwap1, outcrs)
DT_wimwap1 <- setDT(sfheaders::sf_to_df(out_wimwap1, fill = T))
out_wimwap2 <- st_transform(wimwap2, outcrs)
DT_wimwap2 <- setDT(sfheaders::sf_to_df(out_wimwap2, fill = T))
out_harding1 <- st_transform(harding1, outcrs)
DT_harding1 <- setDT(sfheaders::sf_to_df(out_harding1, fill = T))
out_harding2 <- st_transform(harding2, outcrs)
DT_harding2 <- setDT(sfheaders::sf_to_df(harding2, fill = T))
out_wheadon1 <- st_transform(wheadon1, outcrs)
DT_wheadon1 <- setDT(sfheaders::sf_to_df(out_wheadon1, fill = T))
out_wheadon2 <- st_transform(wheadon2, outcrs)
DT_wheadon2 <- setDT(sfheaders::sf_to_df(out_wheadon2, fill = T))
out_wheadon3 <- st_transform(wheadon3, outcrs)
DT_wheadon3 <- setDT(sfheaders::sf_to_df(out_wheadon3, fill = T))

out_bog1 <- st_transform(bog1, outcrs)
DT_bog1 <- setDT(sfheaders::sf_to_df(out_bog1, fill = T))
out_bog2 <- st_transform(bog2, outcrs)
DT_bog2 <- setDT(sfheaders::sf_to_df(out_bog2, fill = T))

out_william <- st_transform(william, outcrs)
DT_william <- setDT(sfheaders::sf_to_df(william, fill = T))
out_wabowden1 <- st_transform(wabowden1, outcrs)
DT_wabowden1 <- setDT(sfheaders::sf_to_df(out_wabowden1, fill = T))
out_wabowden2 <- st_transform(wabowden2, outcrs)
DT_wabowden2 <- setDT(sfheaders::sf_to_df(out_wabowden2, fill = T))
out_wabowden3 <- st_transform(wabowden3, outcrs)
DT_wabowden3 <- setDT(sfheaders::sf_to_df(out_wabowden3, fill = T))



# checking for right formats and grabbing what need
kmb.dt <- as.data.table(kmb)
kmb.dt <- kmb.dt[!(is.na(Animal_ID)) & Animal_ID != 'None',.(id = Animal_ID, Region, Population_Unit, 
                                                             datetime = GMT_FixDateTime, Longitude, Latitude)]
regional.dt <- as.data.table(regional)
regional.dt[, datetime := FixDateTime + hours(8)]
regional.dt <- regional.dt[!(is.na(Animal_ID)) & Animal_ID != 'None',.(id = Animal_ID, Region, Population_Unit, 
                                                                       datetime, Longitude, Latitude)]
dat<- rbind(kmb.dt, regional.dt)

dat_cleaner <- dat[complete.cases(Longitude,Latitude, datetime)]


### convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
crs <- st_crs(4326)$wkt
outcrs <- st_crs(3978)

sfboo <- st_as_sf(dat_cleaner, coords = c('Longitude', 'Latitude'),
                  crs = crs)
outboo <- st_transform(sfboo, outcrs)
boo <- setDT(sfheaders::sf_to_df(outboo, fill = T))



### EXPLORE ----
# check if all observations are complete
all(complete.cases(boo[,.(x,y, datetime)]))


# check for duplicated time stamps
boo[,any(duplicated(datetime)), by = id]

# We have some duplicated time stamps, these need to be removed prior to creating a track.
DT <- unique(boo, by = c('id', 'datetime'))


### track ####
trk <- DT %>% make_track(x,y, datetime, crs = st_crs(3978))
#trk <- trk %>% nest(data = -"id")

View(summarize_sampling_rate(trk))
fixrate <- trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(sr) #%>% unnest(cols = c(sr))
# This is actually all over the place, so I'm going to do based on the 12 hrs discussed
# we know they have shorter fixes during calving
# ~ 12 hour fix rates

# save 'clean' data 
saveRDS(boo, paste0(derived, 'prepped-data/BCprepDat.RDS'))
