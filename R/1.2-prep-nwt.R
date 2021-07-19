### Prepare Data ====
# Julie Turner
# Started: July 14 2021

require(Require)
Require('reproducible')
Require('move')
Require('data.table')
Require('sf')

### Input data ----
raw <- 'data/raw-data/'
raw.nwt <- 'data/raw-data/NWT_data/'
derived <- 'data/derived-data/'

cPath <- file.path(tempdir(), "cache")

# This will have to be set up in the .Renviron on your own computer
loginStored <- movebankLogin(username=Sys.getenv('moveUserID'), 
                             password=Sys.getenv('movePwd'))
# NWT dataset names to fill in loop
dsNames <-c('Dehcho Boreal Woodland', 'Inuvik Boreal Woodland', 
            'North Slave Boreal', 'Sahtu Boreal Woodland', 'South Slave Boreal Woodland') 
# make a list of all data from Movebank
# TODO: update this with prepInputs()
nwt.move <- list()
for (ds in 1:length(dsNames)) {
  #hh = 1
  nwt.move[[ds]]<-getMovebankData(study =paste0( 'ABoVE: NWT ', dsNames[[ds]],' Caribou'),
                                  login = loginStored)
}
#saveRDS(nwt.move, paste0(raw.nwt, 'NWTmoveDat.RDS'))

# pull just the data
hab <-c('dehcho', 'inuvik', 'north.slave', 'sahtu', 'south.slave') 


nwt <- rbindlist(lapply(1:length(hab), function(hh){
  nwt[,.(area=hab[[hh]], dat=list(setDT(nwt.move[[hh]]@data)))]
})
)

#colnames(nwt$dat[[2]])
nwt.long <- rbindlist(lapply(1:length(hab), function(hh){
  nwt$dat[[hh]][,.(area = hab[[hh]], habitat, id=tag_id, 
                location_long, location_lat, datetime = timestamp)]
  
}))

crs <- CRS(st_crs(4326)$wkt)
outcrs <- st_crs(3978)

sfboo <- st_as_sf(nwt.long, coords = c('location_long', 'location_lat'),
                  crs = crs)
outboo <- st_transform(sfboo, outcrs)
boo <- setDT(sfheaders::sf_to_df(outboo, fill = T))


saveRDS(boo, paste0(derived, 'prepped-data/NWTprepDat.RDS'))
