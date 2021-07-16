require(Require)
Require('reproducible')
Require('move')
Require('data.table')

cPath <- file.path(tempdir(), "cache")
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

# pull just the data
habitat <-c('dehcho', 'inuvik', 'north.slave', 'sahtu', 'south.slave') 

nwt<-data.table()
nwt <- rbindlist(lapply(1:length(habitat), function(hh){
  prep_nwt[,.(area=habitat[[hh]], dat=list(setDT(nwt.move[[hh]]@data)))]
})
)
for (hh in 1:length(habitat)) {
  #hh=1
  
}
dat_dehcho <- setDT(dehcho@data)
dat_dehcho <- dat_dehcho[,.(id=tag_id, herd=habitat, 
                            location_long, location_lat, datetime = timestamp)]
