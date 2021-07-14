require(Require)
Require('move')
Require('data.table')

loginStored <- movebankLogin(username=Sys.getenv('moveUserID'), 
                             password=Sys.getenv('movePwd'))
# NWT dataset names to fill in loop
dsNames <-c('Dehcho Boreal Woodland', 'Inuvik Boreal Woodland', 
            'North Slave Boreal', 'Sahtu Boreal Woodland', 'South Slave Boreal Woodland') 
# make a list of all data from Movebank
nwt.move <- list()
for (ds in 1:length(dsNames)) {
  #hh = 1
  nwt.move[[ds]]<-getMovebankData(study =paste0( 'ABoVE: NWT ', dsNames[[ds]],' Caribou'),
                                  login = loginStored)
}

# pull just the data
habitat <-c('Dehcho', 'Inuvik', 'North Slave', 'Sahtu', 'South Slave') 

nwt <- data.table(habitat)

for (hh in 1:length(habitat)) {
  nwt[[hh]]<-nwt[,dat:=list(setDT(nwt.move[[hh]]@data))]
}
dat_dehcho <- setDT(dehcho@data)
dat_dehcho <- dat_dehcho[,.(id=tag_id, herd=habitat, 
                            location_long, location_lat, datetime = timestamp)]
