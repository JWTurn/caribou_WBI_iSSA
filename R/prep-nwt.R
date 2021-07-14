require(Require)
Require('move')
Require('data.table')

loginStored <- movebankLogin(username=Sys.getenv('moveUserID'), 
                             password=Sys.getenv('movePwd'))

dehcho <- getMovebankData(study = 'ABoVE: NWT Dehcho Boreal Woodland Caribou',
                          login = loginStored)
dat_dehcho <- setDT(dehcho@data)
dat_dehcho <- dat_dehcho[,.(id=tag_id, herd=habitat, 
                            location_long, location_lat, datetime = timestamp)]
