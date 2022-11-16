### Combine Prepared Data ====
# Julie Turner
# Started: November 9 2022

require(Require)
Require('reproducible')
Require('data.table')
Require('sf')
Require('dplyr')


### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'
prepped <- ('data/derived-data/prepped-data/')

mb<- readRDS(file.path(prepped, 'MBprepDat.RDS'))
sk<- readRDS(file.path(prepped, 'SKprepDat.RDS'))
bc<- readRDS(file.path(prepped, 'BCprepDat.RDS'))
nwt<- readRDS(file.path(prepped, 'NWTprepDat.RDS'))
yt<- readRDS(file.path(prepped, 'YTprepDat.RDS'))

# mbcoords<- mb%>%st_as_sf(coords = c('x','y'))%>%
#   st_set_crs(st_crs(3978)$wkt)

dat.all <- rbind(nwt[,.(id, jurisdiction = 'nwt', pop = area, subpop = habitat, datetime, x, y)],
                mb[,.(id, jurisdiction = 'mb', pop = Range, subpop = NA, datetime, x, y)],
                sk[,.(id, jurisdiction = 'sk', pop = 'SK1', subpop = NA, datetime, x, y)],
                bc[,.(id, jurisdiction = 'bc', pop = Population_Unit, subpop = NA, datetime, x, y)],
                yt[,.(id, jurisdiction = 'yt', pop = "Yukon", subpop = NA, datetime, x, y)]
                )
saveRDS(dat.all, file.path(prepped, "WBIprepDat.RDS"))

