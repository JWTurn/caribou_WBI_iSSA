require(terra)
require(sf)
require(data.table)



### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
eccc.disturb <- file.path(canada, 'caribou-disturbance')

crs <- st_crs(3978)$wkt


dat<- readRDS(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat.RDS'))

studyArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))

# get a list of ECCC herd names
eccc <- vect(st_read(file.path(eccc.disturb, '_Caribou_51_Ranges_Aires_2012.gdb'), layer = 'Caribou_51_Ranges_Aires_2012'))
# I probably don't even need to do this, I just did, 
# this was just a way of getting a list of herd names semi-easily
if (isFALSE(crs(eccc) == crs)){
  eccc <- project(eccc, crs)
}
eccc$juris <- gsub('[0-9]+', '', eccc$CODE)

# subsets to WBI herds
wbi.herds <- subset(eccc, eccc$juris %in% 
                     c('MB', 'BC', 'NT', 'SK', 'AB'))
# make a list
ls.wbi <- wbi.herds$Herd_OS
# update misspelled and inconsistent names
ls.wbi <- replace(ls.wbi, ls.wbi=='AtikakiBernes', 'AtikakiBerens')
ls.layers <- replace(ls.wbi, ls.wbi== 'EastSideAthabascaRiver', 'ESAR')
ls.layers <- replace(ls.layers, ls.layers== 'WestSideAthabascaRiver', 'WSAR')
ls.layers <- replace(ls.layers, ls.layers== 'SaskatchewanBorealPlain', 'SaskBorealPlain')
ls.layers <- replace(ls.layers, ls.layers== 'SaskatchewanBorealShield', 'SaskBorealShield')
ls.layers <- replace(ls.layers, ls.layers== 'ManitobaSouthConservationUnit', 'ManitobaSouthCU')
ls.layers <- replace(ls.layers, ls.layers== 'ManitobaNorthConservationUnit', 'ManitobaNorthCU')
ls.layers <- replace(ls.layers, ls.layers== 'ManitobaEastConservationUnit', 'ManitobaEastCU')
ls.layers <- replace(ls.layers, ls.layers== 'NorthwestTerritories', 'NWT')

# load the line disturbance layers for each herd in the list
for (i in 1:length(ls.wbi)) { # for each file in the list
  fileName <- paste0(ls.wbi[[i]],'2020.gdb') # save filename of element i
  dataName <- paste0(ls.layers[[i]],'_lines') # save data name of element i
  lyrName <- paste0(ls.layers[[i]],'2020_Disturb_Perturb_Line') # save layer name of element i
  tempData <- vect(st_read (file.path(eccc.disturb, '2020', fileName), layer = lyrName, type = 5)) # read file, force as line
  assign (dataName, tempData, envir=.GlobalEnv)  # assign the results of file to the data named
  
}

# bind all lines together
wbi.lines <- rbind(AtikakiBerens_lines, Bistcho_lines, Calendar_lines,
                   CaribouMountains_lines, Chinchaga_lines, ColdLake_lines,
                   ESAR_lines, Kississing_lines, LittleSmoky_lines, ManitobaEastCU_lines,
                   ManitobaNorthCU_lines, ManitobaSouthCU_lines, Maxhamish_lines, Naosap_lines,
                   Nipisi_lines, NorthInterlake_lines, NWT_lines, OwlFlinstone_lines,
                   Parker_lines, Prophet_lines, RedEarth_lines, Reed_lines,
                   Richardson_lines, SaskBorealPlain_lines, SaskBorealShield_lines,
                   SlaveLake_lines, SnakeSahtahneh_lines, TheBog_lines, Wabowden_lines,
                   Wapisu_lines, WilliamLake_lines, WSAR_lines, Yates_lines)
wbi.lines.proj <- project(wbi.lines, crs)
writeVector(wbi.lines.proj, file.path(raw, 'ECCC_disturbance', 'WB_dist_2020_line.shp'), overwrite = T)


# load the polygonal disturbance layers for each herd in the list
for (i in 1:length(ls.wbi)) { # for each file in the list
  fileName <- paste0(ls.wbi[[i]],'2020.gdb') # save filename of element i
  dataName <- paste0(ls.layers[[i]],'_polys') # save data name of element i
  lyrName <- paste0(ls.layers[[i]],'2020_Disturb_Perturb_Poly') # save layer name of element i
  tempData <- vect(st_read (file.path(eccc.disturb, '2020', fileName), layer = lyrName, type = 6)) # read file, force as polygon
  assign (dataName, tempData, envir=.GlobalEnv)  # assign the results of file to the data named
  
}

# bind all polys together
wbi.polys <- rbind(AtikakiBerens_polys, Bistcho_polys, Calendar_polys,
                   CaribouMountains_polys, Chinchaga_polys, ColdLake_polys,
                   ESAR_polys, Kississing_polys, LittleSmoky_polys, ManitobaEastCU_polys,
                   ManitobaNorthCU_polys, ManitobaSouthCU_polys, Maxhamish_polys, Naosap_polys,
                   Nipisi_polys, NorthInterlake_polys, NWT_polys, OwlFlinstone_polys,
                   Parker_polys, Prophet_polys, RedEarth_polys, Reed_polys,
                   Richardson_polys, SaskBorealPlain_polys, SaskBorealShield_polys,
                   SlaveLake_polys, SnakeSahtahneh_polys, TheBog_polys, Wabowden_polys,
                   Wapisu_polys, WilliamLake_polys, WSAR_polys, Yates_polys)
wbi.polys.proj <- project(wbi.polys, crs)
writeVector(wbi.polys.proj, file.path(raw, 'ECCC_disturbance', 'WB_dist_2020_poly.shp'), overwrite = T)

