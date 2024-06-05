# === Gather WBI herds -------------------------------------
# Julie Turner
# started 24 May 2024

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf',
          'ggplot2', 'tidyterra')
lapply(libs, Require::Require, character.only = TRUE)


### Input data ----
raw <-  file.path('data', 'raw-data')
derived <- file.path('data', 'derived-data')
juris.herds <- file.path(raw, 'juris_herds')

crs <- st_crs(3978)$wkt

#### ECCC data ----
# Extracting the caribou herds needed from the full critical habitat db
# ch <- st_read(file.path(raw, 'CriticalHabitat', 'CriticalHabitat.gdb'), layer = 'CriticalHabitatArea')
# herds <- filter(ch, CommName_E =='Woodland Caribou (Boreal population)')
# st_write(herds, file.path(raw, 'ECCC_boreal_herds.shp'))

# herds$juris <- gsub('[0-9]+', '', herds$SiteID)
# wbi.herds <- subset(herds, herds$juris %in% 
#                       c('BC', 'MB', 'NT', 'SK') 
#                     & !(herds$SiteID %in% c('SK2'))
#                     | (herds$SiteID == c('AB1')))

eccc <- vect(file.path(raw, 'ECCC_boreal_herds.shp'))
if (isFALSE(crs(eccc) == crs)){
  eccc <- project(eccc, crs)
}
eccc$juris <- gsub('[0-9]+', '', eccc$SiteID)
mb.herds <- subset(eccc, eccc$juris %in% 
                      c('MB'))
mb.herds$herd <- mb.herds$SitNm_E

bc <- vect(file.path(juris.herds, 'bc', 'Boreal_caribou_ranges_June2018.shp'))
if (isFALSE(crs(bc) == crs)){
 bc <- project(bc, crs)
}
bc$herd <- bc$Range

nwt <- vect(file.path(juris.herds, 'nwt', 'BoCaPopulationRevisions2023.shp'))
if (isFALSE(crs(nwt) == crs)){
  nwt <- project(nwt, crs)
}
# merging into herds that have pop trends estimated
nwt$herd <- ifelse(nwt$Region == 'Dehcho', 'Dehcho', 
                   ifelse(nwt$Region == 'Beaufort Delta', 'Gwichin', nwt$Name))
nwt2 <- aggregate(nwt, by = 'herd')
# Gwich'in South + Gwich'in North = Gwich'in 
# MVH + Decho North + Dehcho South = Dehcho

sk <- vect(file.path(juris.herds, 'sk', 'Sk_Woodland_Caribou_Range_Bnd.shp'))
if (isFALSE(crs(sk) == crs)){
  sk <- project(sk, crs)
}
sk1 <- subset(sk, sk$RNGEUNIT == 'SK1')
sk1$herd <- 'Boreal Shield'


wbi.herds <- rbind(mb.herds, bc, nwt2, sk1)
plot(wbi.herds)

writeVector(wbi.herds, file.path(juris.herds, 'wbi_herds.shp'))




