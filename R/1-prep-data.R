### Prepare Data ====
# Julie Turner
# Started: June 10 2021


### Packages ----
# remotes::install_github('bbolker/broom.mixed')
# remotes::install_github('ropensci/spatsoc')
libs <- c('data.table', 'readxl', 'fs', 'tidyr', 'purrr', 'janitor')
lapply(libs, require, character.only = TRUE)


#### Functions ----
spaceless <- function(x) {colnames(x) <- gsub(" ", "", colnames(x));x}

### Input data ----
raw <- 'data/raw-data/'
sk <- 'data/raw-data/SK_data/'
derived <- 'data/derived-data/'


ls_sk <- as.character(dir_ls(path = sk))

temp <- data.table()
temp <- rbindlist(lapply(seq(1:length(ls_sk)), function(i){
 temp[,.(file= ls_sk[[i]], 
           data = list(setDT(read_excel(ls_sk[[i]], sheet = 'TelemetryData', skip = 3))))]
}))

temp[,data:= function(x){
  colnames(.SD) <- gsub(' ','', colnames(.SD))}, by = file]
temp$file

#### gather just needed data ####
colnames(temp[1]$data[[1]])
str(temp[1]$data[[1]]$`Sample Date                              (yyyy-mm-dd)`)
spaceless(temp[1]$data[[1]])

prep_sk <- lapply(ls_sk, function(ll){
  temp[file == ll,.(file, 
                    spaceless(data[[1]][,.SD, .SDcols = names(data[[1]]) %like% 'Sample|Latitude|Longitude|UTM|Datum|Individual|Comments & !Sensitive']))]
  })

colnames(prep_sk[[1]]) 
newnames <- c('file','date','time','datum','lat','long','UTMzone','northing','easting','id') 
for (dd in 1:length(ls_sk)) {
  setnames(prep_sk[[dd]], old = colnames(prep_sk[[dd]]), new = newnames)
  prep_sk[[dd]][,`:=`(date = as.IDate(convert_to_date(date)))]
  prep_sk[[dd]][,time:=as.ITime(as.character(time)) ]
  prep_sk[[dd]][,datetime:= as.POSIXct(paste(date,time, sep = ' '))]
}

dat_sk <- rbindlist(prep_sk)

saveRDS(dat_sk, paste0(derived, 'prepped-data/SKprepDat.RDS'))

