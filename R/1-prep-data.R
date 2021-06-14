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
  prep_sk[[dd]][,`:=`(date = convert_to_date(date))]
  prep_sk[[dd]][,time:=as.ITime(as.character(time)) ]
  prep_sk[[dd]][,datetime:= as.POSIXct(paste(date,time, sep = ' '))]
}

dat_sk <- rbindlist(prep_sk)


colnames(prep_sk[[1]])
prep_sk[]

# .(pd_start = colnames(data[[1]]) %like% 'Start Date',
#      pd_end = colnames(data[[1]]) %like% 'End Date',
#      date = colnames(data[[1]]) %like% 'Sample Date',
#      time = as.ITime(as.character(colnames(data[[1]]) %like% 'Start Time')),
#      datum = Datum,
#      precision = as.character(colnames(data[[1]]) %like% 'Precision'),
#      lat = colnames(data[[1]]) %like% 'Latitude',
#      long = colnames(data[[1]]) %like% 'Longitude',
#      UTMzone = colnames(data[[1]]) %like% 'Zone',
#      northing = colnames(data[[1]]) %like% 'Northing',
#      easting =  colnames(data[[1]]) %like% 'Easting',
#      id = as.character(colnames(data[[1]]) %like% 'Individual'),
#      comments = as.character(Comments))])]


dat_sk[,time:=as.ITime(as.character(`Sample Time (24hr Clock)`)) ]
dat_sk[,datetime:= as.POSIXct(paste(`Sample Date                              (yyyy-mm-dd)`, 
                                    time, sep = ' '))]




