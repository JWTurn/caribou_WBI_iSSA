### Prepare Data ====
# Julie Turner
# Started: June 10 2021


### Packages ----
# remotes::install_github('bbolker/broom.mixed')
# remotes::install_github('ropensci/spatsoc')
libs <- c('data.table', 'readxl', 'fs', 'tidyr', 'purrr')
lapply(libs, require, character.only = TRUE)




### Input data ----
raw <- 'data/raw-data/'
sk <- 'data/raw-data/SK_data/'
derived <- 'data/derived-data/'



ls_sk <- as.character(dir_ls(path = sk))

dat_sk <- data.table()
# colTypes <- c('date', 'date', 'logical', 'date','date', 'skip', 'skip',
#               'text', 'text', 'numeric', 'numeric', 'text', 'numeric', 'numeric',
#               'text', 'text','text')
for (dd in 1:length(ls_sk)) {
  #dd=2
  path <- ls_sk[[dd]]
  temp <- setDT(read_excel(path = path, sheet = 'TelemetryData', skip = 3))#, 
                    # col_types = colTypes))
  dat_sk[dd,] <- dat_sk[,`:=`(file = path, data = list(temp))]
}
dat_sk <- data.table()
dat_sk <- rbindlist(lapply(seq(1:length(ls_sk)), function(i){
 dat_sk[,.(file= ls_sk[[i]], 
           data = list(setDT(read_excel(ls_sk[[i]], sheet = 'TelemetryData', skip = 3))))]
}))

dat_sk$file


dat_sk[,time:=as.ITime(as.character(`Sample Time (24hr Clock)`)) ]
dat_sk[,datetime:= as.POSIXct(paste(`Sample Date                              (yyyy-mm-dd)`, 
                                    time, sep = ' '))]



