getwd()
rm(list=ls())
## load library and connect to Bloomberg
library(Rblpapi)
blpConnect()

tickers=c('eousff1 icpl curncy','eousff5 icpl curncy','eousff10 icpl curncy','jybsf1 icpl curncy','jybsf5 icpl curncy','jybsf10 icpl curncy','bpbsf1 icpl curncy','bpbsf5 icpl curncy','bpbsf10 icpl curncy')
raw<-Rblpapi::bdh(tickers,'px_last',start.date = Sys.Date()-365.25*12)

tmp <- list()


for (tic in tickers){
  df <- raw[[tic]] %>% as.data.table()
  df[,ticker:=tic]
  tmp[[length(tmp)+1]] <- df
}

dt <- rbindlist(tmp,use.names = T)

save(dt, file = '../Gordon/oisffxcbasisicpl.RData')


getwd()
