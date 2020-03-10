
require(data.table)
require(lubridate)
require(magrittr)

genfxcdr=function(fp='/if/udata/m1gyl00/data/fxswap/holidays.csv'){
  #' read in a holiday file (downloaded from BBG CDR screen) and generate holiday calendar for each ccy
  require(bizdays)
  holidays <- fread(fp)
  names(holidays) <- str_to_lower(names(holidays)) %>% str_remove(' ')
  holidays[,date:=mdy(date)]
  holidays <- holidays %>% unique(by=c('date','cdrcode'))
  #
  print('TE is Europe; FD is US')
  holidays[,.N,cdrcode] %>% print
  
  holidays %>% setkey(date,cdrcode)
  cdr <- list()
  iccy='ccy'
  cdr[['eur']] <- bizdays::create.calendar(iccy,holidays = holidays[cdrcode %in% c('TE'),date %>% unique()], weekdays=c("saturday", "sunday"),adjust.from = "following",adjust.to='following')
  
  cdr[['usd']] <- bizdays::create.calendar(iccy,holidays = holidays[cdrcode %in% c('FD'),date %>% unique()], weekdays=c("saturday", "sunday"),adjust.from = "following",adjust.to='following')
  
  cdr[['eurusd']] <- bizdays::create.calendar(iccy,holidays = holidays[cdrcode %in% c('TE','FD'),date %>% unique()], weekdays=c("saturday", "sunday"),adjust.from = "following",adjust.to='following')
  
  cdr[['jpy']] <- bizdays::create.calendar(iccy,holidays = holidays[cdrcode %in% c('JN'),date %>% unique()], weekdays=c("saturday", "sunday"),adjust.from = "following",adjust.to='following')
  
  cdr[['jpyusd']] <- bizdays::create.calendar(iccy,holidays = holidays[cdrcode %in% c('JN','FD'),date %>% unique()], weekdays=c("saturday", "sunday"),adjust.from = "following",adjust.to='following')
  
  # weekly calendar convention
  #' To get 1 week settle date: apply settle date on top of 7 plus days mapply(getspotsettledt,bizdays::add.bizdays(date,7,cdr[['weekly']]),'eur')]
  cdr[['weekly']] <- bizdays::create.calendar('weekly',holidays=ymd(str_c(2000:2032,'0101')),weekdays=c("saturday", "sunday"),adjust.from = "following",adjust.to='following')

  return(cdr)
}



getweeksettledt <- function(idate,ccystr='eur'){
  #' calculate week settle datae given horizon date: implements the following: https://en.wikipedia.org/wiki/Foreign_exchange_date_conventions
  
}


getspotsettledt <- function(idate,ccystr='eur'){
  #' calculate spot settle datae given horizon date: implements the following: https://en.wikipedia.org/wiki/Foreign_exchange_date_conventions
  #' require list variable "cdr" that is a list of  bizdays calendar objects for 'eur', 'usd', and joint 'eurusd'
  #' @example getspotsettledt(ymd(20190203))
  #' @example testdt <- data.table(date=c(ymd(20180101),ymd(20180203),ymd(20180402)))
  #' testdt[date %between% c(cdr[['usd']]$start.date,cdr[['usd']]$end.date),settledtspot:=mapply(getspotsettledt,date,'eur')]
  #' testdt[,settledtspot:=as_date(settledtspot)]
  #' testdt
  
  
  require(lubridate); require(bizdays)
  z=as_date(max(add.bizdays(idate,2,cdr[[ccystr]]),add.bizdays(idate,1,cdr[['usd']])))
  return(as_date(adjust.next(z,cal=cdr[[paste0(ccystr,'usd')]])))
}