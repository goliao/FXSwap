
require(stringr);require(lubridate)
setwd('U:\\GMSM\\Gordon\\data')
source('util.R')

#aa<-downloadbbg(list('usd curncy'))

require(Rblpapi)
Rblpapi::blpConnect()
opt <- c("periodicitySelection"='DAILY')

ccy='eur';tenor='on';isrc=' F080'

tmplist<-list()
for (ccy in c('EUR','JPY','CHF','GBP','CAD','AUD')){
  for(tenor in c('ON','TN','SN','')){
    for (isrc in c(' F080','')){
      ticker=str_c(ccy,tenor,isrc,' Curncy')
      print(ticker)
      tmp<-bdh(ticker, c('px_ask','px_bid'), start.date=ymd('2007-01-01'))
      tmplist[[length(tmplist)+1]]<-data.table(tmp)[,ccy:=ccy][,tenor:=tenor][,src:=isrc]
    }
  }
}

dtfwdspot<-rbindlist(tmplist)



ticker=c('eddr01d index','usrg1t icus curncy','usrg1z icus curncy','us00o/n index', 'eonia index','eudr1t curncy','jydr1t curncy','jy00s/n index','bp00o/n index','sfdr1t index','cclc index','addr1t index')
rates <- bdh(ticker, c('px_ask','px_bid'), start.date=ymd('2007-01-01'))

save(dtfwdspot,rates,file = 'FXswapshortterm.RData')



bdh(c('eur curncy','euron curncy'), c('zs090'), start.date=ymd('2019-01-01'))

eurcal <- bds('eur curncy', 'zs090')
jpycal <- bds('jpy curncy', 'zs090')
usdcal <- bds('usd curncy', 'zs090')
chfcal <- bds('chf curncy', 'zs090')
cadcal <- bds('cad curncy', 'zs090')
audcal <- bds('aud curncy', 'zs090')
gbpcal <- bds('gbp curncy', 'zs090')

save(eurcal,jpycal,usdcal,chfcal,cadcal,audcal,gbpcal,file='FXnosettlecalendar.RData')

save.image('bbgdownload_FXontnsntmp.RData')
