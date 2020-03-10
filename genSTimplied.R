rm(list=ls())
require(policyPlot)
source('path.txt')

load('data/FXswapshortterm.RData')
source('fxcalendarutil.R')
source(fpglutil)

cdr <- genfxcdr()

dtfwdspot[,.N,tenor]
dtfwdspot[tenor=='',tenor:='spot']
dtfwdspot[src=='',src:='eod']

#dtfwdspot[,.N,.(tenor,src)] %>% dcast(tenor~src)

## EUR
dton <- dtfwdspot[tenor %in% c('ON','TN','SN','spot') & ccy=='EUR'] %>% melt(id.vars=c('date','ccy','tenor','src')) %>% dcast(date+ccy+src~tenor+variable,value.var='value')

dton[,T2:=mapply(getspotsettledt,date,'eur')]
dton[,T2:=as_date(T2)]

dton[ccy=='EUR',T1:=bizdays::add.bizdays(date,1,cdr[['eurusd']])]
dton[ccy=='EUR',TS1:=bizdays::add.bizdays(T2,1,cdr[['eurusd']])]

dton[,N0_1:=as.numeric(T1-date)]
dton[,N1_2:=as.numeric(T2-T1)]
dton[,NS_1:=as.numeric(TS1-T2)]

require(purrr)
dtr <- rbindlist(rates %>% map(.,data.table) ,idcol = 'tic')
dtr[,.N,.(tic)]
#dtr[tic=='eudr1t curncy']

#eonia <- fame2dt(list(rlc='RIISPUIO_N.B.EU',spot='spot6am.eu'),'fm')
#dton2 <- merge(dton[ccy=='EUR'],eonia,by='date',all.x = T)

dton2 <- merge(dton[ccy=='EUR'],dtr[tic=='eudr1t curncy',.(date,rlc_ask=px_ask,rlc_bid=px_bid)],by='date',all.x = T)
# O/N
dton2[,ONimp_ask:=((1+rlc_ask/100*N0_1/360)*(spot_px_bid-TN_px_ask/10000)/(spot_px_bid-TN_px_ask/10000-ON_px_ask/10000)-1)*100*(360/N0_1)]
dton2[,ONimp_bid:=((1+rlc_bid/100*N0_1/360)*(spot_px_ask-TN_px_bid/10000)/(spot_px_ask-TN_px_bid/10000-ON_px_bid/10000)-1)*100*(360/N0_1)]
# T/N
dton2[N1_2>0,TNimp_ask:=((1+rlc_ask/100*N1_2/360)*(spot_px_bid)/(spot_px_bid-TN_px_ask/10000)-1)*100*(360/N1_2)]
dton2[N1_2>0,TNimp_bid:=((1+rlc_bid/100*N1_2/360)*(spot_px_ask)/(spot_px_ask-TN_px_bid/10000)-1)*100*(360/N1_2)]

# S/N
dton2[,SNimp_ask:=((1+rlc_ask/100*NS_1/360)/(spot_px_ask)*(spot_px_ask+SN_px_ask/10000)-1)*100*(360/NS_1)]
dton2[,SNimp_bid:=((1+rlc_bid/100*NS_1/360)/(spot_px_bid)*(spot_px_bid+SN_px_bid/10000)-1)*100*(360/NS_1)]

dton2[date==ymd(20190917) & src=='eod']
dton2[,ONmid:=(ONimp_ask+ONimp_bid)/2]
dton2[,ONbidask:=ONimp_ask-ONimp_bid]
dton2[,TNmid:=(TNimp_ask+TNimp_bid)/2]
dton2[,TNbidask:=TNimp_ask-TNimp_bid]
dton2[,SNmid:=(SNimp_ask+SNimp_bid)/2]
dton2[,SNbidask:=SNimp_ask-SNimp_bid]

dton2 %>% fwrite('fxrawdata.csv')

euronimp <- dton2[date %ni% cdr[['eurusd']]$holidays] %>% copy()
###

dton2[,.(ONbidask,SNbidask,TNbidask)] %>% summary
euronimp[year(date)>=2007,.(date,ONimp_ask,ONimp_bid,src)] %>% melt(id.var=c('date','src')) %>% dcast(date~src+variable,value.var='value') %>% gpltw()

euronimp[src=='EOD' & year(date)>=2015,.(date,ONimp_bid,ONimp_ask,src)] %>% melt(id.var=c('date','src')) %>% dcast(date~src+variable,value.var='value') %>% gpltw()

euronimp[src==' F080' & year(date)==2019,.(date,ONmid,TNmid,SNmid,src)] %>% melt(id.var=c('date','src')) %>% dcast(date~src+variable,value.var='value') %>% gpltw()

#euronimp[src==' F080',.(date,ONmid,TNmid,SNmid,src)] %>% fwrite('shortimp.csv')


euronimp[year(date)>=2007,.(date,ONimp_ask,ONimp_bid,src)] %>% melt(id.var=c('date','src')) %>% dcast(date~src+variable,value.var='value') %>% gpltw()

euronimp[year(date)>=2018,.(date,ONimp_ask,ONimp_bid,src)] %>% melt(id.var=c('date','src')) %>% dcast(date~src+variable,value.var='value') %>% gpltw()

#eonia <- fame2dt(list(rlc='RIISPUIO_N.B.EU',spot='spot6am.eu'),'fm')

# IOR rates
dtior <- fame2dt(c(iorusd='faimtn',gcf='gcftrsi',sofr='sofr',bgcr='bgcr',tgcr='tgcr',effr='ffedt'),'ifhaver_daily',start=20071215)
dtior
dtcomp=merge(euronimp,dtior,by='date')
#dtcomp[year(date)>=2013 & src==' F080'] %>% fwrite('impliedfx.csv')
dtcomp[year(date)==2018 & src==' F080',.(date,ONmid,SNmid,TNmid,gcf,sofr,effr)] %>% gpltw


dtcomp[year(date)==2017 & src==' F080',.(date,ONmid,SNmid,TNmid,gcf,sofr)] %>% gpltw

monthend <- dtcomp[,max(date),floor_date(date,'month')]$V1

#dtcomp %>% fwrite('EURImpliedShortRatesRaw.csv')
dtcomp[year(date)>=2014 & src==' F080',.(date,ONmid,SNmid,TNmid,gcf,sofr,T1,T2,TS1)] %>% fwrite('EURImpliedShortRates.csv')


####
## JPY



cdr <- genfxcdr()

dtfwdspot[,.N,tenor]
dtfwdspot[tenor=='',tenor:='spot']
dtfwdspot[src=='',src:='eod']

## JPY
dton <- dtfwdspot[tenor %in% c('ON','TN','SN','spot') & ccy=='JPY'] %>% melt(id.vars=c('date','ccy','tenor','src')) %>% dcast(date+ccy+src~tenor+variable,value.var='value')

dton[,T2:=mapply(getspotsettledt,date,'jpy')]
dton[,T2:=as_date(T2)]

dton[ccy=='JPY',T1:=bizdays::add.bizdays(date,1,cdr[['jpyusd']])]
dton[ccy=='JPY',TS1:=bizdays::add.bizdays(T2,1,cdr[['jpyusd']])]

dton[,N0_1:=as.numeric(T1-date)]
dton[,N1_2:=as.numeric(T2-T1)]
dton[,NS_1:=as.numeric(TS1-T2)]

dton[year(date)==2019 & month(date) %in% c(4,5),.(date,T1,T2,TS1,N0_1,N1_2,NS_1)]

dton[,.(date,T1,T2,TS1,N0_1,N1_2,NS_1)] %>% unique(by='date') %>% fwrite('JPYDayCount.csv')



### BBG tickers:

#' dtrates <- rates %>% rbindlist(use.names = T,idcol = 'ticker')
#' 
#' ticker0 <- dtrates[,.N,ticker]# %>% fwrite('data/bbgtickers.csv')
#' 
#' #'
#' #'
#' ccy='eur';tenor='on';isrc=' F080'
#' 
#' tmplist<-list()
#' for (ccy in c('EUR','JPY','CHF','GBP','CAD','AUD')){
#'   for(tenor in c('ON','TN','SN','')){
#'     for (isrc in c(' F080','')){
#'       ticker=str_c(ccy,tenor,isrc,' Curncy')
#'       tmplist[[length(tmplist)+1]]<-data.table(ticker=ticker)
#'     }
#'   }
#' }
#' ticker1<-rbindlist(tmplist)
#' ticker2=data.table(ticker=c('eddr01d index','usrg1t icus curncy','usrg1z icus curncy','us00o/n index', 'eonia index','eudr1t curncy','jydr1t curncy','jy00s/n index','bp00o/n index','sfdr1t index','cclc index','addr1t index'))
#' ticker0
# ticker4=data.table(ticker=c('eousff1 icpl curncy','eousff5 icpl curncy','eousff10 icpl curncy','jybsf1 icpl curncy','jybsf5 icpl curncy','jybsf10 icpl curncy','bpbsf1 icpl curncy','bpbsf5 icpl curncy','bpbsf10 icpl curncy'))
# tickers <- rbindlist(list(ticker0,ticker1,ticker2,ticker4),fill=T)[,.(ticker=unique(ticker))]
# tickers %>% fwrite('data/bbgtickers.csv')
# 

