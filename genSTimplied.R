rm(list=ls())
#require(policyPlot)
source('path.txt')
source('fxcalendarutil.R')
source(fpglutil)
cdr <- genfxcdr('data/holidays.csv')

dt <- fread('data/fxrates191125.csv')
dt[,date:=ymd(date)]

gen_fximplrate <- function(dtin,src='ICFX',ccy='eur'){
	#' calculate fx implied usd rate and fx basis given a data.table of raw bloomerg prices with the columns: ticker, date, px_mid
	#' ticker needs to contain preset tickers
	#' currently uses ior rates in the calculation with boj ior hard coded
	#' usage: dt <- fread('data/fxrates191125.csv');dt[,date:=ymd(date)]; 
	#' dtjpy_tmuq <- dt %>% gen_fximplrate('TMUQ',ccy='jpy')

  if(src!=''){ src=str_c(' ',src)}
  if(ccy=='eur'){
  	FXSWAPTICS=c('irrbioer index','EECBDEPO Index',str_c(c('EUR','EURON','EURTN','EURSN'),src,' Curncy'))
	fwdptfactor=10000
	} else if (ccy=='jpy'){
  	FXSWAPTICS=c('irrbioer index','jydr1t curncy',str_c(c('JPY','JPYON','JPYTN','JPYSN'),src,' Curncy'))
	fwdptfactor=100
	} else{
		warning('incorrect ccy')
	}
  FXSWAPCOLNAMES=c('usdior','frnior','spot','fwdptON','fwdptTN','fwdptSN')
  dtinput <- dtin[ticker %in% FXSWAPTICS,.(date,ticker,px_mid)] %>% dcast(date~ticker,value.var='px_mid') %>% setnames(FXSWAPTICS,FXSWAPCOLNAMES)
  
  # ad hoc jpy modification for jpy IOR rate 
  if (ccy=='jpy'){
  	dtinput[,frnior:=.1]
  	dtinput[date>=ymd(20160129),frnior:=-.1]
  }
  # spot date: T2
  dtinput[,T2:=mapply(getspotsettledt,date,ccy)];dtinput[,T2:=as_date(T2)]
  # T+1
  dtinput[,T1:=bizdays::add.bizdays(date,1,cdr[[str_c(ccy,'usd')]])]
  # T+3/ spot+1
  dtinput[,TS1:=bizdays::add.bizdays(T2,1,cdr[[str_c(ccy,'usd')]])]
  
  # daycount
  dtinput[,N0_1:=as.numeric(T1-date)]
  dtinput[,N1_2:=as.numeric(T2-T1)]
  dtinput[,NS_1:=as.numeric(TS1-T2)]
  
  # need to filter out holidays, otherwise it's ugly rates even if they can be calculated on holiday rates
  if (ccy=='eur'){
  dtinput[date %ni% cdr$eurusd$holidays,imprateON:=((1+frnior/100*N0_1/360)*(spot-fwdptTN/fwdptfactor)/(spot-fwdptTN/fwdptfactor-fwdptON/fwdptfactor)-1)*100*(360/N0_1)]
  dtinput[date %ni% cdr$eurusd$holidays & N1_2>0,imprateTN:=((1+frnior/100*N1_2/360)*(spot)/(spot-fwdptTN/fwdptfactor)-1)*100*(360/N1_2)]
  dtinput[date %ni% cdr$eurusd$holidays,imprateSN:=((1+frnior/100*NS_1/360)/(spot)*(spot+fwdptSN/fwdptfactor)-1)*100*(360/NS_1)]
  } else if(ccy=='jpy'){
  dtinput[date %ni% cdr$jpyusd$holidays,imprateON:=((1+frnior/100*N0_1/360)/(spot-fwdptTN/fwdptfactor)*(spot-fwdptTN/fwdptfactor-fwdptON/fwdptfactor)-1)*100*(360/N0_1)]
  dtinput[date %ni% cdr$eurusd$holidays & N1_2>0,imprateTN:=((1+frnior/100*N1_2/360)/(spot)*(spot-fwdptTN/fwdptfactor)-1)*100*(360/N1_2)]
  dtinput[date %ni% cdr$eurusd$holidays,imprateSN:=((1+frnior/100*NS_1/360)*(spot)/(spot+fwdptSN/fwdptfactor)-1)*100*(360/NS_1)] 	
  }

  dtinput[,iorcipON:=imprateON-usdior]
  dtinput[,iorcipTN:=imprateTN-usdior]
  dtinput[,iorcipSN:=imprateSN-usdior]
  
  dtfximplrate=dtinput[!is.na(iorcipON)] %>% copy()
  dtfximplrate
}


dteur_icap <- dt %>% gen_fximplrate('ICFX')
dteur_cmpn <- dt %>% gen_fximplrate('CMPN')
dteur_def <- dt %>% gen_fximplrate('')
dteur_f080 <- dt %>% gen_fximplrate('F080')
dteur_bgnt <- dt %>% gen_fximplrate('BGNT')


dtjpy_tmuq <- dt %>% gen_fximplrate('TMUQ',ccy='jpy')
dtjpy_cmpn <- dt %>% gen_fximplrate('CMPN',ccy='jpy')
dtjpy_def <- dt %>% gen_fximplrate('',ccy='jpy')
dtjpy_f080 <- dt %>% gen_fximplrate('F080',ccy='jpy')
dtjpy_bgnt <- dt %>% gen_fximplrate('BGNT',ccy='jpy')

save(dteur_icap,dteur_cmpn,dteur_def,dteur_f080,dteur_bgnt,dtjpy_tmuq,dtjpy_cmpn,dtjpy_def,dtjpy_f080,dtjpy_bgnt, file='data/fximplrate191126.RData')


# post generation explore ---------
if(0==1){
  
  load('data/fximplrate191126.RData')
  
}
# explore which source might be better ------------------------
if(0==1){
  
  MENDDAYS <- mday(date) %in% c(seq(25,31),seq(1,5))
	dtjpy_f080[mday(date) %ni% c(seq(25,31),seq(1,5)),.(date,iorcipON,iorcipTN,iorcipSN)] %>% gpltw
	dtjpy_bgnt[year(date)==2019 & month(date)==6,.(date,iorcipON,iorcipTN,iorcipSN)] %>% gpltw
	##

	dt[,.N,.(ticker,name)]
	dt[name %like% 'Effective' & date==ymd(20180917)]
	dt[ticker=='irrbioer index',.(date,amt=px_mid)] %>% gpltw
	dt[ticker=='EECBDEPO Index',.(date,amt=px_mid)] %>% gpltw
	dt[ticker=='EURON Curncy',.(date,amt=px_mid)] %>% gpltw

	dt[ticker %like% '^EURON',.N,.(ticker,name)]

	# based on bid/ask spread, CMPN seems to be the most liquid for EUR ON YE2018; ICFX seems generally good by bid-ask of 3rd quartile, and F080 is also very good
	dt[,bidasksprd:=px_ask-px_bid]
	
	
	dt[ticker=='EURON ICFX Curncy',.(bidasksprd)] %>% summary
	dt[ticker=='EURON CMPN Curncy' & date==ymd(20181231)]
	dt[ticker=='EURON BGNT Curncy' & date==ymd(20181231)]
	dt[ticker=='EURON Curncy' & date==ymd(20181231)]
	dt[ticker=='EURON F080 Curncy' & date==ymd(20181231)]
	dt[ticker=='EURON ICFX Curncy' & date==ymd(20181231)]
	dt[ticker=='EUR ICFX Curncy' & date==ymd(20181231)]
	dt[ticker=='EUR CMPN Curncy' & date==ymd(20181231)]

	# Japan
	dt[ticker %like% '^JPYON',.N,.(ticker,name)]
	dt[ticker=='JPYON CMPN Curncy',.(bidasksprd)] %>% summary
	dt[ticker=='JPYON Curncy',.(bidasksprd)] %>% summary
	dt[ticker=='JPYON BGNT Curncy',.(bidasksprd)] %>% summary
	dt[ticker=='JPYON BGNT Curncy',.(bidasksprd)] %>% summary
	
	#gen iorjpy=0.1 if mofd(date)>=tm(2008m12)
#replace iorjpy=-0.1 if date>=td(29jan2016)
}

# old calculation with bid/ask and matching bbg exactly ---------------------------------------------------------------------
if(0==1){

	load('data/FXswapshortterm.RData')

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

}

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



# get asset prices
* usd libor: ir.usd
* eur libor: libor.
* 3m OIS EUR/USD basis swap (negative) = 3m Libor XC basis + 3m EUR FRA-OIS basis - 3m USD FRA-OIS basis
* 1w IOR EUR/USD basis (positive) = 1w FX basis (positive) - 1w EUR libor-ior + 1w USD Libor-ois

```{r}
# EUR, JPY avg CIP deviation 
fmtickers <- c(cipeur1w='dollarbasis.eur1w',
               liborusd1w='ir.usd1w',
               liboreur1w='libor.eur1w',
               ioreur='RICSGO_N.B.EU',
               oisbasiseur='oisbasis.eur3m.bb',
               liboroiseur3m='liboroisspread.3m.eu',
               liboroisusd3m='liboroisspread.3m.us',
               liborxc.3m='dollarbasis.eur3m.bb'
               )
fmpx <- fame2dt(fmtickers,'fm',start=20100101)[!is.na(cipeur1w)] 
dtior <- fame2dt(c(iorusd='faimtn'),'ifhaver_daily')
dtr <- merge(fmpx,dtior,by='date')
dtr[,lxc3m:=-liborxc.3m]
dtr[,oxc3m:=-oisbasiseur]

dtr[,.(date,lxc3m,oxc3m)] %>% ggplotw()
dtr[,oxc3mcalc:=lxc3m+liboroisusd3m-liboroiseur3m]

dtr[,.(date,lxc3m,oxc3m,oxc3mcalc)] %>% ggplotw()


dtr[,cipioreur1w:=cipeur1w+100*(liborusd1w-iorusd)-100*(liboreur1w-ioreur)]

dtr[,.(date,usdliborior=100*(liborusd1w-iorusd), eurliborior=100*(liboreur1w-ioreur))] %>% ggplotw
dtr[,.(date,usdeurliborior=100*(liborusd1w-iorusd)-100*(liboreur1w-ioreur))] %>% ggplotw


dtr[,.(date,cipeur1w,cipioreur1w)] %>% ggplotw()
dtr[,.(date,cipioreur1w)] %>% ggplotw()
dtr %>% lm(cipeur1w~cipioreur1w,data=.) %>% summary()

```

