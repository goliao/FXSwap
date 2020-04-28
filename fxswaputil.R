
source('fxcalendarutil.R')
cdr <- genfxcdr('data/holidays.csv')


fame2dt. <- function (serlist, db, ...)
{
  return(data.table::data.table(fame2df.(serlist,
                                                    db, ...)))
}

fame2df. <- function (serlist, db, ...)
{
  require(fame); require(frb)
  if (length(db) == 1) {
    serlist <- list(serlist)
  }
  if (length(serlist) != length(db)) {
    stop("serlist needs to be the same length as db")
  }
  for (s in 1:length(serlist)) {
    data <- fame::getfame(serlist[[s]], db[[s]], ...)
    data2 <- list()
    for (i in 1:length(serlist[[s]])) {
      data2[[i]] <- data.frame(date = as.Date(tis::jul(time(data[[i]]))),
                               data[[i]])
      colnames(data2[[i]]) <- c("date", paste0("x.", i))
    }
    data3 <- Reduce(function(x, y) merge(x, y, all = TRUE),
                    data2[1:length(serlist[[s]])])
    if (!is.null(names(serlist[[s]]))) {
      colnames(data3) <- c("date", names(serlist[[s]]))
    }
    else {
      colnames(data3) <- c("date", serlist[[s]])
    }
    if (s == 1) {
      data4 <- data3
    }
    else {
      data4 <- merge(data4, data3, by = "date", all = TRUE)
    }
  }
  for (i in 1:ncol(data4)) {
    if (is.tis(data4[, i])) {
      data4[, i] <- as.numeric(data4[, i])
    }
  }
  return(data4)
}



gen.fx.basis <- function(dtin,ccy='eur',tickers=c('US0003M Index','JY0003M Index','EUR3M Curncy','EUR Curncy'),term='3m',fwdptfactor=10000){
  #' calculate fx implied usd rate and fx basis given a data.table of raw bloomerg prices with the columns: ticker, date, px_mid
  #' tickers is ordinal, USD rate, EUR rate, fwdpt, spot
  #' works for spot next/1w/1m/3m but not on/tn, use gen_fximplrate instead for on/tn

  if(ccy %in% c('eur','chf','aud')){
      fwdptfactor=10000
  } else if (ccy=='jpy'){
    fwdptfactor=100
  } else{
    warning('custom fwd factor for ccy')
  }


  FXSWAPTICS=tickers


  FXSWAPCOLNAMES=c('r_usd','r_frn','fwdpt','spot')
  dtinput <- dtin[ticker %in% FXSWAPTICS] %>% dcast(date~ticker,value.var='value') %>% setnames(FXSWAPTICS,FXSWAPCOLNAMES,skip_absent=T)

  # spot date/SN: T2
  dtinput[,T2:=mapply(getspotsettledt,date,ccy)];dtinput[,T2:=as_date(T2)]

  # T+1/ ON
  #dtinput[,T1:=bizdays::add.bizdays(date,1,cdr[[str_c(ccy,'usd')]])]

  termtype=str_extract(term,'\\D')
  termnumeric=str_extract(term,'\\d') %>% as.numeric()

  if(termtype=='d'){
    # T+3/ spot+1/ only works for spot next
    dtinput[,TSN:=bizdays::add.bizdays(T2,termnumeric,cdr[[str_c(ccy,'usd')]])]
  } else if(termtype=='w'){
    #week
    dtinput[,TSN:=bizdays::adjust.next(T2 %m+% weeks(termnumeric),cdr[[str_c(ccy,'usd')]])]
  } else if (termtype=='m'){
    #month

    dtinput$TSN <- lapply(dtinput$T2,addmonth,ccy=ccy,n=termnumeric) %>% unlist %>% as.Date
#    dtinput[,TSN:=purrr::pmap(.(T2=T2,ccy=ccy,n=termnumeric),addmonth)]
  }

  # daycount
  #dtinput[,N0_1:=as.numeric(T1-date)]
  #dtinput[,N1_2:=as.numeric(T2-T1)]
  dtinput[,NS_N:=as.numeric(TSN-T2)]

  # need to filter out holidays, otherwise it's ugly rates even if they can be calculated on holiday rates
  if (ccy %in% c('eur','gbp','aud','nzd')){
  dtinput[date %ni% cdr$eurusd$holidays,imprate:=((1+r_frn/100*NS_N/360)/(spot)*(spot+fwdpt/fwdptfactor)-1)*100*(360/NS_N)]
  } else{
  dtinput[date %ni% cdr$jpyusd$holidays,imprate:=((1+r_frn/100*NS_N/360)*(spot)/(spot+fwdpt/fwdptfactor)-1)*100*(360/NS_N)]
  }
  dtinput[,fxbasis:=(imprate-r_usd)*100]

  dtfximplrate=dtinput[!is.na(fxbasis)] %>% copy()
  dtfximplrate
}

gen_fximplrate <- function(dtin,src='ICFX',ccy='eur',tickers=''){
	#' calculate fx implied usd rate and fx basis given a data.table of raw bloomerg prices with the columns: ticker, date, px_mid
  #' this is used to generate short-term O/N T/N S/N rates
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
  if(tickers!='') FXSWAPTICS=tickers
  FXSWAPCOLNAMES=c('usdior','frnior','spot','fwdptON','fwdptTN','fwdptSN')
  dtinput <- dtin[ticker %in% FXSWAPTICS] %>% dcast(date~ticker,value.var='value') %>% setnames(FXSWAPTICS,FXSWAPCOLNAMES,skip_absent=T)

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
  dtinput[date %ni% cdr$jpyusd$holidays & N1_2>0,imprateTN:=((1+frnior/100*N1_2/360)/(spot)*(spot-fwdptTN/fwdptfactor)-1)*100*(360/N1_2)]
  dtinput[date %ni% cdr$jpyusd$holidays,imprateSN:=((1+frnior/100*NS_1/360)*(spot)/(spot+fwdptSN/fwdptfactor)-1)*100*(360/NS_1)]
  }

  dtinput[,fxbasisON:=(imprateON-usdior)*100]
  dtinput[,fxbasisTN:=(imprateTN-usdior)*100]
  dtinput[,fxbasisSN:=(imprateSN-usdior)*100]

  dtfximplrate=dtinput[!is.na(fxbasisON)] %>% copy()
  dtfximplrate
}


calc_ois_basis <- function(startdt='2018-01-01'){
# read ticker lookup
    tic <- fread('meta/lookup_tickers.csv')
    # get data from mfma on fraois and 3m basis in lbior and ois
    liborois <- bdhmp(tic$ticker,startdt=(startdt)) %>% merge(tic,by='ticker')

    fraoisw <- liborois %>% dcast(date~type+ccy+tenor,value.var='value')
    fraoisw[,eur.fraois.1w:=libor_eur_1w-ois_eur_1w];fraoisw[,usd.fraois.1w:=libor_usd_1w-ois_usd_1w];fraoisw[,usdeur.fraois.1w:=100*(usd.fraois.1w-eur.fraois.1w)]
    fraoisw[,eur.fraois.1m:=libor_eur_1m-ois_eur_1m];fraoisw[,usd.fraois.1m:=libor_usd_1m-ois_usd_1m];fraoisw[,usdeur.fraois.1m:=100*(usd.fraois.1m-eur.fraois.1m)]
    fraoisw[,jpy.fraois.1w:=libor_jpy_1w-ois_jpy_1w];fraoisw[,usd.fraois.1w:=libor_usd_1w-ois_usd_1w];fraoisw[,usdjpy.fraois.1w:=100*(usd.fraois.1w-jpy.fraois.1w)]
    fraoisw[,jpy.fraois.1m:=libor_jpy_1m-ois_jpy_1m];fraoisw[,usd.fraois.1m:=libor_usd_1m-ois_usd_1m];fraoisw[,usdjpy.fraois.1m:=100*(usd.fraois.1m-jpy.fraois.1m)]
    fraoisw[,libor.basis.eur.3m:=-libor.basis_eur_3m]
    fraoisw[,libor.basis.jpy.3m:=-libor.basis_jpy_3m]
    fraoisw[,ois.basis.eur.3m:=-ois.basis_eur_3m]
    fraoisw[,ois.basis.jpy.3m:=-ois.basis_jpy_3m]



    # get fame libor basis
    fmtickers <- c(libor.basis.eur.1w='dollarbasis.eur1w',
                   libor.basis.eur.1m='dollarbasis.eur1m',
                   libor.basis.jpy.1w='dollarbasis.jpy1w',
                   libor.basis.jpy.1m='dollarbasis.jpy1m'
    )
    fmpx <- fame2dt.(fmtickers,'fm',start=startdt)[!is.na(libor.basis.eur.1w)]

    # mergae and adjust libor to ois

    dt <- merge(fmpx,fraoisw,by='date',all=T)

    dt[,ois.basis.eur.1w:=libor.basis.eur.1w+usdeur.fraois.1w]
    dt[,ois.basis.eur.1m:=libor.basis.eur.1m+usdeur.fraois.1m]
    dt[,ois.basis.jpy.1w:=libor.basis.jpy.1w+usdeur.fraois.1w]
    dt[,ois.basis.jpy.1m:=libor.basis.jpy.1m+usdeur.fraois.1m]
    dt[,repo.ois.usd.3m:=repo_usd_3m-ois_usd_3m]
    dt[,repo.ois.usd.1m:=repo_usd_1m-ois_usd_1m]
    dt[,repo.ois.usd.1w:=repo_usd_1w-ois_usd_1w]
    dt[,repo.ois.usd.1d:=repo_usd_1d-ois_usd_1w]

    dt
  }
