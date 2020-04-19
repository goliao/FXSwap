require(RPostgreSQL)
require(sqldf)
# test
#aa <- sqldf("select bloomberg.bdh('USRGCGA ICUS Curncy',  'PX_LAST','2009-01-01','2020-01-01')", host="pgdp", dbname="patty", user = Sys.getenv("USER")) %>% as.data.table()


bdh <- function(ticker='EU1D Curncy',field='PX_LAST',startdt='2008-01-01'){
  # emulate bdh
  sqlstr=paste('SELECT trade_date as date, val as value FROM bloomberg.data_daily_eod JOIN bloomberg.tickers on ticker_id = tickers.id JOIN bloomberg.fields on field_id = fields.id WHERE tickers.ticker_name =\'',ticker,'\' AND fields.field_name = \'',field,'\' AND trade_date >= \'',startdt, '\' ORDER BY trade_date ASC',sep = '')
  aa <- sqldf(sqlstr, host="pgdp", dbname="patty", user = Sys.getenv("USER")) %>% as.data.table()
  aa
}


bdhm <- function(ticker=c('EU1D Curncy','EU2D Curncy'),field='PX_LAST',startdt='2008-01-01'){
  # multiple tickers bdh
    tickerstr=paste(ticker,collapse = '\',\'')
  sqlstr=paste('SELECT trade_date as date, tickers.ticker_name as ticker, val as value FROM bloomberg.data_daily_eod JOIN bloomberg.tickers on ticker_id = tickers.id JOIN bloomberg.fields on field_id = fields.id WHERE tickers.ticker_name in (\'',tickerstr,'\') AND fields.field_name = \'',field,'\' AND trade_date >= \'',startdt, '\' ORDER BY trade_date ASC',sep = '')
  sqlstr
  aa <- sqldf(sqlstr, host="pgdp", dbname="patty", user = Sys.getenv("USER")) %>% as.data.table()
  aa
}


bdh5 <- function(ticker='EUBSC Curncy',field='PX_LAST',startdt='2020-03-01'){
  # emulate bdh for intraday
  sqlstr=paste('SELECT trade_timestamp as date, val as value FROM bloomberg.data_daily_five_minute JOIN bloomberg.tickers on ticker_id = tickers.id JOIN bloomberg.fields on field_id = fields.id WHERE tickers.ticker_name =\'',ticker,'\' AND fields.field_name = \'',field,'\' AND trade_timestamp >= \'',startdt, '\' ORDER BY trade_timestamp ASC',sep = '')
  aa <- sqldf(sqlstr, host="pgdp", dbname="patty", user = Sys.getenv("USER")) %>% as.data.table()
  aa
}


bdh5m <- function(ticker=c('EUBSC Curncy','EOUSFFC Curncy','JYBSC Curncy','JYBSFC ICPL Curncy'),field='PX_LAST',startdt=as.character(today()-1)){
  # emulate bdh for intraday
  
  tickerstr=paste(ticker,collapse = '\',\'')
  sqlstr=paste('SELECT trade_timestamp as date, tickers.ticker_name as ticker, val as value FROM bloomberg.data_daily_five_minute JOIN 
               bloomberg.tickers on ticker_id = tickers.id JOIN bloomberg.fields on field_id = fields.id WHERE tickers.ticker_name in (\'',tickerstr,'\')  AND fields.field_name = \'',field,'\' AND trade_timestamp >= \'',startdt, '\' ORDER BY trade_timestamp ASC',sep = '')
  aa <- sqldf(sqlstr, host="pgdp", dbname="patty", user = Sys.getenv("USER")) %>% as.data.table()
  aa
}

bdhi <- function(ticker=c('EUBSC Curncy','EOUSFFC Curncy','JYBSC Curncy','JYBSFC ICPL Curncy'),field=c('PX_BID','PX_ASK','PX_LAST'),startdt=as.character(today()-1)){
  # intraday table
  # "SELECT data_intradaily.updated_at as date, field_name as fld, val as value FROM bloomberg.data_intradaily JOIN bloomberg.tickers on ticker_id = tickers.id JOIN bloomberg.fields on field_id = fields.id WHERE tickers.ticker_name ='EUBSC Curncy' AND 
  # fields.field_name in ('PX_LAST','PX_BID','PX_ASK')"
  
  tickerstr=paste(ticker,collapse = '\',\'')
  
  fieldstr=paste(field,collapse = '\',\'')
  sqlstr=paste('SELECT data_intradaily.updated_at as date, field_name as fld, tickers.ticker_name as ticker, val as value 
               FROM bloomberg.data_intradaily JOIN bloomberg.tickers on ticker_id = tickers.id JOIN bloomberg.fields on field_id = fields.id 
               WHERE tickers.ticker_name in (\'',tickerstr,'\')  AND fields.field_name in (\'',fieldstr,'\') AND data_intradaily.updated_at >= \'',startdt, '\' 
               ORDER BY data_intradaily.updated_at ASC',sep = '')
  aa <- sqldf(sqlstr, host="pgdp", dbname="patty", user = Sys.getenv("USER")) %>% as.data.table()
  aa
}
  

bdp <- function(ticker=c('EUBSC Curncy','EOUSFFC Curncy','JYBSC Curncy','JYBSFC ICPL Curncy'),field=c('PX_LAST')){
  # intraday table
  # "SELECT data_intradaily.updated_at as date, field_name as fld, val as value FROM bloomberg.data_intradaily JOIN bloomberg.tickers on ticker_id = tickers.id JOIN bloomberg.fields on field_id = fields.id WHERE tickers.ticker_name ='EUBSC Curncy' AND 
  # fields.field_name in ('PX_LAST','PX_BID','PX_ASK')"
  
  tickerstr=paste(ticker,collapse = '\',\'')
  n=length(ticker)
  fieldstr=paste(field,collapse = '\',\'')
  sqlstr=paste('SELECT data_intradaily.updated_at as date, field_name as fld, tickers.ticker_name as ticker, val as value 
               FROM bloomberg.data_intradaily JOIN bloomberg.tickers on ticker_id = tickers.id JOIN bloomberg.fields on field_id = fields.id 
               WHERE tickers.ticker_name in (\'',tickerstr,'\')  AND fields.field_name in (\'',fieldstr,'\') 
               ORDER BY data_intradaily.updated_at desc limit ', n,sep = '')
  aa <- sqldf(sqlstr, host="pgdp", dbname="patty", user = Sys.getenv("USER")) %>% as.data.table()
  # in case there are multiple pulls of the same ticker
  aa[,hr:=floor_date(date,'hour')]
  aa <- aa[hr==max(hr)]
  aa[,hr:=NULL]
  aa
}


bdhmp <- function(ticker=c('EU1D Curncy','EU2D Curncy'),field='PX_LAST',startdt='2008-01-01'){
  #' bdhm + bdp combined to get the latest intraday value
  historical <- bdhm(ticker=ticker,field=field,startdt=startdt)
  intraday <- bdp(ticker=ticker,field=field)
  if(intraday[1,as_date(date)]>historical[,max(date)]) combined <- rbind(historical,intraday[,.(date=as_date(date),ticker,value)])
  combined
}
