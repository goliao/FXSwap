
require(stringr);require(lubridate); require(data.table)
blpConnect()

require(Rblpapi)
opt <- c("periodicitySelection"='MONTHLY')

tmplist<-list()
for (ccy in c('cmtusd','cmteur','cmtjpy','cmtchf','comgbp')){
  for(tenor in c('1y','2y','5y','7y','10','15','20','30')){
    
    ticker=str_c(ccy,tenor,' index')
    print(ticker)
    
    
    tmp<-bdh(ticker, 'px_last', start.date=ymd('2000-01-01'))
    tmplist[[length(tmplist)+1]]<-data.table(tmp)[,ccy:=ccy][,tenor:=tenor]
    
  }
}

dt<-rbindlist(tmplist)

dt %>% write.csv(.,file = 'cmtyld.csv')

