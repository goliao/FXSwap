
source('fxswaputil.R')
compiler::loadcmp('../gllib/glutil.Rc')
require(Rblpapi)
require(readxl)
require(fst)


lookup_tickers <- read_excel('meta/lookup_tickers.xlsx',1) %>% as.data.table()
dtraw <- downloadbbg(lookup_tickers$Ticker,startdt = ymd(20000101),filestr='xccy1y_apr2020_extended.RData',periodstr = 'DAILY' )

dt <- dtraw %>% merge(lookup_tickers,by.x='pk',by.y='Ticker')


# dtraw[,.N]
# dt[,.N]
dt %>% setnames('Currency','ccy')
# mxn is quoted with multiplier -1
dt[ccy!='MXN',value:=-value]


saveRDS(dt,file='data/xccy1yextended.rds')


dt %>% head()
