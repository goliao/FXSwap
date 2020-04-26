
compiler::loadcmp('../gllib/glutil.Rc')
source('fxswaputil.R')
require(Rblpapi)
require(readxl)
require(fst)


lookup_tickers <- read_excel('meta/lookup_tickers.xlsx',1) %>% as.data.table()
lookup_tickers[,.N]
dtraw <- downloadbbg(lookup_tickers$Ticker,startdt = ymd(20000101),filestr='xccy1y_apr2020_extended.RData',periodstr = 'DAILY' )

dt <- dtraw %>% merge(lookup_tickers,by.x='pk',by.y='Ticker')


# dtraw[,.N]
# dt[,.N]
dt %>% setnames('Currency','ccy')
# mxn is quoted with multiplier -1; do it later
# dt[ccy=='MXN' & Type=='xccy',value:=-value]
saveRDS(dt,file='data/xccy1yextendedv2.rds')


dt[ccy=='NOK',.(date,value)] %>% gpltw
dt %>% head()
