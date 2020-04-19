
source('fxswaputil.R')
source('../gllib/glutil.R')

require(Rblpapi)

#INR IDR TRY COP CNH ZAR PLN HUF MXN RUB KRW TWD
ticraw <- fread('data/xccytickers.csv')
tic <- ticraw[Term=='1 Year' &!is.na(Spread),.(ccy=Currency, tic=Ticker)]
tic[,pk:=str_c(tic,' Curncy')]
#tic[,pk:=str_c(str_replace(tic,'1','C'),' Curncy')]
tic$pk


dtraw <- downloadbbg(tic$pk,startdt = ymd(20070101),filestr='xccy1y_apr2020.RData',periodstr = 'DAILY' )

dt <- dtraw %>% merge(tic,by='pk')
# mxn is quoted with multiplier -1
dt[ccy!='MXN',value:=-value]


saveRDS(dt,file='xccy1y.rds')


dt %>% head()
