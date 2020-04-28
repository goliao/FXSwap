
compiler::loadcmp('../gllib/glutil.Rc')
source('fxswaputil.R')

require(Rblpapi)

# chf 3m basis example
tic <- c('USSOC Curncy','SFSNTC Curncy','CHF3M Curncy','CHF Curncy')

blpConnect()
dt <- bdh(securities = tic,fields = 'px_last',start.date = ymd(20150101)) %>% rbindlist(use.names = T,idcol = 'ticker') %>% setnames('px_last','value')
blpDisconnect()

dt <- dt[date<=today()]
fxbs <- gen.fx.basis(dt,ccy='chf',tickers=tic,term='3m')
fxbs[,.(date,fxbasis)] %>% gpltw

# AUD 3m
tic <- c('US0003M Index','BBSW3M Index','AUD3M Curncy','AUD Curncy')
blpConnect()
dt <- bdh(securities = tic,fields = 'px_last',start.date = ymd(20070101)) %>% rbindlist(use.names = T,idcol = 'ticker') %>% setnames('px_last','value')
xccy <- bdh(securities =c('adbs1 icpl curncy'),fields = 'px_last',start.date = ymd(20070101))  %>% setnames('px_last','value') %>% as.data.table
xccy[,date:=ymd(date)]
blpDisconnect()
dt <- dt[date<=today()]
fxbs <- gen.fx.basis(dt,ccy='aud',tickers=tic,term='3m')
mm <- fxbs %>% merge(xccy[,.(date,xccy=-value)],by='date',all.x=T)
# fxbs[,.(date,fxbasis)] %>% gpltw
mm[,.(date,fxbasis,xccy)] %>% gpltw

mm %>% tail
dt[date==ymd(20200424)]
mm[,xcapprox:=-100*(r_usd - r_frn - (fwdpt / spot / 100) * 4)]
mm[,.(date,r_usd - r_frn , (fwdpt / spot / 10) * 4)] %>% tail

mm %>% tail
wth=20;options(repr.plot.width =wth, repr.plot.height = wth/1.61)
mm[,.(date,xccy,xcapprox,fxbasis)]  %>% melt(id.var="date") %>% ggplot(aes(x=date,y=value,color=variable))+geom_line()

mm[date>ymd(20140101),.(date,xccy,xcapprox,fxbasis)] %>% eventstudyperiodend(cn='fxbasis',period='year',window=100)

# AUD 1M
tic <- c('US0001M Index','BBSW1M Index','AUD1M Curncy','AUD Curncy')
# blpConnect()
dt <- bdh(securities = tic,fields = 'px_last',start.date = ymd(20070101)) %>% rbindlist(use.names = T,idcol = 'ticker') %>% setnames('px_last','value')
# blpDisconnect()
dt <- dt[date<=today()]
fxbs <- gen.fx.basis(dt,ccy='aud',tickers=tic,term='1m')
fxbs[,.(date,fxbasis)] %>% gpltw

source('../gllib/eventstudytools.R')
eventstudyqend(fxbs[date>ymd(20140101) & date<ymd(20200101),.(date,amt=fxbasis)],window=60)




# AUD 1W
tic <- c('USSO1Z BGN Curncy','ADSO1Z BGN Curncy','AUD1W Curncy','AUD Curncy')
# blpConnect()
dt <- bdh(securities = tic,fields = 'px_last',start.date = ymd(20070101)) %>% rbindlist(use.names = T,idcol = 'ticker') %>% setnames('px_last','value')
# blpDisconnect()
dt <- dt[date<=today()]
fxbs <- gen.fx.basis(dt,ccy='aud',tickers=tic,term='1w')
fxbs[,.(date,fxbasis)] %>% gpltw
fxbs[year(date)==2016]  %>% ggplot(aes(x=date,y=fxbasis))+geom_line()#+geom_vline(xintercept=ymd(20200315))
fxbs[] %>% tail(60)  %>% ggplot(aes(x=date,y=fxbasis))+geom_line()+geom_vline(xintercept=ymd(20200315))
fxbs[] %>% tail(10)

source('../gllib/eventstudytools.R')
eventstudyqend(fxbs[date>ymd(20140101) & date<ymd(20200101),.(date,amt=fxbasis)],window=30)
