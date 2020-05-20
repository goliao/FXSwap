source('../gllib/glutil.R')
require(Rblpapi)
compiler::loadcmp('../gllib/glutil.Rc')
source('fxswaputil.R')
src=' F080'
tic_eur <- c('IRRBIORR Index','EECBDEPO Index',str_c(c('EUR','EU1D','EU2D','EU3D'),src,' Curncy'))
tic_jpy <- c('IRRBIORR curncy','BOJDPBAL Index',str_c(c('JPY','JY1D','JY2D','JY3D'),src,' Curncy'))
tic <- c(tic_eur,tic_jpy) %>% unique()



  dt <- downloadbbg(tic,startdt = ymd(20080101),periodstr = 'DAILY' )
  dt %>% setnames('pk','ticker')
  dt <- dt[date<=today()]
  dt[,field:=NULL]
  dtw <- dt %>% dcast(date~ticker)
  dtw[.N,`BOJDPBAL Index`:=-.1]
  dtl <- dtw %>% melt(id.var='date',variable.name='ticker')

  dtbasis.eur <- dtl %>% gen_fximplrate(ccy='eur',src='',tickers=tic_eur)
  dtbasis.jpy <- dtl %>% gen_fximplrate(ccy='jpy',src='',tickers=tic_jpy)
  dtbasis <- merge(dtbasis.eur[,.(date,`EUR_ON_basis`=fxbasisON)],dtbasis.jpy[,.(date,`JPY_ON_basis`=fxbasisON)],by='date',all=T)

  # dtl %>% head
  dtbasisall <- merge(dtbasis.eur[,.(date,fxbasisON,fxbasisTN,fxbasisSN)],dtbasis.jpy[,.(date,fxbasisON,fxbasisTN,fxbasisSN)],by='date',all=T,suffix=c('eur','jpy'))

  fwrite(dtbasisall,file='../data/fxswap/ontnsnbasis_eurjpy.csv')
  fwrite(dtbasis,file='../data/fxswap/onbasis_eurjpy.csv')



# load('IORBasis.RData')
startdt=ymd(20190801)
dtbasis[date>startdt,.(date,`EUR_ON_basis`,`JPY_ON_basis`)] %>% gpltw+theme_classic()+xlab('')+ylab('basis points')+geom_hline(yintercept = 0)+theme(legend.title = element_blank())

ggsave('IORbasis.pdf',width=8,height=6)


# dtbasis[date %ni% c(ymd(20161229)) & date>startdt & quarterend==F,.(date,`EUR O/N basis`,`JPY O/N basis`)] %>% gpltw+theme_classic()+xlab('')+ylab('basis points')+geom_hline(yintercept = 0)+theme(legend.title = element_blank())
#
#
# dtbasis[date %ni% c(ymd(20161229)) & date>startdt & monthend==F,.(date,`EUR O/N basis`,`JPY O/N basis`)] %>% gpltw+theme_classic()+xlab('')+ylab('basis points')+geom_hline(yintercept = 0)+theme(legend.title = element_blank())
#
#
# dtbasis[date %ni% c(ymd(20161229)) & date>startdt & monthend==F,.(date,`EUR O/N basis`,`JPY O/N basis`)][`JPY O/N basis`>400]
```
