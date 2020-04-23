require(compiler)
compiler::loadcmp('../gllib/glutil.Rc')
source('fxswaputil.R')
require(Rblpapi)


startdt=ymd(20070101)
#90 day CP US: DCPB090D Index
tic=c('EUCM3M SHBT Curncy', 'DCPB090D Index','EUR Curncy', 'EUR3M Curncy','US0003M Index','EUR003M Index','EUBSC Curncy')
dtp <- downloadbbg(tic,startdt = startdt,filestr='xccy3m_cp_apr2020.RData',periodstr = 'DAILY' ) %>% dcast(date~pk,value.var='value')

dy <- downloadbbg(c('GTDEM3M Govt','GB3 Govt'),fields=c('YLD_YTM_MID'),startdt = startdt,filestr='xccy3m_bill_apr2020.RData',periodstr = 'DAILY' ) %>% dcast(date~pk,value.var='value')

dt <- merge(dtp,dy,by='date',all.x=T)
# use the FRB convention for positive indicating strains
dt[, EUR_CP:=-100*(`DCPB090D Index` - `EUCM3M SHBT Curncy` - (`EUR3M Curncy` / `EUR Curncy` / 100) * 4)]
dt[, EUR_BILL:=-100*(`GB3 Govt` - `GTDEM3M Govt` - (`EUR3M Curncy` / `EUR Curncy` / 100) * 4)]
dt[, EUR_LIBOR_CALC:=-100*(`US0003M Index` - `EUR003M Index` - (`EUR3M Curncy` / `EUR Curncy` / 100) * 4)]
dt[, EUR_LIBOR_BBG:=-`EUBSC Curncy`]
dt[, USD_EUR_SPRD:=100*(`US0003M Index` - `EUR003M Index`)]
require(ggthemes)
startdt=ymd(20200201)
dtl=dt[date>startdt,.(date,EUR_CP,EUR_BILL,EUR_LIBOR_CALC,EUR_LIBOR_BBG)] %>% melt(id.var='date')
dtl[!is.na(value)] %>% ggplot(aes(x=date,y=value,color=variable))+geom_line()+ggthemes::theme_clean()+theme(legend.position='bottom',legend.title=element_blank())+xlab('')+ylab('basis points')+geom_vline(xintercept=ymd(20200315))+annotate('text',x=ymd(20200315),y=150,label='swap line \n rate cut')+geom_hline(yintercept=0,linetype='dashed')

ggsave(height=5,width=7,filename='cpswaplong.pdf')
