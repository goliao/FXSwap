source('../gllib/glutil.R')

source('fxswaputil.R')
require(Rblpapi)

tic=c('MXIB91DT Index','BCSFDPDV Curncy','CTSGD3M Govt','KRBO3M Index','US0003M Index',
'MXN Curncy', 'MXN3M Curncy','KRW Curncy', 'KRW3M Curncy','BRL Curncy', 'BCN3M Curncy','SGD Curncy',
'SGD3M Curncy','CB3 Govt', 'USSOC Curncy','EUR3M Curncy','EUR Curncy','JPY3M Curncy','JPY Curncy')

# •	MXN: 91-day TIIE Rate (benchmark interbank deposit rate), BBG ticker MXIB91DT Index
# •	BRL: 84-day OIS, BBG ticker BCSFDPDV Curncy
# •	SGD: 3month bill rate,  BBG ticker CTSGD3M Govt
# •	KRW: 3month KORIBOR, BBG ticker KRBO3M Index

dtraw <- downloadbbg(tic,startdt = ymd(20200201),filestr='xccy3m_em_apr2020.RData',periodstr = 'DAILY' )

dt <- dtraw %>% copy() %>% dcast(date~pk,value.var='value')

dt %>% names
dt[.N,.(date,`MXN Curncy`,`MXN3M Curncy`,`KRW Curncy`,`KRW3M Curncy`,`JPY Curncy`,`JPY3M Curncy`,`EUR Curncy`,`EUR3M Curncy`,`BRL Curncy`,`BCN3M Curncy`)]
# expected interest rate differential:
# brl in percent
# 242/5.05/100*4
# dt[.N,.(`BCSFDPDV Curncy`)]
# # mxn in percent
# 3590/24.7/100*4
#
# # KRW
# 3.3/1229*100*4
#
# # JPY
# 18.75/108*4
# # eur is in percent
# 23/1.07/100*4

# use the FRB convention for positive indicating strains
dt[, MXN:=-100*(`US0003M Index` - `MXIB91DT Index` + (`MXN3M Curncy` / `MXN Curncy` / 100) * 4)]
dt[, BRL:=-100*(`US0003M Index` - `BCSFDPDV Curncy` + (`BCN3M Curncy` / `BRL Curncy` / 100) * 4)]
dt[, KRW:=-100*(`US0003M Index` - `KRBO3M Index` + (`KRW3M Curncy` / `KRW Curncy` * 100) * 4)]
dt[, SGD:=-100*(`CB3 Govt` - `CTSGD3M Govt` + (`SGD3M Curncy` / `SGD Curncy` / 100) * 4)]

dt[, USFRAOIS:=100*(`US0003M Index`-`USSOC Curncy`)]
require(ggthemes)
dtl=dt[date>ymd(20200201),.(date,MXN,BRL,KRW,USFRAOIS)] %>% melt(id.var='date')
dtl[!is.na(value)] %>% ggplot(aes(x=date,y=value,color=variable))+geom_line()+ggthemes::theme_clean()+theme(legend.position='bottom',legend.title=element_blank())+xlab('')+ylab('basis points')+geom_vline(xintercept=ymd(20200320))+annotate('text',x=ymd(20200320),y=150,label='swap counterparty \n extension')+geom_hline(yintercept=0,linetype='dashed')


ggsave(height=5,width=7,filename='emswap.pdf')

dt[,.(date,)] %>% tail()
#
# # use the FRB convention for positive indicating strains
# MXN=-100*(`US0003M Index` - `MXIB91DT Index` + (`MXN3M Curncy` / `MXN Curncy` / 100) * 4)
# BRL=-100*(`US0003M Index` - `BCSFDPDV Curncy` + (`BCN3M Curncy` / `BRL Curncy` / 100) * 4)
# KRW=-100*(`US0003M Index` - `KRBO3M Index` + (`KRW3M Curncy` / `KRW Curncy` * 100) * 4)
#
