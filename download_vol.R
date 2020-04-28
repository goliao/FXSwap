
compiler::loadcmp('../gllib/glutil.Rc')
source('fxswaputil.R')
require(Rblpapi)
require(readxl)
require(fst)


dtraw <- downloadbbg(c('CVIX Index','USDJPYV1M Curncy','USDJPYV3M Curncy','USDJPYV6M Curncy','USDJPYV1Y Curncy'),startdt = ymd(20000101),filestr='fxvol_apr2020_cvixjpy.RData',periodstr = 'DAILY' )


saveRDS(dtraw,file='data/fxvol_cvixjpy.rds')
