# adjust FRA-OIS


rm(list=ls())
require(policyPlot)
source('path.txt')
source('fxcalendarutil.R')
source('../gllib/glutil.R')
cdr <- genfxcdr('data/holidays.csv')
source('fxswaputil.R')
source('mfmabbgutil.R')

source("/if/appl/R/Functions/IFfunctions.r")



dt <- calc_ois_basis(startdt = '2007-01-01')

dt

