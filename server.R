
library(shiny)
.libPaths("/rsma/shiny/if/sections/gmsm/Rlibrary")
library(plotly)
library(dplyr)
library(lubridate)
library(magrittr)
require(data.table)
source('fxswaputil.R')
source('mfmabbgutil.R')
require(stringr)

function(input, output, session) {
  
  # intraday 5 min
    intraday5plot=reactiveVal()
    observeEvent(input$intraday5button,{
      startdt=input$Date[1] %>% as.character()
      dt.intraday <- bdh5m(ticker=c('EUBSC Curncy','EOUSFFC Curncy','JYBSC Curncy','JYBSFC ICPL Curncy'),field='PX_LAST',startdt=startdt)
      dt.i <- dt.intraday %>% merge(fread('meta/lookup_tickers.csv'),by='ticker')
      fxbs.i <- dt.i %>% dcast(date~type+ccy+tenor,value.var='value')
      fxbs.i[,libor.basis.eur.3m:=-libor.basis_eur_3m]
      fxbs.i[,libor.basis.jpy.3m:=-libor.basis_jpy_3m]
      fxbs.i[,ois.basis.eur.3m:=-ois.basis_eur_3m]
      fxbs.i[,ois.basis.jpy.3m:=-ois.basis_jpy_3m]
    
      intraday5plot(plot_ly(fxbs.i, x = ~date, y = ~libor.basis.eur.3m, name = 'libor.basis.eur.3m', type = 'scatter', mode = 'lines',line = list(color = 'red', width = 2)) %>%
        add_trace(y = ~libor.basis.jpy.3m, name = 'libor.basis.jpy.3m', line = list(color = 'blue', width = 2 )) %>%
        add_trace(y = ~ois.basis.eur.3m, name = 'ois.basis.eur.3m', line = list(color = 'green', width = 2 )) %>%
        add_trace(y = ~ois.basis.jpy.3m, name = 'ois.basis.jpy.3m', line = list(color = 'orange', width = 2 )) %>%
        layout(xaxis = list(title = ""),yaxis = list (title = "Basis Points")))
      })
      
      output$plotfxbasis <- renderPlotly({intraday5plot()})

    
    # intradaily snaps
    snaptable=reactiveVal()
    snapplot=reactiveVal()
    observeEvent(input$snapbutton,{
        
      startdt=input$snapDate[1] %>% as.character()
      dt.snap <- bdhi(ticker=c('EUBSC Curncy','EOUSFFC Curncy','JYBSC Curncy','JYBSFC ICPL Curncy'),startdt=startdt) %>% merge(fread('meta/lookup_tickers.csv'),by='ticker')
      dt.snap[,date:=round_date(date,'minute')]
      fxbs.snap <- dt.snap %>% dcast(date+fld~type+ccy+tenor,value.var='value')
      fxbs.snap[,libor.basis.eur.3m:=-libor.basis_eur_3m]
      fxbs.snap[,libor.basis.jpy.3m:=-libor.basis_jpy_3m]
      fxbs.snap[,ois.basis.eur.3m:=-ois.basis_eur_3m]
      fxbs.snap[,ois.basis.jpy.3m:=-ois.basis_jpy_3m]
      
      snapplot({plot_ly(fxbs.snap[fld=='PX_LAST'], x = ~date, y = ~libor.basis.eur.3m, name = 'libor.basis.eur.3m', type = 'scatter', mode = 'lines',line = list(color = 'red', width = 2)) %>%
        add_trace(y = ~libor.basis.jpy.3m, name = 'libor.basis.jpy.3m', line = list(color = 'blue', width = 2 )) %>%
          add_trace(y = ~ois.basis.eur.3m, name = 'ois.basis.eur.3m', line = list(color = 'green', width = 2 )) %>%
          add_trace(y = ~ois.basis.jpy.3m, name = 'lois.basis.jpy.3m', line = list(color = 'orange', width = 2 )) %>%
        layout(xaxis = list(title = ""),yaxis = list (title = "Basis Points"))})
      
      snaptable({tail(fxbs.snap[fld %in% c('PX_LAST'),.(time=format(date,"%Y-%m-%d %H:%M"),libor.basis.eur.3m,libor.basis.jpy.3m,ois.basis.eur.3m,ois.basis.jpy.3m)])})
    },ignoreNULL = T)
    output$snapplot <- renderPlotly({snapplot()})
    output$snaptable <- renderTable(snaptable())
    
    # associated with EOD
    eodplot = reactiveVal()
    eodtable = reactiveVal()
    observeEvent(input$eodbutton,{
      startdt=input$eod.Date[1] %>% as.character()
      df1 <- bdhm(ticker=c('EUBSC Curncy','EOUSFFC Curncy','JYBSC Curncy','JYBSFC ICPL Curncy'),field='PX_LAST',startdt=startdt)
      dt1 <- df1 %>% merge(fread('meta/lookup_tickers.csv'),by='ticker')
      fxbs <- dt1 %>% dcast(date~type+ccy+tenor,value.var='value')
      fxbs[,libor.basis.eur.3m:=-libor.basis_eur_3m]
      fxbs[,libor.basis.jpy.3m:=-libor.basis_jpy_3m]
      fxbs[,ois.basis.eur.3m:=-ois.basis_eur_3m]
      fxbs[,ois.basis.jpy.3m:=-ois.basis_jpy_3m]
      
      eodplot({
        plot_ly(fxbs, x = ~date, y = ~libor.basis.eur.3m, name = 'libor.basis.eur.3m', type = 'scatter', mode = 'lines',line = list(color = 'red', width = 2)) %>%
          add_trace(y = ~libor.basis.jpy.3m, name = 'libor.basis.jpy.3m', line = list(color = 'blue', width = 2 )) %>%
          add_trace(y = ~ois.basis.eur.3m, name = 'ois.basis.eur.3m', line = list(color = 'green', width = 2 )) %>%
          add_trace(y = ~ois.basis.jpy.3m, name = 'lois.basis.jpy.3m', line = list(color = 'orange', width = 2 )) %>%
          layout(xaxis = list(title = ""),yaxis = list (title = "Basis Points"))
        
      })
      
      eodtable({tail(fxbs[,.(date=format(date,"%Y-%m-%d"),libor.basis.eur.3m,libor.basis.jpy.3m,ois.basis.eur.3m,ois.basis.jpy.3m)])})
    })
    
    output$plot.eod <- renderPlotly({
      eodplot()
    })
    output$table.eod <- renderTable({
      eodtable()
    })
    
    
#' FX basis all ois:
    
    fxallplot = reactiveVal()
    fxalltable = reactiveVal()
    
    observeEvent(input$fxbasisallbutton,{
      dt <- calc_ois_basis(input$all.Date[1] %>% as.character())
      ck <- input$oischeckbox
      
      dt[,dt:=format(date,'%Y-%m-%d')] 
      plt <- plot_ly(dt, x=~date,y=0,name='', type = 'scatter', mode = 'lines',line = list(color = 'red', width = 0)) %>%
        layout(xaxis = list(title = ""),yaxis = list (title = "Basis Points"))
      colorlist=list('red','green','blue','purple','orange','grey','black','brown','pink','dark blue','dark green','yellow','light blue')
      i=0
      for (cki in ck){
        i=i+1
       plt <- plt %>% add_trace(y = eval(parse(text=str_c('~',cki))), name = cki, line = list(color=colorlist[[i]], width = 2 )) 
      }
      
      fxallplot(plt)
      
      
      fxalltable({tail(dt[,c('dt',ck),with=F])})
    })
#      format(date,'%Y-%m-%d'),ois.basis.eur.1w,libor.basis.eur.1w,ois.basis.eur.1m,libor.basis.eur.1m,ois.basis.eur.3m,libor.basis.eur.3m,ois.basis.jpy.1w,libor.basis.jpy.1w,ois.basis.jpy.1m,libor.basis.jpy.1m,ois.basis.jpy.3m,libor.basis.jpy.3m)])})

    output$plotfxbasisall <- renderPlotly({
      fxallplot()
      })
    output$tablefxbasisall <- renderTable({
      fxalltable()
    })
    
  # Intraday Financials Others
    otherplot=reactiveVal()
    observeEvent(input$otherbutton,{
      startdt=input$other.Date[1] %>% as.character()
      df2 <- bdh5(ticker=input$other.ticker,field='PX_LAST',startdt=startdt)
      otherplot({plot_ly(df2, x = ~date, y = ~value, name = input$if.ticker, type = 'scatter', mode = 'lines',line = list(color = 'red', width = 2)) %>%
          layout(xaxis = list(title = ""),yaxis = list (title = ""))})
    })
    
    output$plot.other <- renderPlotly({
      otherplot()
    })
     
    
  
}


