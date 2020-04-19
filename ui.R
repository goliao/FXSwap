library(shinydashboard)
library(plotly,lib.loc="/rsma/shiny/if/sections/gmsm/Rlibrary")
library(lubridate)
library(magrittr)
dashboardPage(
  dashboardHeader(title = "GMSM FX Swaps"),
  dashboardSidebar(
    width = 150,
    sidebarMenu(
      menuItem("OISBasis", tabName = "fxbasisall", icon = icon("th")),
      menuItem("IntradaySnap", tabName = "snap", icon = icon("th")),
      menuItem("Intraday5min", tabName = "fxbasis", icon = icon("th")),
      menuItem("DailyEoD", tabName = "fxbasisdaily", icon = icon("th")),
      menuItem("OtherIntradays", tabName = "intradayfinancial", icon = icon("th"))
      
    )

  ),
  dashboardBody(
    
    tabItems(
     
      
      # intraday snap
      tabItem(tabName = "snap",
              verticalLayout( 
                h3('Intraday snaps 4 time a day'),
                wellPanel(
                  dateInput("snapDate", "start date",value = ((lubridate::today()-2) %>% as.character()),min='2020-03-16'),
                  actionButton('snapbutton','Retrieve')
                ),
                tableOutput("snaptable"),
                box(width=15,plotlyOutput("snapplot", height = 450))
              ),
              h4('source: MFMA Bloomberg')
      ),
      
      # intraday 5min
      tabItem(tabName = "fxbasis",
              verticalLayout( 
                h3('Intraday 5 min interval data retreived at 5 pm daily'),
                wellPanel(
                  dateInput("Date", "start date",value = (today()-2) %>% as.character(),min='2020-03-16'),
                  actionButton('intraday5button','Retrieve')
                ),
                box(width=15,plotlyOutput("plotfxbasis", height = 450))
                
                
              ),
              h4('source: MFMA Bloomberg')
      ),
      
      # EOD
      tabItem(tabName = "fxbasisdaily",
              verticalLayout( 
               h3('End of day bloomberg prices'),
                wellPanel(
                  dateInput("eod.Date", "start date",value = '2016-01-01'),
                  actionButton('eodbutton','Retrieve')
                ),
                box(width=15,plotlyOutput("plot.eod", height = 450)),
                
                tableOutput("table.eod")
              ),
              h4('source: MFMA Bloomberg')
      ),

      
      # OIS EOD
      #
      tabItem(tabName = "fxbasisall",
              sidebarLayout( 
                sidebarPanel(
                  dateInput("all.Date", "start date",value = (today()-360) %>% as.character(),min='2006-03-16'),
                  checkboxGroupInput('oischeckbox','',
                                     c('ois.basis.eur.1w','ois.basis.eur.1m','ois.basis.eur.3m','ois.basis.jpy.1w','ois.basis.jpy.1m','ois.basis.jpy.3m','libor.basis.eur.1w',
                                       'libor.basis.eur.1m','libor.basis.eur.3m','libor.basis.jpy.1w','libor.basis.jpy.1m','libor.basis.jpy.3m'),
                                     c('ois.basis.eur.1w','ois.basis.eur.3m','ois.basis.jpy.1w','ois.basis.jpy.3m')
                                     ),
                  actionButton('fxbasisallbutton','Retrieve')
                  
                ),mainPanel(
                plotlyOutput("plotfxbasisall", height = 450),
                
                tableOutput("tablefxbasisall"),
                h4('source: MFMA Bloomberg, GMSM')
                
                )
              
              )
      ),
      
      tabItem(tabName = "intradayfinancial",
              verticalLayout( 
                h3('Intraday 5 min interval data retreived at 5 pm daily'),
                wellPanel(
                  selectInput('other.ticker','ticker',list('DXY Curncy', 'EUR Curncy','GBP Curncy', 'JPY Curncy', 'SPX Index', 'SP1 Index', 'VIX Index', 'SX5E Index', 'DJA Index', 'TU2 Comdty', 'USSO1C Curncy', 'EUSWEC Curncy', 'USSWAP10 Curncy', 'EUSWI10 CMPN Curncy', 'USGG10YR Index', 'USGGBE10 Index', 'GOLDS Comdty'),'EUR Curncy'),
                  dateInput("other.Date", "start date",value = (today()-3) %>% as.character()),
                  actionButton('otherbutton','Retrieve')
                ),
                
                box(width=15,plotlyOutput("plot.other", height = 450))
              ),
              h4('source: MFMA Bloomberg')
      )

      #, 
      # # First tab content
      # tabItem(tabName = "onrrp",
      #         fluidRow( 
                
      #           box(width=3,
      #               title = "Controls",
      #               dateRangeInput("TradeDate", "date range:",
      #                              start = "2019-11-01",
      #                              startview = "year")
      #           ),
                
      #           #box(plotOutput("plot1", height = 450))
      #           box(width=8,plotlyOutput("plot2", height = 450))
                
      #         )
      # ),
      # #second tab
      # tabItem(tabName = "dashboard",
      #         fluidRow( 
                
      #           box(width=3,
      #               title = "Controls",
      #               dateRangeInput("Date", "date range:",
      #                              start = "2019-11-01",
      #                              startview = "year")
      #           ),
                
      #           #box(plotOutput("plot1", height = 450))
      #           box(width=8,plotlyOutput("plot1", height = 450))
                
      #         )
      # )
    )

    
  ) # end of dashboardBody
)
