require(quantmod)
require(PerformanceAnalytics)


Symbols<-c ( "AFL", "AMGN", "AMT","CSX","EW","HOLX","HON","ITW","JD","MMC","MMM","MU","NUE","NVDA","OMC","RF","SHW","WDC","LRCX","VRTX","TSLA","VRTS" )


Symbols<-c ( "NVDA","OMC","RF","SHW","WDC","LRCX","VRTX","TSLA","VRTS" )


length(Symbols)


#Set start date
start_date=as.Date("2017-03-01")

#Create New environment to contain stock price data
dataEnv<-new.env()

#download data          
getSymbols(Symbols,env=dataEnv,from=start_date)


#You have 19 symbols, the time series data for all the symbols might not be aligned 


#Load Systematic investor toolbox for helpful functions


con = gzcon(url("http://www.systematicportfolio.com/sit.gz"))
source(con)
close(con)

#helper function for extracting Closing price of getsymbols output and for date alignment 

bt.prep(dataEnv,align='remove.na')

#Now all your time series are correctly aligned

#prices data

stock_prices = dataEnv$prices
head(stock_prices[,1:3])
# head(stock_prices[,1:3])
#             BAC    CVX   DIS
#2014-01-02 16.10 124.14 76.27
#2014-01-03 16.41 124.35 76.11
#2014-01-06 16.66 124.02 75.82
#2014-01-07 16.50 125.07 76.34
#2014-01-08 16.58 123.29 75.22
#2014-01-09 16.83 123.29 74.90

#calculate returns
stock_returns = Return.calculate(stock_prices, method = c("discrete"))
head(stock_returns[,1:3])
# head(stock_returns[,1:3])
#                    BAC          CVX          DIS
#2014-01-02           NA           NA           NA
#2014-01-03  0.019254658  0.001691638 -0.002097810
#2014-01-06  0.015234613 -0.002653800 -0.003810275
#2014-01-07 -0.009603842  0.008466376  0.006858349
#2014-01-08  0.004848485 -0.014232030 -0.014671208
#2014-01-09  0.015078408  0.000000000 -0.004254188

#Plot Performance for first three stocks
charts.PerformanceSummary(stock_returns[,1:length(Symbols)],main='Stock Absolute Performance',legend.loc="topleft")
charts.RollingPerformance(stock_returns[,1:length(Symbols)], width = 21, legend.loc = "topleft")

