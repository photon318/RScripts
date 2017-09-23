###############################################################################
# Load Systematic Investor Toolbox (SIT)
# <a class="vglnk" href="http://systematicinvestor.wordpress.com/systematic-investor-toolbox/" rel="nofollow"><span>http</span><span>://</span><span>systematicinvestor</span><span>.</span><span>wordpress</span><span>.</span><span>com</span><span>/</span><span>systematic</span><span>-</span><span>investor</span><span>-</span><span>toolbox</span><span>/</span></a>
###############################################################################
#setInternet2(TRUE)

#download.file("http://www.systematicportfolio.com/sit.gz", "sit.gz" )

con = gzcon(url("http://www.systematicportfolio.com/sit.gz"))
source(con)
close(con)


#*****************************************************************
# Create Efficient Frontier
#******************************************************************
# create sample historical input assumptions
ia = az_ia() # used custom override to use data till 2017 
# ia = aa.test.create.ia()
#options(max.print = 10000)
#ia

#t(ia$correlation[rev(order(row.names(ia$correlation))),])
  
image(ia$correlation, axes = FALSE) 
#contour(ia$correlation, axes = FALSE)
#image(ia$correlation, axes = FALSE)
axis(3, at =seq(0,1, length = 8), labels = colnames(ia$correlation))
axis(2, at =seq(0,1, length = 8), labels = rownames(ia$correlation))

      
# create long-only, fully invested efficient frontier
n = ia$n


# 0 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(diag(n), type = '>=', b = 0, constraints)
constraints = add.constraints(diag(n), type = '<=', b = 1, constraints)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)

# create efficient frontier
ef = portopt(ia, constraints, 50, 'Efficient Frontier')

#*****************************************************************
# Create Plot
#******************************************************************
# plot efficient frontier
plot.ef(ia, list(ef), transition.map = F)

# find maximum sharpe portfolio
max(portfolio.return(ef$weight, ia) /  portfolio.risk(ef$weight, ia))

# plot minimum variance portfolio
weight = min.var.portfolio(ia, constraints)
points(
  100 * portfolio.risk(weight, ia),
  100 * portfolio.return(weight, ia),
  pch = 15,
  col = 'red'
)
portfolio.return(weight, ia) /  portfolio.risk(weight, ia)

# plot maximum Sharpe or tangency portfolio
weight = max.sharpe.portfolio()(ia, constraints)
points(
  100 * portfolio.risk(weight, ia),
  100 * portfolio.return(weight, ia),
  pch = 15,
  col = 'orange'
)
portfolio.return(weight, ia) /  portfolio.risk(weight, ia)

plota.legend('Minimum Variance,Maximum Sharpe', 'red,orange', x = 'topright')



#*****************************************************************
# Examples of Maximum Sharpe or Tangency portfolios construction
#******************************************************************     
weight = max.sharpe.portfolio('long-only')(ia,constraints)  
round(weight,2)
round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)

weight = max.sharpe.portfolio('long-short')(ia,constraints)         
round(weight,2)
round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)

weight = max.sharpe.portfolio('market-neutral')(ia,constraints)         
round(weight,2)
round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)  


#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')

tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                            
bt.prep(data, align='keep.all', dates='2004:12::')

#*****************************************************************
# Code Strategies
#******************************************************************
prices = data$prices  
n = ncol(prices)

models = list()

#*****************************************************************
# Code Strategies
#******************************************************************
# find period ends
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]

n.mom = 180
n.vol = 60
n.top = 4        
momentum = prices / mlag(prices, n.mom)  

obj = portfolio.allocation.helper(data$prices, period.ends=period.ends,
                                  lookback.len = n.vol, universe = ntop(momentum[period.ends,], n.top) > 0,
                                  min.risk.fns = list(
                                                      #EW=equal.weight.portfolio
                                                      #,RP=risk.parity.portfolio
                                                      #,MV=min.var.portfolio
                                                      #,MD=max.div.portfolio
                                                      #MC=min.corr.portfolio
                                                      #,MC2=min.corr2.portfolio
                                                      #,MCE=min.corr.excel.portfolio
                                                      MS=max.sharpe.portfolio()
                                                      )
) 

models = create.strategies(obj, data)$models

#*****************************************************************
# Create Report
#******************************************************************    
strategy.performance.snapshoot(models, T)

plotbt.custom.report.part2(models$MS)

# Plot Portfolio Turnover for each strategy
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')


az_ia <- function()
{
  load.packages('quantmod,quadprog')
  symbols = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
  symbol.names = spl(
    'S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year Treasury,U.S. Real Estate,Gold'
  )
  getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)
  hist.prices = merge(SPY, QQQ, EEM, IWM, EFA, TLT, IYR, GLD)
  month.ends = endpoints(hist.prices, 'months')
  hist.prices = Ad(hist.prices)[month.ends, ]
  colnames(hist.prices) = symbols
  hist.prices = na.omit(hist.prices['1995::2017'])
  hist.returns = na.omit(ROC(hist.prices, type = 'discrete'))
  ia = create.historical.ia(hist.returns, 12, symbols, symbol.names)
  return(ia)
}
