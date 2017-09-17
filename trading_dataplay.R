library(quantmod)
library(ggplot2)

symb = getSymbols("TSLA",
                  src = "google",
                  auto.assign = FALSE) # autoassign false to load into variable.

#chartSeries(MSFT)

#chartSeries(MSFT, subset='last 4 months')

chartSeries(
  symb,
  subset = '2016/2017',
  TA = c(addRSI(2), addROC(), addVo()),
  show.grid = TRUE,
  theme = chartTheme("black")
)



#chartSeries(PCLN, subset='2017', TA=(add_RSI(14)), show.grid = TRUE, theme =chartTheme("black"))

add_RSI(n = 2)


chart_Series(MSFT_ret, subset = '2016/2017')

chart_Series(PCLN ,  subset = '2017')

add_RSI(n = 2)




#chartSeries(NVDA,theme=chartTheme('white'))
#chartSeries(NVDA,TA=NULL)   #no volume
#chartSeries(NVDA,TA=c(addVo(),addBBands()))  #add volume and Bollinger Bands from TTR

#addMACD()   #  add MACD indicator to current chart

#Plotting retuns of the symbol with caculated STD,
symbol_name = "AAPL"
symb = getSymbols(symbol_name,
                  src = "google",
                  auto.assign = FALSE) # autoassign false to load into variable.

returns = periodReturn(symb, period = 'monthly', subset = '2000/2017') * 100
std = sd(returns)
#plotting
barplot(returns,
        ylab = "Returns, %",
        xlab = "Months",
        main = symbol_name)
abline(h =  -std, col = "red")
abline(h =  std, col = "red")
abline(h =  mean(returns), col = 'yellow')
abline(h =  median(returns), col = 'orange')

#plot(MSFT_ret)

#getSymbols("PCLN",
#           src = "google",
#           auto.assign = TRUE) # autoassign false to load into variable.

#MSFT_ret = as.integer(periodReturn(MSFT, period = 'weekly', subset = '2006/2016') * 100)
returns = periodReturn(symb, period = 'monthly', subset = '2006/2017') * 100

ret_3d = matrix(returns, 11, 12, dimnames = list(c(2006:2016), month.abb))

ret_3d

persp( x = c(2006:2016),  y=c(1:12),
  ret_3d,
  theta = -30,
  phi = 45,
  expand = 0.5,
  col = "lightblue",
  ltheta = 120,
  shade = 0.75,
  ticktype = "detailed", nticks = 12
  ,  xlab = "Years", ylab = "Months", zlab = "Retruns, %"
)


#ggplot(MSFT_ret)


#persp(msft_3d, theta = 30, phi = 30, expand = 0.5, col = "lightblue")


image(
  ret_3d,
  main = c(symbol_name, " returns"),
  xlab = "Years",
  ylab = "Months"
)

#x <- -100:0
#y <- (x + 1) / (x - 1)
#plot(x, y, type = "l")


#x <- -20:0
#y  <- -2 / 5 * (x + 1) ^ 2
#plot(x, y, type = "l")
