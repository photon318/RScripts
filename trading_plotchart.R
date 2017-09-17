library(quantmod)


S = getSymbols("VRTX", src = "google", auto.assign = FALSE); 

chartSeries(S
            ,subset = "2017/201710"
            ,TA=c(addRSI(2), addATR(2))
            ,type = c("bars") 
            ,theme = chartTheme("black")
            #,up.col = FALSE, dn.col = FALSE, color.vol = FALSE
            
            )



