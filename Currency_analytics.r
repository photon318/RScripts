

setwd("~/Documents/GitHub/RScripts")
system.time(curr <- read.csv("Data/Rates2017.csv", header = TRUE ))

plot(curr$Date, curr$EUR, type = 'b')

