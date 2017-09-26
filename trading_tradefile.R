#setwd("/")
require(graphics)

system.time(tr <- read.csv("TR001.csv", header = TRUE ))

# Net amount - 30
# Comission amount 23
# Principal 22
# Price Amount 20
# Quantity 19

#tr[17,]

# Net amount check
#print(tr[19] * tr[20] + tr[23])

#for(r in tr)
#  print(r["Principal"])

matplot(ifelse(tr[19] * tr[20] + tr[23] == tr[30],  "Passed", tr[19] * tr[20] + tr[23]))



print(tr["Security.Description"])
principal = tr["Quantity"] * tr["PriceAmount"]
print(principal)
print(tr["Principal"])
while(principal = tr["Principal"]) print(principal)


