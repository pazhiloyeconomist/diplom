library(seasonal)
library(readxl)
library(tseries)
macro <- read_excel("C:/Users/Lenovo/Desktop/диплом/data/зад+выпсук.xlsx")
macrots <- ts(macro[,c(2:7)], start=c(2011,1,1), end=c(2023,11,1), frequency = 12)
#seasonal adjustment using x-13arima-seats
output <- seas(macrots)
final(output)
out <- as.data.frame(final(output))
out$ruo <- macro$ruo
out$date <- macro$date
out <- out[,c(8,1:7)]
out$cum_inf <- NA
out$cum_inf[1] <- 1+out$inf[1]/100
for (i in c(2:155)){
  out$cum_inf[i] <- out$cum_inf[i-1]*(1+out$inf[i]/100)
}

#приведем данные к ценам 2010 года
out$gov <- out$gov/out$cum_inf
out$debt <- out$debt/out$cum_inf

adf.test(ts(out$oil)) #не стационарен
adf.test(ts(diff(out$oil))) #стационарен

write.csv(out,                   
          "C:/Users/Lenovo/Desktop/диплом/data/macro_adj.csv") 

