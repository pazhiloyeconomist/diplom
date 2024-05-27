library(stats)
library(factoextra)
library(BigVAR)
library(ggpubr)
library(forecast)
library(vars)
library(tibble)
library(lmtest)
library(sandwich)
library(ggplot2)
library(lpirfs)
library(minqa)
library(mvnfast)
library(ustyc)
library(YieldCurve)
library(tseries)
library(dplyr)
library(lubridate)
library(readxl)
NSBetas_monthly_withshocks <- read_excel("C:/Users/Lenovo/Desktop/диплом/data/NSBetas_monthly_withshocks.xlsx")
NSLPdata <- NSBetas_monthly_withshocks
macro_adj <- read_excel("C:/Users/Lenovo/Desktop/диплом/data/macro_adj.xlsx")
macro_adj
plot(as.Date(macro_adj$date), macro_adj$ruo, type="l", col="black", pch="", ylab="RUONIA", 
     xlab='Дата', lty=1, lwd=2.0)
lines(as.Date(NSLPdata$Date), NSLPdata$beta_0, type='l', col='blue')
legend(x = "top",
       legend = c("RUONIA", expression(paste(beta,"0"))),  
       lty = c(1, 1),
       col=c("black","blue"),
       lwd = 2.0)
cor.test(macro_adj$ruo[c(nrow(macro_adj)-nrow(NSLPdata)+1):nrow(macro_adj)],NSLPdata$beta_0)
############################ Nelson Siegel monthly ######################################
######################### Посчитаем ошибку оценки ###############################
mat <- c(1/4.3,2/4.3,1,2,3,6,12,24)
y <- NSrates(as.xts(as.ts(NSLPdata[,c(2:5)])), maturity=mat)
y <- as.data.frame(y)
y$Date
y$Date <- as.Date(NSLPdata$Date)
yields_monthly <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/yields_monthly.csv")
yields_monthly <- yields_monthly[-153,-1]
y <- as.data.frame(y)
y$Date
y$Date <- as.Date(NSLPdata$Date)
fiterns1w <- yields_monthly$X1W-y[,1]
fiterns2w <- yields_monthly$X2W-y[,2]
fiterns1m <- yields_monthly$X1M-y[,3]
fiterns2m <- yields_monthly$X2M-y[,4]
fiterns3m <- yields_monthly$X3M-y[,5]
fiterns6m <- yields_monthly$X6M-y[,6]
fiterns1y <- yields_monthly$X1Y-y[,7]
fiterns2y <- yields_monthly$X2Y-y[,8]

sqrt(sum(na.omit(fiterns1w^2))/length(na.omit(fiterns1w)))
sqrt(sum(na.omit(fiterns2w^2))/length(na.omit(fiterns2w)))
sqrt(sum(na.omit(fiterns1m^2))/length(na.omit(fiterns1m)))
sqrt(sum(na.omit(fiterns2m^2))/length(na.omit(fiterns2m)))
sqrt(sum(na.omit(fiterns3m^2))/length(na.omit(fiterns3m)))
sqrt(sum(na.omit(fiterns6m^2))/length(na.omit(fiterns6m)))
sqrt(sum(na.omit(fiterns1y^2))/length(na.omit(fiterns1y)))
sqrt(sum(na.omit(fiterns2y^2))/length(na.omit(fiterns2y)))


####################### построим прогнозы #######################################
length(y[,1])
mat <- 1/30
for (i in c(2:750)){
  mat <- c(mat,i/30 )
}
y <- NSrates(as.xts(as.ts(NSLPdata[,c(2:5)])), maturity=mat)
y <- as.data.frame(y)
y$Date
y$Date <- NSLPdata$Date
y <- y[,c((length(mat)+1),1:length(mat))]
y$Date <- as.Date(y$Date)+14
len <- nrow(y)

for (i in c(71:nrow(y))){
Dates <- seq.Date(from=(as.Date(y$Date[i])+1),to=(as.Date(y$Date[i])+365),length.out = 365)  
lines(Dates,y[i,c(2:366)], col="blue", lty=1, type="l", lwd=1.0)  
}
as.Date(y$Date[12])
x <- as.Date(y$Date[12])+month(3)
x
#рассчитаем ошибку среднемесячных прогнозов в каждый момент времени
rate <- macro_adj[c(5:nrow(macro_adj)),c(1,8)] 
nser1m <- c()
nser2m <- c()
nser3m <- c()
nser4m <- c()
nser5m <- c()
nser6m <- c()
nser7m <- c()
nser8m <- c()
nser9m <- c()
nser10m <- c()
nser11m <- c()
nser1y <- c()

nser13m <- c()
nser14m <- c()
nser15m <- c()
nser16m <- c()
nser17m <- c()
nser18m <- c()
nser19m <- c()
nser20m <- c()
nser21m <- c()
nser22m <- c()
nser23m <- c()
nser2y <- c()
#будем прогнозить до 2023.11, уберем одно значение
len <- nrow(y)
y <- y[c(1:(len-1)),]
#прогноз на iй месяц по Нельсону Сигелю представляет собой 
#среднее значений участка кривой доходности, попадающего на iй месяц
for (i in c(71:nrow(y))){
  if (i<nrow(y)){
    diff <- as.numeric(as.Date(rate$date[i+1])-as.Date(y$Date[i]))
    f1m <- mean(as.numeric(y[i,c((diff+1):(diff+29))]))
    nser1m <- c(nser1m,(f1m-rate$ruo[i+1]))
  }
  if (i<(nrow(y)-1)){
    diff <- as.numeric(as.Date(rate$date[i+2])-as.Date(y$Date[i]))
    f2m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser2m <- c(nser2m,(f2m-rate$ruo[i+2]))
  }
  if (i<(nrow(y)-2)){
    diff <- as.numeric(as.Date(rate$date[i+3])-as.Date(y$Date[i]))
    f3m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser3m <- c(nser3m,(f3m-rate$ruo[i+3]))
  }
  if (i<(nrow(y)-3)){
    diff <- as.numeric(as.Date(rate$date[i+4])-as.Date(y$Date[i]))
    f4m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser4m <- c(nser4m,(f4m-rate$ruo[i+4]))
  }
  if (i<(nrow(y)-4)){
    diff <- as.numeric(as.Date(rate$date[i+5])-as.Date(y$Date[i]))
    f5m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser5m <- c(nser5m,(f5m-rate$ruo[i+5]))
  }
  if (i<(nrow(y)-5)){
    diff <- as.numeric(as.Date(rate$date[i+6])-as.Date(y$Date[i]))
    f6m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser6m <- c(nser6m,(f6m-rate$ruo[i+6]))
  }
  if (i<(nrow(y)-6)){
    diff <- as.numeric(as.Date(rate$date[i+7])-as.Date(y$Date[i]))
    f7m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser7m <- c(nser7m,(f7m-rate$ruo[i+7]))
  }
  if (i<(nrow(y)-7)){
    diff <- as.numeric(as.Date(rate$date[i+8])-as.Date(y$Date[i]))
    f8m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser8m <- c(nser8m,(f8m-rate$ruo[i+8]))
  }
  if (i<(nrow(y)-8)){
    diff <- as.numeric(as.Date(rate$date[i+9])-as.Date(y$Date[i]))
    f9m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser9m <- c(nser9m,(f9m-rate$ruo[i+9]))
  }
  if (i<(nrow(y)-9)){
    diff <- as.numeric(as.Date(rate$date[i+10])-as.Date(y$Date[i]))
    f10m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser10m <- c(nser10m,(f8m-rate$ruo[i+10]))
  }
  if (i<(nrow(y)-10)){
    diff <- as.numeric(as.Date(rate$date[i+11])-as.Date(y$Date[i]))
    f11m <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser11m <- c(nser11m,(f11m-rate$ruo[i+11]))
  }
  if (i<(nrow(y)-11)){
    diff <- as.numeric(as.Date(rate$date[i+12])-as.Date(y$Date[i]))
    f1y <- mean(as.numeric(y[i,c((diff+1):(diff+30))]))
    nser1y <- c(nser1y,(f1y-rate$ruo[i+12]))
  }
}
sqrt(sum((nser1m^2))/length(nser1m))
sqrt(sum((nser2m^2))/length(nser2m))
sqrt(sum((nser3m^2))/length(nser3m))
sqrt(sum((nser4m^2))/length(nser4m))
sqrt(sum((nser5m^2))/length(nser5m))
sqrt(sum((nser6m^2))/length(nser6m))
sqrt(sum((nser7m^2))/length(nser7m))
sqrt(sum((nser8m^2))/length(nser8m))
sqrt(sum((nser9m^2))/length(nser9m))
sqrt(sum((nser10m^2))/length(nser10m))
sqrt(sum((nser11m^2))/length(nser11m))
sqrt(sum((nser1y^2))/length(nser1y))
length(nser1m)

############################ Svensson monthly ######################################
SVBetas <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/SVBetas_monthly.csv")
SVBetas <- SVBetas[,-1]
########################### Посчитаем ошибку оценки Свенссона #####################

mat <- c(1/4.3,2/4.3,1,2,3,6,12,24)
y <- Srates(as.xts(as.ts(SVBetas[,c(2:7)])), maturity=mat)
y <- as.data.frame(y)
y$Date <- SVBetas$Date
yields_monthly <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/yields_monthly.csv")
yields_monthly <- yields_monthly[-153,-1]
fitersv1w <- yields_monthly$X1W[c(1:58)]-y[,1]
fitersv2w <- yields_monthly$X2W[c(1:58)]-y[,2]
fitersv1m <- yields_monthly$X1M-y[,3]
fitersv2m <- yields_monthly$X2M-y[,4]
fitersv3m <- yields_monthly$X3M-y[,5]
fitersv6m <- yields_monthly$X6M[c(1:58)]-y[,6]
fitersv1y <- yields_monthly$X1Y-y[,7]
fitersv2y <- yields_monthly$X2Y-y[,8]

sqrt(sum(na.omit(fitersv1w^2))/length(na.omit(fitersv1w)))
sqrt(sum(na.omit(fitersv2w^2))/length(na.omit(fitersv2w)))
sqrt(sum(na.omit(fitersv1m^2))/length(na.omit(fitersv1m)))
sqrt(sum(na.omit(fitersv2m^2))/length(na.omit(fitersv2m)))
sqrt(sum(na.omit(fitersv3m^2))/length(na.omit(fitersv3m)))
sqrt(sum(na.omit(fitersv6m^2))/length(na.omit(fitersv6m)))
sqrt(sum(na.omit(fitersv1y^2))/length(na.omit(fitersv1y)))
sqrt(sum(na.omit(fitersv2y^2))/length(na.omit(fitersv2y)))





ys <- Srates(as.xts(as.ts(SVBetas[,c(2:7)])), maturity=mat)
ys <- as.data.frame(ys)
ys$Date <- NSLPdata$Date
ys <- ys[,c((length(mat)+1),1:length(mat))]
ys$Date <- as.Date(ys$Date)+14
len <- nrow(ys)

plot(as.Date(macro_adj$date), macro_adj$ruo, type="l", col="black", pch="", ylab="RUONIA", 
     xlab='Дата', lty=1, lwd=2.0)
for (i in c(71:nrow(ys))){
  Dates <- seq.Date(from=(as.Date(ys$Date[i])+1),to=(as.Date(ys$Date[i])+365),length.out = 365)  
  lines(Dates,ys[i,c(2:366)], col="blue", lty=1, type="l", lwd=1.0)  
}

#рассчитаем ошибку среднемесячных прогнозов в каждый момент времени

rate <- macro_adj[c(5:nrow(macro_adj)),c(1,8)] 
sver1m <- c()
sver2m <- c()
sver3m <- c()
sver4m <- c()
sver5m <- c()
sver6m <- c()
sver7m <- c()
sver8m <- c()
sver9m <- c()
sver10m <- c()
sver11m <- c()
sver1y <- c()
#будем прогнозить до 2023.11, уберем одно значение
ys <- ys[c(1:(len-1)),]
for (i in c(71:nrow(ys))){
  if (i<nrow(ys)){
    diff <- as.numeric(as.Date(rate$date[i+1])-as.Date(ys$Date[i]))
    f1m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver1m <- c(sver1m,(f1m-rate$ruo[i+1]))
  }
  if (i<(nrow(ys)-1)){
    diff <- as.numeric(as.Date(rate$date[i+2])-as.Date(ys$Date[i]))
    f2m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver2m <- c(sver2m,(f2m-rate$ruo[i+2]))
  }
  if (i<(nrow(ys)-2)){
    diff <- as.numeric(as.Date(rate$date[i+3])-as.Date(ys$Date[i]))
    f3m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver3m <- c(sver3m,(f3m-rate$ruo[i+3]))
  }
  if (i<(nrow(ys)-3)){
    diff <- as.numeric(as.Date(rate$date[i+4])-as.Date(ys$Date[i]))
    f4m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver4m <- c(sver4m,(f4m-rate$ruo[i+4]))
  }
  if (i<(nrow(ys)-4)){
    diff <- as.numeric(as.Date(rate$date[i+5])-as.Date(ys$Date[i]))
    f5m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver5m <- c(sver5m,(f5m-rate$ruo[i+5]))
  }
  if (i<(nrow(ys)-5)){
    diff <- as.numeric(as.Date(rate$date[i+6])-as.Date(ys$Date[i]))
    f6m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver6m <- c(sver6m,(f6m-rate$ruo[i+6]))
  }
  if (i<(nrow(ys)-6)){
    diff <- as.numeric(as.Date(rate$date[i+7])-as.Date(ys$Date[i]))
    f7m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver7m <- c(sver7m,(f7m-rate$ruo[i+7]))
  }
  if (i<(nrow(ys)-7)){
    diff <- as.numeric(as.Date(rate$date[i+8])-as.Date(ys$Date[i]))
    f8m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver8m <- c(sver8m,(f8m-rate$ruo[i+8]))
  }
  if (i<(nrow(ys)-8)){
    diff <- as.numeric(as.Date(rate$date[i+9])-as.Date(ys$Date[i]))
    f9m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver9m <- c(sver9m,(f9m-rate$ruo[i+9]))
  }
  if (i<(nrow(ys)-9)){
    diff <- as.numeric(as.Date(rate$date[i+10])-as.Date(ys$Date[i]))
    f10m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver10m <- c(sver10m,(f10m-rate$ruo[i+10]))
  }
  if (i<(nrow(ys)-10)){
    diff <- as.numeric(as.Date(rate$date[i+11])-as.Date(ys$Date[i]))
    f11m <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver11m <- c(sver11m,(f11m-rate$ruo[i+11]))
  }
  if (i<(nrow(ys)-11)){
    diff <- as.numeric(as.Date(rate$date[i+12])-as.Date(ys$Date[i]))
    f1y <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    sver1y <- c(sver1y,(f1y-rate$ruo[i+12]))
  }
}
sqrt(sum((sver1m^2))/length(sver1m))
sqrt(sum((sver2m^2))/length(sver2m))
sqrt(sum((sver3m^2))/length(sver3m))
sqrt(sum((sver4m^2))/length(sver4m))
sqrt(sum((sver5m^2))/length(sver5m))
sqrt(sum((sver6m^2))/length(sver6m))
sqrt(sum((sver7m^2))/length(sver7m))
sqrt(sum((sver8m^2))/length(sver8m))
sqrt(sum((sver9m^2))/length(sver9m))
sqrt(sum((sver10m^2))/length(sver10m))
sqrt(sum((sver11m^2))/length(sver11m))
sqrt(sum((sver1y^2))/length(sver1y))
length(sver1m)

##################### Создадим таблицу с прогнозами Свенссон ###################
forecastsSV <- matrix(ncol = 12)
forecastsSV[1,c(1:12)] <- NA
#наблюдение, с которого начинать тестирование модели
win <- 71
for (i in c(win:nrow(ys))){
  for (j in 1:12){
    diff <- as.numeric(as.Date(rate$date[i])+months(j)-as.Date(ys$Date[i]))
    f <- mean(as.numeric(ys[i,c((diff+1):(diff+30))]))
    forecastsSV[(i-win+1),j] <- f
  }
  forecastsSV <- rbind(forecastsSV,rep(NA,12))
}
#уберем последние NA
v <- nrow(forecastsSV)
forecastsSV <- forecastsSV[c(1:(v-1)),]
forecastsSV <- as.data.frame(forecastsSV)
colnames(forecastsSV) <- c('1M','2M','3M','4M','5M','6M',
                           '7M','8M','9M','10M','11M','1Y')
forecastsSV$Date <- ys$Date[c(win:nrow(ys))]
forecastsSV <- forecastsSV[,c(13,1:12)]
############################## Создадим таблицу ошибок Свенссона #################
sverrors <- matrix(nrow = (length(sver1m)+1), ncol=13)
#ошибка прогноза узнается только потом, поэтому в первый раз все веса по 1/2
sverrors[,1] <- as.character(forecastsSV$Date)
sverrors[(2:(length(sver1m)+1)),2] <- sver1m
sverrors[(3:(length(sver1m)+1)),3] <- sver2m
sverrors[(4:(length(sver1m)+1)),4] <- sver3m
sverrors[(5:(length(sver1m)+1)),5] <- sver4m
sverrors[(6:(length(sver1m)+1)),6] <- sver5m
sverrors[(7:(length(sver1m)+1)),7] <- sver6m
sverrors[(8:(length(sver1m)+1)),8] <- sver7m
sverrors[(9:(length(sver1m)+1)),9] <- sver8m
sverrors[(10:(length(sver1m)+1)),10] <- sver9m
sverrors[(11:(length(sver1m)+1)),11] <- sver10m
sverrors[(12:(length(sver1m)+1)),12] <- sver11m
sverrors[(13:(length(sver1m)+1)),13] <- sver1y
sverrors <- as.data.frame(sverrors)
colnames(sverrors) <- c('Dates','1M','2M','3M','4M','5M','6M',
                            '7M','8M','9M','10M','11M','1Y')
#на всякий сохраню 
write.csv(sverrors,"C:/Users/Lenovo/Desktop/диплом/data/sverrors.csv")
########################### Комбинация прогнозов через среднее 1/2 ############################
#подгрузим прогнозы от var pca
forecastsVARPCA <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/forecastsVARPCA.csv")
#уберем 1-й столбец
forecastsVARPCA <- forecastsVARPCA[,-1]

fcstVPCASV <- matrix(ncol=13,nrow = nrow(forecastsSV))
fcstVPCASV[,1] <- forecastsVARPCA$Date 
for (i in (1:nrow(forecastsSV))){
  for (j in (1:12)){
    fcstVPCASV[i,(j+1)] <- (forecastsVARPCA[i,(j+1)]+forecastsSV[i,(j+1)])/2
  }
}
fcstVPCASV <- as.data.frame(fcstVPCASV)
colnames(fcstVPCASV) <- c('Date','1M','2M','3M','4M','5M','6M',
                          '7M','8M','9M','10M','11M','1Y')

plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))

fcster1m <- c()
fcster2m <- c()
fcster3m <- c()
fcster4m <- c()
fcster5m <- c()
fcster6m <- c()
fcster7m <- c()
fcster8m <- c()
fcster9m <- c()
fcster10m <- c()
fcster11m <- c()
fcster1y <- c()

for (i in (1:nrow(fcstVPCASV))){
  a <- cbind(macro_adj$ruo[74+i], fcstVPCASV[i,c(2:13)])
  lines(seq(from=as.Date(fcstVPCASV$Date[i]), to=(as.Date(fcstVPCASV$Date[i])+months(12)),length.out = 13),
            a, col="blue", lty=1, type="l", lwd=1.0)
  if (i<nrow(fcstVPCASV)){
    fcster1m <- c(fcster1m,(as.numeric(fcstVPCASV[i,2])-macro_adj$ruo[74+i+1]))
  }
  if (i<(nrow(fcstVPCASV)-1)){
    fcster2m <- c(fcster2m,(as.numeric(fcstVPCASV[i,3])-macro_adj$ruo[74+i+2]))
  }
  if (i<(nrow(fcstVPCASV)-2)){
    fcster3m <- c(fcster3m,(as.numeric(fcstVPCASV[i,4])-macro_adj$ruo[74+i+3]))
  }
  if (i<(nrow(fcstVPCASV)-3)){
    fcster4m <- c(fcster4m,(as.numeric(fcstVPCASV[i,5])-macro_adj$ruo[74+i+4]))
  }
  if (i<(nrow(fcstVPCASV)-4)){
    fcster5m <- c(fcster5m,(as.numeric(fcstVPCASV[i,6])-macro_adj$ruo[74+i+5]))
  }
  if (i<(nrow(fcstVPCASV)-5)){
    fcster6m <- c(fcster6m,(as.numeric(fcstVPCASV[i,7])-macro_adj$ruo[74+i+6]))
  }
  if (i<(nrow(fcstVPCASV)-6)){
    fcster7m <- c(fcster7m,(as.numeric(fcstVPCASV[i,8])-macro_adj$ruo[74+i+7]))
  }
  if (i<(nrow(fcstVPCASV)-7)){
    fcster8m <- c(fcster8m,(as.numeric(fcstVPCASV[i,9])-macro_adj$ruo[74+i+8]))
  }
  if (i<(nrow(fcstVPCASV)-8)){
    fcster9m <- c(fcster9m,(as.numeric(fcstVPCASV[i,10])-macro_adj$ruo[74+i+9]))
  }
  if (i<(nrow(fcstVPCASV)-9)){
    fcster10m <- c(fcster10m,(as.numeric(fcstVPCASV[i,11])-macro_adj$ruo[74+i+10]))
  }
  if (i<(nrow(fcstVPCASV)-10)){
    fcster11m <- c(fcster11m,(as.numeric(fcstVPCASV[i,12])-macro_adj$ruo[74+i+11]))
  }
  if (i<(nrow(fcstVPCASV)-11)){
    fcster1y <- c(fcster1y,(as.numeric(fcstVPCASV[i,13])-macro_adj$ruo[74+i+12]))
  }
}

sqrt(sum((fcster1m^2))/length(fcster1m))
sqrt(sum((fcster2m^2))/length(fcster2m))
sqrt(sum((fcster3m^2))/length(fcster3m))
sqrt(sum((fcster4m^2))/length(fcster4m))
sqrt(sum((fcster5m^2))/length(fcster5m))
sqrt(sum((fcster6m^2))/length(fcster6m))
sqrt(sum((fcster7m^2))/length(fcster7m))
sqrt(sum((fcster8m^2))/length(fcster8m))
sqrt(sum((fcster9m^2))/length(fcster9m))
sqrt(sum((fcster10m^2))/length(fcster10m))
sqrt(sum((fcster11m^2))/length(fcster11m))
sqrt(sum((fcster1y^2))/length(fcster1y))
length(fcster1m)

dm.test(fcster1y,nser1y,alternative = 'less')

########################## Комбинация через взвешивание по RMSE ################
sverrors <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/sverrors.csv")
#уберем лишний столбец 
sverrors <- sverrors[,-1]


sum(na.omit(1,NA))
#таблица весов
w <- matrix(ncol=13,nrow = nrow(forecastsSV))
w[,1] <- forecastsVARPCA$Date 
#зададим первоначальный вектор весов
w[1,2:13] <- rep(0.5,12)
for (i in (1:nrow(w))){
  for (j in (2:ncol(w))){
    if (is.na(varpcaerrors[i,j])==TRUE){
      w[i,j]=0.5
    }
    else{
      a <- 1/sqrt(sum(na.omit(as.numeric(varpcaerrors[(1:i),j]))^2)/(length(varpcaerrors[(1:i),j])-j+1))
      b <- 1/sqrt(sum(na.omit(as.numeric(sverrors[(1:i),j]))^2)/(length(sverrors[(1:i),j])-j+1))
      w[i,j] <- a/(a+b)
    }
  }
}
w <- as.data.frame(w)

fcstVPCASV2 <- matrix(ncol=13,nrow = nrow(forecastsSV))
fcstVPCASV2[,1] <- forecastsVARPCA$Date 
for (i in (1:nrow(forecastsSV))){
  for (j in (1:12)){
    fcstVPCASV2[i,(j+1)] <- as.numeric(w[i,(j+1)])*as.numeric(forecastsVARPCA[i,(j+1)])+(1-as.numeric(w[i,(j+1)]))*as.numeric(forecastsSV[i,(j+1)])  
  }
}
fcstVPCASV2 <- as.data.frame(fcstVPCASV2)
colnames(fcstVPCASV2) <- c('Date','1M','2M','3M','4M','5M','6M',
                          '7M','8M','9M','10M','11M','1Y')
#на всякий случай сохраню
write.csv(fcstVPCASV2,"C:/Users/Lenovo/Desktop/диплом/data/fcstVPCASV2.csv")

#построим график прогнозов
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))

fcsterrmse1m <- c()
fcsterrmse2m <- c()
fcsterrmse3m <- c()
fcsterrmse4m <- c()
fcsterrmse5m <- c()
fcsterrmse6m <- c()
fcsterrmse7m <- c()
fcsterrmse8m <- c()
fcsterrmse9m <- c()
fcsterrmse10m <- c()
fcsterrmse11m <- c()
fcsterrmse1y <- c()

for (i in (1:nrow(fcstVPCASV2))){
  a <- cbind(macro_adj$ruo[74+i], fcstVPCASV2[i,c(2:13)])
  lines(seq(from=as.Date(fcstVPCASV2$Date[i]), to=(as.Date(fcstVPCASV2$Date[i])+months(12)),length.out = 13),
        a, col="blue", lty=1, type="l", lwd=1.0)
  if (i<nrow(fcstVPCASV2)){
    fcsterrmse1m <- c(fcsterrmse1m,(as.numeric(fcstVPCASV2[i,2])-macro_adj$ruo[74+i+1]))
  }
  if (i<(nrow(fcstVPCASV2)-1)){
    fcsterrmse2m <- c(fcsterrmse2m,(as.numeric(fcstVPCASV2[i,3])-macro_adj$ruo[74+i+2]))
  }
  if (i<(nrow(fcstVPCASV2)-2)){
    fcsterrmse3m <- c(fcsterrmse3m,(as.numeric(fcstVPCASV2[i,4])-macro_adj$ruo[74+i+3]))
  }
  if (i<(nrow(fcstVPCASV2)-3)){
    fcsterrmse4m <- c(fcsterrmse4m,(as.numeric(fcstVPCASV2[i,5])-macro_adj$ruo[74+i+4]))
  }
  if (i<(nrow(fcstVPCASV2)-4)){
    fcsterrmse5m <- c(fcsterrmse5m,(as.numeric(fcstVPCASV2[i,6])-macro_adj$ruo[74+i+5]))
  }
  if (i<(nrow(fcstVPCASV2)-5)){
    fcsterrmse6m <- c(fcsterrmse6m,(as.numeric(fcstVPCASV2[i,7])-macro_adj$ruo[74+i+6]))
  }
  if (i<(nrow(fcstVPCASV2)-6)){
    fcsterrmse7m <- c(fcsterrmse7m,(as.numeric(fcstVPCASV2[i,8])-macro_adj$ruo[74+i+7]))
  }
  if (i<(nrow(fcstVPCASV2)-7)){
    fcsterrmse8m <- c(fcsterrmse8m,(as.numeric(fcstVPCASV2[i,9])-macro_adj$ruo[74+i+8]))
  }
  if (i<(nrow(fcstVPCASV2)-8)){
    fcsterrmse9m <- c(fcsterrmse9m,(as.numeric(fcstVPCASV2[i,10])-macro_adj$ruo[74+i+9]))
  }
  if (i<(nrow(fcstVPCASV2)-9)){
    fcsterrmse10m <- c(fcsterrmse10m,(as.numeric(fcstVPCASV2[i,11])-macro_adj$ruo[74+i+10]))
  }
  if (i<(nrow(fcstVPCASV2)-10)){
    fcsterrmse11m <- c(fcsterrmse11m,(as.numeric(fcstVPCASV2[i,12])-macro_adj$ruo[74+i+11]))
  }
  if (i<(nrow(fcstVPCASV2)-11)){
    fcsterrmse1y <- c(fcsterrmse1y,(as.numeric(fcstVPCASV2[i,13])-macro_adj$ruo[74+i+12]))
  }
}

sqrt(sum((fcsterrmse1m^2))/length(fcsterrmse1m))
sqrt(sum((fcsterrmse2m^2))/length(fcsterrmse2m))
sqrt(sum((fcsterrmse3m^2))/length(fcsterrmse3m))
sqrt(sum((fcsterrmse4m^2))/length(fcsterrmse4m))
sqrt(sum((fcsterrmse5m^2))/length(fcsterrmse5m))
sqrt(sum((fcsterrmse6m^2))/length(fcsterrmse6m))
sqrt(sum((fcsterrmse7m^2))/length(fcsterrmse7m))
sqrt(sum((fcsterrmse8m^2))/length(fcsterrmse8m))
sqrt(sum((fcsterrmse9m^2))/length(fcsterrmse9m))
sqrt(sum((fcsterrmse10m^2))/length(fcsterrmse10m))
sqrt(sum((fcsterrmse11m^2))/length(fcsterrmse11m))
sqrt(sum((fcsterrmse1y^2))/length(fcsterrmse1y))
length(fcsterrmse11m)


################################################################################
###############################                        #########################
############################### Вышла серьезная ошибка #########################
###############################                        #########################
################################################################################
NSBetas_monthly_withshocks <- read_excel("C:/Users/Lenovo/Desktop/диплом/data/NSBetas_monthly_withshocks.xlsx")
NSLPdata <- NSBetas_monthly_withshocks
macro_adj <- read_excel("C:/Users/Lenovo/Desktop/диплом/data/macro_adj.xlsx")

mat <- 1/30
for (i in c(2:750)){
  mat <- c(mat,i/30 )
}
y <- NSrates(as.xts(as.ts(NSLPdata[,c(2:5)])), maturity=mat)
y <- as.data.frame(y)
y$Date
y$Date <- NSLPdata$Date
y <- y[,c((length(mat)+1),1:length(mat))]
y$Date <- as.Date(y$Date)+14
len <- nrow(y)

yf <- y
for (i in(1:nrow(yf))){
  for (j in (3:ncol(yf))){
    a <- (1+y[i,j]/100)^((j-1)/365)
    b <- (1+y[i,(j-1)]/100)^((j-2)/365)
    yf[i,j] <- 100*((a/b)^(365)-1)
  }
}
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))

for (i in c(71:(nrow(yf)-1))){
  Dates <- seq.Date(from=(as.Date(yf$Date[i])+1),to=(as.Date(yf$Date[i])+365),length.out = 365)  
  lines(Dates,yf[i,c(2:366)], col="blue", lty=1, type="l", lwd=1.0)  
}


rate <- macro_adj[c(5:nrow(macro_adj)),c(1,8)] 
nser1m <- c()
nser2m <- c()
nser3m <- c()
nser4m <- c()
nser5m <- c()
nser6m <- c()
nser7m <- c()
nser8m <- c()
nser9m <- c()
nser10m <- c()
nser11m <- c()
nser1y <- c()

nser13m <- c()
nser14m <- c()
nser15m <- c()
nser16m <- c()
nser17m <- c()
nser18m <- c()
nser19m <- c()
nser20m <- c()
nser21m <- c()
nser22m <- c()
nser23m <- c()
nser2y <- c()
#будем прогнозить до 2023.11, уберем одно значение
len <- nrow(yf)
yf <- yf[c(1:(len-1)),]
#прогноз на iй месяц по Нельсону Сигелю представляет собой 
#среднее значений участка кривой доходности, попадающего на iй месяц
for (i in c(71:nrow(yf))){
  if (i<nrow(yf)){
    diff <- as.numeric(as.Date(rate$date[i+1])-as.Date(yf$Date[i]))
    f1m <- mean(as.numeric(yf[i,c((diff+1):(diff+29))]))
    nser1m <- c(nser1m,(f1m-rate$ruo[i+1]))
  }
  if (i<(nrow(yf)-1)){
    diff <- as.numeric(as.Date(rate$date[i+2])-as.Date(yf$Date[i]))
    f2m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser2m <- c(nser2m,(f2m-rate$ruo[i+2]))
  }
  if (i<(nrow(yf)-2)){
    diff <- as.numeric(as.Date(rate$date[i+3])-as.Date(yf$Date[i]))
    f3m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser3m <- c(nser3m,(f3m-rate$ruo[i+3]))
  }
  if (i<(nrow(yf)-3)){
    diff <- as.numeric(as.Date(rate$date[i+4])-as.Date(yf$Date[i]))
    f4m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser4m <- c(nser4m,(f4m-rate$ruo[i+4]))
  }
  if (i<(nrow(yf)-4)){
    diff <- as.numeric(as.Date(rate$date[i+5])-as.Date(yf$Date[i]))
    f5m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser5m <- c(nser5m,(f5m-rate$ruo[i+5]))
  }
  if (i<(nrow(yf)-5)){
    diff <- as.numeric(as.Date(rate$date[i+6])-as.Date(yf$Date[i]))
    f6m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser6m <- c(nser6m,(f6m-rate$ruo[i+6]))
  }
  if (i<(nrow(yf)-6)){
    diff <- as.numeric(as.Date(rate$date[i+7])-as.Date(yf$Date[i]))
    f7m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser7m <- c(nser7m,(f7m-rate$ruo[i+7]))
  }
  if (i<(nrow(yf)-7)){
    diff <- as.numeric(as.Date(rate$date[i+8])-as.Date(yf$Date[i]))
    f8m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser8m <- c(nser8m,(f8m-rate$ruo[i+8]))
  }
  if (i<(nrow(yf)-8)){
    diff <- as.numeric(as.Date(rate$date[i+9])-as.Date(yf$Date[i]))
    f9m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser9m <- c(nser9m,(f9m-rate$ruo[i+9]))
  }
  if (i<(nrow(yf)-9)){
    diff <- as.numeric(as.Date(rate$date[i+10])-as.Date(yf$Date[i]))
    f10m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser10m <- c(nser10m,(f8m-rate$ruo[i+10]))
  }
  if (i<(nrow(yf)-10)){
    diff <- as.numeric(as.Date(rate$date[i+11])-as.Date(yf$Date[i]))
    f11m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser11m <- c(nser11m,(f11m-rate$ruo[i+11]))
  }
  if (i<(nrow(yf)-11)){
    diff <- as.numeric(as.Date(rate$date[i+12])-as.Date(yf$Date[i]))
    f1y <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser1y <- c(nser1y,(f1y-rate$ruo[i+12]))
  }
  
  if (i<(nrow(yf)-12)){
    diff <- as.numeric(as.Date(rate$date[i+13])-as.Date(yf$Date[i]))
    f13m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser13m <- c(nser13m,(f13m-rate$ruo[i+13]))
  }
  if (i<(nrow(yf)-13)){
    diff <- as.numeric(as.Date(rate$date[i+14])-as.Date(yf$Date[i]))
    f14m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser14m <- c(nser14m,(f14m-rate$ruo[i+14]))
  }
  if (i<(nrow(yf)-14)){
    diff <- as.numeric(as.Date(rate$date[i+15])-as.Date(yf$Date[i]))
    f15m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser15m <- c(nser15m,(f15m-rate$ruo[i+15]))
  }
  if (i<(nrow(yf)-15)){
    diff <- as.numeric(as.Date(rate$date[i+16])-as.Date(yf$Date[i]))
    f16m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser16m <- c(nser16m,(f16m-rate$ruo[i+16]))
  }
  if (i<(nrow(yf)-16)){
    diff <- as.numeric(as.Date(rate$date[i+17])-as.Date(yf$Date[i]))
    f17m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser17m <- c(nser17m,(f17m-rate$ruo[i+17]))
  }
  if (i<(nrow(yf)-17)){
    diff <- as.numeric(as.Date(rate$date[i+18])-as.Date(yf$Date[i]))
    f18m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser18m <- c(nser18m,(f18m-rate$ruo[i+18]))
  }
  if (i<(nrow(yf)-18)){
    diff <- as.numeric(as.Date(rate$date[i+19])-as.Date(yf$Date[i]))
    f19m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser19m <- c(nser19m,(f19m-rate$ruo[i+19]))
  }
  if (i<(nrow(yf)-19)){
    diff <- as.numeric(as.Date(rate$date[i+20])-as.Date(yf$Date[i]))
    f20m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser20m <- c(nser20m,(f20m-rate$ruo[i+20]))
  }
  if (i<(nrow(yf)-20)){
    diff <- as.numeric(as.Date(rate$date[i+21])-as.Date(yf$Date[i]))
    f21m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser21m <- c(nser21m,(f21m-rate$ruo[i+21]))
  }
  if (i<(nrow(yf)-21)){
    diff <- as.numeric(as.Date(rate$date[i+22])-as.Date(yf$Date[i]))
    f22m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser22m <- c(nser22m,(f22m-rate$ruo[i+22]))
  }
  if (i<(nrow(yf)-22)){
    diff <- as.numeric(as.Date(rate$date[i+23])-as.Date(yf$Date[i]))
    f23m <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser23m <- c(nser23m,(f23m-rate$ruo[i+23]))
  }
  if (i<(nrow(yf)-23)){
    diff <- as.numeric(as.Date(rate$date[i+24])-as.Date(yf$Date[i]))
    f2y <- mean(as.numeric(yf[i,c((diff+1):(diff+30))]))
    nser2y <- c(nser2y,(f2y-rate$ruo[i+24]))
  }
}
sqrt(sum((nser1m^2))/length(nser1m))
sqrt(sum((nser2m^2))/length(nser2m))
sqrt(sum((nser3m^2))/length(nser3m))
sqrt(sum((nser4m^2))/length(nser4m))
sqrt(sum((nser5m^2))/length(nser5m))
sqrt(sum((nser6m^2))/length(nser6m))
sqrt(sum((nser7m^2))/length(nser7m))
sqrt(sum((nser8m^2))/length(nser8m))
sqrt(sum((nser9m^2))/length(nser9m))
sqrt(sum((nser10m^2))/length(nser10m))
sqrt(sum((nser11m^2))/length(nser11m))
sqrt(sum((nser1y^2))/length(nser1y))

sqrt(sum((nser13m^2))/length(nser13m))
sqrt(sum((nser14m^2))/length(nser14m))
sqrt(sum((nser15m^2))/length(nser15m))
sqrt(sum((nser16m^2))/length(nser16m))
sqrt(sum((nser17m^2))/length(nser17m))
sqrt(sum((nser18m^2))/length(nser18m))
sqrt(sum((nser19m^2))/length(nser19m))
sqrt(sum((nser20m^2))/length(nser20m))
sqrt(sum((nser21m^2))/length(nser21m))
sqrt(sum((nser22m^2))/length(nser22m))
sqrt(sum((nser23m^2))/length(nser23m))
sqrt(sum((nser2y^2))/length(nser2y))
length(nser2y)


############################## Свенссон ########################################
SVBetas <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/SVBetas_monthly.csv")
SVBetas <- SVBetas[,-1]
mat <- 1/30
for (i in c(2:750)){
  mat <- c(mat,i/30 )
}

ys <- Srates(as.xts(as.ts(SVBetas[,c(2:7)])), maturity=mat)
ys <- as.data.frame(ys)
ys$Date <- NSLPdata$Date
ys <- ys[,c((length(mat)+1),1:length(mat))]
ys$Date <- as.Date(ys$Date)+14
len <- nrow(ys)

ysf <- ys
for (i in(1:nrow(ysf))){
  for (j in (3:ncol(ysf))){
    a <- (1+ys[i,j]/100)^((j-1)/365)
    b <- (1+ys[i,(j-1)]/100)^((j-2)/365)
    ysf[i,j] <- 100*((a/b)^(365)-1)
  }
}

plot(as.Date(macro_adj$date), macro_adj$ruo, type="l", col="black", pch="", ylab="RUONIA", 
     xlab='Дата', lty=1, lwd=2.0,
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))
for (i in c(71:(nrow(ysf)-1))){
  Dates <- seq.Date(from=(as.Date(ysf$Date[i])+1),to=(as.Date(ysf$Date[i])+365),length.out = 365)  
  lines(Dates,ysf[i,c(2:366)], col="blue", lty=1, type="l", lwd=1.0)  
}

#ошибки прогнозов
rate <- macro_adj[c(5:nrow(macro_adj)),c(1,8)] 
sver1m <- c()
sver2m <- c()
sver3m <- c()
sver4m <- c()
sver5m <- c()
sver6m <- c()
sver7m <- c()
sver8m <- c()
sver9m <- c()
sver10m <- c()
sver11m <- c()
sver1y <- c()

sver13m <- c()
sver14m <- c()
sver15m <- c()
sver16m <- c()
sver17m <- c()
sver18m <- c()
sver19m <- c()
sver20m <- c()
sver21m <- c()
sver22m <- c()
sver23m <- c()
sver2y <- c()
#будем прогнозить до 2023.11, уберем одно значение
ysf <- ysf[c(1:(len-1)),]
for (i in c(71:nrow(ysf))){
  if (i<nrow(ysf)){
    diff <- as.numeric(as.Date(rate$date[i+1])-as.Date(ysf$Date[i]))
    f1m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver1m <- c(sver1m,(f1m-rate$ruo[i+1]))
  }
  if (i<(nrow(ysf)-1)){
    diff <- as.numeric(as.Date(rate$date[i+2])-as.Date(ysf$Date[i]))
    f2m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver2m <- c(sver2m,(f2m-rate$ruo[i+2]))
  }
  if (i<(nrow(ysf)-2)){
    diff <- as.numeric(as.Date(rate$date[i+3])-as.Date(ysf$Date[i]))
    f3m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver3m <- c(sver3m,(f3m-rate$ruo[i+3]))
  }
  if (i<(nrow(ysf)-3)){
    diff <- as.numeric(as.Date(rate$date[i+4])-as.Date(ysf$Date[i]))
    f4m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver4m <- c(sver4m,(f4m-rate$ruo[i+4]))
  }
  if (i<(nrow(ysf)-4)){
    diff <- as.numeric(as.Date(rate$date[i+5])-as.Date(ysf$Date[i]))
    f5m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver5m <- c(sver5m,(f5m-rate$ruo[i+5]))
  }
  if (i<(nrow(ysf)-5)){
    diff <- as.numeric(as.Date(rate$date[i+6])-as.Date(ysf$Date[i]))
    f6m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver6m <- c(sver6m,(f6m-rate$ruo[i+6]))
  }
  if (i<(nrow(ysf)-6)){
    diff <- as.numeric(as.Date(rate$date[i+7])-as.Date(ysf$Date[i]))
    f7m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver7m <- c(sver7m,(f7m-rate$ruo[i+7]))
  }
  if (i<(nrow(ysf)-7)){
    diff <- as.numeric(as.Date(rate$date[i+8])-as.Date(ysf$Date[i]))
    f8m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver8m <- c(sver8m,(f8m-rate$ruo[i+8]))
  }
  if (i<(nrow(ysf)-8)){
    diff <- as.numeric(as.Date(rate$date[i+9])-as.Date(ysf$Date[i]))
    f9m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver9m <- c(sver9m,(f9m-rate$ruo[i+9]))
  }
  if (i<(nrow(ysf)-9)){
    diff <- as.numeric(as.Date(rate$date[i+10])-as.Date(ysf$Date[i]))
    f10m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver10m <- c(sver10m,(f10m-rate$ruo[i+10]))
  }
  if (i<(nrow(ysf)-10)){
    diff <- as.numeric(as.Date(rate$date[i+11])-as.Date(ysf$Date[i]))
    f11m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver11m <- c(sver11m,(f11m-rate$ruo[i+11]))
  }
  if (i<(nrow(ysf)-11)){
    diff <- as.numeric(as.Date(rate$date[i+12])-as.Date(ysf$Date[i]))
    f1y <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver1y <- c(sver1y,(f1y-rate$ruo[i+12]))
  }
  
  if (i<(nrow(ysf)-12)){
    diff <- as.numeric(as.Date(rate$date[i+13])-as.Date(ysf$Date[i]))
    f13m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver13m <- c(sver13m,(f13m-rate$ruo[i+13]))
  }
  if (i<(nrow(ysf)-13)){
    diff <- as.numeric(as.Date(rate$date[i+14])-as.Date(ysf$Date[i]))
    f14m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver14m <- c(sver14m,(f14m-rate$ruo[i+14]))
  }
  if (i<(nrow(ysf)-14)){
    diff <- as.numeric(as.Date(rate$date[i+15])-as.Date(ysf$Date[i]))
    f15m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver15m <- c(sver15m,(f15m-rate$ruo[i+15]))
  }
  if (i<(nrow(ysf)-15)){
    diff <- as.numeric(as.Date(rate$date[i+16])-as.Date(ysf$Date[i]))
    f16m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver16m <- c(sver16m,(f16m-rate$ruo[i+16]))
  }
  if (i<(nrow(ysf)-16)){
    diff <- as.numeric(as.Date(rate$date[i+17])-as.Date(ysf$Date[i]))
    f17m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver17m <- c(sver17m,(f17m-rate$ruo[i+17]))
  }
  if (i<(nrow(ysf)-17)){
    diff <- as.numeric(as.Date(rate$date[i+18])-as.Date(ysf$Date[i]))
    f18m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver18m <- c(sver18m,(f18m-rate$ruo[i+18]))
  }
  if (i<(nrow(ysf)-18)){
    diff <- as.numeric(as.Date(rate$date[i+19])-as.Date(ysf$Date[i]))
    f19m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver19m <- c(sver19m,(f19m-rate$ruo[i+19]))
  }
  if (i<(nrow(ysf)-19)){
    diff <- as.numeric(as.Date(rate$date[i+20])-as.Date(ysf$Date[i]))
    f20m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver20m <- c(sver20m,(f20m-rate$ruo[i+20]))
  }
  if (i<(nrow(ysf)-20)){
    diff <- as.numeric(as.Date(rate$date[i+21])-as.Date(ysf$Date[i]))
    f21m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver21m <- c(sver21m,(f21m-rate$ruo[i+21]))
  }
  if (i<(nrow(ysf)-21)){
    diff <- as.numeric(as.Date(rate$date[i+22])-as.Date(ysf$Date[i]))
    f22m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver22m <- c(sver22m,(f22m-rate$ruo[i+22]))
  }
  if (i<(nrow(ysf)-22)){
    diff <- as.numeric(as.Date(rate$date[i+23])-as.Date(ysf$Date[i]))
    f23m <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver23m <- c(sver23m,(f23m-rate$ruo[i+23]))
  }
  if (i<(nrow(ysf)-23)){
    diff <- as.numeric(as.Date(rate$date[i+24])-as.Date(ysf$Date[i]))
    f2y <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    sver2y <- c(sver2y,(f2y-rate$ruo[i+24]))
  }
  
}
sqrt(sum((sver1m^2))/length(sver1m))
sqrt(sum((sver2m^2))/length(sver2m))
sqrt(sum((sver3m^2))/length(sver3m))
sqrt(sum((sver4m^2))/length(sver4m))
sqrt(sum((sver5m^2))/length(sver5m))
sqrt(sum((sver6m^2))/length(sver6m))
sqrt(sum((sver7m^2))/length(sver7m))
sqrt(sum((sver8m^2))/length(sver8m))
sqrt(sum((sver9m^2))/length(sver9m))
sqrt(sum((sver10m^2))/length(sver10m))
sqrt(sum((sver11m^2))/length(sver11m))
sqrt(sum((sver1y^2))/length(sver1y))

sqrt(sum((sver13m^2))/length(sver13m))
sqrt(sum((sver14m^2))/length(sver14m))
sqrt(sum((sver15m^2))/length(sver15m))
sqrt(sum((sver16m^2))/length(sver16m))
sqrt(sum((sver17m^2))/length(sver17m))
sqrt(sum((sver18m^2))/length(sver18m))
sqrt(sum((sver19m^2))/length(sver19m))
sqrt(sum((sver20m^2))/length(sver20m))
sqrt(sum((sver21m^2))/length(sver21m))
sqrt(sum((sver22m^2))/length(sver22m))
sqrt(sum((sver23m^2))/length(sver23m))
sqrt(sum((sver2y^2))/length(sver2y))
length(sver2y)

##################### Создадим таблицу с прогнозами Свенссон ###################
forecastsSV <- matrix(ncol = 24)
forecastsSV[1,c(1:24)] <- NA
#наблюдение, с которого начинать тестирование модели
win <- 71
for (i in c(win:nrow(ysf))){
  for (j in 1:24){
    diff <- as.numeric(as.Date(rate$date[i])+months(j)-as.Date(ysf$Date[i]))
    f <- mean(as.numeric(ysf[i,c((diff+1):(diff+30))]))
    forecastsSV[(i-win+1),j] <- f
  }
  forecastsSV <- rbind(forecastsSV,rep(NA,24))
}
#уберем последние NA
v <- nrow(forecastsSV)
forecastsSV <- forecastsSV[c(1:(v-1)),]
forecastsSV <- as.data.frame(forecastsSV)
colnames(forecastsSV) <- c('1M','2M','3M','4M','5M','6M',
                           '7M','8M','9M','10M','11M','1Y',
                           '13M','14M','15M','16M','17M','18M',
                           '19M','20M','21M','22M','23M','2Y')
forecastsSV$Date <- ysf$Date[c(win:nrow(ysf))]
forecastsSV <- forecastsSV[,c(25,1:24)]
write.csv(forecastsSV, "C:/Users/Lenovo/Desktop/диплом/data/forecastsSV.csv")
########################### Комбинация прогнозов через среднее 1/2 ############################
#подгрузим прогнозы от var pca
forecastsVARPCA <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/forecastsVARPCA.csv")
#уберем 1-й столбец
forecastsVARPCA <- forecastsVARPCA[,-1]
fcstVPCASV <- matrix(ncol=25,nrow = nrow(forecastsSV))
nrow(forecastsVARPCA)
fcstVPCASV[,1] <- forecastsVARPCA$Date 
for (i in (1:nrow(forecastsSV))){
  for (j in (1:24)){
    fcstVPCASV[i,(j+1)] <- (forecastsVARPCA[i,(j+1)]+forecastsSV[i,(j+1)])/2
  }
}
fcstVPCASV <- as.data.frame(fcstVPCASV)
colnames(fcstVPCASV) <- c('Date','1M','2M','3M','4M','5M','6M',
                          '7M','8M','9M','10M','11M','1Y',
                          '13M','14M','15M','16M','17M','18M',
                          '19M','20M','21M','22M','23M','2Y')

plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))

fcster1m <- c()
fcster2m <- c()
fcster3m <- c()
fcster4m <- c()
fcster5m <- c()
fcster6m <- c()
fcster7m <- c()
fcster8m <- c()
fcster9m <- c()
fcster10m <- c()
fcster11m <- c()
fcster1y <- c()

fcster13m <- c()
fcster14m <- c()
fcster15m <- c()
fcster16m <- c()
fcster17m <- c()
fcster18m <- c()
fcster19m <- c()
fcster20m <- c()
fcster21m <- c()
fcster22m <- c()
fcster23m <- c()
fcster2y <- c()

for (i in (1:nrow(fcstVPCASV))){
  a <- cbind(macro_adj$ruo[74+i], fcstVPCASV[i,c(2:25)])
  #lines(seq(from=as.Date(fcstVPCASV$Date[i]), to=(as.Date(fcstVPCASV$Date[i])+months(12)),length.out = 13),
   #     a, col="blue", lty=1, type="l", lwd=1.0)
  if (i<nrow(fcstVPCASV)){
    fcster1m <- c(fcster1m,(as.numeric(fcstVPCASV[i,2])-macro_adj$ruo[74+i+1]))
  }
  if (i<(nrow(fcstVPCASV)-1)){
    fcster2m <- c(fcster2m,(as.numeric(fcstVPCASV[i,3])-macro_adj$ruo[74+i+2]))
  }
  if (i<(nrow(fcstVPCASV)-2)){
    fcster3m <- c(fcster3m,(as.numeric(fcstVPCASV[i,4])-macro_adj$ruo[74+i+3]))
  }
  if (i<(nrow(fcstVPCASV)-3)){
    fcster4m <- c(fcster4m,(as.numeric(fcstVPCASV[i,5])-macro_adj$ruo[74+i+4]))
  }
  if (i<(nrow(fcstVPCASV)-4)){
    fcster5m <- c(fcster5m,(as.numeric(fcstVPCASV[i,6])-macro_adj$ruo[74+i+5]))
  }
  if (i<(nrow(fcstVPCASV)-5)){
    fcster6m <- c(fcster6m,(as.numeric(fcstVPCASV[i,7])-macro_adj$ruo[74+i+6]))
  }
  if (i<(nrow(fcstVPCASV)-6)){
    fcster7m <- c(fcster7m,(as.numeric(fcstVPCASV[i,8])-macro_adj$ruo[74+i+7]))
  }
  if (i<(nrow(fcstVPCASV)-7)){
    fcster8m <- c(fcster8m,(as.numeric(fcstVPCASV[i,9])-macro_adj$ruo[74+i+8]))
  }
  if (i<(nrow(fcstVPCASV)-8)){
    fcster9m <- c(fcster9m,(as.numeric(fcstVPCASV[i,10])-macro_adj$ruo[74+i+9]))
  }
  if (i<(nrow(fcstVPCASV)-9)){
    fcster10m <- c(fcster10m,(as.numeric(fcstVPCASV[i,11])-macro_adj$ruo[74+i+10]))
  }
  if (i<(nrow(fcstVPCASV)-10)){
    fcster11m <- c(fcster11m,(as.numeric(fcstVPCASV[i,12])-macro_adj$ruo[74+i+11]))
  }
  if (i<(nrow(fcstVPCASV)-11)){
    fcster1y <- c(fcster1y,(as.numeric(fcstVPCASV[i,13])-macro_adj$ruo[74+i+12]))
  }
  
  if (i<(nrow(fcstVPCASV)-12)){
    fcster13m <- c(fcster13m,(as.numeric(fcstVPCASV[i,14])-macro_adj$ruo[74+i+13]))
  }
  if (i<(nrow(fcstVPCASV)-13)){
    fcster14m <- c(fcster14m,(as.numeric(fcstVPCASV[i,15])-macro_adj$ruo[74+i+14]))
  }
  if (i<(nrow(fcstVPCASV)-14)){
    fcster15m <- c(fcster15m,(as.numeric(fcstVPCASV[i,16])-macro_adj$ruo[74+i+15]))
  }
  if (i<(nrow(fcstVPCASV)-15)){
    fcster16m <- c(fcster16m,(as.numeric(fcstVPCASV[i,17])-macro_adj$ruo[74+i+16]))
  }
  if (i<(nrow(fcstVPCASV)-16)){
    fcster17m <- c(fcster17m,(as.numeric(fcstVPCASV[i,18])-macro_adj$ruo[74+i+17]))
  }
  if (i<(nrow(fcstVPCASV)-17)){
    fcster18m <- c(fcster18m,(as.numeric(fcstVPCASV[i,19])-macro_adj$ruo[74+i+18]))
  }
  if (i<(nrow(fcstVPCASV)-18)){
    fcster19m <- c(fcster19m,(as.numeric(fcstVPCASV[i,20])-macro_adj$ruo[74+i+19]))
  }
  if (i<(nrow(fcstVPCASV)-19)){
    fcster20m <- c(fcster20m,(as.numeric(fcstVPCASV[i,21])-macro_adj$ruo[74+i+20]))
  }
  if (i<(nrow(fcstVPCASV)-20)){
    fcster21m <- c(fcster21m,(as.numeric(fcstVPCASV[i,22])-macro_adj$ruo[74+i+21]))
  }
  if (i<(nrow(fcstVPCASV)-21)){
    fcster22m <- c(fcster22m,(as.numeric(fcstVPCASV[i,23])-macro_adj$ruo[74+i+22]))
  }
  if (i<(nrow(fcstVPCASV)-22)){
    fcster23m <- c(fcster23m,(as.numeric(fcstVPCASV[i,24])-macro_adj$ruo[74+i+23]))
  }
  if (i<(nrow(fcstVPCASV)-23)){
    fcster2y <- c(fcster2y,(as.numeric(fcstVPCASV[i,25])-macro_adj$ruo[74+i+24]))
  }
}

sqrt(sum((fcster1m^2))/length(fcster1m))
sqrt(sum((fcster2m^2))/length(fcster2m))
sqrt(sum((fcster3m^2))/length(fcster3m))
sqrt(sum((fcster4m^2))/length(fcster4m))
sqrt(sum((fcster5m^2))/length(fcster5m))
sqrt(sum((fcster6m^2))/length(fcster6m))
sqrt(sum((fcster7m^2))/length(fcster7m))
sqrt(sum((fcster8m^2))/length(fcster8m))
sqrt(sum((fcster9m^2))/length(fcster9m))
sqrt(sum((fcster10m^2))/length(fcster10m))
sqrt(sum((fcster11m^2))/length(fcster11m))
sqrt(sum((fcster1y^2))/length(fcster1y))

sqrt(sum((fcster13m^2))/length(fcster13m))
sqrt(sum((fcster14m^2))/length(fcster14m))
sqrt(sum((fcster15m^2))/length(fcster15m))
sqrt(sum((fcster16m^2))/length(fcster16m))
sqrt(sum((fcster17m^2))/length(fcster17m))
sqrt(sum((fcster18m^2))/length(fcster18m))
sqrt(sum((fcster19m^2))/length(fcster19m))
sqrt(sum((fcster20m^2))/length(fcster20m))
sqrt(sum((fcster21m^2))/length(fcster21m))
sqrt(sum((fcster22m^2))/length(fcster22m))
sqrt(sum((fcster23m^2))/length(fcster23m))
sqrt(sum((fcster2y^2))/length(fcster2y))
length(fcster2y)
length(fcster1m)

dm.test(fcster18m,nser18m, h=20, alternative = 'less')

varpcaerrors <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/varpcaerrors.csv")
varpcaerrors <- varpcaerrors[,-1]
varpcaerrors
dm.test(na.omit(as.numeric(varpcaerrors[,21])),nser20m, alternative = 'less')

dm.test(na.omit(as.numeric(varpcaerrors[,7])),fcster6m, alternative = 'less')

############################## Создадим таблицу ошибок Свенссона #################
sverrors <- matrix(nrow = (length(sver1m)+1), ncol=25)
#ошибка прогноза узнается только потом, поэтому в первый раз все веса по 1/2
sverrors[,1] <- as.character(forecastsSV$Date)
sverrors[(2:(length(sver1m)+1)),2] <- sver1m
sverrors[(3:(length(sver1m)+1)),3] <- sver2m
sverrors[(4:(length(sver1m)+1)),4] <- sver3m
sverrors[(5:(length(sver1m)+1)),5] <- sver4m
sverrors[(6:(length(sver1m)+1)),6] <- sver5m
sverrors[(7:(length(sver1m)+1)),7] <- sver6m
sverrors[(8:(length(sver1m)+1)),8] <- sver7m
sverrors[(9:(length(sver1m)+1)),9] <- sver8m
sverrors[(10:(length(sver1m)+1)),10] <- sver9m
sverrors[(11:(length(sver1m)+1)),11] <- sver10m
sverrors[(12:(length(sver1m)+1)),12] <- sver11m
sverrors[(13:(length(sver1m)+1)),13] <- sver1y

sverrors[(13:(length(macro_adj$ruo)-win)),14] <- sver13m
sverrors[(14:(length(macro_adj$ruo)-win)),15] <- sver14m
sverrors[(15:(length(macro_adj$ruo)-win)),16] <- sver15m
sverrors[(16:(length(macro_adj$ruo)-win)),17] <- sver16m
sverrors[(17:(length(macro_adj$ruo)-win)),18] <- sver17m
sverrors[(18:(length(macro_adj$ruo)-win)),19] <- sver18m
sverrors[(19:(length(macro_adj$ruo)-win)),20] <- sver19m
sverrors[(20:(length(macro_adj$ruo)-win)),21] <- sver20m
sverrors[(21:(length(macro_adj$ruo)-win)),22] <- sver21m
sverrors[(22:(length(macro_adj$ruo)-win)),23] <- sver22m
sverrors[(23:(length(macro_adj$ruo)-win)),24] <- sver23m
sverrors[(24:(length(macro_adj$ruo)-win)),25] <- sver2y
sverrors <- as.data.frame(sverrors)
colnames(sverrors) <- c('Dates','1M','2M','3M','4M','5M','6M',
                        '7M','8M','9M','10M','11M','1Y',
                        '13M','14M','15M','16M','17M','18M',
                        '19M','20M','21M','22M','23M','2Y')
#на всякий сохраню 
write.csv(sverrors,"C:/Users/Lenovo/Desktop/диплом/data/sverrors.csv")


########################## Комбинация через взвешивание по RMSE ################
sverrors <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/sverrors.csv")
#уберем лишний столбец 
sverrors <- sverrors[,-1]
varpcaerrors <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/varpcaerrors.csv")
varpcaerrors <- varpcaerrors[,-1]
varpcaerrors <- rbind(sverrors[1,],varpcaerrors)
sum(na.omit(1,NA))
#таблица весов
w <- matrix(ncol=25,nrow = nrow(forecastsSV))
w[,1] <- forecastsVARPCA$Date 
#зададим первоначальный вектор весов
okno <- 12
w[1,2:25] <- rep(0.5,24)
for (i in (2:nrow(w))){
  for (j in (2:ncol(w))){
    if (is.na(varpcaerrors[(i),j])==TRUE){
      w[i,j]=0.5
    }
    else{
      if ((i-j)<okno){
        a <- 1/sqrt(sum(na.omit(as.numeric(varpcaerrors[(1:i),j]))^2)/(length(varpcaerrors[(1:i),j])-j+1))
        b <- 1/sqrt(sum(na.omit(as.numeric(sverrors[(1:i),j]))^2)/(length(sverrors[(1:i),j])-j+1))
        w[i,j] <- a/(a+b)
      }
      else{
        beg <- i-okno+1
        a <- 1/sqrt(sum(na.omit(as.numeric(varpcaerrors[(beg:i),j]))^2)/(length(varpcaerrors[(beg:i),j])))
        b <- 1/sqrt(sum(na.omit(as.numeric(sverrors[(beg:i),j]))^2)/(length(sverrors[(beg:i),j])))
        w[i,j] <- a/(a+b)
      }
    }
  }
}
w <- as.data.frame(w)
w
forecastsSV <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/forecastsSV.csv")
forecastsSV <- forecastsSV[,-1]

fcstVPCASV2 <- matrix(ncol=25,nrow = nrow(forecastsSV))
fcstVPCASV2[,1] <- forecastsVARPCA$Date 
for (i in (1:nrow(forecastsSV))){
  for (j in (1:24)){
    fcstVPCASV2[i,(j+1)] <- as.numeric(w[i,(j+1)])*as.numeric(forecastsVARPCA[i,(j+1)])+(1-as.numeric(w[i,(j+1)]))*as.numeric(forecastsSV[i,(j+1)])  
  }
}
fcstVPCASV2 <- as.data.frame(fcstVPCASV2)
colnames(fcstVPCASV2) <- c('Date','1M','2M','3M','4M','5M','6M',
                           '7M','8M','9M','10M','11M','1Y',
                           '13M','14M','15M','16M','17M','18M',
                           '19M','20M','21M','22M','23M','2Y')
#на всякий случай сохраню
write.csv(fcstVPCASV2,"C:/Users/Lenovo/Desktop/диплом/data/fcstVPCASV2.csv")

#построим график прогнозов
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))

fcsterrmse1m <- c()
fcsterrmse2m <- c()
fcsterrmse3m <- c()
fcsterrmse4m <- c()
fcsterrmse5m <- c()
fcsterrmse6m <- c()
fcsterrmse7m <- c()
fcsterrmse8m <- c()
fcsterrmse9m <- c()
fcsterrmse10m <- c()
fcsterrmse11m <- c()
fcsterrmse1y <- c()

fcsterrmse13m <- c()
fcsterrmse14m <- c()
fcsterrmse15m <- c()
fcsterrmse16m <- c()
fcsterrmse17m <- c()
fcsterrmse18m <- c()
fcsterrmse19m <- c()
fcsterrmse20m <- c()
fcsterrmse21m <- c()
fcsterrmse22m <- c()
fcsterrmse23m <- c()
fcsterrmse2y <- c()

for (i in (1:nrow(fcstVPCASV2))){
  a <- cbind(macro_adj$ruo[74+i], fcstVPCASV2[i,c(2:13)])
  #lines(seq(from=as.Date(fcstVPCASV2$Date[i]), to=(as.Date(fcstVPCASV2$Date[i])+months(12)),length.out = 13),
   #     a, col="blue", lty=1, type="l", lwd=1.0)
  if (i<nrow(fcstVPCASV2)){
    fcsterrmse1m <- c(fcsterrmse1m,(as.numeric(fcstVPCASV2[i,2])-macro_adj$ruo[74+i+1]))
  }
  if (i<(nrow(fcstVPCASV2)-1)){
    fcsterrmse2m <- c(fcsterrmse2m,(as.numeric(fcstVPCASV2[i,3])-macro_adj$ruo[74+i+2]))
  }
  if (i<(nrow(fcstVPCASV2)-2)){
    fcsterrmse3m <- c(fcsterrmse3m,(as.numeric(fcstVPCASV2[i,4])-macro_adj$ruo[74+i+3]))
  }
  if (i<(nrow(fcstVPCASV2)-3)){
    fcsterrmse4m <- c(fcsterrmse4m,(as.numeric(fcstVPCASV2[i,5])-macro_adj$ruo[74+i+4]))
  }
  if (i<(nrow(fcstVPCASV2)-4)){
    fcsterrmse5m <- c(fcsterrmse5m,(as.numeric(fcstVPCASV2[i,6])-macro_adj$ruo[74+i+5]))
  }
  if (i<(nrow(fcstVPCASV2)-5)){
    fcsterrmse6m <- c(fcsterrmse6m,(as.numeric(fcstVPCASV2[i,7])-macro_adj$ruo[74+i+6]))
  }
  if (i<(nrow(fcstVPCASV2)-6)){
    fcsterrmse7m <- c(fcsterrmse7m,(as.numeric(fcstVPCASV2[i,8])-macro_adj$ruo[74+i+7]))
  }
  if (i<(nrow(fcstVPCASV2)-7)){
    fcsterrmse8m <- c(fcsterrmse8m,(as.numeric(fcstVPCASV2[i,9])-macro_adj$ruo[74+i+8]))
  }
  if (i<(nrow(fcstVPCASV2)-8)){
    fcsterrmse9m <- c(fcsterrmse9m,(as.numeric(fcstVPCASV2[i,10])-macro_adj$ruo[74+i+9]))
  }
  if (i<(nrow(fcstVPCASV2)-9)){
    fcsterrmse10m <- c(fcsterrmse10m,(as.numeric(fcstVPCASV2[i,11])-macro_adj$ruo[74+i+10]))
  }
  if (i<(nrow(fcstVPCASV2)-10)){
    fcsterrmse11m <- c(fcsterrmse11m,(as.numeric(fcstVPCASV2[i,12])-macro_adj$ruo[74+i+11]))
  }
  if (i<(nrow(fcstVPCASV2)-11)){
    fcsterrmse1y <- c(fcsterrmse1y,(as.numeric(fcstVPCASV2[i,13])-macro_adj$ruo[74+i+12]))
  }
  
  if (i<(nrow(fcstVPCASV2)-12)){
    fcsterrmse13m <- c(fcsterrmse13m,(as.numeric(fcstVPCASV2[i,14])-macro_adj$ruo[74+i+13]))
  }
  if (i<(nrow(fcstVPCASV2)-13)){
    fcsterrmse14m <- c(fcsterrmse14m,(as.numeric(fcstVPCASV2[i,15])-macro_adj$ruo[74+i+14]))
  }
  if (i<(nrow(fcstVPCASV2)-14)){
    fcsterrmse15m <- c(fcsterrmse15m,(as.numeric(fcstVPCASV2[i,16])-macro_adj$ruo[74+i+15]))
  }
  if (i<(nrow(fcstVPCASV2)-15)){
    fcsterrmse16m <- c(fcsterrmse16m,(as.numeric(fcstVPCASV2[i,17])-macro_adj$ruo[74+i+16]))
  }
  if (i<(nrow(fcstVPCASV2)-16)){
    fcsterrmse17m <- c(fcsterrmse17m,(as.numeric(fcstVPCASV2[i,18])-macro_adj$ruo[74+i+17]))
  }
  if (i<(nrow(fcstVPCASV2)-17)){
    fcsterrmse18m <- c(fcsterrmse18m,(as.numeric(fcstVPCASV2[i,19])-macro_adj$ruo[74+i+18]))
  }
  if (i<(nrow(fcstVPCASV2)-18)){
    fcsterrmse19m <- c(fcsterrmse19m,(as.numeric(fcstVPCASV2[i,20])-macro_adj$ruo[74+i+19]))
  }
  if (i<(nrow(fcstVPCASV2)-19)){
    fcsterrmse20m <- c(fcsterrmse20m,(as.numeric(fcstVPCASV2[i,21])-macro_adj$ruo[74+i+20]))
  }
  if (i<(nrow(fcstVPCASV2)-20)){
    fcsterrmse21m <- c(fcsterrmse21m,(as.numeric(fcstVPCASV2[i,22])-macro_adj$ruo[74+i+21]))
  }
  if (i<(nrow(fcstVPCASV2)-21)){
    fcsterrmse22m <- c(fcsterrmse22m,(as.numeric(fcstVPCASV2[i,23])-macro_adj$ruo[74+i+22]))
  }
  if (i<(nrow(fcstVPCASV2)-22)){
    fcsterrmse23m <- c(fcsterrmse23m,(as.numeric(fcstVPCASV2[i,24])-macro_adj$ruo[74+i+23]))
  }
  if (i<(nrow(fcstVPCASV2)-23)){
    fcsterrmse2y <- c(fcsterrmse2y,(as.numeric(fcstVPCASV2[i,25])-macro_adj$ruo[74+i+24]))
  }
  
}

sqrt(sum((fcsterrmse1m^2))/length(fcsterrmse1m))
sqrt(sum((fcsterrmse2m^2))/length(fcsterrmse2m))
sqrt(sum((fcsterrmse3m^2))/length(fcsterrmse3m))
sqrt(sum((fcsterrmse4m^2))/length(fcsterrmse4m))
sqrt(sum((fcsterrmse5m^2))/length(fcsterrmse5m))
sqrt(sum((fcsterrmse6m^2))/length(fcsterrmse6m))
sqrt(sum((fcsterrmse7m^2))/length(fcsterrmse7m))
sqrt(sum((fcsterrmse8m^2))/length(fcsterrmse8m))
sqrt(sum((fcsterrmse9m^2))/length(fcsterrmse9m))
sqrt(sum((fcsterrmse10m^2))/length(fcsterrmse10m))
sqrt(sum((fcsterrmse11m^2))/length(fcsterrmse11m))
sqrt(sum((fcsterrmse1y^2))/length(fcsterrmse1y))

sqrt(sum((fcsterrmse13m^2))/length(fcsterrmse13m))
sqrt(sum((fcsterrmse14m^2))/length(fcsterrmse14m))
sqrt(sum((fcsterrmse15m^2))/length(fcsterrmse15m))
sqrt(sum((fcsterrmse16m^2))/length(fcsterrmse16m))
sqrt(sum((fcsterrmse17m^2))/length(fcsterrmse17m))
sqrt(sum((fcsterrmse18m^2))/length(fcsterrmse18m))
sqrt(sum((fcsterrmse19m^2))/length(fcsterrmse19m))
sqrt(sum((fcsterrmse20m^2))/length(fcsterrmse20m))
sqrt(sum((fcsterrmse21m^2))/length(fcsterrmse21m))
sqrt(sum((fcsterrmse22m^2))/length(fcsterrmse22m))
sqrt(sum((fcsterrmse23m^2))/length(fcsterrmse23m))
sqrt(sum((fcsterrmse2y^2))/length(fcsterrmse2y))
length(fcsterrmse2y)
