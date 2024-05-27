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

forecastsVARPCA <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/forecastsVARPCA.csv")
forecastsVARPCA <- forecastsVARPCA[,-1]

macro_adj <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/macro_adj.csv")
macro_adj <- macro_adj[,-1]

sverrors <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/sverrors.csv")
#уберем лишний столбец 
sverrors <- sverrors[,-1]
varpcaerrors <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/varpcaerrors.csv")
varpcaerrors <- varpcaerrors[,-1]
varpcaerrors <- rbind(sverrors[1,],varpcaerrors)
sum(na.omit(1,NA))
#таблица весов
forecastsSV <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/forecastsSV.csv")
forecastsSV <- forecastsSV[,-1]
w <- matrix(ncol=25,nrow = nrow(forecastsSV))
w[,1] <- forecastsVARPCA$Date 
#зададим первоначальный вектор весов
okno <- 1000
w[1,2:25] <- rep(0.5,24)

for (i in (2:nrow(w))){
  for (j in (2:ncol(w))){
    if (is.na(varpcaerrors[(i),j])==TRUE){
      w[i,j]=0.5
    } else if ((i-j)<okno){
      a <- 1/sqrt(sum(na.omit(as.numeric(varpcaerrors[(1:i),j]))^2)/(length(varpcaerrors[(1:i),j])-j+1))
      b <- 1/sqrt(sum(na.omit(as.numeric(sverrors[(1:i),j]))^2)/(length(sverrors[(1:i),j])-j+1))
      w[i,j] <- as.numeric(a)/(as.numeric(a)+as.numeric(b))
    }else if ((i-j)>=okno){
      beg <- i-okno+1
      a <- 1/sqrt(sum(na.omit(as.numeric(varpcaerrors[(beg:i),j]))^2)/(length(varpcaerrors[(beg:i),j])))
      b <- 1/sqrt(sum(na.omit(as.numeric(sverrors[(beg:i),j]))^2)/(length(sverrors[(beg:i),j])))
      w[i,j] <- as.numeric(a)/(as.numeric(a)+as.numeric(b))
    }
  }
}


w <- as.data.frame(w)
w


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
macro_adj
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
