#install.packages("YieldCurve")
#install.packages('ustyc')
#install.packages("minqa")
#install.packages("HI")
#install.packages("mvnfast")
#install.packages('lpirfs')
#install.packages('ggpubr')
#install.packages('BigVAR')
#install.packages('factoextra')
#install.packages('stats')
#install.packages('fastVAR')
#install.packages('BVAR')
#install.packages("remotes")
install.packages("MTS")
install.packages('vars')
library(BVAR)
library(MTS)
library(remotes)
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
library(stargazer)
library(writexl)
?VARpred
ROISFIX_2012_11_01_2024_01_20 <- read_excel("C:/Users/Lenovo/Desktop/диплом/data/ROISFIX 2012-11-01_2024-01-20.xlsx",
                                            sheet = "Worksheet")
class(yield$Date)
yield <- ROISFIX_2012_11_01_2024_01_20
yield <- yield[c(1:3134),c(1:19)]

yield <- yield[order(yield$Date),]


################ ЭТО ВОЗМОЖНО НЕВЕРНАЯ ПРЕМИЯ ЗА СРОЧОСТЬ ##################
#short term
plot(yield$Date,yield$TP1W, type="l", col="black", pch="", ylab="Премия за срочность, п.п.", 
     xlab='Дата', lty=1, lwd=2.0)
lines(yield$Date, 
      yield$TP2W, col="blue",lty=2, lwd=2.0)
lines(yield$Date, 
      yield$TP1M, col="red",lty=3, lwd=2.0)
legend(x = "bottom",
       legend = c("1 неделя", "2 недели", "1 месяц"),  
       lty = c(1, 2, 3),
       col=c("black","blue","red"),
       lwd = 2.0)
tp1w <- ts(yield$TP1W)

##################################################################
#заменим нули и всякую шляпу на NA
yield$TP1Y<- NA
yield$TP2Y<- NA

#надо пересчитать премию за срок
yield$TP2M <- yield$`2M`-yield$`1W`
yield$TP3M <- yield$`3M`-yield$`1W`
yield$TP6M <- yield$`6M`-yield$`1W`
yield$TP1Y[c(1198:3134)] <- yield$`1Y`[c(1198:3134)]-yield$`1W`[c(1198:3134)]
yield$TP2Y[c(2699:3134)] <- yield$`2Y`[c(2699:3134)]-yield$`1W`[c(2699:3134)]




# middle term
plot(yield$Date,yield$TP2M, type="l", col="black", pch="", ylab="Премия за срочность, п.п.", 
     xlab='Дата', lty=1, lwd=2.0)
lines(yield$Date, 
      yield$TP3M, col="blue",lty=2, lwd=2.0)
lines(yield$Date, 
      yield$TP6M, col="red",lty=3, lwd=2.0)
legend(x = "top",
       legend = c("2 месяца", "3 месяца", "6 месяцев"),  
       lty = c(1, 2, 3),
       col=c("black","blue","red"),
       lwd = 2.0)

# long term
plot(yield$Date[c(1198:3134)],yield$TP1Y[c(1198:3134)], type="l", col="black", pch="", ylab="Премия за срочность", 
     xlab='Дата', lty=1, lwd=2.0, ylim=c(-5,12))
lines(yield$Date[c(2699:3134)], 
      yield$TP2Y[c(2699:3134)], col="blue",lty=2, lwd=2.0)
legend(x = "top",
       legend = c("1 год","2 года"),  
       lty = c(1,2),
       col=c("black", "blue"),
       lwd = 2.0)


#создаем временные ряды
TP1W <- ts(yield$TP1W)
TP2W <- ts(yield$TP2W)
TP1M <- ts(yield$TP1M)
TP2M <- ts(yield$TP2M)
TP3M <- ts(yield$TP3M)
TP6M <- ts(yield$TP6M)
TP1Y <- ts(yield$TP1Y)
TP2Y <- ts(yield$TP2Y)

#проверка на стационарность
adf.test(TP1W)
adf.test(TP2W)
adf.test(na.omit(TP1M))
adf.test(na.omit(TP2M))
adf.test(na.omit(TP3M))
adf.test(na.omit(TP6M))
adf.test(na.omit(TP1Y))
adf.test(na.omit(TP2Y))

#среднее значение премии за срочность
mean(TP1W)
mean(TP2W)
mean(na.omit(TP1M))
mean(na.omit(TP2M))
mean(na.omit(TP3M))
mean(na.omit(TP6M))
mean(na.omit(TP1Y))
mean(na.omit(TP2Y))

#простой Нельсон Сигель
#данные
class(yield$Date)
NStable <- yield
NStable <- NStable[,c(1:9)]
NStable$`2Y`[c(2693:2698)] <- NA 
class(NStable[1,1])
#убираем премию за срочность
NStable$`1W` <- NStable$`1W`-mean(TP1W)
NStable$`2W` <- NStable$`2W`-mean(TP2W)
NStable$`1M` <- NStable$`1M`-mean(na.omit(TP1M))
NStable$`2M` <- NStable$`2M`-mean(na.omit(TP2M))
NStable$`3M` <- NStable$`3M`-mean(na.omit(TP3M))
NStable$`6M` <- NStable$`6M`-mean(na.omit(TP6M))
NStable$`1Y`[c(1198:3134)] <- NStable$`1Y`[c(1198:3134)]-mean(na.omit(TP1Y))
NStable$`2Y`[c(2699:3134)] <- NStable$`2Y`[c(2699:3134)]-mean(na.omit(TP2Y))

maturity <- c(1/4.3,2/4.3,1,2,3,6)

#возьмем данные на первый день месяца
NSmonthly <- NStable[1,]
month(NSmonthly$Date[1])
month(NStable$Date[1])!= month(NStable$Date[2])
for (i in c(1:3134)){
 if (month(NStable$Date[i])!= month(NStable$Date[i+1])){
   NSmonthly[nrow(NSmonthly)+1,] <- NStable[i+1,]
 }
}


#расчет параметров NS для тех наблюдений, где не существует 1у и 2у
maturity <- c(1/4.3,2/4.3,1,2,3,6)
NSParameters <- Nelson.Siegel(rate= NSmonthly[c(1:59),c(2:7)], maturity=maturity)
NSParameters[1,1]
NSParameters <- as.ts(NSParameters)
NSParameters <- as.xts(NSParameters)
y <- NSrates(NSParameters, maturity=c(1/4.3,2/4.3,1,2,3,6,12))
y <- as.data.frame(y)
y$Date <- NSmonthly$Date[c(1:59)]
y <- y[,c(8,1:7)]
NSmonthly[1,1]

#строим графики ruonia и ожиданий через каждые два месяца
Dates <- NSmonthly$Date[1]

class(yield$DT)
a <- yield[yield$DT==Dates,]
a
#Dates <- as.POSIXct(NSmonthly[1,1], tz="UTC")
for (i in c(1:364)){
  Dates <- c(Dates, NSmonthly[1,1]+i*86400)
}

mat <- 1/30
for (i in c(2:365)){
  mat <- c(mat,i/30 )
}

maturity <- c(1/4.3,2/4.3,1,2,3,6,12)
y1 <- NSrates(NSParameters[1,], mat)
#pdf("C:/Users/Lenovo/Desktop/диплом/NS1")
plot(yield$DT, yield$ruo, type="l", lty=1, lwd=0.8, ylab="Ставка, п.п.",xlab="Время" )
lines(Dates,y1, col="blue", lty=1, type="l", lwd=1.0)

for (i in seq(3,59,2)){
  Dates <- NSmonthly[i,1]
  for (j in c(1:364)){
    Dates <- c(Dates, NSmonthly[i,1]+j*86400)
  }
  y1 <- NSrates(NSParameters[i,], mat)
  lines(Dates,y1, col="blue", lty=1, type="l", lwd=1.0)
}

#dev.off()
#расчет параметров NS для тех наблюдений, где существует 1у, но нет 2y
maturity <- c(1/4.3,2/4.3,1,2,3,6,12)
NSParameters2 <- Nelson.Siegel(rate= NSmonthly[c(60:132),c(2:8)], maturity=maturity)
NSParameters2 <- as.ts(NSParameters2)
NSParameters2 <- as.xts(NSParameters2)
y2 <- NSrates(NSParameters2, maturity=c(1/4.3,2/4.3,1,2,3,6,12))
y2 <- as.data.frame(y2)
y2$Date <- NSmonthly$Date[c(60:132)]
y2 <- y2[,c(8,1:7)]

#строим графики ожиданий через каждые два месяца
for (i in seq(60,132,2)){
  Dates <- NSmonthly[i,1]+86400
  for (j in c(1:364)){
    Dates <- c(Dates, NSmonthly[i,1]+j*86400)
  }
  y3 <- NSrates(NSParameters2[i-59,], mat)
  lines(Dates,y3, col="blue", lty=1, type="l", lwd=1.0)
}

#поскольку мало данных для 2у, то премию за срочность для 2у 

#график ождианий для оставшихся дат
maturity <- c(1/4.3,2/4.3,1,2,3,6,12,24)
NSParameters2 <- Nelson.Siegel(rate= NSmonthly[c(133:153),c(2:9)], maturity=maturity)
NSParameters2 <- as.ts(NSParameters2)
NSParameters2 <- as.xts(NSParameters2)

for (i in seq(133,153,2)){
  Dates <- NSmonthly[i,1]+86400
  for (j in c(1:364)){
    Dates <- c(Dates, NSmonthly[i,1]+j*86400)
  }
  y3 <- NSrates(NSParameters2[i-132,], mat)
  lines(Dates,y3, col="blue", lty=1, type="l", lwd=1.0)
}
legend(x = "topright",
              legend = c("RUONIA","Рыночные ожидания"), 
              lty = 1,
              col=c("black", "blue"))
#dev.off()

#######################################################
#######################################################
#######################################################
#сделаем аналогично с моделью Свенссона
#png("svensson.png", type="cairo")
#расчет параметров NS для тех наблюдений, где не существует 1у и 2у
maturity <- c(1/4.3,2/4.3,1,2,3,6)
SVParameters <- Svensson(rate= NSmonthly[c(1:59),c(2:7)], maturity=maturity)
SVParameters <- as.ts(SVParameters)
SVParameters <- as.xts(SVParameters)
sy <- Srates(SVParameters, maturity=c(1/4.3,2/4.3,1,2,3,6,12))
sy <- as.data.frame(sy)
sy$Date <- NSmonthly$Date[c(1:59)]
sy <- sy[,c(8,1:7)]


#строим графики ruonia и ожиданий через каждые два месяца
Dates <- NSmonthly[1,1]+86400
for (i in c(1:364)){
  Dates <- c(Dates, NSmonthly[1,1]+i*86400)
}

mat <- 1/30
for (i in c(2:365)){
  mat <- c(mat,i/30 )
}

sy1 <- Srates(SVParameters[1,], mat)
sy1
length(Dates)
plot(yield$DT, yield$ruo, type="l", lty=1, lwd=0.5, ylab="Ставка",xlab="Время" )
lines(Dates, sy1, col="blue", lty=1, lwd=1.0)

for (i in seq(3,59,2)){
  Dates <- NSmonthly[i,1]
  for (j in c(1:364)){
    Dates <- c(Dates, NSmonthly[i,1]+j*86400)
  }
  sy1 <- Srates(SVParameters[i,], mat)
  lines(Dates,sy1, col="blue", lty=1, type="l", lwd=1.0)
}


#расчет параметров SV для тех наблюдений, где существует 1у, но нет 2y
maturity <- c(1/4.3,2/4.3,1,2,3,6,12)
SVParameters2 <- Svensson(rate= NSmonthly[c(60:132),c(2:8)], maturity=maturity)
SVParameters2 <- as.ts(SVParameters2)
SVParameters2 <- as.xts(SVParameters2)
sy2 <- Srates(SVParameters2, maturity=c(1/4.3,2/4.3,1,2,3,6,12))
sy2 <- as.data.frame(sy2)
sy2$Date <- NSmonthly$Date[c(60:132)]
sy2 <- sy2[,c(8,1:7)]

#строим графики ожиданий через каждые два месяца
for (i in seq(60,132,2)){
  Dates <- NSmonthly[i,1]+86400
  for (j in c(1:364)){
    Dates <- c(Dates, NSmonthly[i,1]+j*86400)
  }
  sy3 <- Srates(SVParameters2[i-59,], mat)
  lines(Dates,sy3, col="blue", lty=1, type="l", lwd=1.0)
}

#поскольку мало данных для 2у, то премию за срочность для 2у 
#рассчитаем по методу из FRED, разницу доходностей между 2у и 1w



#график ождианий для оставшихся дат
maturity <- c(1/4.3,2/4.3,1,2,3,6,12,24)
SVParameters2 <- Svensson(rate= NSmonthly[c(133:153),c(2:9)], maturity=maturity)
SVParameters2 <- as.ts(SVParameters2)
SVParameters2 <- as.xts(SVParameters2)

for (i in seq(133,153,2)){
  Dates <- NSmonthly[i,1]+86400
  for (j in c(1:364)){
    Dates <- c(Dates, NSmonthly[i,1]+j*86400)
  }
  sy3 <- Srates(SVParameters2[i-132,], mat)
  lines(Dates,sy3, col="blue", lty=1, type="l", lwd=1.0)
}
legend(x = "topright",
       legend = c("RUONIA","Рыночные ожидания"), 
       lty = 1,
       col=c("black", "blue"))
#dev.off()
#ради фана глянем, какие ожидания были 2022-03-02
#######    ДОДЕЛАТЬ!!!!!!!!!!!!!!!!!!!!!!!!! #######
maturity <- c(1/4.3,2/4.3,1,2,3,6,12)
SVmarch <- Svensson(rate= NStable[2294,c(2:8)], maturity=maturity)
SVmarch <- as.ts(SVmarch)
SVmarch <- as.xts(SVmarch)
Dates <- NStable[2294,1]
for (j in c(1:364)){
  Dates <- c(Dates, NStable[2294,1]+j*86400)
}
sy3
sy3 <- Srates(SVmarch, mat)
lines(Dates,sy3, col="blue", lty=1, type="l", lwd=1.0)

############################################################
###################### Графики бет Нельсон Сигель ##########
############################################################
maturity <- c(1/4.3,2/4.3,1,2,3,6)
NSBetas <- Nelson.Siegel(rate= NStable[c(1:1197),c(2:7)], maturity=maturity)

maturity <- c(1/4.3,2/4.3,1,2,3,6,12)
NSBetas2 <-Nelson.Siegel(rate= NStable[c(1198:2698),c(2:8)], maturity=maturity) 

maturity <- c(1/4.3,2/4.3,1,2,3,6,12,24)
NSBetas3 <-Nelson.Siegel(rate= NStable[c(2699:3134),c(2:9)], maturity=maturity) 

plot(yield$DT, yield$ruo, type="l", lty=1, lwd=0.8, ylab="Ставка",xlab="Время" )
#legend(x = "top",
 #      legend = "RUONIA", 
  #     lty = 1,
   #    col="black")
lines(NStable$Date[c(1:1197)],NSBetas[,1], col="blue", lty=1, type="l", lwd=1.0)
lines(NStable$Date[c(1198:2698)],NSBetas2[,1], col="blue", lty=1, type="l", lwd=1.0)
lines(NStable$Date[c(2699:3134)],NSBetas3[,1], col="blue", lty=1, type="l", lwd=1.0)

#сравним беты0 для разных сроков (02.05.2024)
# период с 2022 года
plot(yield$DT[c(2699:3134)], yield$ruo[c(2699:3134)], type="l", lty=1, lwd=0.8, ylab="Ставка",xlab="Время" )
lines(NStable$Date[c(2699:3134)],NSBetas3[,1], col="blue", lty=1, type="l", lwd=1.0)

maturity <- c(1/4.3,2/4.3,1,2,3,6,12)
NSBetasother <-Nelson.Siegel(rate= NStable[c(2699:3134),c(2:8)], maturity=maturity)
lines(NStable$Date[c(2699:3134)],NSBetasother[,1], col="red", lty=1, type="l", lwd=1.0)

legend(x = "top",
       legend = c("RUONIA", "beta0-2y", "beta0-1y"),  
       lty = c(1, 1, 1),
       col=c("black","blue","red"),
       lwd = 1.0)

cor.test(NSBetas3[,1],NSBetasother[,1])

#период с 2016 года
plot(yield$DT[c(1198:2698)], yield$ruo[c(1198:2698)], type="l", lty=1, lwd=0.8, ylab="Ставка",xlab="Время" )
lines(NStable$Date[c(1198:2698)],NSBetas2[,1], col="blue", lty=1, type="l", lwd=1.0)

maturity <- c(1/4.3,2/4.3,1,2,3,6)
NSBetasothe2 <-Nelson.Siegel(rate= NStable[c(1198:2698),c(2:7)], maturity=maturity)
lines(NStable$Date[c(1198:2698)],NSBetasothe2[,1], col="red", lty=1, type="l", lwd=1.0)

legend(x = "top",
       legend = c("RUONIA", "beta0-1y", "beta0-6m"),  
       lty = c(1, 1, 1),
       col=c("black","blue","red"),
       lwd = 1.0)

#abline(v = ymd_hms("2016-03-04 12:15:00"),col='red', lwd=1.5)
#abline(v = ymd_hms("2022-04-15 12:15:00"),col='red', lwd=1.5)

legend(x = "top",
       legend = c("RUONIA",expression(paste(beta,"0"))), 
       lty = 1,
       col=c("black", "blue"))


min(NSBetas[,2],NSBetas2[,2],NSBetas3[,2])
plot(yield$DT, yield$ruo, type="l", lty=1, lwd=0.8, ylab="Ставка",xlab="Время", ylim=c(-11.5,30))
lines(NStable$Date[c(1:1197)],NSBetas[,2], col="blue", lty=1, type="l", lwd=1.0)
lines(NStable$Date[c(1198:2698)],NSBetas2[,2], col="blue", lty=1, type="l", lwd=1.0)
lines(NStable$Date[c(2699:3134)],NSBetas3[,2], col="blue", lty=1, type="l", lwd=1.0)
legend(x = "top",
       legend = c("RUONIA",expression(paste(beta,"1"))), 
       lty = 1,
       col=c("black", "blue"))

min(NSBetas[,3],NSBetas2[,3],NSBetas3[,3])
plot(yield$DT, yield$ruo, type="l", lty=1, lwd=0.8, ylab="Ставка",xlab="Время", ylim=c(-9,30))
lines(NStable$Date[c(1:1197)],NSBetas[,3], col="blue", lty=1, type="l", lwd=1.0)
lines(NStable$Date[c(1198:2698)],NSBetas2[,3], col="blue", lty=1, type="l", lwd=1.0)
lines(NStable$Date[c(2699:3134)],NSBetas3[,3], col="blue", lty=1, type="l", lwd=1.0)
legend(x = "top",
       legend = c("RUONIA",expression(paste(beta,"2"))), 
       lty = 1,
       col=c("black", "blue"))

############################################################
###################### Графики бет Свенссон ################
############################################################
maturity <- c(1/4.3,2/4.3,1,2,3,6)
SVBetas <- Svensson(rate= NStable[c(1:1197),c(2:7)], maturity=maturity)

maturity <- c(1/4.3,2/4.3,1,2,3,6,12)
SVBetas2 <-Svensson(rate= NStable[c(1198:2698),c(2:8)], maturity=maturity) 

maturity <- c(1/4.3,2/4.3,1,2,3,6,12,24)
SVBetas3 <-Svensson(rate= NStable[c(2699:3134),c(2:9)], maturity=maturity) 


################ Считаем ошибку прогноза ##############
#######################################################

#посчитаем ежедневные прогнозы
NSParameters_all<- rbind(NSBetas,NSBetas2, NSBetas3)
NSParameters_all <- as.data.frame(NSParameters_all)
NSParameters_all$Date <- NStable$Date
NSParameters_all <- NSParameters_all[,c(5,1:4)]
y_all <- NSrates(as.xts(as.ts(NSParameters_all[,c(2:5)])), maturity=c(1/4.3,2/4.3,1,2,3,6,12))
NSParameters_all <- cbind(NSParameters_all, y_all)
NSParameters_all <- cbind(NSParameters_all, yield$ruo)
NSParameters_all <- cbind(NSParameters_all, yield$DT)

colnames(NSParameters_all) <- c('Date', 'beta_0','beta_1', 'beta_2', 
                                'lambda', '1W','2W', '1M',
                                '2M','3M','6M','1Y','ruo','DT')
NSParameters_all$Date[1]+30*86400
NSParameters_all[NSParameters_all$DT==(NSParameters_all$Date[1]+32*86400),]

############################### для 1 месяца ###################################
RMSE_1M <- 0
for (i in c(1:3125)){
  k <- 0
  if (nrow(NSParameters_all[NSParameters_all$DT==(NSParameters_all$Date[i]+30*86400),])==0){
    while (nrow(NSParameters_all[NSParameters_all$DT==(NSParameters_all$Date[i]+(30+k)*86400),])==0){
      k <- k+1
    }
  }
  RMSE_1M <- RMSE_1M+(NSParameters_all$ruo[NSParameters_all$DT==(NSParameters_all$Date[i]+(30+k)*86400)] -
                      NSParameters_all$`1M`[i])^2
}
RMSE_1M <- sqrt(1/3125*RMSE_1M)
RMSE_1M

############################### для 3 месяцев ###################################
RMSE_3M <- 0
for (i in c(1:3086)){
  k <- 0
  if (nrow(NSParameters_all[NSParameters_all$DT==(NSParameters_all$Date[i]+90*86400),])==0){
    while (nrow(NSParameters_all[NSParameters_all$DT==(NSParameters_all$Date[i]+(90+k)*86400),])==0){
      k <- k+1
    }
  }
  RMSE_3M <- RMSE_3M+(NSParameters_all$ruo[NSParameters_all$DT==(NSParameters_all$Date[i]+(90+k)*86400)] -
                        NSParameters_all$`3M`[i])^2
}
RMSE_3M <- sqrt(1/3086*RMSE_3M)
RMSE_3M

############################### для 6 месяцев ###################################
RMSE_6M <- 0
for (i in c(1:3021)){
  k <- 0
  if (nrow(NSParameters_all[NSParameters_all$DT==(NSParameters_all$Date[i]+120*86400),])==0){
    while (nrow(NSParameters_all[NSParameters_all$DT==(NSParameters_all$Date[i]+(120+k)*86400),])==0){
      k <- k+1
    }
  }
  RMSE_6M <- RMSE_6M+(NSParameters_all$ruo[NSParameters_all$DT==(NSParameters_all$Date[i]+(120+k)*86400)] -
                        NSParameters_all$`6M`[i])^2
}
RMSE_6M <- sqrt(1/3021*RMSE_6M)
RMSE_6M

############################### для 1 года ###################################
RMSE_1Y <- 0
for (i in c(1:2899)){
  k <- 0
  if (nrow(NSParameters_all[NSParameters_all$DT==(NSParameters_all$Date[i]+360*86400),])==0){
    while (nrow(NSParameters_all[NSParameters_all$DT==(NSParameters_all$Date[i]+(360+k)*86400),])==0){
      k <- k+1
    }
  }
  RMSE_1Y <- RMSE_1Y+(NSParameters_all$ruo[NSParameters_all$DT==(NSParameters_all$Date[i]+(360+k)*86400)] -
                        NSParameters_all$`1Y`[i])^2
  
}
RMSE_1Y <- sqrt(1/2899*RMSE_1Y)
RMSE_1Y

############### Для Свенссона #####################################
SVParameters_all<- rbind(SVBetas,SVBetas2, SVBetas3)
SVParameters_all <- as.data.frame(SVParameters_all)

SVParameters_all$Date <- NStable$Date
SVParameters_all <- SVParameters_all[,c(7,1:6)]
y_all_sv <- Srates(as.xts(as.ts(SVParameters_all[,c(2:7)])), maturity=c(1/4.3,2/4.3,1,2,3,6,12))
SVParameters_all <- cbind(SVParameters_all, y_all_sv)
SVParameters_all <- cbind(SVParameters_all, yield$ruo)
SVParameters_all <- cbind(SVParameters_all, yield$DT)


colnames(SVParameters_all) <- c('Date', 'beta_0','beta_1', 'beta_2','beta_3', 
                                'tau1','tau2', '1W','2W', '1M',
                                '2M','3M','6M','1Y','ruo','DT')

######################### 1 месяц ###############################
RMSE_1M_SV <- 0
for (i in c(1:3125)){
  k <- 0
  if (nrow(SVParameters_all[SVParameters_all$DT==(SVParameters_all$Date[i]+30*86400),])==0){
    while (nrow(SVParameters_all[SVParameters_all$DT==(SVParameters_all$Date[i]+(30+k)*86400),])==0){
      k <- k+1
    }
  }
  RMSE_1M_SV <- RMSE_1M_SV+(SVParameters_all$ruo[SVParameters_all$DT==(SVParameters_all$Date[i]+(30+k)*86400)] -
                        SVParameters_all$`1M`[i])^2
}
RMSE_1M_SV <- sqrt(1/3125*RMSE_1M_SV)
RMSE_1M_SV

######################### 3 месяца ###############################
RMSE_3M_SV <- 0
for (i in c(1:3086)){
  k <- 0
  if (nrow(SVParameters_all[SVParameters_all$DT==(SVParameters_all$Date[i]+90*86400),])==0){
    while (nrow(SVParameters_all[SVParameters_all$DT==(SVParameters_all$Date[i]+(90+k)*86400),])==0){
      k <- k+1
    }
  }
  RMSE_3M_SV <- RMSE_3M_SV+(SVParameters_all$ruo[SVParameters_all$DT==(SVParameters_all$Date[i]+(90+k)*86400)] -
                        SVParameters_all$`3M`[i])^2
}
RMSE_3M_SV <- sqrt(1/3086*RMSE_3M_SV)
RMSE_3M_SV

############################### для 6 месяцев ###################################
RMSE_6M_SV <- 0
for (i in c(1:3021)){
  k <- 0
  if (nrow(SVParameters_all[SVParameters_all$DT==(SVParameters_all$Date[i]+120*86400),])==0){
    while (nrow(SVParameters_all[SVParameters_all$DT==(SVParameters_all$Date[i]+(120+k)*86400),])==0){
      k <- k+1
    }
  }
  RMSE_6M_SV <- RMSE_6M_SV+(SVParameters_all$ruo[SVParameters_all$DT==(SVParameters_all$Date[i]+(120+k)*86400)] -
                        SVParameters_all$`6M`[i])^2
}
RMSE_6M_SV <- sqrt(1/3021*RMSE_6M_SV)
RMSE_6M_SV

############################### для 1 года ###################################
RMSE_1Y_SV <- 0
for (i in c(1:2899)){
  k <- 0
  if (nrow(SVParameters_all[SVParameters_all$DT==(SVParameters_all$Date[i]+360*86400),])==0){
    while (nrow(SVParameters_all[SVParameters_all$DT==(SVParameters_all$Date[i]+(360+k)*86400),])==0){
      k <- k+1
    }
  }
  RMSE_1Y_SV <- RMSE_1Y_SV+(SVParameters_all$ruo[SVParameters_all$DT==(SVParameters_all$Date[i]+(360+k)*86400)] -
                        SVParameters_all$`1Y`[i])^2
  
}
RMSE_1Y_SV <- sqrt(1/2899*RMSE_1Y_SV)
RMSE_1Y_SV
###################################################################
########################## Только беты ############################
par(mfrow = c(1,1), no.readonly = TRUE )

plot(yield$DT, yield$ruo, type="l", lty=1, lwd=0.5, ylab="Ставка",xlab="",
     main="RUONIA")

plot(NStable$Date[c(1:820)],NSBetas[,1], col="blue", lty=1, type="l", lwd=1.0, ylab="Значение коэффициента",
     xlab="", xlim=c(NStable$Date[1],NStable$Date[2757]), ylim=c(3,25))
lines(NStable$Date[c(821:2321)],NSBetas2[,1], col="blue", lty=1, type="l", lwd=1.0)
lines(NStable$Date[c(2322:2757)],NSBetas3[,1], col="blue", lty=1, type="l", lwd=1.0)
legend(x = "top",
       legend = expression(paste(beta,"0 ")), 
       lty = 1,
       col="blue")

plot(NStable$Date[c(1:820)],NSBetas[,2], col="blue", lty=1, type="l", lwd=1.0, ylab="Значение коэффициента",
     xlab="Время", xlim=c(NStable$Date[1],NStable$Date[2757]), ylim=c(-12,11), 
     main=expression(paste(beta,"1 ")))
lines(NStable$Date[c(821:2321)],NSBetas2[,2], col="blue", lty=1, type="l", lwd=1.0)
lines(NStable$Date[c(2322:2757)],NSBetas3[,2], col="blue", lty=1, type="l", lwd=1.0)
#legend(x = "bottom",
 #      legend = expression(paste(beta,"1 ")), 
  #     lty = 1,
   #    col="blue")

plot(NStable$Date[c(1:820)],NSBetas[,3], col="blue", lty=1, type="l", lwd=1.0, ylab="Значение коэффициента",
     xlab="Время", xlim=c(NStable$Date[1],NStable$Date[2757]), ylim=c(-10,30),
     main=expression(paste(beta,"2 ")))
lines(NStable$Date[c(821:2321)],NSBetas2[,3], col="blue", lty=1, type="l", lwd=1.0)
lines(NStable$Date[c(2322:2757)],NSBetas3[,3], col="blue", lty=1, type="l", lwd=1.0)
#legend(x = "top",
 #      legend = expression(paste(beta,"2 ")), 
  #     lty = 1,
   #    col="blue")

#график премии за срочность
plot(yield$Date[c(2325:2760)], yield$TP2Y[c(2325:2760)], type="l", ylab="Премия за срочность",
     xlab="Время")
legend(x = "bottom",
       legend = "2 года",  
       lty = 1,
       col="black")
############################# Приведем данные к месячным ##########
############################ Усредним доходность ##################
k <- 0
a <- 0
y_monthly <- NStable[1,]
for (i in c(2:7)){
  y_monthly[1,i] <- sum(NStable[c(1:20),i])/20
}
y_monthly[nrow(y_monthly)+1,] <- NA
y_monthly$Date[nrow(y_monthly)] <- NStable$Date[21]

#для тех наблюдений, где нет 1Y,2Y
for (i in c(21:1196)){
  k <- k+1
  if (month(y_monthly$Date[nrow(y_monthly)])!= month(NStable$Date[i+1])){
    a <- i-k+1
    y_monthly$`1W`[nrow(y_monthly)] <- mean(NStable$`1W`[c(a:i)])
    y_monthly$`2W`[nrow(y_monthly)] <- mean(NStable$`2W`[c(a:i)])
    y_monthly$`1M`[nrow(y_monthly)] <- mean(NStable$`1M`[c(a:i)])
    y_monthly$`2M`[nrow(y_monthly)] <- mean(NStable$`2M`[c(a:i)])
    y_monthly$`3M`[nrow(y_monthly)] <- mean(NStable$`3M`[c(a:i)])
    y_monthly$`6M`[nrow(y_monthly)] <- mean(NStable$`6M`[c(a:i)])
    k <- 0
    y_monthly$Date[nrow(y_monthly)] <- NStable$Date[a] 
    y_monthly[nrow(y_monthly)+1,] <- NA
    y_monthly$Date[nrow(y_monthly)] <- NStable$Date[i+1]
  }
    
}

#для тех наблюдений, где нет 2Y
k <- 0
for (i in c(1195:2697)){
  k <- k+1
  if (month(y_monthly$Date[nrow(y_monthly)])!= month(NStable$Date[i+1])){
    a <- i-k+1
    y_monthly$`1W`[nrow(y_monthly)] <- mean(NStable$`1W`[c(a:i)])
    y_monthly$`2W`[nrow(y_monthly)] <- mean(NStable$`2W`[c(a:i)])
    y_monthly$`1M`[nrow(y_monthly)] <- mean(NStable$`1M`[c(a:i)])
    y_monthly$`2M`[nrow(y_monthly)] <- mean(NStable$`2M`[c(a:i)])
    y_monthly$`3M`[nrow(y_monthly)] <- mean(NStable$`3M`[c(a:i)])
    y_monthly$`6M`[nrow(y_monthly)] <- mean(NStable$`6M`[c(a:i)])
    y_monthly$`1Y`[nrow(y_monthly)] <- mean(na.omit(NStable$`1Y`[c(a:i)]))
    k <- 0
    y_monthly$Date[nrow(y_monthly)] <- NStable$Date[a] 
    y_monthly[nrow(y_monthly)+1,] <- NA
    y_monthly$Date[nrow(y_monthly)] <- NStable$Date[i+1]
  }
  
}

#для тех наблюдений, где есть 1Y, 2Y
k <- 0
for (i in c(2689:3134)){
  k <- k+1
  if (month(y_monthly$Date[nrow(y_monthly)])!= month(NStable$Date[i+1])){
    a <- i-k+1
    y_monthly$`1W`[nrow(y_monthly)] <- mean(NStable$`1W`[c(a:i)])
    y_monthly$`2W`[nrow(y_monthly)] <- mean(NStable$`2W`[c(a:i)])
    y_monthly$`1M`[nrow(y_monthly)] <- mean(NStable$`1M`[c(a:i)])
    y_monthly$`2M`[nrow(y_monthly)] <- mean(NStable$`2M`[c(a:i)])
    y_monthly$`3M`[nrow(y_monthly)] <- mean(NStable$`3M`[c(a:i)])
    y_monthly$`6M`[nrow(y_monthly)] <- mean(NStable$`6M`[c(a:i)])
    y_monthly$`1Y`[nrow(y_monthly)] <- mean(na.omit(NStable$`1Y`[c(a:i)]))
    y_monthly$`2Y`[nrow(y_monthly)] <- mean(na.omit(NStable$`2Y`[c(a:i)]))
    k <- 0
    y_monthly$Date[nrow(y_monthly)] <- NStable$Date[a] 
    y_monthly[nrow(y_monthly)+1,] <- NA
    y_monthly$Date[nrow(y_monthly)] <- NStable$Date[i+1]
  }
  
}
#уберем одно наблюдение среднего 2Y для апреля 2022, так как в этом месяце
#мало наблюдений в этом месяце

y_monthly$`2Y`[132] <- NA
write.csv(y_monthly,                   
          "C:/Users/Lenovo/Desktop/диплом/data/yields_monthly.csv")
############################################################################
######################### сделаем данные бет NS по месяцам #################
############################################################################
maturity <- c(1/4.3,2/4.3,1,2,3,6)
NSMBetas <- Nelson.Siegel(rate= y_monthly[c(1:58),c(2:7)], maturity=maturity)

maturity <- c(1/4.3,2/4.3,1,2,3,6,12)
NSMBetas2 <-Nelson.Siegel(rate= y_monthly[c(59:132),c(2:8)], maturity=maturity) 

maturity <- c(1/4.3,2/4.3,1,2,3,6,12,24)
NSMBetas3 <-Nelson.Siegel(rate= y_monthly[c(133:152),c(2:9)], maturity=maturity) 

NSBetas_monthly <- rbind(NSMBetas, NSMBetas2, NSMBetas3)
NSBetas_monthly <- as.data.frame(NSBetas_monthly)
NSBetas_monthly$Date <- y_monthly$Date[c(1:152)] 
NSBetas_monthly <- NSBetas_monthly[,c(5,1:4)]

write.csv(NSBetas_monthly,                   
          "C:/Users/Lenovo/Desktop/диплом/data/NSBetas_monthly.csv")

############################################################################
##################### сделаем данные бет Svensson по месяцам ###############
############################################################################
maturity <- c(1/4.3,2/4.3,1,2,3,6)
SVMBetas <- Svensson(rate= y_monthly[c(1:58),c(2:7)], maturity=maturity)

maturity <- c(1/4.3,2/4.3,1,2,3,6,12)
SVMBetas2 <- Svensson(rate= y_monthly[c(59:132),c(2:8)], maturity=maturity) 

maturity <- c(1/4.3,2/4.3,1,2,3,6,12,24)
SVMBetas3 <- Svensson(rate= y_monthly[c(133:152),c(2:9)], maturity=maturity) 

SVBetas_monthly <- rbind(SVMBetas, SVMBetas2, SVMBetas3)
SVBetas_monthly <- as.data.frame(SVBetas_monthly)
SVBetas_monthly$Date <- y_monthly$Date[c(1:152)] 
SVBetas_monthly <- SVBetas_monthly[,c(7,1:6)]

write.csv(SVBetas_monthly,                   
          "C:/Users/Lenovo/Desktop/диплом/data/SVBetas_monthly.csv")




############################################################################
################################ макроданные ##################################
############################################################################
macro_adj <- read_xlsx("C:/Users/Lenovo/Desktop/диплом/data/macro_adj.xlsx")
adf.test(ts(macro_adj$gdp))
adf.test(ts(diff(macro_adj$oil)))
adf.test(ts(macro_adj$inf))
#описательные статистики будут в разделе VAR моделей
############################################################################
############################# прогнозные модели для ruonia c кросс валидацией #######
############################ randow walk rolling window with drift #############################
NSLPdata <- as.data.frame(NSLPdata)
RUONIA <- ts(macro_adj$ruo, start=c(2011, 1), end=c(2023,11), frequency = 12)
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))
rwer1m <- c()
rwer2m <- c()
rwer3m <- c()
rwer4m <- c()
rwer5m <- c()
rwer6m <- c()
rwer7m <- c()
rwer8m <- c()
rwer9m <- c()
rwer10m <- c()
rwer11m <- c()
rwer1y <- c()

rwer13m <- c()
rwer14m <- c()
rwer15m <- c()
rwer16m <- c()
rwer17m <- c()
rwer18m <- c()
rwer19m <- c()
rwer20m <- c()
rwer21m <- c()
rwer22m <- c()
rwer23m <- c()
rwer2y <- c()
for (i in (75:length(macro_adj$ruo))){
rwf <- rwf(RUONIA[c((i-35):i)], h=24, drift=TRUE)
rwf <- as.data.frame(rwf)
count <- count+1
if (i<=(length(macro_adj$ruo)-1)){
  rwer1m <- c(rwer1m,((rwf$`Point Forecast`[1])-RUONIA[i+1]))
}
if (i<=(length(macro_adj$ruo)-2)){
  rwer2m <- c(rwer2m,((rwf$`Point Forecast`[2])-RUONIA[i+2]))
}
if (i<=(length(macro_adj$ruo)-3)){
  rwer3m <- c(rwer3m,((rwf$`Point Forecast`[3])-RUONIA[i+3]))
}
if (i<=(length(macro_adj$ruo)-4)){
  rwer4m <- c(rwer4m,((rwf$`Point Forecast`[4])-RUONIA[i+4]))
}
if (i<=(length(macro_adj$ruo)-5)){
  rwer5m <- c(rwer5m,((rwf$`Point Forecast`[5])-RUONIA[i+5]))
}
if (i<=(length(macro_adj$ruo)-6)){
  rwer6m <- c(rwer6m,((rwf$`Point Forecast`[6])-RUONIA[i+6]))
}
if (i<=(length(macro_adj$ruo)-7)){
  rwer7m <- c(rwer7m,((rwf$`Point Forecast`[7])-RUONIA[i+7]))
}
if (i<=(length(macro_adj$ruo)-8)){
  rwer8m <- c(rwer8m,((rwf$`Point Forecast`[8])-RUONIA[i+8]))
}
if (i<=(length(macro_adj$ruo)-9)){
  rwer9m <- c(rwer9m,((rwf$`Point Forecast`[9])-RUONIA[i+9]))
}
if (i<=(length(macro_adj$ruo)-10)){
  rwer10m <- c(rwer10m,((rwf$`Point Forecast`[10])-RUONIA[i+10]))
}
if (i<=(length(macro_adj$ruo)-11)){
  rwer11m <- c(rwer11m,((rwf$`Point Forecast`[11])-RUONIA[i+11]))
}
if (i<=(length(macro_adj$ruo)-12)){
  rwer1y <- c(rwer1y,((rwf$`Point Forecast`[12])-RUONIA[i+12]))
}

if (i<=(length(macro_adj$ruo)-13)){
  rwer13m <- c(rwer13m,((rwf$`Point Forecast`[13])-RUONIA[i+13]))
}
if (i<=(length(macro_adj$ruo)-14)){
  rwer14m <- c(rwer14m,((rwf$`Point Forecast`[14])-RUONIA[i+14]))
}
if (i<=(length(macro_adj$ruo)-15)){
  rwer15m <- c(rwer15m,((rwf$`Point Forecast`[15])-RUONIA[i+15]))
}
if (i<=(length(macro_adj$ruo)-16)){
  rwer16m <- c(rwer16m,((rwf$`Point Forecast`[16])-RUONIA[i+16]))
}
if (i<=(length(macro_adj$ruo)-17)){
  rwer17m <- c(rwer17m,((rwf$`Point Forecast`[17])-RUONIA[i+17]))
}
if (i<=(length(macro_adj$ruo)-18)){
  rwer18m <- c(rwer18m,((rwf$`Point Forecast`[18])-RUONIA[i+18]))
}
if (i<=(length(macro_adj$ruo)-19)){
  rwer19m <- c(rwer19m,((rwf$`Point Forecast`[19])-RUONIA[i+19]))
}
if (i<=(length(macro_adj$ruo)-20)){
  rwer20m <- c(rwer20m,((rwf$`Point Forecast`[20])-RUONIA[i+20]))
}
if (i<=(length(macro_adj$ruo)-21)){
  rwer21m <- c(rwer21m,((rwf$`Point Forecast`[21])-RUONIA[i+21]))
}
if (i<=(length(macro_adj$ruo)-22)){
  rwer22m <- c(rwer22m,((rwf$`Point Forecast`[22])-RUONIA[i+22]))
}
if (i<=(length(macro_adj$ruo)-23)){
  rwer23m <- c(rwer23m,((rwf$`Point Forecast`[23])-RUONIA[i+23]))
}
if (i<=(length(macro_adj$ruo)-24)){
  rwer2y <- c(rwer2y,((rwf$`Point Forecast`[24])-RUONIA[i+24]))
}
a <- c(1:25)
a[1] <- RUONIA[i]
a[2:25] <- rwf$`Point Forecast`
#lines(seq.Date(as.Date(macro_adj$date[i]),as.Date(macro_adj$date[i])+365,30),a ,col='blue', type='l')
}

#legend(x = "topleft",
 #      legend = c("RUONIA", "Прогнозы RW"),  
  #     lty = c(1, 1),
   #    col=c("black","blue"),
    #   lwd = 1.0)
plot(as.Date(NSLPdata$Date[c(36:(151-1))]),rwer1m, type='l')
sqrt(sum(rwer1m^2/(length(rwer1m))))
sqrt(sum(rwer2m^2/(length(rwer2m))))
sqrt(sum(rwer3m^2/(length(rwer3m)))) 
sqrt(sum(rwer4m^2/(length(rwer4m))))
sqrt(sum(rwer5m^2/(length(rwer5m))))
sqrt(sum(rwer6m^2/(length(rwer6m))))
sqrt(sum(rwer7m^2/(length(rwer7m))))
sqrt(sum(rwer8m^2/(length(rwer8m))))
sqrt(sum(rwer9m^2/(length(rwer9m))))
sqrt(sum(rwer10m^2/(length(rwer10m))))
sqrt(sum(rwer11m^2/(length(rwer11m))))
sqrt(sum(rwer1y^2/(length(rwer1y))))

sqrt(sum(rwer13m^2/(length(rwer13m))))
sqrt(sum(rwer14m^2/(length(rwer14m))))
sqrt(sum(rwer15m^2/(length(rwer15m)))) 
sqrt(sum(rwer16m^2/(length(rwer16m))))
sqrt(sum(rwer17m^2/(length(rwer17m))))
sqrt(sum(rwer18m^2/(length(rwer18m))))
sqrt(sum(rwer19m^2/(length(rwer19m))))
sqrt(sum(rwer20m^2/(length(rwer20m))))
sqrt(sum(rwer21m^2/(length(rwer21m))))
sqrt(sum(rwer22m^2/(length(rwer22m))))
sqrt(sum(rwer23m^2/(length(rwer23m))))
sqrt(sum(rwer2y^2/(length(rwer2y))))

length(rwer1m)
############################ randow walk without drift #############################
RUONIA <- ts(macro_adj$ruo, start=c(2011, 1), end=c(2023,11), frequency = 12)
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))
rwer1m <- c()
rwer2m <- c()
rwer3m <- c()
rwer4m <- c()
rwer5m <- c()
rwer6m <- c()
rwer7m <- c()
rwer8m <- c()
rwer9m <- c()
rwer10m <- c()
rwer11m <- c()
rwer1y <- c()

rwer13m <- c()
rwer14m <- c()
rwer15m <- c()
rwer16m <- c()
rwer17m <- c()
rwer18m <- c()
rwer19m <- c()
rwer20m <- c()
rwer21m <- c()
rwer22m <- c()
rwer23m <- c()
rwer2y <- c()
for (i in (75:length(macro_adj$ruo))){
  rwf <- rwf(RUONIA[c(1:i)], h=24, drift=FALSE)
  rwf <- as.data.frame(rwf)
  if (i<=(length(macro_adj$ruo)-1)){
    rwer1m <- c(rwer1m,((rwf$`Point Forecast`[1])-RUONIA[i+1]))
  }
  if (i<=(length(macro_adj$ruo)-2)){
    rwer2m <- c(rwer2m,((rwf$`Point Forecast`[2])-RUONIA[i+2]))
  }
  if (i<=(length(macro_adj$ruo)-3)){
    rwer3m <- c(rwer3m,((rwf$`Point Forecast`[3])-RUONIA[i+3]))
  }
  if (i<=(length(macro_adj$ruo)-4)){
    rwer4m <- c(rwer4m,((rwf$`Point Forecast`[4])-RUONIA[i+4]))
  }
  if (i<=(length(macro_adj$ruo)-5)){
    rwer5m <- c(rwer5m,((rwf$`Point Forecast`[5])-RUONIA[i+5]))
  }
  if (i<=(length(macro_adj$ruo)-6)){
    rwer6m <- c(rwer6m,((rwf$`Point Forecast`[6])-RUONIA[i+6]))
  }
  if (i<=(length(macro_adj$ruo)-7)){
    rwer7m <- c(rwer7m,((rwf$`Point Forecast`[7])-RUONIA[i+7]))
  }
  if (i<=(length(macro_adj$ruo)-8)){
    rwer8m <- c(rwer8m,((rwf$`Point Forecast`[8])-RUONIA[i+8]))
  }
  if (i<=(length(macro_adj$ruo)-9)){
    rwer9m <- c(rwer9m,((rwf$`Point Forecast`[9])-RUONIA[i+9]))
  }
  if (i<=(length(macro_adj$ruo)-10)){
    rwer10m <- c(rwer10m,((rwf$`Point Forecast`[10])-RUONIA[i+10]))
  }
  if (i<=(length(macro_adj$ruo)-11)){
    rwer11m <- c(rwer11m,((rwf$`Point Forecast`[11])-RUONIA[i+11]))
  }
  if (i<=(length(macro_adj$ruo)-12)){
    rwer1y <- c(rwer1y,((rwf$`Point Forecast`[12])-RUONIA[i+12]))
  }
  
  if (i<=(length(macro_adj$ruo)-13)){
    rwer13m <- c(rwer13m,((rwf$`Point Forecast`[13])-RUONIA[i+13]))
  }
  if (i<=(length(macro_adj$ruo)-14)){
    rwer14m <- c(rwer14m,((rwf$`Point Forecast`[14])-RUONIA[i+14]))
  }
  if (i<=(length(macro_adj$ruo)-15)){
    rwer15m <- c(rwer15m,((rwf$`Point Forecast`[15])-RUONIA[i+15]))
  }
  if (i<=(length(macro_adj$ruo)-16)){
    rwer16m <- c(rwer16m,((rwf$`Point Forecast`[16])-RUONIA[i+16]))
  }
  if (i<=(length(macro_adj$ruo)-17)){
    rwer17m <- c(rwer17m,((rwf$`Point Forecast`[17])-RUONIA[i+17]))
  }
  if (i<=(length(macro_adj$ruo)-18)){
    rwer18m <- c(rwer18m,((rwf$`Point Forecast`[18])-RUONIA[i+18]))
  }
  if (i<=(length(macro_adj$ruo)-19)){
    rwer19m <- c(rwer19m,((rwf$`Point Forecast`[19])-RUONIA[i+19]))
  }
  if (i<=(length(macro_adj$ruo)-20)){
    rwer20m <- c(rwer20m,((rwf$`Point Forecast`[20])-RUONIA[i+20]))
  }
  if (i<=(length(macro_adj$ruo)-21)){
    rwer21m <- c(rwer21m,((rwf$`Point Forecast`[21])-RUONIA[i+21]))
  }
  if (i<=(length(macro_adj$ruo)-22)){
    rwer22m <- c(rwer22m,((rwf$`Point Forecast`[22])-RUONIA[i+22]))
  }
  if (i<=(length(macro_adj$ruo)-23)){
    rwer23m <- c(rwer23m,((rwf$`Point Forecast`[23])-RUONIA[i+23]))
  }
  if (i<=(length(macro_adj$ruo)-24)){
    rwer2y <- c(rwer2y,((rwf$`Point Forecast`[24])-RUONIA[i+24]))
  }
  a <- c(1:25)
  a[1] <- RUONIA[i]
  a[2:25] <- rwf$`Point Forecast`
  #lines(seq.Date(as.Date(macro_adj$date[i]),as.Date(macro_adj$date[i])+365,30),a ,col='blue', type='l')
}
#legend(x = "topleft",
#      legend = c("RUONIA", "Прогнозы RW"),  
#     lty = c(1, 1),
#    col=c("black","blue"),
#   lwd = 1.0)
sqrt(sum(rwer1m^2/(length(rwer1m))))
sqrt(sum(rwer2m^2/(length(rwer2m))))
sqrt(sum(rwer3m^2/(length(rwer3m)))) 
sqrt(sum(rwer4m^2/(length(rwer4m))))
sqrt(sum(rwer5m^2/(length(rwer5m))))
sqrt(sum(rwer6m^2/(length(rwer6m))))
sqrt(sum(rwer7m^2/(length(rwer7m))))
sqrt(sum(rwer8m^2/(length(rwer8m))))
sqrt(sum(rwer9m^2/(length(rwer9m))))
sqrt(sum(rwer10m^2/(length(rwer10m))))
sqrt(sum(rwer11m^2/(length(rwer11m))))
sqrt(sum(rwer1y^2/(length(rwer1y))))

sqrt(sum(rwer13m^2/(length(rwer13m))))
sqrt(sum(rwer14m^2/(length(rwer14m))))
sqrt(sum(rwer15m^2/(length(rwer15m)))) 
sqrt(sum(rwer16m^2/(length(rwer16m))))
sqrt(sum(rwer17m^2/(length(rwer17m))))
sqrt(sum(rwer18m^2/(length(rwer18m))))
sqrt(sum(rwer19m^2/(length(rwer19m))))
sqrt(sum(rwer20m^2/(length(rwer20m))))
sqrt(sum(rwer21m^2/(length(rwer21m))))
sqrt(sum(rwer22m^2/(length(rwer22m))))
sqrt(sum(rwer23m^2/(length(rwer23m))))
sqrt(sum(rwer2y^2/(length(rwer2y))))
length(rwer1m)
######################### ARIMA model ###########################
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))

armaer1m <- c()
armaer2m <- c()
armaer3m <- c()
armaer4m <- c()
armaer5m <- c()
armaer6m <- c()
armaer7m <- c()
armaer8m <- c()
armaer9m <- c()
armaer10m <- c()
armaer11m <- c()
armaer1y <- c()

armaer13m <- c()
armaer14m <- c()
armaer15m <- c()
armaer16m <- c()
armaer17m <- c()
armaer18m <- c()
armaer19m <- c()
armaer20m <- c()
armaer21m <- c()
armaer22m <- c()
armaer23m <- c()
armaer2y <- c()

for (i in 75:length(macro_adj$ruo)){
  mod <- auto.arima(RUONIA[c(1:i)])
  armaf <- forecast(mod, h=24)
  armaf <- as.data.frame(armaf)
  if (i<=(length(macro_adj$ruo)-1)){
    armaer1m <- c(armaer1m,((armaf$`Point Forecast`[1])-RUONIA[i+1]))
  }
  if (i<=(length(macro_adj$ruo)-2)){
    armaer2m <- c(armaer2m,((armaf$`Point Forecast`[2])-RUONIA[i+2]))
  }
  if (i<=(length(macro_adj$ruo)-3)){
    armaer3m <- c(armaer3m,((armaf$`Point Forecast`[3])-RUONIA[i+3]))
  }
  if (i<=(length(macro_adj$ruo)-4)){
    armaer4m <- c(armaer4m,((armaf$`Point Forecast`[4])-RUONIA[i+4]))
  }
  if (i<=(length(macro_adj$ruo)-5)){
    armaer5m <- c(armaer5m,((armaf$`Point Forecast`[5])-RUONIA[i+5]))
  }
  if (i<=(length(macro_adj$ruo)-6)){
    armaer6m <- c(armaer6m,((armaf$`Point Forecast`[6])-RUONIA[i+6]))
  }
  if (i<=(length(macro_adj$ruo)-7)){
    armaer7m <- c(armaer7m,((armaf$`Point Forecast`[7])-RUONIA[i+7]))
  }
  if (i<=(length(macro_adj$ruo)-8)){
    armaer8m <- c(armaer8m,((armaf$`Point Forecast`[8])-RUONIA[i+8]))
  }
  if (i<=(length(macro_adj$ruo)-9)){
    armaer9m <- c(armaer9m,((armaf$`Point Forecast`[9])-RUONIA[i+9]))
  }
  if (i<=(length(macro_adj$ruo)-10)){
    armaer10m <- c(armaer10m,((armaf$`Point Forecast`[10])-RUONIA[i+10]))
  }
  if (i<=(length(macro_adj$ruo)-11)){
    armaer11m <- c(armaer11m,((armaf$`Point Forecast`[11])-RUONIA[i+11]))
  }
  if (i<=(length(macro_adj$ruo)-12)){
    armaer1y <- c(armaer1y,((armaf$`Point Forecast`[12])-RUONIA[i+12]))
  }
  
  if (i<=(length(macro_adj$ruo)-13)){
    armaer13m <- c(armaer13m,((armaf$`Point Forecast`[13])-RUONIA[i+13]))
  }
  if (i<=(length(macro_adj$ruo)-14)){
    armaer14m <- c(armaer14m,((armaf$`Point Forecast`[14])-RUONIA[i+14]))
  }
  if (i<=(length(macro_adj$ruo)-15)){
    armaer15m <- c(armaer15m,((armaf$`Point Forecast`[15])-RUONIA[i+15]))
  }
  if (i<=(length(macro_adj$ruo)-16)){
    armaer16m <- c(armaer16m,((armaf$`Point Forecast`[16])-RUONIA[i+16]))
  }
  if (i<=(length(macro_adj$ruo)-17)){
    armaer17m <- c(armaer17m,((armaf$`Point Forecast`[17])-RUONIA[i+17]))
  }
  if (i<=(length(macro_adj$ruo)-18)){
    armaer18m <- c(armaer18m,((armaf$`Point Forecast`[18])-RUONIA[i+18]))
  }
  if (i<=(length(macro_adj$ruo)-19)){
    armaer19m <- c(armaer19m,((armaf$`Point Forecast`[19])-RUONIA[i+19]))
  }
  if (i<=(length(macro_adj$ruo)-20)){
    armaer20m <- c(armaer20m,((armaf$`Point Forecast`[20])-RUONIA[i+20]))
  }
  if (i<=(length(macro_adj$ruo)-21)){
    armaer21m <- c(armaer21m,((armaf$`Point Forecast`[21])-RUONIA[i+21]))
  }
  if (i<=(length(macro_adj$ruo)-22)){
    armaer22m <- c(armaer22m,((armaf$`Point Forecast`[22])-RUONIA[i+22]))
  }
  if (i<=(length(macro_adj$ruo)-23)){
    armaer23m <- c(armaer23m,((armaf$`Point Forecast`[23])-RUONIA[i+23]))
  }
  if (i<=(length(macro_adj$ruo)-24)){
    armaer2y <- c(armaer2y,((armaf$`Point Forecast`[24])-RUONIA[i+24]))
  }
  a <- c(1:13)
  a[1] <- RUONIA[i]
  a[2:13] <- armaf$`Point Forecast`[c(1:12)]
  #lines(seq.Date(as.Date(macro_adj$date[i]),as.Date(macro_adj$date[i])+365,30),a ,col='blue', type='l')
}

#legend(x = "topleft",
#      legend = c("RUONIA", "Прогнозы RW"),  
#     lty = c(1, 1),
#    col=c("black","blue"),
#   lwd = 1.0)
sqrt(sum(armaer1m^2/(length(armaer1m))))
sqrt(sum(armaer2m^2/(length(armaer2m))))
sqrt(sum(armaer3m^2/(length(armaer3m)))) 
sqrt(sum(armaer4m^2/(length(armaer4m))))
sqrt(sum(armaer5m^2/(length(armaer5m))))
sqrt(sum(armaer6m^2/(length(armaer6m))))
sqrt(sum(armaer7m^2/(length(armaer7m))))
sqrt(sum(armaer8m^2/(length(armaer8m))))
sqrt(sum(armaer9m^2/(length(armaer9m))))
sqrt(sum(armaer10m^2/(length(armaer10m))))
sqrt(sum(armaer11m^2/(length(armaer11m))))
sqrt(sum(armaer1y^2/(length(armaer1y))))

sqrt(sum(armaer13m^2/(length(armaer13m))))
sqrt(sum(armaer14m^2/(length(armaer14m))))
sqrt(sum(armaer15m^2/(length(armaer15m)))) 
sqrt(sum(armaer16m^2/(length(armaer16m))))
sqrt(sum(armaer17m^2/(length(armaer17m))))
sqrt(sum(armaer18m^2/(length(armaer18m))))
sqrt(sum(armaer19m^2/(length(armaer19m))))
sqrt(sum(armaer20m^2/(length(armaer20m))))
sqrt(sum(armaer21m^2/(length(armaer21m))))
sqrt(sum(armaer22m^2/(length(armaer22m))))
sqrt(sum(armaer23m^2/(length(armaer23m))))
sqrt(sum(armaer2y^2/(length(armaer2y))))

length(armaer1m)

######################### VAR-macro model ###########################
#сделаем ряды стационарными
adf.test(ts(macro_adj$gov))
macro_adj$dgov <- NA
#macro_adj$dgov[c(2:length(macro_adj$dgov))] <- 100*diff(log(macro_adj$gov))
for (i in c(2:length(macro_adj$gov))){
  macro_adj$dgov[i] <- 100*((macro_adj$gov[i]/macro_adj$gov[i-1]) - 1)
}
adf.test(na.omit(ts(macro_adj$dgov)))
#macro_adj$dgov <- macro_adj$dgov*100

adf.test(ts(log(macro_adj$debt)))
macro_adj$ddebt <- NA
#macro_adj$ddebt[c(2:length(macro_adj$ddebt))] <- 100*diff(log(macro_adj$debt))
for (i in c(2:length(macro_adj$debt))){
  macro_adj$ddebt[i] <- 100*((macro_adj$debt[i]/macro_adj$debt[i-1]) - 1)
}
adf.test(na.omit(ts(macro_adj$ddebt)))
#macro_adj$ddebt <- macro_adj$ddebt*100

adf.test(ts(macro_adj$gdp))
adf.test(ts(macro_adj$oil))
macro_adj$doil <- NA
#macro_adj$doil[c(2:length(macro_adj$oil))] <- 100*diff(log(macro_adj$oil))
for (i in c(2:length(macro_adj$oil))){
  macro_adj$doil[i] <- 100*((macro_adj$oil[i]/macro_adj$oil[i-1]) - 1)
}
adf.test(na.omit(ts(macro_adj$doil)))

adf.test(ts(macro_adj$inf))
adf.test(ts(macro_adj$usd))
macro_adj$dusd <- NA
#macro_adj$dusd[c(2:length(macro_adj$debt))] <- 100*diff(log(macro_adj$usd))
for (i in c(2:length(macro_adj$usd))){
  macro_adj$dusd[i] <- 100*((macro_adj$usd[i]/macro_adj$usd[i-1]) - 1)
}
class(macro_adj$gov)
adf.test(na.omit(ts(macro_adj$dusd)))
macro_adj <- as.data.frame(macro_adj)
############################### Описательные статистики ############################
#выведем описательные статистики для переменных
banks <- read_excel("C:/Users/Lenovo/Desktop/BEAR-toolbox-legacyCode-2/data.xlsx",
                   sheet='data')
banks <- as.data.frame(banks)

macr <- dplyr::select(macro_adj,
                      c(gdp,inf,ruo, dgov, ddebt, doil, dusd))
k <- nrow(banks)
#чтобы можно было соединить таблички
for (i in (1:(nrow(macr)-k))){
banks <-rbind(rep(NA,5),banks)
}
cbind(macr,banks)
stargazer(cbind(macr,banks[,c(-2,-4)]), type='html', out='macro.html')
################## Сделаем таблицу с тестами на стационарность #################  
tests <- matrix(nrow=2,ncol=ncol(cbind(macr,banks[,c(-1,-2,-4)])))
for (i in (1:ncol(cbind(macr,banks[,c(-1,-2,-4)])))){
  for (j in (1:2)){
    if (j==1){
      tests[j,i] <- as.numeric(adf.test(na.omit(ts(cbind(macr,banks[,c(-1,-2,-4)])[,i])))[4])
    }
    if (j==2){
      tests[j,i] <-  tests[j,i] <-as.numeric(length(na.omit(ts(cbind(macr,banks[,c(-1,-2,-4)])[,i]))))
    }
  }
}
tests <- as.data.frame(tests)
colnames(tests) <- colnames(cbind(macr,banks[,c(-1,-2,-4)]))
rownames(tests) <- c('p-значение теста Дики-Фулера','Число наблюдений')
write_xlsx(tests,'C:/Users/Lenovo/Desktop/диплом/data/tests.xlsx')
#оцениваем простую var модель
RUONIA <- ts(macro_adj$ruo,start=c(2011, 1), end=c(2023,11), frequency = 12 )
VARdata <- window(ts.union(ts(macro_adj$gdp[c(1:155)]), ts(macro_adj$inf[c(1:155)]),
                           ts(macro_adj$ruo[c(1:155)])))
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))
varer1m <- c()
varer2m <- c()
varer3m <- c()
varer4m <- c()
varer5m <- c()
varer6m <- c()
varer7m <- c()
varer8m <- c()
varer9m <- c()
varer10m <- c()
varer11m <- c()
varer1y <- c()

varer13m <- c()
varer14m <- c()
varer15m <- c()
varer16m <- c()
varer17m <- c()
varer18m <- c()
varer19m <- c()
varer20m <- c()
varer21m <- c()
varer22m <- c()
varer23m <- c()
varer2y <- c()
?VAR
for (i in 75:length(macro_adj$ruo)){
  var <- VARselect(VARdata[c(2:(i)),])
  vares <- VAR(VARdata[c(2:(i)),],
                p=var$selection[3])
  a <- VARpred(vares, h = 24)
  varf <- as.data.frame(a$pred[,3])
  if (i<=(length(macro_adj$ruo)-1)){
    varer1m <- c(varer1m,((varf[1,1])-RUONIA[i+1]))
  }
  if (i<=(length(macro_adj$ruo)-2)){
    varer2m <- c(varer2m,((varf[2,1])-RUONIA[i+2]))
  }
  if (i<=(length(macro_adj$ruo)-3)){
    varer3m <- c(varer3m,((varf[3,1])-RUONIA[i+3]))
  }
  if (i<=(length(macro_adj$ruo)-4)){
    varer4m <- c(varer4m,((varf[4,1])-RUONIA[i+4]))
  }
  if (i<=(length(macro_adj$ruo)-5)){
    varer5m <- c(varer5m,((varf[5,1])-RUONIA[i+5]))
  }
  if (i<=(length(macro_adj$ruo)-6)){
    varer6m <- c(varer6m,((varf[6,1])-RUONIA[i+6]))
  }
  if (i<=(length(macro_adj$ruo)-7)){
    varer7m <- c(varer7m,((varf[7,1])-RUONIA[i+7]))
  }
  if (i<=(length(macro_adj$ruo)-8)){
    varer8m <- c(varer8m,((varf[8,1])-RUONIA[i+8]))
  }
  if (i<=(length(macro_adj$ruo)-9)){
    varer9m <- c(varer9m,((varf[9,1])-RUONIA[i+9]))
  }
  if (i<=(length(macro_adj$ruo)-10)){
    varer10m <- c(varer10m,((varf[10,1])-RUONIA[i+10]))
  }
  if (i<=(length(macro_adj$ruo)-11)){
    varer11m <- c(varer11m,((varf[11,1])-RUONIA[i+11]))
  }
  if (i<=(length(macro_adj$ruo)-12)){
    varer1y <- c(varer1y,((varf[12,1])-RUONIA[i+12]))
  }
  
  if (i<=(length(macro_adj$ruo)-13)){
    varer13m <- c(varer13m,((varf[13,1])-RUONIA[i+13]))
  }
  if (i<=(length(macro_adj$ruo)-14)){
    varer14m <- c(varer14m,((varf[14,1])-RUONIA[i+14]))
  }
  if (i<=(length(macro_adj$ruo)-15)){
    varer15m <- c(varer15m,((varf[15,1])-RUONIA[i+15]))
  }
  if (i<=(length(macro_adj$ruo)-16)){
    varer16m <- c(varer16m,((varf[16,1])-RUONIA[i+16]))
  }
  if (i<=(length(macro_adj$ruo)-17)){
    varer17m <- c(varer17m,((varf[17,1])-RUONIA[i+17]))
  }
  if (i<=(length(macro_adj$ruo)-18)){
    varer18m <- c(varer18m,((varf[18,1])-RUONIA[i+18]))
  }
  if (i<=(length(macro_adj$ruo)-19)){
    varer19m <- c(varer19m,((varf[19,1])-RUONIA[i+19]))
  }
  if (i<=(length(macro_adj$ruo)-20)){
    varer20m <- c(varer20m,((varf[20,1])-RUONIA[i+20]))
  }
  if (i<=(length(macro_adj$ruo)-21)){
    varer21m <- c(varer21m,((varf[21,1])-RUONIA[i+21]))
  }
  if (i<=(length(macro_adj$ruo)-22)){
    varer22m <- c(varer22m,((varf[22,1])-RUONIA[i+22]))
  }
  if (i<=(length(macro_adj$ruo)-23)){
    varer23m <- c(varer23m,((varf[23,1])-RUONIA[i+23]))
  }
  if (i<=(length(macro_adj$ruo)-24)){
    varer2y <- c(varer2y,((varf[24,1])-RUONIA[i+24]))
  }
  a <- c(1:13)
  a[1] <- RUONIA[i]
  a[2:13] <- varf[c(1:12),1]
  #lines(seq.Date(as.Date(macro_adj$date[i]),as.Date(macro_adj$date[i])+365,30),a ,col='blue', type='l')
}

sqrt(sum(varer1m^2/(length(varer1m))))
sqrt(sum(varer2m^2/(length(varer2m))))
sqrt(sum(varer3m^2/(length(varer3m)))) 
sqrt(sum(varer4m^2/(length(varer4m))))
sqrt(sum(varer5m^2/(length(varer5m))))
sqrt(sum(varer6m^2/(length(varer6m))))
sqrt(sum(varer7m^2/(length(varer7m))))
sqrt(sum(varer8m^2/(length(varer8m))))
sqrt(sum(varer9m^2/(length(varer9m))))
sqrt(sum(varer10m^2/(length(varer10m))))
sqrt(sum(varer11m^2/(length(varer11m))))
sqrt(sum(varer1y^2/(length(varer1y))))

sqrt(sum(varer13m^2/(length(varer13m))))
sqrt(sum(varer14m^2/(length(varer14m))))
sqrt(sum(varer15m^2/(length(varer15m)))) 
sqrt(sum(varer16m^2/(length(varer16m))))
sqrt(sum(varer17m^2/(length(varer17m))))
sqrt(sum(varer18m^2/(length(varer18m))))
sqrt(sum(varer19m^2/(length(varer19m))))
sqrt(sum(varer20m^2/(length(varer20m))))
sqrt(sum(varer21m^2/(length(varer21m))))
sqrt(sum(varer22m^2/(length(varer22m))))
sqrt(sum(varer23m^2/(length(varer23m))))
sqrt(sum(varer2y^2/(length(varer2y))))

length(varer1m)
######################### VARX-macro model ###########################
RUONIA <- ts(macro_adj$ruo,start=c(2011, 1), end=c(2023,11), frequency = 12 )
VARdata <- window(ts.union(ts(macro_adj$gdp[c(1:155)]), ts(macro_adj$inf[c(1:155)]),
                           ts(macro_adj$ruo[c(1:155)])))#,ts(macro_adj$doil[c(1:155)]) ))
doil <- ts(macro_adj$doil,start=c(2011, 1), end=c(2023,11), frequency = 12 )
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))
varxer1m <- c()
varxer2m <- c()
varxer3m <- c()
varxer4m <- c()
varxer5m <- c()
varxer6m <- c()
varxer7m <- c()
varxer8m <- c()
varxer9m <- c()
varxer10m <- c()
varxer11m <- c()
varxer1y <- c()

varxer13m <- c()
varxer14m <- c()
varxer15m <- c()
varxer16m <- c()
varxer17m <- c()
varxer18m <- c()
varxer19m <- c()
varxer20m <- c()
varxer21m <- c()
varxer22m <- c()
varxer23m <- c()
varxer2y <- c()
for (i in 75:length(macro_adj$ruo)){
  var <- VARselect(VARdata[c(2:(i)),],exogen =ts(macro_adj$doil[c(2:i)]) )
  vares <- VARX(zt=VARdata[c(2:(i)),], xt=ts(macro_adj$doil[c(2:(i))]),
               p=var$selection[3])
  mod <- auto.arima(ts(macro_adj$doil[c(2:(i))]))
  oilf <- forecast(mod, h=24)
  oilf <- ts(oilf$mean)
  a <- as.data.frame(VARXpred(vares, hstep = 24, newxt=oilf))
  varf <- as.data.frame(a[,3])
  if (i<=(length(macro_adj$ruo)-1)){
    varxer1m <- c(varxer1m,((varf[1,1])-RUONIA[i+1]))
  }
  if (i<=(length(macro_adj$ruo)-2)){
    varxer2m <- c(varxer2m,((varf[2,1])-RUONIA[i+2]))
  }
  if (i<=(length(macro_adj$ruo)-3)){
    varxer3m <- c(varxer3m,((varf[3,1])-RUONIA[i+3]))
  }
  if (i<=(length(macro_adj$ruo)-4)){
    varxer4m <- c(varxer4m,((varf[4,1])-RUONIA[i+4]))
  }
  if (i<=(length(macro_adj$ruo)-5)){
    varxer5m <- c(varxer5m,((varf[5,1])-RUONIA[i+5]))
  }
  if (i<=(length(macro_adj$ruo)-6)){
    varxer6m <- c(varxer6m,((varf[6,1])-RUONIA[i+6]))
  }
  if (i<=(length(macro_adj$ruo)-7)){
    varxer7m <- c(varxer7m,((varf[7,1])-RUONIA[i+7]))
  }
  if (i<=(length(macro_adj$ruo)-8)){
    varxer8m <- c(varxer8m,((varf[8,1])-RUONIA[i+8]))
  }
  if (i<=(length(macro_adj$ruo)-9)){
    varxer9m <- c(varxer9m,((varf[9,1])-RUONIA[i+9]))
  }
  if (i<=(length(macro_adj$ruo)-10)){
    varxer10m <- c(varxer10m,((varf[10,1])-RUONIA[i+10]))
  }
  if (i<=(length(macro_adj$ruo)-11)){
    varxer11m <- c(varxer11m,((varf[11,1])-RUONIA[i+11]))
  }
  if (i<=(length(macro_adj$ruo)-12)){
    varxer1y <- c(varxer1y,((varf[12,1])-RUONIA[i+12]))
  }
  
  if (i<=(length(macro_adj$ruo)-13)){
    varxer13m <- c(varxer13m,((varf[13,1])-RUONIA[i+13]))
  }
  if (i<=(length(macro_adj$ruo)-14)){
    varxer14m <- c(varxer14m,((varf[14,1])-RUONIA[i+14]))
  }
  if (i<=(length(macro_adj$ruo)-15)){
    varxer15m <- c(varxer15m,((varf[15,1])-RUONIA[i+15]))
  }
  if (i<=(length(macro_adj$ruo)-16)){
    varxer16m <- c(varxer16m,((varf[16,1])-RUONIA[i+16]))
  }
  if (i<=(length(macro_adj$ruo)-17)){
    varxer17m <- c(varxer17m,((varf[17,1])-RUONIA[i+17]))
  }
  if (i<=(length(macro_adj$ruo)-18)){
    varxer18m <- c(varxer18m,((varf[18,1])-RUONIA[i+18]))
  }
  if (i<=(length(macro_adj$ruo)-19)){
    varxer19m <- c(varxer19m,((varf[19,1])-RUONIA[i+19]))
  }
  if (i<=(length(macro_adj$ruo)-20)){
    varxer20m <- c(varxer20m,((varf[20,1])-RUONIA[i+20]))
  }
  if (i<=(length(macro_adj$ruo)-21)){
    varxer21m <- c(varxer21m,((varf[21,1])-RUONIA[i+21]))
  }
  if (i<=(length(macro_adj$ruo)-22)){
    varxer22m <- c(varxer22m,((varf[22,1])-RUONIA[i+22]))
  }
  if (i<=(length(macro_adj$ruo)-23)){
    varxer23m <- c(varxer23m,((varf[23,1])-RUONIA[i+23]))
  }
  if (i<=(length(macro_adj$ruo)-24)){
    varxer2y <- c(varxer2y,((varf[24,1])-RUONIA[i+24]))
  }
  a <- c(1:13)
  a[1] <- RUONIA[i]
  a[2:13] <- varf[c(1:12),1]
  #lines(seq.Date(as.Date(macro_adj$date[i]),as.Date(macro_adj$date[i])+365,30),a ,col='blue', type='l')
}

sqrt(sum(varxer1m^2/(length(varxer1m))))
sqrt(sum(varxer2m^2/(length(varxer2m))))
sqrt(sum(varxer3m^2/(length(varxer3m)))) 
sqrt(sum(varxer4m^2/(length(varxer4m))))
sqrt(sum(varxer5m^2/(length(varxer5m))))
sqrt(sum(varxer6m^2/(length(varxer6m))))
sqrt(sum(varxer7m^2/(length(varxer7m))))
sqrt(sum(varxer8m^2/(length(varxer8m))))
sqrt(sum(varxer9m^2/(length(varxer9m))))
sqrt(sum(varxer10m^2/(length(varxer10m))))
sqrt(sum(varxer11m^2/(length(varxer11m))))
sqrt(sum(varxer1y^2/(length(varxer1y))))

sqrt(sum(varxer13m^2/(length(varxer13m))))
sqrt(sum(varxer14m^2/(length(varxer14m))))
sqrt(sum(varxer15m^2/(length(varxer15m)))) 
sqrt(sum(varxer16m^2/(length(varxer16m))))
sqrt(sum(varxer17m^2/(length(varxer17m))))
sqrt(sum(varxer18m^2/(length(varxer18m))))
sqrt(sum(varxer19m^2/(length(varxer19m))))
sqrt(sum(varxer20m^2/(length(varxer20m))))
sqrt(sum(varxer21m^2/(length(varxer21m))))
sqrt(sum(varxer22m^2/(length(varxer22m))))
sqrt(sum(varxer23m^2/(length(varxer23m))))
sqrt(sum(varxer2y^2/(length(varxer2y))))

length(varxer2y)

######################### VAR all variables model ###########################
RUONIA <- ts(macro_adj$ruo,start=c(2011, 1), end=c(2023,11), frequency = 12 )
VARalldata <- window(ts.union(ts(macro_adj$gdp[c(1:155)]), ts(macro_adj$inf[c(1:155)]),
                           ts(macro_adj$ruo[c(1:155)]), 
                           ts(macro_adj$dgov[c(1:155)]), ts(macro_adj$ddebt[c(1:155)]),
                           ts(macro_adj$dusd[c(1:155)])))

plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))

varaller1m <- c()
varaller2m <- c()
varaller3m <- c()
varaller4m <- c()
varaller5m <- c()
varaller6m <- c()
varaller7m <- c()
varaller8m <- c()
varaller9m <- c()
varaller10m <- c()
varaller11m <- c()
varaller1y <- c()

varaller13m <- c()
varaller14m <- c()
varaller15m <- c()
varaller16m <- c()
varaller17m <- c()
varaller18m <- c()
varaller19m <- c()
varaller20m <- c()
varaller21m <- c()
varaller22m <- c()
varaller23m <- c()
varaller2y <- c()

for (i in 75:length(macro_adj$ruo)){
  var <- VARselect(VARalldata[c(2:(i)),],exogen =ts(macro_adj$doil[c(2:i)]) )
  vares <- VARX(zt=VARdata[c(2:(i)),], xt=ts(macro_adj$doil[c(2:(i))]),
                p=var$selection[3])
  mod <- auto.arima(ts(macro_adj$doil[c(2:(i))]))
  oilf <- forecast(mod, h=24)
  oilf <- ts(oilf$mean)
  a <- as.data.frame(VARXpred(vares, hstep = 24, newxt=oilf))
  varf <- as.data.frame(a[,3])
  if (i<=(length(macro_adj$ruo)-1)){
    varaller1m <- c(varaller1m,((varf[1,1])-RUONIA[i+1]))
  }
  if (i<=(length(macro_adj$ruo)-2)){
    varaller2m <- c(varaller2m,((varf[2,1])-RUONIA[i+2]))
  }
  if (i<=(length(macro_adj$ruo)-3)){
    varaller3m <- c(varaller3m,((varf[3,1])-RUONIA[i+3]))
  }
  if (i<=(length(macro_adj$ruo)-4)){
    varaller4m <- c(varaller4m,((varf[4,1])-RUONIA[i+4]))
  }
  if (i<=(length(macro_adj$ruo)-5)){
    varaller5m <- c(varaller5m,((varf[5,1])-RUONIA[i+5]))
  }
  if (i<=(length(macro_adj$ruo)-6)){
    varaller6m <- c(varaller6m,((varf[6,1])-RUONIA[i+6]))
  }
  if (i<=(length(macro_adj$ruo)-7)){
    varaller7m <- c(varaller7m,((varf[7,1])-RUONIA[i+7]))
  }
  if (i<=(length(macro_adj$ruo)-8)){
    varaller8m <- c(varaller8m,((varf[8,1])-RUONIA[i+8]))
  }
  if (i<=(length(macro_adj$ruo)-9)){
    varaller9m <- c(varaller9m,((varf[9,1])-RUONIA[i+9]))
  }
  if (i<=(length(macro_adj$ruo)-10)){
    varaller10m <- c(varaller10m,((varf[10,1])-RUONIA[i+10]))
  }
  if (i<=(length(macro_adj$ruo)-11)){
    varaller11m <- c(varaller11m,((varf[11,1])-RUONIA[i+11]))
  }
  if (i<=(length(macro_adj$ruo)-12)){
    varaller1y <- c(varaller1y,((varf[12,1])-RUONIA[i+12]))
  }
  
  if (i<=(length(macro_adj$ruo)-13)){
    varaller13m <- c(varaller13m,((varf[13,1])-RUONIA[i+13]))
  }
  if (i<=(length(macro_adj$ruo)-14)){
    varaller14m <- c(varaller14m,((varf[14,1])-RUONIA[i+14]))
  }
  if (i<=(length(macro_adj$ruo)-15)){
    varaller15m <- c(varaller15m,((varf[15,1])-RUONIA[i+15]))
  }
  if (i<=(length(macro_adj$ruo)-16)){
    varaller16m <- c(varaller16m,((varf[16,1])-RUONIA[i+16]))
  }
  if (i<=(length(macro_adj$ruo)-17)){
    varaller17m <- c(varaller17m,((varf[17,1])-RUONIA[i+17]))
  }
  if (i<=(length(macro_adj$ruo)-18)){
    varaller18m <- c(varaller18m,((varf[18,1])-RUONIA[i+18]))
  }
  if (i<=(length(macro_adj$ruo)-19)){
    varaller19m <- c(varaller19m,((varf[19,1])-RUONIA[i+19]))
  }
  if (i<=(length(macro_adj$ruo)-20)){
    varaller20m <- c(varaller20m,((varf[20,1])-RUONIA[i+20]))
  }
  if (i<=(length(macro_adj$ruo)-21)){
    varaller21m <- c(varaller21m,((varf[21,1])-RUONIA[i+21]))
  }
  if (i<=(length(macro_adj$ruo)-22)){
    varaller22m <- c(varaller22m,((varf[22,1])-RUONIA[i+22]))
  }
  if (i<=(length(macro_adj$ruo)-23)){
    varaller23m <- c(varaller23m,((varf[23,1])-RUONIA[i+23]))
  }
  if (i<=(length(macro_adj$ruo)-24)){
    varaller2y <- c(varaller2y,((varf[24,1])-RUONIA[i+24]))
  }
  a <- c(1:13)
  a[1] <- RUONIA[i]
  a[2:13] <- varf[c(1:12),1]
  #lines(seq.Date(as.Date(macro_adj$date[i]),as.Date(macro_adj$date[i])+365,30),a ,col='blue', type='l')
}

sqrt(sum(varaller1m^2/(length(varaller1m))))
sqrt(sum(varaller2m^2/(length(varaller2m))))
sqrt(sum(varaller3m^2/(length(varaller3m)))) 
sqrt(sum(varaller4m^2/(length(varaller4m))))
sqrt(sum(varaller5m^2/(length(varaller5m))))
sqrt(sum(varaller6m^2/(length(varaller6m))))
sqrt(sum(varaller7m^2/(length(varaller7m))))
sqrt(sum(varaller8m^2/(length(varaller8m))))
sqrt(sum(varaller9m^2/(length(varaller9m))))
sqrt(sum(varaller10m^2/(length(varaller10m))))
sqrt(sum(varaller11m^2/(length(varaller11m))))
sqrt(sum(varaller1y^2/(length(varaller1y))))

sqrt(sum(varaller13m^2/(length(varaller13m))))
sqrt(sum(varaller14m^2/(length(varaller14m))))
sqrt(sum(varaller15m^2/(length(varaller15m)))) 
sqrt(sum(varaller16m^2/(length(varaller16m))))
sqrt(sum(varaller17m^2/(length(varaller17m))))
sqrt(sum(varaller18m^2/(length(varaller18m))))
sqrt(sum(varaller19m^2/(length(varaller19m))))
sqrt(sum(varaller20m^2/(length(varaller20m))))
sqrt(sum(varaller21m^2/(length(varaller21m))))
sqrt(sum(varaller22m^2/(length(varaller22m))))
sqrt(sum(varaller23m^2/(length(varaller23m))))
sqrt(sum(varaller2y^2/(length(varaller2y))))
length(varaller1m)
######################### VAR + PCA ###########################################
PCAdata <- as.data.frame(cbind(macro_adj$gdp[c(2:155)],macro_adj$inf[c(2:155)],
                               macro_adj$doil[c(2:155)], 
                     macro_adj$dgov[c(2:155)], macro_adj$ddebt[c(2:155)],
                     macro_adj$dusd[c(2:155)]))
a <- prcomp(PCAdata, scale=TRUE)
comp <- as.data.frame(a$x)
comp <- comp[,c(1:4)]
comp <- rbind(c(NA,NA,NA,NA), comp)
comp
VARPCAdata <- ts(comp)
a$rotation
b <- summary(a)
c <- as.data.frame(b$importance)
c[3,3]
res.pca <- prcomp(PCAdata, scale = TRUE)
res.PCA <- get_pca(res.pca)
res.PCA$contrib
res.pca
adf.test(na.omit(ts(comp$PC1)))
adf.test(na.omit(ts(comp$PC2)))
adf.test(na.omit(ts(comp$PC3)))
adf.test(na.omit(ts(comp$PC4)))
#стационарны
doil <- ts(macro_adj$doil,start=c(2011, 2), end=c(2023,11), frequency = 12 )
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))
varpcaer1m <- c()
varpcaer2m <- c()
varpcaer3m <- c()
varpcaer4m <- c()
varpcaer5m <- c()
varpcaer6m <- c()
varpcaer7m <- c()
varpcaer8m <- c()
varpcaer9m <- c()
varpcaer10m <- c()
varpcaer11m <- c()
varpcaer1y <- c()

varpcaer13m <- c()
varpcaer14m <- c()
varpcaer15m <- c()
varpcaer16m <- c()
varpcaer17m <- c()
varpcaer18m <- c()
varpcaer19m <- c()
varpcaer20m <- c()
varpcaer21m <- c()
varpcaer22m <- c()
varpcaer23m <- c()
varpcaer2y <- c()
forecastsVARPCA <- matrix(ncol = 24)
forecastsVARPCA[1,c(1:24)] <- NA
#наблюдение, с которого начинать тестирование модели
win <- 75
for (i in win:length(macro_adj$ruo)){
  a <- prcomp(PCAdata[c(1:(i-1)),], scale=TRUE)
  comp <- as.data.frame(a$x)
  b <- summary(a)
  c <- as.data.frame(b$importance)
  comp <- comp[,c(1:4)]
  comp <- rbind(c(NA,NA,NA,NA), comp)
  if (c[3,3]>=0.65){
    VARPCAdata <- cbind(ts(macro_adj$ruo[c(2:i)]),ts(comp[c(2:i),c(1:3)]))
  }
  if (c[3,3]<0.65){
    VARPCAdata <- cbind(ts(macro_adj$ruo[c(2:i)]),ts(comp[(2:i),c(1:4)]))
  }
  var <- VARselect(VARPCAdata)
  vares <- VARs(VARPCAdata,
               lags=(var$selection[3]))
  a <- VARpred(vares, h = 24)
  varf <- as.data.frame(a$pred[,1])
  forecastsVARPCA[(i+1-win),] <- t(varf)
  forecastsVARPCA <- rbind(forecastsVARPCA, rep(NA,24))
  if (i<=(length(macro_adj$ruo)-1)){
    varpcaer1m <- c(varpcaer1m,((varf[1,1])-RUONIA[i+1]))
  }
  if (i<=(length(macro_adj$ruo)-2)){
    varpcaer2m <- c(varpcaer2m,((varf[2,1])-RUONIA[i+2]))
  }
  if (i<=(length(macro_adj$ruo)-3)){
    varpcaer3m <- c(varpcaer3m,((varf[3,1])-RUONIA[i+3]))
  }
  if (i<=(length(macro_adj$ruo)-4)){
    varpcaer4m <- c(varpcaer4m,((varf[4,1])-RUONIA[i+4]))
  }
  if (i<=(length(macro_adj$ruo)-5)){
    varpcaer5m <- c(varpcaer5m,((varf[5,1])-RUONIA[i+5]))
  }
  if (i<=(length(macro_adj$ruo)-6)){
    varpcaer6m <- c(varpcaer6m,((varf[6,1])-RUONIA[i+6]))
  }
  if (i<=(length(macro_adj$ruo)-7)){
    varpcaer7m <- c(varpcaer7m,((varf[7,1])-RUONIA[i+7]))
  }
  if (i<=(length(macro_adj$ruo)-8)){
    varpcaer8m <- c(varpcaer8m,((varf[8,1])-RUONIA[i+8]))
  }
  if (i<=(length(macro_adj$ruo)-9)){
    varpcaer9m <- c(varpcaer9m,((varf[9,1])-RUONIA[i+9]))
  }
  if (i<=(length(macro_adj$ruo)-10)){
    varpcaer10m <- c(varpcaer10m,((varf[10,1])-RUONIA[i+10]))
  }
  if (i<=(length(macro_adj$ruo)-11)){
    varpcaer11m <- c(varpcaer11m,((varf[11,1])-RUONIA[i+11]))
  }
  if (i<=(length(macro_adj$ruo)-12)){
    varpcaer1y <- c(varpcaer1y,((varf[12,1])-RUONIA[i+12]))
  }
  
  if (i<=(length(macro_adj$ruo)-13)){
    varpcaer13m <- c(varpcaer13m,((varf[13,1])-RUONIA[i+13]))
  }
  if (i<=(length(macro_adj$ruo)-14)){
    varpcaer14m <- c(varpcaer14m,((varf[14,1])-RUONIA[i+14]))
  }
  if (i<=(length(macro_adj$ruo)-15)){
    varpcaer15m <- c(varpcaer15m,((varf[15,1])-RUONIA[i+15]))
  }
  if (i<=(length(macro_adj$ruo)-16)){
    varpcaer16m <- c(varpcaer16m,((varf[16,1])-RUONIA[i+16]))
  }
  if (i<=(length(macro_adj$ruo)-17)){
    varpcaer17m <- c(varpcaer17m,((varf[17,1])-RUONIA[i+17]))
  }
  if (i<=(length(macro_adj$ruo)-18)){
    varpcaer18m <- c(varpcaer18m,((varf[18,1])-RUONIA[i+18]))
  }
  if (i<=(length(macro_adj$ruo)-19)){
    varpcaer19m <- c(varpcaer19m,((varf[19,1])-RUONIA[i+19]))
  }
  if (i<=(length(macro_adj$ruo)-20)){
    varpcaer20m <- c(varpcaer20m,((varf[20,1])-RUONIA[i+20]))
  }
  if (i<=(length(macro_adj$ruo)-21)){
    varpcaer21m <- c(varpcaer21m,((varf[21,1])-RUONIA[i+21]))
  }
  if (i<=(length(macro_adj$ruo)-22)){
    varpcaer22m <- c(varpcaer22m,((varf[22,1])-RUONIA[i+22]))
  }
  if (i<=(length(macro_adj$ruo)-23)){
    varpcaer23m <- c(varpcaer23m,((varf[23,1])-RUONIA[i+23]))
  }
  if (i<=(length(macro_adj$ruo)-24)){
    varpcaer2y <- c(varpcaer2y,((varf[24,1])-RUONIA[i+24]))
  }
  
  a <- c(1:13)
  a[1] <- RUONIA[i]
  a[2:13] <- varf[c(1:12),1]
  #lines(seq.Date(as.Date(macro_adj$date[i]),as.Date(macro_adj$date[i])+365,30),a ,col='blue', type='l')
}

sqrt(sum(varpcaer1m^2/(length(varpcaer1m))))
sqrt(sum(varpcaer2m^2/(length(varpcaer2m))))
sqrt(sum(varpcaer3m^2/(length(varpcaer3m))))
sqrt(sum(varpcaer4m^2/(length(varpcaer4m))))
sqrt(sum(varpcaer5m^2/(length(varpcaer5m))))
sqrt(sum(varpcaer6m^2/(length(varpcaer6m))))
sqrt(sum(varpcaer7m^2/(length(varpcaer7m))))
sqrt(sum(varpcaer8m^2/(length(varpcaer8m))))
sqrt(sum(varpcaer9m^2/(length(varpcaer9m))))
sqrt(sum(varpcaer10m^2/(length(varpcaer10m))))
sqrt(sum(varpcaer11m^2/(length(varpcaer11m))))
sqrt(sum(varpcaer1y^2/(length(varpcaer1y))))

sqrt(sum(varpcaer13m^2/(length(varpcaer13m))))
sqrt(sum(varpcaer14m^2/(length(varpcaer14m))))
sqrt(sum(varpcaer15m^2/(length(varpcaer15m)))) 
sqrt(sum(varpcaer16m^2/(length(varpcaer16m))))
sqrt(sum(varpcaer17m^2/(length(varpcaer17m))))
sqrt(sum(varpcaer18m^2/(length(varpcaer18m))))
sqrt(sum(varpcaer19m^2/(length(varpcaer19m))))
sqrt(sum(varpcaer20m^2/(length(varpcaer20m))))
sqrt(sum(varpcaer21m^2/(length(varpcaer21m))))
sqrt(sum(varpcaer22m^2/(length(varpcaer22m))))
sqrt(sum(varpcaer23m^2/(length(varpcaer23m))))
sqrt(sum(varpcaer2y^2/(length(varpcaer2y))))

length(varpcaer2y)

length(macro_adj$ruo)-win
varpcaerrors <- matrix(nrow = length(varpcaer1m), ncol=25)
varpcaerrors[,1] <- macro_adj$date[c((win+1):nrow(macro_adj))]
varpcaerrors[,2] <- varpcaer1m
varpcaerrors[(2:(length(macro_adj$ruo)-win)),3] <- varpcaer2m
varpcaerrors[(3:(length(macro_adj$ruo)-win)),4] <- varpcaer3m
varpcaerrors[(4:(length(macro_adj$ruo)-win)),5] <- varpcaer4m
varpcaerrors[(5:(length(macro_adj$ruo)-win)),6] <- varpcaer5m
varpcaerrors[(6:(length(macro_adj$ruo)-win)),7] <- varpcaer6m
varpcaerrors[(7:(length(macro_adj$ruo)-win)),8] <- varpcaer7m
varpcaerrors[(8:(length(macro_adj$ruo)-win)),9] <- varpcaer8m
varpcaerrors[(9:(length(macro_adj$ruo)-win)),10] <- varpcaer9m
varpcaerrors[(10:(length(macro_adj$ruo)-win)),11] <- varpcaer10m
varpcaerrors[(11:(length(macro_adj$ruo)-win)),12] <- varpcaer11m
varpcaerrors[(12:(length(macro_adj$ruo)-win)),13] <- varpcaer1y

varpcaerrors[(13:(length(macro_adj$ruo)-win)),14] <- varpcaer13m
varpcaerrors[(14:(length(macro_adj$ruo)-win)),15] <- varpcaer14m
varpcaerrors[(15:(length(macro_adj$ruo)-win)),16] <- varpcaer15m
varpcaerrors[(16:(length(macro_adj$ruo)-win)),17] <- varpcaer16m
varpcaerrors[(17:(length(macro_adj$ruo)-win)),18] <- varpcaer17m
varpcaerrors[(18:(length(macro_adj$ruo)-win)),19] <- varpcaer18m
varpcaerrors[(19:(length(macro_adj$ruo)-win)),20] <- varpcaer19m
varpcaerrors[(20:(length(macro_adj$ruo)-win)),21] <- varpcaer20m
varpcaerrors[(21:(length(macro_adj$ruo)-win)),22] <- varpcaer21m
varpcaerrors[(22:(length(macro_adj$ruo)-win)),23] <- varpcaer22m
varpcaerrors[(23:(length(macro_adj$ruo)-win)),24] <- varpcaer23m
varpcaerrors[(24:(length(macro_adj$ruo)-win)),25] <- varpcaer2y

varpcaerrors <- as.data.frame(varpcaerrors)
colnames(varpcaerrors) <- c('Dates','1M','2M','3M','4M','5M','6M',
                            '7M','8M','9M','10M','11M','1Y',
                            '13M','14M','15M','16M','17M','18M',
                            '19M','20M','21M','22M','23M','2Y')
write.csv(varpcaerrors,                   
          "C:/Users/Lenovo/Desktop/диплом/data/varpcaerrors.csv")
#сохраним данные об ошибках прогнозов 

forecastsVARPCA
#уберем последние NA
v <- nrow(forecastsVARPCA)
forecastsVARPCA <- forecastsVARPCA[c(1:(v-1)),]
forecastsVARPCA <- as.data.frame(forecastsVARPCA)
colnames(forecastsVARPCA) <- c('1M','2M','3M','4M','5M','6M',
                           '7M','8M','9M','10M','11M','1Y',
                           '13M','14M','15M','16M','17M','18M',
                           '19M','20M','21M','22M','23M','2Y')
forecastsVARPCA$Date <- macro_adj$date[c(win:nrow(macro_adj))]
forecastsVARPCA <- forecastsVARPCA[,c(25,1:24)]
#сохраним прогнозы от VAR PCA
write.csv(forecastsVARPCA,                   
          "C:/Users/Lenovo/Desktop/диплом/data/forecastsVARPCA.csv")

######################### BVAR-macro model ###########################
VARdata <- window(ts.union(ts(macro_adj$gdp[c(1:155)]), ts(macro_adj$inf[c(1:155)]),
                           ts(macro_adj$ruo[c(1:155)])))
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))

bvarer1m <- c()
bvarer3m <- c()
bvarer6m <- c()
bvarer1y <- c()
for (i in 75:length(macro_adj$ruo)){
  var <- VARselect(VARdata[c(2:(i)),])
  vares <- bvar(VARdata[c(2:(i)),],
             lags = 6, n_draw = 5000L, 
               n_burn = 1000L, nthin=2L, verbose = FALSE)
  a <- predict(vares, h = 12,conf_bands=c(0.05, 0.1))
  a <- as.data.frame(a$fcast)
  varf <- c()
  for (j in (25:36)){
    varf <- cbind(varf,mean(a[,j]))
  } 
  if (i<=(length(macro_adj$ruo)-1)){
    bvarer1m <- c(bvarer1m,((varf[1])-RUONIA[i+1]))
  }
  if (i<=(length(macro_adj$ruo)-3)){
    bvarer3m <- c(bvarer3m,((varf[3])-RUONIA[i+3]))
  }
  if (i<=(length(macro_adj$ruo)-6)){
    bvarer6m <- c(bvarer6m,((varf[6])-RUONIA[i+6]))
  }
  if (i<=(length(macro_adj$ruo)-12)){
    bvarer1y <- c(bvarer1y,((varf[12])-RUONIA[i+12]))
  }
  a <- c(1:13)
  a[1] <- RUONIA[i]
  a[2:13] <- varf
  lines(seq.Date(as.Date(macro_adj$date[i]),as.Date(macro_adj$date[i])+365,30),a ,col='blue', type='l')
}

sqrt(sum(bvarer1m^2/(length(bvarer1m))))
sqrt(sum(bvarer3m^2/(length(bvarer3m))))                   
sqrt(sum(bvarer6m^2/(length(bvarer6m))))
sqrt(sum(bvarer1y^2/(length(bvarer1y))))
length(bvarer1m)





######################### Графички для обзора ##################################
maturity=seq(0.1,8,0.05)
lambda <- 1
b0 <- 7.5
b1 <- 1
b2 <- 0.5
coef <- as.xts(as.ts(t(c(b0,b1,b2,lambda))))
a <- NSrates(coef,maturity)

# Нельсон-Сигель
ggplot()+theme_classic()+geom_line(aes(x=c(0,maturity),y=c((b1+b0),a)))+
  labs(x='Месяцы',y='Доходность к погашению, п.п.')+
  scale_y_continuous(expand = c(0, 0.1))+
  scale_x_continuous(expand = c(0, 0),breaks = seq(0, 8, 1))+
  geom_line(aes(x=c(0,maturity),y=c((0+8.5),NSrates(as.xts(as.ts(t(c(8.5,0,0,lambda)))),maturity))),color='red')+
  geom_line(aes(x=c(0,maturity),y=c((-1.5+10),NSrates(as.xts(as.ts(t(c(10,-1.5,-5,lambda)))),maturity))), lty=2)+
  geom_line(aes(x=c(0,maturity),y=c((1.5+7),NSrates(as.xts(as.ts(t(c(7,1.5,5,lambda)))),maturity))),color='blue')+
  geom_line(aes(x=c(0,maturity),y=c((-1+9.5),NSrates(as.xts(as.ts(t(c(9.5,-1,-0.5,lambda)))),maturity))))


maturity=seq(0.1,30,0.05)
#Свенссон
ggplot()+theme_classic()+labs(x='Месяцы',y='Доходность к погашению, п.п.')+
  scale_y_continuous(expand = c(0, 0.1))+
  scale_x_continuous(expand = c(0, 0),breaks = seq(0, 30, 5))+
  geom_line(aes(x=c(0,maturity),y=c((8+2),Srates(as.xts(as.ts(t(c(8,2,10,-9,2,5)))),maturity))))

######################### График-рафик для подгона моделей #####################
yields_monthly <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/yields_monthly.csv")
yields_monthly <- yields_monthly[,-1]

maturity=seq(0.1,32,0.05)
yields_monthly[149,]
times <- c(7/30,14/30,1,2,3,6,12,24)
a <- 145
yields_monthly[a,]
plot(times,yields_monthly[a,c(2:9)],xlab='Месяцы', ylab='RUONIA',
  ylim=c(7.4,9),xlim=c(0,30),type='p',pch=4 ,col='blue', main='Кривые доходности на май 2022')
lines(maturity, NSrates(as.xts(as.ts(Nelson.Siegel(yields_monthly[a,c(2:9)],times))),
                        maturity), type='l')

#### форвардная кривая
yns <- NSrates(as.xts(as.ts(Nelson.Siegel(yields_monthly[a,c(2:9)],times))),
                maturity)
yfor <- as.data.frame(yns)
yns <- as.data.frame(yns)

for (i in(1:nrow(yfor))){
  for (j in (2:ncol(yfor))){
    a <- (1+yns[i,j]/100)^((j-1)/365)
    b <- (1+yns[i,(j-1)]/100)^((j-2)/365)
    yfor[i,j] <- 100*((a/b)^(365)-1)
  }
}
lines(maturity, yfor, type='l',col='red')

lines(maturity, Srates(as.xts(as.ts(Svensson(yields_monthly[a,c(2:9)],times))),
                        maturity), type='l',col='blue')
legend(x = "bottom",
       legend = c("Фактические доходности", "Нельсон-Сигель", "Свенссон"),  
       col=c("blue","black","red"),
       lty=c(0,1,1),
       pch = c(4, NA, NA)
       )
yields_monthly[a,1]



######################### VAR + PCA на дельтах #################################
RUONIA <- ts(macro_adj$ruo,start=c(2011, 1), end=c(2023,11), frequency = 12 )
PCAdata <- as.data.frame(cbind(macro_adj$gdp[c(2:155)],macro_adj$inf[c(2:155)],
                               macro_adj$doil[c(2:155)], 
                               macro_adj$dgov[c(2:155)], macro_adj$ddebt[c(2:155)],
                               macro_adj$dusd[c(2:155)]))
macro_adj$druo<- NA
macro_adj$druo[c(2:155)] <- diff(macro_adj$ruo)
adf.test(na.omit(ts(macro_adj$druo)))

a <- prcomp(PCAdata, scale=TRUE)
comp <- as.data.frame(a$x)
comp <- comp[,c(1:4)]
comp <- rbind(c(NA,NA,NA,NA), comp)
comp
VARPCAdata <- ts(comp)
a$rotation
b <- summary(a)
c <- as.data.frame(b$importance)
c[3,3]
res.pca <- prcomp(PCAdata, scale = TRUE)
res.PCA <- get_pca(res.pca)
res.PCA$contrib
res.pca
adf.test(na.omit(ts(comp$PC1)))
adf.test(na.omit(ts(comp$PC2)))
adf.test(na.omit(ts(comp$PC3)))
adf.test(na.omit(ts(comp$PC4)))
#стационарны
doil <- ts(macro_adj$doil,start=c(2011, 2), end=c(2023,11), frequency = 12 )
plot(as.Date(macro_adj$date),macro_adj$ruo, xlab="Время", ylab="RUONIA", type='l', lwd=2.0, 
     xlim=c(as.Date(macro_adj$date[1]),as.Date(macro_adj$date[nrow(macro_adj)])+360))
varpcadeler1m <- c()
varpcadeler2m <- c()
varpcadeler3m <- c()
varpcadeler4m <- c()
varpcadeler5m <- c()
varpcadeler6m <- c()
varpcadeler7m <- c()
varpcadeler8m <- c()
varpcadeler9m <- c()
varpcadeler10m <- c()
varpcadeler11m <- c()
varpcadeler1y <- c()

varpcadeler13m <- c()
varpcadeler14m <- c()
varpcadeler15m <- c()
varpcadeler16m <- c()
varpcadeler17m <- c()
varpcadeler18m <- c()
varpcadeler19m <- c()
varpcadeler20m <- c()
varpcadeler21m <- c()
varpcadeler22m <- c()
varpcadeler23m <- c()
varpcadeler2y <- c()
forecastsVARPCAdel <- matrix(ncol = 24)
forecastsVARPCAdel[1,c(1:24)] <- NA
#наблюдение, с которого начинать тестирование модели
win <- 75
for (i in win:length(macro_adj$ruo)){
  a <- prcomp(PCAdata[c(1:(i-1)),], scale=TRUE)
  comp <- as.data.frame(a$x)
  b <- summary(a)
  c <- as.data.frame(b$importance)
  comp <- comp[,c(1:4)]
  comp <- rbind(c(NA,NA,NA,NA), comp)
  if (c[3,3]>=0.65){
    VARPCAdata <- cbind(ts(macro_adj$druo[c(2:i)]),ts(comp[c(2:i),c(1:3)]))
  }
  if (c[3,3]<0.65){
    VARPCAdata <- cbind(ts(macro_adj$druo[c(2:i)]),ts(comp[(2:i),c(1:4)]))
  }
  var <- VARselect(VARPCAdata)
  vares <- VARs(VARPCAdata,lags=var$selection[3])
  a <- VARpred(vares, h = 24)
  varf <- as.data.frame(a$pred[,1])
  forecastsVARPCAdel[(i+1-win),] <- t(varf)
  forecastsVARPCAdel <- rbind(forecastsVARPCAdel, rep(NA,24))
  if (i<=(length(macro_adj$ruo)-1)){
    varpcadeler1m <- c(varpcadeler1m,((RUONIA[i]+varf[1,1])-RUONIA[i+1]))
  }
  if (i<=(length(macro_adj$ruo)-2)){
    varpcadeler2m <- c(varpcadeler2m,((RUONIA[i]+sum(varf[c(1:2),1]))-RUONIA[i+2]))
  }
  if (i<=(length(macro_adj$ruo)-3)){
    varpcadeler3m <- c(varpcadeler3m,(RUONIA[i]+sum(varf[c(1:3),1])-RUONIA[i+3]))
  }
  if (i<=(length(macro_adj$ruo)-4)){
    varpcadeler4m <- c(varpcadeler4m,(RUONIA[i]+sum(varf[c(1:4),1])-RUONIA[i+4]))
  }
  if (i<=(length(macro_adj$ruo)-5)){
    varpcadeler5m <- c(varpcadeler5m,(RUONIA[i]+sum(varf[c(1:5),1])-RUONIA[i+5]))
  }
  if (i<=(length(macro_adj$ruo)-6)){
    varpcadeler6m <- c(varpcadeler6m,(RUONIA[i]+sum(varf[c(1:6),1])-RUONIA[i+6]))
  }
  if (i<=(length(macro_adj$ruo)-7)){
    varpcadeler7m <- c(varpcadeler7m,(RUONIA[i]+sum(varf[c(1:7),1])-RUONIA[i+7]))
  }
  if (i<=(length(macro_adj$ruo)-8)){
    varpcadeler8m <- c(varpcadeler8m,(RUONIA[i]+sum(varf[c(1:8),1])-RUONIA[i+8]))
  }
  if (i<=(length(macro_adj$ruo)-9)){
    varpcadeler9m <- c(varpcadeler9m,(RUONIA[i]+sum(varf[c(1:9),1])-RUONIA[i+9]))
  }
  if (i<=(length(macro_adj$ruo)-10)){
    varpcadeler10m <- c(varpcadeler10m,(RUONIA[i]+sum(varf[c(1:10),1])-RUONIA[i+10]))
  }
  if (i<=(length(macro_adj$ruo)-11)){
    varpcadeler11m <- c(varpcadeler11m,(RUONIA[i]+sum(varf[c(1:11),1])-RUONIA[i+11]))
  }
  if (i<=(length(macro_adj$ruo)-12)){
    varpcadeler1y <- c(varpcadeler1y,(RUONIA[i]+sum(varf[c(1:12),1])-RUONIA[i+12]))
  }
  
  if (i<=(length(macro_adj$ruo)-13)){
    varpcadeler13m <- c(varpcadeler13m,(RUONIA[i]+sum(varf[c(1:13),1])-RUONIA[i+13]))
  }
  if (i<=(length(macro_adj$ruo)-14)){
    varpcadeler14m <- c(varpcadeler14m,(RUONIA[i]+sum(varf[c(1:14),1])-RUONIA[i+14]))
  }
  if (i<=(length(macro_adj$ruo)-15)){
    varpcadeler15m <- c(varpcadeler15m,(RUONIA[i]+sum(varf[c(1:15),1])-RUONIA[i+15]))
  }
  if (i<=(length(macro_adj$ruo)-16)){
    varpcadeler16m <- c(varpcadeler16m,(RUONIA[i]+sum(varf[c(1:16),1])-RUONIA[i+16]))
  }
  if (i<=(length(macro_adj$ruo)-17)){
    varpcadeler17m <- c(varpcadeler17m,(RUONIA[i]+sum(varf[c(1:17),1])-RUONIA[i+17]))
  }
  if (i<=(length(macro_adj$ruo)-18)){
    varpcadeler18m <- c(varpcadeler18m,(RUONIA[i]+sum(varf[c(1:18),1])-RUONIA[i+18]))
  }
  if (i<=(length(macro_adj$ruo)-19)){
    varpcadeler19m <- c(varpcadeler19m,(RUONIA[i]+sum(varf[c(1:19),1])-RUONIA[i+19]))
  }
  if (i<=(length(macro_adj$ruo)-20)){
    varpcadeler20m <- c(varpcadeler20m,(RUONIA[i]+sum(varf[c(1:20),1])-RUONIA[i+20]))
  }
  if (i<=(length(macro_adj$ruo)-21)){
    varpcadeler21m <- c(varpcadeler21m,(RUONIA[i]+sum(varf[c(1:21),1])-RUONIA[i+21]))
  }
  if (i<=(length(macro_adj$ruo)-22)){
    varpcadeler22m <- c(varpcadeler22m,(RUONIA[i]+sum(varf[c(1:22),1])-RUONIA[i+22]))
  }
  if (i<=(length(macro_adj$ruo)-23)){
    varpcadeler23m <- c(varpcadeler23m,(RUONIA[i]+sum(varf[c(1:23),1])-RUONIA[i+23]))
  }
  if (i<=(length(macro_adj$ruo)-24)){
    varpcadeler2y <- c(varpcadeler2y,(RUONIA[i]+sum(varf[c(1:24),1])-RUONIA[i+24]))
  }
  
  a <- c(1:13)
  a[1] <- RUONIA[i]
  a[2:13] <- varf[c(1:12),1]
  #lines(seq.Date(as.Date(macro_adj$date[i]),as.Date(macro_adj$date[i])+365,30),a ,col='blue', type='l')
}


sqrt(sum(varpcadeler1m^2/(length(varpcadeler1m))))
sqrt(sum(varpcadeler2m^2/(length(varpcadeler2m))))
sqrt(sum(varpcadeler3m^2/(length(varpcadeler3m))))
sqrt(sum(varpcadeler4m^2/(length(varpcadeler4m))))
sqrt(sum(varpcadeler5m^2/(length(varpcadeler5m))))
sqrt(sum(varpcadeler6m^2/(length(varpcadeler6m))))
sqrt(sum(varpcadeler7m^2/(length(varpcadeler7m))))
sqrt(sum(varpcadeler8m^2/(length(varpcadeler8m))))
sqrt(sum(varpcadeler9m^2/(length(varpcadeler9m))))
sqrt(sum(varpcadeler10m^2/(length(varpcadeler10m))))
sqrt(sum(varpcadeler11m^2/(length(varpcadeler11m))))
sqrt(sum(varpcadeler1y^2/(length(varpcadeler1y))))

sqrt(sum(varpcadeler13m^2/(length(varpcadeler13m))))
sqrt(sum(varpcadeler14m^2/(length(varpcadeler14m))))
sqrt(sum(varpcadeler15m^2/(length(varpcadeler15m)))) 
sqrt(sum(varpcadeler16m^2/(length(varpcadeler16m))))
sqrt(sum(varpcadeler17m^2/(length(varpcadeler17m))))
sqrt(sum(varpcadeler18m^2/(length(varpcadeler18m))))
sqrt(sum(varpcadeler19m^2/(length(varpcadeler19m))))
sqrt(sum(varpcadeler20m^2/(length(varpcadeler20m))))
sqrt(sum(varpcadeler21m^2/(length(varpcadeler21m))))
sqrt(sum(varpcadeler22m^2/(length(varpcadeler22m))))
sqrt(sum(varpcadeler23m^2/(length(varpcadeler23m))))
sqrt(sum(varpcadeler2y^2/(length(varpcadeler2y))))

length(varpcadeler1m)

