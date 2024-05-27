library(sandwich)
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
library(ggpubr)
library(jpeg)
library(stargazer)
library(grid)
#install.packages('jpeg')
NSBetas_monthly_withshocks <- read_excel("C:/Users/Lenovo/Desktop/диплом/data/NSBetas_monthly_withshocks.xlsx")
NSLPdata <- NSBetas_monthly_withshocks
irfs <- read_excel("C:/Users/Lenovo/Desktop/диплом/data/responses ruo.xlsx")
acf(na.omit(ts(NSLPdata$shocks3)))
#не автокоррелированы
############################ beta 0 ##########################################
endog_data <- as.data.frame(NSLPdata$beta_0)
colnames(endog_data) <- 'beta 0'
shock <- as.data.frame(NSLPdata$shocks3)
results <- lp_lin_iv(endog_data = endog_data, shock=shock,
                     hor=13,confint=1.96, lags_endog_lin = 1, trend=0)

irfs$beta0[1:(match(0,irfs$period)-1)] <- 0
#функция мэтч выводит номер элемента, в котором период равен 0
#то есть когда произошел шок

#в пакете график почему то рисуется с 1-го периода, а не с 0
results$irf_lin_mean
match(0,irfs$period)
j <- 12
b <- as.data.frame(cbind(t(results$irf_lin_low), t(results$irf_lin_mean),
                         t(results$irf_lin_up)))
colnames(b) <- c('ci_l','irf','ci_u')
b$t <- 0:j
b
expression(paste("Отклик ", beta,tolower(0)))
#поскольку пакет не считает мгновенный отклик, то сделаем самостоятельно
b0 <- ggplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(expression(paste("Отклик ",beta[0])))+
  geom_line(b, mapping = aes(x=t, y=ci_l), colour="blue")+
  geom_line(b, mapping = aes(x=t, y=irf, ),colour="red", size=1.2)+
  geom_line(b, mapping = aes(x=t, y=ci_u), colour="blue")+
  ylab("Единицы")+xlab("месяцы")+geom_smooth()+
  geom_ribbon(data   = b, aes(x = t, ymin = ci_l, ymax = ci_u), col = 'grey',
              fill   = 'grey', alpha  = 0.3) +
  theme_classic()+geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, 1)) 
#дополним таблицу 
for (i in (match(0,irfs$period):((match(0,irfs$period)+12)))){
  irfs$beta0[i] <- irfs$beta0[1]+results$irf_lin_mean[i-match(0,irfs$period)+1] #irfs$resp[i]-irfs$beta1[i]
}
############################ beta 1 ##########################################
endog_data <- as.data.frame(NSLPdata$beta_1)
shock <- as.data.frame(NSLPdata$shocks3)
results <- lp_lin_iv(endog_data = endog_data, shock=shock,
                     hor=13,confint=1.96, lags_endog_lin = 5, trend=0)

irfs$beta1[1:(match(0,irfs$period)-1)] <- 0

#в пакете график почему то рисуется с 1-го периода, а не с 0
results$irf_lin_mean
j <- 12
b <- as.data.frame(cbind(t(results$irf_lin_low), t(results$irf_lin_mean),
                         t(results$irf_lin_up)))
colnames(b) <- c('ci_l','irf','ci_u')
b$t <- 0:j
#поскольку пакет не считает мгновенный отклик, то сделаем самостоятельно
b1 <- ggplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(expression(paste("Отклик ",beta[1])))+
  geom_line(b, mapping = aes(x=t, y=ci_l), colour="blue")+
  geom_line(b, mapping = aes(x=t, y=irf, ),colour="red", size=1.2)+
  geom_line(b, mapping = aes(x=t, y=ci_u), colour="blue")+
  ylab("Единицы")+xlab("месяцы")+geom_smooth()+
  geom_ribbon(data   = b, aes(x = t, ymin = ci_l, ymax = ci_u), col = 'grey',
              fill   = 'grey', alpha  = 0.3) +
  theme_classic()+geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, 1)) 

#дополним таблицу 
for (i in (match(0,irfs$period):((match(0,irfs$period)+12)))){
  irfs$beta1[i] <- irfs$beta1[1]+results$irf_lin_mean[i-match(0,irfs$period)+1]
}

############################ beta 2 ##########################################
endog_data <- as.data.frame(NSLPdata$beta_2)
shock <- as.data.frame(NSLPdata$shocks3)
results <- lp_lin_iv(endog_data = endog_data, shock=shock,
                     hor=13,confint=1.96, lags_endog_lin = 1, trend=0)

irfs$beta2[1:(match(0,irfs$period)-1)] <- 0


#в пакете график почему то рисуется с 1-го периода, а не с 0
results$irf_lin_mean
j <- 12
b <- as.data.frame(cbind(t(results$irf_lin_low), t(results$irf_lin_mean),
                         t(results$irf_lin_up)))
colnames(b) <- c('ci_l','irf','ci_u')
b$t <- 0:j
#поскольку пакет не считает мгновенный отклик, то сделаем самостоятельно
b2 <- ggplot()+
  theme(plot.title = element_text(hjust = 1))+
  ggtitle(expression(paste("Отклик ",beta[2])))+
  geom_line(b, mapping = aes(x=t, y=ci_l), colour="blue")+
  geom_line(b, mapping = aes(x=t, y=irf, ),colour="red", size=1.2)+
  geom_line(b, mapping = aes(x=t, y=ci_u), colour="blue")+
  ylab("Единицы")+xlab("месяцы")+geom_smooth()+
  geom_ribbon(data   = b, aes(x = t, ymin = ci_l, ymax = ci_u), col = 'grey',
              fill   = 'grey', alpha  = 0.3) +
  theme_classic()+geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, 1)) 
#дополним таблицу 
for (i in (match(0,irfs$period):((match(0,irfs$period)+12)))){
  irfs$beta2[i] <- irfs$beta2[1]+results$irf_lin_mean[i-match(0,irfs$period)+1]
}

grid.newpage()
# Создать расположение: nrow = 2, ncol = 4
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 4)))
# Вспомогательная функция для задания области в расположении
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Упорядочить графики
print(b0, vp = define_region(row = 1, col = 1:2))   # Расположить в двух колонках
print(b1, vp = define_region(row = 1, col = 3:4))
print(b2, vp = define_region(row = 2, col = 2:3))

ggarrange(b0,ggarrange(b1,b2,ncol=2),nrow=2)

############################ lambda ##########################################
endog_data <- as.data.frame(NSLPdata$lambda)
shock <- as.data.frame(NSLPdata$shocks3)
results <- lp_lin_iv(endog_data = endog_data, shock=shock,
                     hor=13,confint=1.96, lags_endog_lin = 1, trend=0)




#в пакете график почему то рисуется с 1-го периода, а не с 0
results$irf_lin_mean
j <- 12
b <- as.data.frame(cbind(t(results$irf_lin_low), t(results$irf_lin_mean),
                         t(results$irf_lin_up)))
colnames(b) <- c('ci_l','irf','ci_u')
b$t <- 0:j
#поскольку пакет не считает мгновенный отклик, то сделаем самостоятельно
lam <- ggplot()+
  theme(plot.title = element_text(hjust = 1))+
  ggtitle(expression(paste("Отклик ", lambda)))+
  geom_line(b, mapping = aes(x=t, y=ci_l), colour="blue")+
  geom_line(b, mapping = aes(x=t, y=irf, ),colour="red", size=1.2)+
  geom_line(b, mapping = aes(x=t, y=ci_u), colour="blue")+
  ylab("Единицы")+xlab("месяцы")+geom_smooth()+
  geom_ribbon(data   = b, aes(x = t, ymin = ci_l, ymax = ci_u), col = 'grey',
              fill   = 'grey', alpha  = 0.3) +
  theme_classic()+geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, 1)) 
#дополним таблицу
#отклик лямбды не значим всюду, возьмем, для всех периодов среднее
irfs$lambda[1:nrow(irfs)] <- mean(NSLPdata$lambda[c(10:151)])



############################ 1 WEEK ##########################################
#y_monthly существуют с 05.2011 по 12.2023
#а шоки с 08.2011 по 11.2023
#обрежем y_monthly, чтобы совпадали данные
y_monthly <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/yields_monthly.csv",)
y_monthly <- y_monthly[c(1:nrow(NSLPdata)),]
y_monthly <- y_monthly[,-1]
y_monthly$Date <- NSLPdata$Date
colnames(y_monthly) <- c('Date', '1W','2W','1M','2M','3M','6M','1Y','2Y')
NSLPdata <- cbind(NSLPdata, y_monthly)
NSLPdata <- NSLPdata[,-12]

endog_data <- as.data.frame(NSLPdata$`1W`)
shock <- as.data.frame(NSLPdata$shocks3)
results <- lp_lin_iv(endog_data = endog_data, shock=shock,
                     hor=13,confint=1.96, lags_endog_lin = 2, trend=0)
#в пакете график почему то рисуется с 1-го периода, а не с 0
j <- 12
b <- as.data.frame(cbind(t(results$irf_lin_low), t(results$irf_lin_mean),
                         t(results$irf_lin_up)))
colnames(b) <- c('ci_l','irf','ci_u')
b$t <- 0:j
#поскольку пакет не считает мгновенный отклик, то сделаем самостоятельно
w1 <- ggplot()+
  ggtitle("1-недельные")+
  geom_line(b, mapping = aes(x=t, y=ci_l), colour="blue")+
  geom_line(b, mapping = aes(x=t, y=irf, ),colour="red", size=1.2)+
  geom_line(b, mapping = aes(x=t, y=ci_u), colour="blue")+
  ylab('п.п.')+xlab("месяцы")+geom_smooth()+
  geom_ribbon(data   = b, aes(x = t, ymin = ci_l, ymax = ci_u), col = 'grey',
              fill   = 'grey', alpha  = 0.3) +
  theme_classic()+geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, 1)) 


############################ 1 MONTH ##########################################
endog_data <- as.data.frame(NSLPdata$`1M`)
shock <- as.data.frame(NSLPdata$shocks3)
results <- lp_lin_iv(endog_data = endog_data, shock=shock,
                     hor=13,confint=1.96, lags_endog_lin = 2, trend=0)
#в пакете график почему то рисуется с 1-го периода, а не с 0
j <- 12
b <- as.data.frame(cbind(t(results$irf_lin_low), t(results$irf_lin_mean),
                         t(results$irf_lin_up)))
colnames(b) <- c('ci_l','irf','ci_u')
b$t <- 0:j
#поскольку пакет не считает мгновенный отклик, то сделаем самостоятельно
m1 <- ggplot()+
  ggtitle("1-месячные")+
  geom_line(b, mapping = aes(x=t, y=ci_l), colour="blue")+
  geom_line(b, mapping = aes(x=t, y=irf, ),colour="red", size=1.2)+
  geom_line(b, mapping = aes(x=t, y=ci_u), colour="blue")+
  ylab('п.п.')+xlab("месяцы")+geom_smooth()+
  geom_ribbon(data   = b, aes(x = t, ymin = ci_l, ymax = ci_u), col = 'grey',
              fill   = 'grey', alpha  = 0.3) +
  theme_classic()+geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, 1)) 

############################ 3 MONTHS ##########################################
endog_data <- as.data.frame(NSLPdata$`3M`)
shock <- as.data.frame(NSLPdata$shocks3)
results <- lp_lin_iv(endog_data = endog_data, shock=shock,
                     hor=13,confint=1.96, lags_endog_lin = 2, trend=0)
#в пакете график почему то рисуется с 1-го периода, а не с 0
j <- 12
b <- as.data.frame(cbind(t(results$irf_lin_low), t(results$irf_lin_mean),
                         t(results$irf_lin_up)))
colnames(b) <- c('ci_l','irf','ci_u')
b$t <- 0:j
#поскольку пакет не считает мгновенный отклик, то сделаем самостоятельно
m3 <- ggplot()+
  ggtitle("3-месячные")+
  geom_line(b, mapping = aes(x=t, y=ci_l), colour="blue")+
  geom_line(b, mapping = aes(x=t, y=irf, ),colour="red", size=1.2)+
  geom_line(b, mapping = aes(x=t, y=ci_u), colour="blue")+
  ylab('п.п.')+xlab("месяцы")+geom_smooth()+
  geom_ribbon(data   = b, aes(x = t, ymin = ci_l, ymax = ci_u), col = 'grey',
              fill   = 'grey', alpha  = 0.3) +
  theme_classic()+geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, 1)) 

############################ 6 MONTHS ##########################################
endog_data <- as.data.frame(NSLPdata$`6M`)
shock <- as.data.frame(NSLPdata$shocks3)
results <- lp_lin_iv(endog_data = endog_data, shock=shock,
                     hor=13,confint=1.96, lags_endog_lin = 2, trend=0)
#в пакете график почему то рисуется с 1-го периода, а не с 0
j <- 12
b <- as.data.frame(cbind(t(results$irf_lin_low), t(results$irf_lin_mean),
                         t(results$irf_lin_up)))
colnames(b) <- c('ci_l','irf','ci_u')
b$t <- 0:j
#поскольку пакет не считает мгновенный отклик, то сделаем самостоятельно
m6 <- ggplot()+
  ggtitle("6-месячные")+
  geom_line(b, mapping = aes(x=t, y=ci_l), colour="blue")+
  geom_line(b, mapping = aes(x=t, y=irf, ),colour="red", size=1.2)+
  geom_line(b, mapping = aes(x=t, y=ci_u), colour="blue")+
  ylab('п.п.')+xlab("месяцы")+geom_smooth()+
  geom_ribbon(data   = b, aes(x = t, ymin = ci_l, ymax = ci_u), col = 'grey',
              fill   = 'grey', alpha  = 0.3) +
  theme_classic()+geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, 1))  

############################ 1 YEAR ##########################################
endog_data <- as.data.frame(NSLPdata$`1Y`)
shock <- as.data.frame(NSLPdata$shocks3)
results <- lp_lin_iv(endog_data = endog_data,lags_endog_lin = 1, shock=shock,
                     hor=13,confint=1.96, trend=0, 
                     cumul_mult = TRUE)
#в пакете график почему то рисуется с 1-го периода, а не с 0
results$irf_lin_mean
results
j <- 12
b <- as.data.frame(cbind(t(results$irf_lin_low), t(results$irf_lin_mean),
                         t(results$irf_lin_up)))
colnames(b) <- c('ci_l','irf','ci_u')
b$t <- 0:j
#поскольку пакет не считает мгновенный отклик, то сделаем самостоятельно
y1 <- ggplot()+
  ggtitle("1-летние")+
  geom_line(b, mapping = aes(x=t, y=ci_l), colour="blue")+
  geom_line(b, mapping = aes(x=t, y=irf, ),colour="red", size=1.2)+
  geom_line(b, mapping = aes(x=t, y=ci_u), colour="blue")+
  ylab('п.п.')+xlab("месяцы")+geom_smooth()+
  geom_ribbon(data   = b, aes(x = t, ymin = ci_l, ymax = ci_u), col = 'grey',
              fill   = 'grey', alpha  = 0.3) +
  theme_classic()+geom_hline(yintercept = 0, col = "black", linewidth = 0.25, linetype = "dashed")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 12, 1))  
ggarrange(ggarrange(m1,m6,ncol=2),y1,nrow=2)
ggarrange(ggarrange(w1,m1, ncol=2),ggarrange(m3,m6,ncol=2),y1,nrow=3)
ggarrange(w1,m1,nrow=2)

grid.newpage()
# Создать расположение: nrow = 2, ncol = 4
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 4)))
# Вспомогательная функция для задания области в расположении
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Упорядочить графики
print(m1, vp = define_region(row = 1, col = 1:2))   # Расположить в двух колонках
print(m6, vp = define_region(row = 1, col = 3:4))
print(y1, vp = define_region(row = 2, col = 2:3))



###################### построим отклики траекторий на шок ####################
##################### по локальным проекциям #################################
#нужно определить значение ставки в отсутствие шока
#почему я раньше не додумался до функции match :-((((((
macro_adj <- read_excel("C:/Users/Lenovo/Desktop/диплом/data/macro_adj.xlsx")
begin <- match(NSLPdata$Date[1],macro_adj$date)
ending <- match(NSLPdata$Date[nrow(NSLPdata)-1], macro_adj$date)
NSLPdata$ruo <- NA
NSLPdata$ruo[1:(nrow(NSLPdata)-1)] <- macro_adj$ruo[begin:ending]
mod <- lm(ruo~shocks3+lag(ruo)+lag(ruo,2), data=NSLPdata)
#третий лаг не значим
summary(mod)
#возьмем значение ставки при нулевом шоке
irfs$ruo <- NA
#стеди стейт
irfs$ruo[1:(match(0,irfs$period)-1)] <- mod$coefficients[1]/(1-mod$coefficients[3]-mod$coefficients[4])
for (i in (match(0,irfs$period):(nrow(irfs)))){
  irfs$ruo[i] <- irfs$ruo[1]+irfs$resp[i]
}



#рассчитаем значения прогнозируемых ставок в каждый момент времени
#сделаем много сроков, чтобы не было углов в кривых
irfs$`1D` <- NA
irfs$`2D` <- NA
irfs$`3D` <- NA
irfs$`4D` <- NA
irfs$`5D` <- NA
irfs$`6D` <- NA
irfs$`1W` <- NA
irfs$`1W` <- NA
irfs$`2W` <- NA
irfs$`1M` <- NA
irfs$`2M` <- NA
irfs$`3M` <- NA
irfs$`4M` <- NA
irfs$`5M` <- NA
irfs$`6M` <- NA
irfs$`7M` <- NA
irfs$`8M` <- NA
irfs$`9M` <- NA
irfs$`10M` <- NA
irfs$`11M` <- NA
irfs$`1Y` <- NA

mat <- c(seq(1/30,7/30, length.out=7),14/30,seq(1,12))
mat
#тревожная кнопка, нажимать в крайнем случае
#irfs <- irfs[,c(1:7)]

for (i in (1:nrow(irfs))){
  irfs[i,c(8:ncol(irfs))] <- NSrates(as.xts(as.ts(irfs[i,(3:6)])), maturity = mat)
}

plot(irfs$period[c(1:(match(0,irfs$period)))], irfs$resp[c(1:(match(0,irfs$period)))], 
     type='s', ylim=c(-0.1,1.5), xlim=c(-3,15), xlab='Месяцы',
     ylab='RUONIA', lwd=2.0)
lines(irfs$period[c((match(0,irfs$period)):(match(0,irfs$period)+15))], 
      irfs$resp[c((match(0,irfs$period)):(match(0,irfs$period)+15))], 
      type='l', lwd=2.0)

for (i in c(1:9)){
  times <- c((rep(irfs$period[i], length(mat))+mat))
  lines(times, 
        c(irfs[i,c(8:ncol(irfs))]), 
        type='l', col='blue',lwd=1.0 ) 
}

##################################### Ошибочка, переделаем #####################
mat <- 1/30
for (i in c(2:365)){
  mat <- c(mat,i/30 )
}

y <- NSrates(as.xts(as.ts(irfs[,c(3:6)])), maturity=mat)
y <- as.data.frame(y)
y$period
y$period <- irfs$period
y <- y[,c((length(mat)+1),1:length(mat))]
len <- nrow(y)

yf <- y
for (i in(1:nrow(yf))){
  for (j in (3:ncol(yf))){
    a <- (1+y[i,j]/100)^((j-1)/365)
    b <- (1+y[i,(j-1)]/100)^((j-2)/365)
    yf[i,j] <- 100*((a/b)^(365)-1)
  }
}

plot(irfs$period[c(1:(match(0,irfs$period)))], irfs$resp[c(1:(match(0,irfs$period)))], 
     type='s', ylim=c(-0.1,1.5), xlim=c(-2,15), xlab='Месяцы',
     ylab='RUONIA', lwd=2.0)
lines(irfs$period[c((match(0,irfs$period)):(match(0,irfs$period)+18))], 
      irfs$resp[c((match(0,irfs$period)):(match(0,irfs$period)+18))], 
      type='l', lwd=2.0)
for (i in c(1:9)){
  times <- c((rep(irfs$period[i], length(mat))+mat))
  lines(times, 
        c(yf[i,c(2:ncol(yf))]), 
        type='l', col='blue',lwd=1.0 ) 
}

############## скорректируем так, чтобы ожидания были прикреплены к ставке #####
irfs$beta0 <- irfs$resp-irfs$beta1

for (i in (1:nrow(irfs))){
  irfs[i,c(8:ncol(irfs))] <- NSrates(as.xts(as.ts(irfs[i,(3:6)])), maturity = mat)
}

plot(irfs$period[c(1:(match(0,irfs$period)))], irfs$resp[c(1:(match(0,irfs$period)))], 
     type='s', ylim=c(-0.1,1.2), xlim=c(-3,15), xlab='Месяцы',
     ylab='RUONIA', lwd=2.0)
lines(irfs$period[c((match(0,irfs$period)):(match(0,irfs$period)+15))], 
      irfs$resp[c((match(0,irfs$period)):(match(0,irfs$period)+15))], 
      type='l', lwd=2.0)

for (i in c(1:10)){
  times <- c((rep(irfs$period[i], length(mat))+mat))
  lines(times, 
        c(irfs[i,c(8:ncol(irfs))]), 
        type='l', col='blue',lwd=1.0 ) 
}


############################# переделаем ошибку ################################
irfs$beta0 <- irfs$resp-irfs$beta1

mat <- 1/30
for (i in c(2:365)){
  mat <- c(mat,i/30 )
}

y <- NSrates(as.xts(as.ts(irfs[,c(3:6)])), maturity=mat)
y <- as.data.frame(y)
y$period
y$period <- irfs$period
y <- y[,c((length(mat)+1),1:length(mat))]
len <- nrow(y)

yf <- y
for (i in(1:nrow(yf))){
  for (j in (3:ncol(yf))){
    a <- (1+y[i,j]/100)^((j-1)/365)
    b <- (1+y[i,(j-1)]/100)^((j-2)/365)
    yf[i,j] <- 100*((a/b)^(365)-1)
  }
}

plot(irfs$period[c(1:(match(0,irfs$period)))], irfs$resp[c(1:(match(0,irfs$period)))], 
     type='s', ylim=c(-0.1,1.2), xlim=c(-2,15), xlab='Месяцы',
     ylab='RUONIA', lwd=2.0)
lines(irfs$period[c((match(0,irfs$period)):(match(0,irfs$period)+18))], 
      irfs$resp[c((match(0,irfs$period)):(match(0,irfs$period)+18))], 
      type='l', lwd=2.0)
for (i in c(1:9)){
  times <- c((rep(irfs$period[i], length(mat))+mat))
  lines(times, 
        c(yf[i,c(2:ncol(yf))]), 
        type='l', col='blue',lwd=1.0 ) 
}


################## отклики не по методу LP, а через произведение коэффов ########
########################### beta 0 ##############################################
#посмотрим сколько лучше лагов использовать
#а бета 0 не стационарен, его подгонять будем


########################### beta 1 ##############################################
mod <- lm(beta_1~0+shocks3+lag(beta_1)+lag(beta_1,2), data=NSLPdata)
summary(mod)
#третий лаг не значим

a <- mod$coefficients[1]# 0.193529
b1 <-mod$coefficients[2]# 1.14439 
b2 <-  mod$coefficients[3]# -0.304372

irfs$beta1[1:(match(0,irfs$period)-1)] <- 0
irfs$beta1[match(0,irfs$period)] <- a
irfs$beta1[(match(0,irfs$period)+1)] <- a*b1
irfs$beta1[(match(0,irfs$period)+2)] <- a*(b1^2)+b2*a

for (i in ((match(0,irfs$period)+3):(match(0,irfs$period)+12))){
  irfs$beta1[i] <- b1*irfs$beta1[i-2]+b2*irfs$beta1[i-1]
}


#подгоним уровень, чтобы левый конец совпадал с RUONIA
for (i in (1:(match(0,irfs$period)+12))){
  irfs$beta0[i] <- irfs$resp[i]-irfs$beta1[i]
}
########################### beta 2 ##############################################
mod <- lm(beta_2~shocks3+lag(beta_2), data=NSLPdata)
summary(mod)

#второй лаг не значим
a <- mod$coef[2]# 0.193529
b1 <-mod$coef[3]# 1.14439 

irfs$beta2[1:(match(0,irfs$period)-1)] <- 0

for (i in ((match(0,irfs$period)):(match(0,irfs$period)+12))){
  irfs$beta2[i] <- a*(b1^(i-match(0,irfs$period)))
}



mat <- c(seq(1/30,7/30, length.out=7),14/30,seq(1,12))
mat

for (i in (1:nrow(irfs))){
  irfs[i,c(8:ncol(irfs))] <- NSrates(as.xts(as.ts(irfs[i,(3:6)])), maturity = mat)
}

plot(irfs$period[c(1:(match(0,irfs$period)))], irfs$resp[c(1:(match(0,irfs$period)))], 
     type='s', ylim=c(-0.1,1.2), xlim=c(-3,15), xlab='Месяцы',
     ylab='RUONIA', lwd=2.0)
lines(irfs$period[c((match(0,irfs$period)):(match(0,irfs$period)+22))], 
      irfs$resp[c((match(0,irfs$period)):(match(0,irfs$period)+22))], 
      type='l', lwd=2.0)

for (i in c(1:12)){
  times <- c((rep(irfs$period[i], length(mat))+mat))
  lines(times, c(irfs[i,c(8:ncol(irfs))]), 
        type='l', col='blue',lwd=1.0 ) 
}


############################## Исправляем ошибку ###############################
mat <- 1/30
for (i in c(2:365)){
  mat <- c(mat,i/30 )
}

y <- NSrates(as.xts(as.ts(irfs[,c(3:6)])), maturity=mat)
y <- as.data.frame(y)
y$period
y$period <- irfs$period
y <- y[,c((length(mat)+1),1:length(mat))]
len <- nrow(y)

yf <- y
for (i in(1:nrow(yf))){
  for (j in (3:ncol(yf))){
    a <- (1+y[i,j]/100)^((j-1)/365)
    b <- (1+y[i,(j-1)]/100)^((j-2)/365)
    yf[i,j] <- 100*((a/b)^(365)-1)
  }
}

plot(irfs$period[c(1:(match(0,irfs$period)))], irfs$resp[c(1:(match(0,irfs$period)))], 
     type='s', ylim=c(-0.1,1.2), xlim=c(-2,15), xlab='Месяцы',
     ylab='RUONIA', lwd=2.0)
lines(irfs$period[c((match(0,irfs$period)):(match(0,irfs$period)+18))], 
      irfs$resp[c((match(0,irfs$period)):(match(0,irfs$period)+18))], 
      type='l', lwd=2.0)
for (i in c(1:10)){
  times <- c((rep(irfs$period[i], length(mat))+mat))
  lines(times, 
        c(yf[i,c(2:ncol(yf))]), 
        type='l', col='blue',lwd=1.0 ) 
}

  





########################### Результаты моделей в табличку #####################
mod1 <- lm(beta_1~0+shocks3+lag(beta_1)+lag(beta_1,2), data=NSLPdata)
mod2 <- lm(beta_2~shocks3+lag(beta_2), data=NSLPdata)
summary(mod2)

cse = function(reg){
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)}
stargazer(mod1, mod2, se=list(cse(mod1),cse(mod2)), type='latex',
          out='models.tex')
?stargazer



############## А сейчас ваще шок, отклики доходностей для траекторий ############  
yirfs <- irfs[,c(1,7)]
length(na.omit(NSLPdata$shocks3))
#в последней строке тоже NA 
yields_monthly <- read.csv("C:/Users/Lenovo/Desktop/диплом/data/yields_monthly.csv")  
# первый столбец лишний
yields_monthly <- yields_monthly[,-1]
yshocks <- cbind(NSLPdata$Date, NSLPdata$shocks3, NSLPdata$ruo, yields_monthly[c(1:nrow(NSLPdata)),c(2:8)])
colnames(yshocks) <- c('Date','shocks3','ruo','1W','2W','1M','2M','3M',
                       '6M','1Y')
#подберем модель для каждого свопа
############################### 1 WEEK #########################################
mod <- lm(`1W`~ shocks3+lag(`1W`)+lag(`1W`,2), data=yshocks)
summary(mod)
adf.test(na.omit(ts(yshocks[c(10:151),10])))
#третий лаг уже не значим
a <- mod$coefficients[2]
b1 <- mod$coefficients[3]
b2 <- mod$coefficients[4]

impulse <- c(a,a*b1,a*b1^2+b2*a)
for (i in (3:23)){
  impulse <- c(impulse,b1*impulse[i-2]+b2*impulse[i-1])
}
length(impulse)

yirfs$`1W` <- NA
yirfs$`1W`[1:(match(0,yirfs$period)-1)] <- mod$coefficients[1]/(1-mod$coefficients[3]-mod$coefficients[4])
adf.test(na.omit(ts(yshocks$ruo[c(10:152)])))
for (i in((match(0,yirfs$period)):(match(0,yirfs$period)+23))){
  yirfs$`1W`[i] <- yirfs$`1W`[1]+impulse[i-match(0,yirfs$period)+1]
}

############################### 2 WEEK #########################################
mod <- lm(`2W`~ shocks3+lag(`2W`)+lag(`2W`,2), data=yshocks)
summary(mod)
#третий лаг уже не значим
a <- mod$coefficients[2]
b1 <- mod$coefficients[3]
b2 <- mod$coefficients[4]

impulse <- c(a,a*b1,a*b1^2+b2*a)
for (i in (3:23)){
  impulse <- c(impulse,b1*impulse[i-2]+b2*impulse[i-1])
}
impulse

yirfs$`2W` <- NA
yirfs$`2W`[1:(match(0,yirfs$period)-1)] <- mod$coefficients[1]/(1-mod$coefficients[3]-mod$coefficients[4])

for (i in((match(0,yirfs$period)):(match(0,yirfs$period)+23))){
  yirfs$`2W`[i] <- yirfs$`2W`[1]+impulse[i-match(0,yirfs$period)+1]
}

############################### 1 MONTH  #########################################
mod <- lm(`1M`~ shocks3+lag(`1M`)+lag(`1M`,2), data=yshocks)
summary(mod)
#третий лаг уже не значим
a <- mod$coefficients[2]
b1 <- mod$coefficients[3]
b2 <- mod$coefficients[4]

impulse <- c(a,a*b1,a*b1^2+b2*a)
for (i in (3:23)){
  impulse <- c(impulse,b1*impulse[i-2]+b2*impulse[i-1])
}
impulse

yirfs$`1M` <- NA
yirfs$`1M`[1:(match(0,yirfs$period)-1)] <- mod$coefficients[1]/(1-mod$coefficients[3]-mod$coefficients[4])

for (i in((match(0,yirfs$period)):(match(0,yirfs$period)+23))){
  yirfs$`1M`[i] <- yirfs$`1M`[1]+impulse[i-match(0,yirfs$period)+1]
}

############################### 2 MONTHS  #########################################
mod <- lm(`2M`~ shocks3+lag(`2M`)+lag(`2M`,2), data=yshocks)
summary(mod)
#третий лаг уже не значим
a <- mod$coefficients[2]
b1 <- mod$coefficients[3]
b2 <- mod$coefficients[4]

impulse <- c(a,a*b1,a*b1^2+b2*a)
for (i in (3:23)){
  impulse <- c(impulse,b1*impulse[i-2]+b2*impulse[i-1])
}
impulse

yirfs$`2M` <- NA
yirfs$`2M`[1:(match(0,yirfs$period)-1)] <- mod$coefficients[1]/(1-mod$coefficients[3]-mod$coefficients[4])

for (i in((match(0,yirfs$period)):(match(0,yirfs$period)+23))){
  yirfs$`2M`[i] <- yirfs$`2M`[1]+impulse[i-match(0,yirfs$period)+1]
}

############################### 3 MONTHS  #########################################
mod <- lm(`3M`~ shocks3+lag(`3M`)+lag(`3M`,2), data=yshocks)
summary(mod)
#третий лаг уже не значим
a <- mod$coefficients[2]
b1 <- mod$coefficients[3]
b2 <- mod$coefficients[4]

impulse <- c(a,a*b1,a*b1^2+b2*a)
for (i in (3:23)){
  impulse <- c(impulse,b1*impulse[i-2]+b2*impulse[i-1])
}
impulse

yirfs$`3M` <- NA
yirfs$`3M`[1:(match(0,yirfs$period)-1)] <- mod$coefficients[1]/(1-mod$coefficients[3]-mod$coefficients[4])

for (i in((match(0,yirfs$period)):(match(0,yirfs$period)+23))){
  yirfs$`3M`[i] <- yirfs$`3M`[1]+impulse[i-match(0,yirfs$period)+1]
}

############################### 6 MONTHS  #########################################
mod <- lm(`6M`~ shocks3+lag(`6M`), data=yshocks)
summary(mod)
#третий лаг уже не значим
a <- mod$coefficients[2]
b1 <- mod$coefficients[3]
b2 <- mod$coefficients[4]

impulse <- c(a,a*b1,a*b1^2+b2*a)
for (i in (3:23)){
  impulse <- c(impulse,b1*impulse[i-2]+b2*impulse[i-1])
}
impulse

yirfs$`6M` <- NA
yirfs$`6M`[1:(match(0,yirfs$period)-1)] <- mod$coefficients[1]/(1-mod$coefficients[3]-mod$coefficients[4])

for (i in((match(0,yirfs$period)):(match(0,yirfs$period)+23))){
  yirfs$`6M`[i] <- yirfs$`6M`[1]+impulse[i-match(0,yirfs$period)+1]
}
#а если ограничиться одним лагом?
yirfs$`6M`[1:(match(0,yirfs$period)-1)] <- mod$coefficients[1]/(1-mod$coefficients[3])
for (i in((match(0,yirfs$period)):(match(0,yirfs$period)+23))){
  yirfs$`6M`[i] <- yirfs$`6M`[1]+mod$coefficients[2]*mod$coefficients[3]^(i-match(0,yirfs$period))
}


############################### 1 YEAR  #########################################
mod <- lm(`1Y`~ shocks3+lag(`1Y`), data=yshocks)
summary(mod)
#второй лаг уже не значим

yirfs$`1Y` <- NA
yirfs$`1Y`[1:(match(0,yirfs$period)-1)] <- mod$coefficients[1]/(1-mod$coefficients[3])

for (i in((match(0,yirfs$period)):(match(0,yirfs$period)+23))){
  yirfs$`1Y`[i] <- yirfs$`1Y`[1]+mod$coefficients[2]*mod$coefficients[3]^(i-match(0,yirfs$period))
}

####################### отклики траекторий моим методом ########################
plot(yirfs$period, yirfs$ruo, type='l',ylim=c(8.3,10), xlab='Периоды',
     ylab='RUONIA', lwd=2.0)

mat <- c(seq(1/30,7/30, length.out=7),14/30,seq(1,12))
maturity <- c(7/30,14/30,1,2,3,6,12)
for (i in (1:nrow(yirfs))){
  times <- c((rep(yirfs$period[i], length(mat))+mat))
  x <- NSrates(as.xts(as.ts(Nelson.Siegel(yirfs[i,c(3:9)],maturity))),maturity=mat)
  x
  lines(times, 
       x, 
        type='l', col='blue',lwd=1.0 )
}
plot(yirfs$period, yirfs$ruo, type='l',ylim=c(8.3,10), xlab='Периоды',
     ylab='RUONIA', lwd=2.0)
lines(yirfs$period, 
      yirfs$`1W`, 
      type='l', col='blue',lwd=1.0 )


x
##################### А что было в 2022 году? ##################################
#единичный шок дкп ведет к мгновенному росту ставки на 
irfs$resp[match(0,irfs$period)] #п.п.
# цб поднял ставку в 22 году 28 февраля примерно на 10,5 п.п
#значение ставки в среднем в марте:
NSLPdata$ruo[match('2022-03-01',NSLPdata$Date)]
# это эквивалентно шоку в размере
bum <- (NSLPdata$ruo[match('2022-03-01',NSLPdata$Date)]-NSLPdata$ruo[match('2022-02-01',NSLPdata$Date)])/irfs$resp[match(0,irfs$period)]
rise <- match('2022-03-01',NSLPdata$Date)

ruonia <- c(NSLPdata$ruo[c((rise-2):(rise))])
for (i in(c(rise+1):(rise+12))){
  ruonia <- c(ruonia, NSLPdata$ruo[rise-1]+bum*irfs$resp[(i+match(1,irfs$period)-rise-1)])
}

plot(as.Date(NSLPdata$Date[c(rise-2):(rise+12)]),
       ruonia, type='l',xlab='Время',ylab='RUONIA',
     ylim=c(7,25),
     xlim= as.Date(c((NSLPdata$Date[(rise-2)]),(NSLPdata$Date[(rise+10)]))),
     main='Симуляция шока ДКП 28.02.22')
mat <- c(seq(1/30,7/30, length.out=7),14/30,seq(1,12))

lam <- rep(irfs$lambda[1],10)
num <- match(0,irfs$period)
bum
b1 <- 0
b2 <- 0

# ПЕРЕД ЭТИМ НАДО ПРОГНАТЬ КОД, КОТОРЫЙ В IRFS ПРИСВАИВАЕТ
# ЗНАЧЕНИЯ ОТКЛИКОВ ПО ЛОКАЛЬНЫМ ПРОЕКЦИЯМ

b1 <- c(b1,bum*irfs$beta1[c(match(0,irfs$period):(match(0,irfs$period)+8))])
b2 <- c(b2,bum*irfs$beta2[c(match(0,irfs$period):(match(0,irfs$period)+8))])
b0 <- ruonia[c(2:11)]-b1

for (i in c(2:(length(b0)-2))){
  times <- c((rep(as.Date(NSLPdata$Date[(rise-1+i-1)]), length(mat))+mat*30))
  lines(times,NSrates(as.xts(as.ts(t(c(b0[i],b1[i],b2[i],lam[i])))),mat), 
      type='l', col='blue')
}


#как выглядело в реальности
plot(as.Date(NSLPdata$Date[c(rise-2):(rise+12)]),
     c(NSLPdata$ruo[c((rise-2):(rise+12))]), type='l',xlab='Время',ylab='RUONIA',
     ylim=c(7,25),xlim= as.Date(c((NSLPdata$Date[(rise-2)]),(NSLPdata$Date[(rise+10)]))),
     main='Динамика ставки RUONIA и ожиданий в 2022 году')
mat <- c(seq(1/30,7/30, length.out=7),14/30,seq(1,12))

for (i in c((rise-1):(rise+8))){
  times <- c((rep(as.Date(NSLPdata$Date[i]), length(mat))+mat*30))
  lines(times,NSrates(as.xts(as.ts(NSLPdata[i,c(2:5)])),mat), 
        type='l', col='blue')
}

############################## Переделаем ошибку ###############################
#### симуляция
plot(as.Date(NSLPdata$Date[c(rise-2):(rise+12)]),
     ruonia, type='l',xlab='Время',ylab='RUONIA',
     ylim=c(7,25),
     xlim= as.Date(c((NSLPdata$Date[(rise-2)]),(NSLPdata$Date[(rise+10)]))),
     main='Симуляция шока ДКП 28.02.22')

lam <- rep(irfs$lambda[1],10)
num <- match(0,irfs$period)
bum
b1 <- 0
b2 <- 0

# ПЕРЕД ЭТИМ НАДО ПРОГНАТЬ КОД, КОТОРЫЙ В IRFS ПРИСВАИВАЕТ
# ЗНАЧЕНИЯ ОТКЛИКОВ ПО ЛОКАЛЬНЫМ ПРОЕКЦИЯМ

b1 <- c(b1,bum*irfs$beta1[c(match(0,irfs$period):(match(0,irfs$period)+8))])
b2 <- c(b2,bum*irfs$beta2[c(match(0,irfs$period):(match(0,irfs$period)+8))])
b0 <- ruonia[c(2:11)]-b1

mat <- 1/30
for (i in c(2:365)){
  mat <- c(mat,i/30 )
}

z <- cbind(b0,b1,b2)
z

y <- NSrates(as.xts(as.ts(cbind(b0,b1,b2,lam))), maturity=mat)
y <- as.data.frame(y)

y <- y[,c((length(mat)+1),1:length(mat))]
len <- nrow(y)

yf <- y
for (i in(1:nrow(yf))){
  for (j in (2:ncol(yf))){
    a <- (1+y[i,j]/100)^((j-1)/365)
    b <- (1+y[i,(j-1)]/100)^((j-2)/365)
    yf[i,j] <- 100*((a/b)^(365)-1)
  }
}
i
length(times)
length(yf[i])
for (i in c(2:(length(b0)-2))){
  times <- c((rep(as.Date(NSLPdata$Date[(rise-1+i-1)]), length(mat))+mat*30))
  lines(times,yf[i,], 
        type='l', col='blue')
}

################################### реальность ################################# 
plot(as.Date(NSLPdata$Date[c(rise-2):(rise+12)]),
     c(NSLPdata$ruo[c((rise-2):(rise+12))]), type='l',xlab='Время',ylab='RUONIA',
     ylim=c(7,25),xlim= as.Date(c((NSLPdata$Date[(rise-2)]),(NSLPdata$Date[(rise+10)]))),
     main='Динамика ставки RUONIA и ожиданий в 2022 году')


y <- NSrates(as.xts(as.ts(NSLPdata[,c(2:5)])), maturity=mat)
y <- as.data.frame(y)



yf <- y
for (i in(1:nrow(yf))){
  for (j in (2:ncol(yf))){
    a <- (1+y[i,j]/100)^((j-1)/365)
    b <- (1+y[i,(j-1)]/100)^((j-2)/365)
    yf[i,j] <- 100*((a/b)^(365)-1)
  }
}

for (i in c((rise):(rise+7))){
  times <- c((rep(as.Date(NSLPdata$Date[i]), length(mat))+mat*30))
  lines(times,yf[i,], 
        type='l', col='blue')
}
