setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df1 <- read.delim("USA_Icecream.tsv", skip=21, sep="", stringsAsFactors = FALSE) 

y <- log(df1$VALUE)

plot.ts(y)

h <- 24 # делаем прогноз на 2 года последних
TT <- length(y) - h
sarma <- arima(y[1:TT], order=c(1,1,1),  # period - сколько сезонов. сезонность у нас не меняется - стационарная
      seasonal = list(order = c(1, 0, 1), period=12)) # order (p, d, q)- порядок arima, d - сколько разностей взять для стационарности
sarma 

plot(residuals(sarma)) # белый щум, который мы оценили - остатки

yp <- predict(sarma, n.ahead=h)$pred # строим прогноз, h - горизонт прогноза. первая часть - точечный прогноз, вторая - сигма е.
sp <- predict(sarma, n.ahead=h)$se
plot.ts(y[(TT+1):(TT+h)], ylim=c(1, 4))
points(y[(TT+1):(TT+h)], ylim=c(1, 4))
lines(c(yp), col="blue") # точечный прогноз

# интервальный прогноз, предполагаем нормальность
L <- yp - qnorm(1-0.05/2)*sp # lower, 95 % интервал
U <- yp + qnorm(1-0.05/2)*sp # upper
lines(c(L), col="red") # у arima интервал расширяется, у arma нет, он стабилизируется.
lines(c(U), col="green")



# Оценка волатильости !!!!!
df2 <- read.csv("US_sp500.csv", stringsAsFactors = FALSE)
r <- diff(log(df2$Close)) * 100 # логарифмические доходности

plot.ts(r)
abline(h=0, col="blue")
abline(h=mean(r), col="red")

acf(r^2) # АКФ, синий - дов. интервал для белого шума 
acf(abs(r)) # волатильность сильно автокоррелирована
var(r)

# Riskmetrics
a <- 0.05
s2 <- 1 # дисперсия начальная, быстро забывается
TT <- length(r)
for (t in 2:TT)
  s2[t] <- a * r[t-1]^2 + (1-a)*s2[t-1]
vol <- sqrt(s2) # волатильность
plot.ts(vol) # в процентных пунктах

plot.ts(r)
lines(vol, col="red", lwd=2)

# GARCH
library(fGarch)

ga <- garchFit(r~ garch(1,1), data.frame(r),  
               cond.dist="sstd", # условное распределение, sstd - скошенный стьюдент
               trace=FALSE)
ga # mu - среднее мю (значимо), shape - параметр распределения (7 степени свободы - толстые хвосты, больше 20 - нормальное почти). skew - скошенность значима


plot.ts(r)
lines(volatility(ga), col="blue", lwd=2) # volatility - корень из сигма^2
