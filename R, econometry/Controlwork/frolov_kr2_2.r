setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
df <- read_excel("task2.xlsx", skip = 0)

# 1)
plot.ts(df$GAZ_C)

y <- log(df$GAZ_C) # взяли сезонные данные и пролагорифмировали
acf(y, lag.max=50)
plot.ts(y)

h <- 24 # делаем прогноз на 2 года последних
TT <- length(y) - h
sarma <- arima(y[1:TT], order=c(1,1,1),  # period - сколько сезонов. сезонность у нас не меняется - стационарная
               seasonal = list(order = c(1, 0, 1), period=12)) # order (p, d, q)- порядок arima, d - сколько разностей взять для стационарности
sarma 

plot(residuals(sarma)) # белый щум, который мы оценили - остатки

yp <- predict(sarma, n.ahead=h)$pred # строим прогноз, h - горизонт прогноза. первая часть - точечный прогноз, вторая - сигма е.
sp <- predict(sarma, n.ahead=h)$se
plot.ts(y[(TT+1):(TT+h)])
points(y[(TT+1):(TT+h)])
lines(c(yp), col="blue")

L <- yp - qnorm(1-0.05/2)*sp # lower, 95 % интервал
U <- yp + qnorm(1-0.05/2)*sp # upper
lines(c(L), col="red") # у arima интервал расширяется, у arma нет, он стабилизируется.
lines(c(U), col="green")


epep <- y[(TT+1):(TT+h)] - yp # вычисляем ошибки
ts.plot(epep) # график ошибок
abline(h=0, col="blue") 

# MSE
MSE <- mean(epep^2)
RMSE <- sqrt(MSE)
RMSE
MAE <- mean(abs(epep))
MAE
