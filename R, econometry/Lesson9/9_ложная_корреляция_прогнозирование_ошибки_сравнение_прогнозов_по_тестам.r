# Прогнозирование
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.delim("Exchange_Rate_Report.tsv", skip=0, sep="") 
Sys.setlocale("LC_TIME", "C")
df$Date <- as.Date(df$Date, "%d-%b-%Y") # преобразовываем дату

x <- df$Swiss_franc
plot.ts(x)

# 1 задание: Разделить на ряды по 20 наблюдений и создать из них матрицу (matrix(x, nrow=20))
XX <- matrix(x, nrow=20)
XX <- XX[, -24]
# Рассчитать матрицу корреляций.
RR <- cor(XX)
rr <- c(RR[lower.tri(RR)]) # нижний треугольник

hist(rr) # получили много ложных корреляций и на гистограмме это хорошо видно
# Задание 2. Многошаговое прогнозирование валютного курса с помощью тренда
train <- df$Date < "2020-01-03" # На тех же данных. Тренировочный период 1 год – построить линейный тренд. Нарисовать.
test <- ! train
tr <- 1:nrow(df)
reg1 <- lm(x ~ tr, subset=train) # линейная регрессия на первом году
plot.ts(x[train])
lines(fitted(reg1), col="blue") # расчетное значение

xp <- predict(reg1, newdata=list(tr=tr[test])) # получаем прогноз

plot.ts(x[test])
lines(xp, col="blue", lw=2)

tr2 <- tr^2 # сделаем для квадратичного тренда
reg2 <- lm(x ~ tr+tr2, subset=train) # линейная регрессия на первом году
plot.ts(x[train])
lines(fitted(reg2), col="blue") 
xp2 <- predict(reg2, newdata=list(tr=tr[test], tr2=tr2[test])) # получаем прогноз
plot.ts(x[test])
lines(xp2, col="blue", lw=2)

# Задание 3. 
x1 <- c(x[1], x[-length(x)]) #первый лаг
x2 <- c(x1[1], x1[-length(x)])

AR1 <- lm(x ~ x1, subset=train)
cfAR1 <- coef(AR1)

AR2 <- lm(x ~ x1+x2, subset=train)
cfAR2 <- coef(AR2)

AR02 <- lm(x ~ x2, subset=train)
cfAR02 <- coef(AR02)

xpRW <- x1[test]
xpAR1 <- cfAR1[1] + cfAR1[2] * x1[test]
xpAR2 <- cfAR2[1] + cfAR2[2] * x1[test] + cfAR2[3] * x2[test]
xpAR02 <- cfAR02[1] + cfAR02[2] * x2[test]

xpxp <- cbind(xpRW, xpAR1, xpAR2, xpAR02) # прогнозные значения
ts.plot(xpxp, col=1:4)

# ошибки прогноза
epep <- x[test] - xpxp
ts.plot(epep, col=1:4)
abline(h=0)

MSE <- colMeans(epep^2) # среднее по столбцам. Получаем средний квадрат ошибки
RMSE <- sqrt(MSE)
RMSE

# Значимость, 1 задание
epep[,1]
library(lmtest)
library(sandwich)

reg11 <- lm(epep[,4]^2 - epep[,1]^2 ~ 1) # строим регрессию от const

lmtest::coeftest(reg11, vcov=vcovHAC) # прогноз на основе предыдующего значения значим лучше, чем прогноз со 2 лагом. p-value 10^-10


