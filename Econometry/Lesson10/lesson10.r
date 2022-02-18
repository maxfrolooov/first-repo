setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df1 <- read.delim("USA_Icecream.tsv", skip=21, sep="", stringsAsFactors = FALSE) 
df2 <- read.delim("DEU_Dairy.tsv", skip=16, sep="") 

# второе задание, ложная корреляция при сезонности
cor.test(df1$VALUE, df2$dairy) # cor = 0.49031, p-value = 10^-16. Значимо отличается от 0.


# Прогнозирование с помощью тренда и сезонных фиктивных переменных
x <- df1$VALUE
plot.ts(x)
train <- df1$DATE < "1939-01-01"
test <- ! train # Тестовый период последние 24 месяца

#прогноз на последние 24 месяца по модели с линейным трендом и месячными фиктивными переменными.
tr <- 1:nrow(df1)
month <- rep(1:12, 23) # 12 месяцев, 23 года период
reg1 <- lm(x ~ tr + factor(month), subset=train) # регрессия на train периоде
plot.ts(x[train])
lines(fitted(reg1), col="blue")

xp <- predict(reg1, newdata=list(tr=tr[test], month=month[test])) # получаем прогноз

plot.ts(x[test])
lines(xp, col="blue", lw=2) # график прогноза

epep <- x[test] - xp # вычисляем ошибки
ts.plot(epep) # график ошибок
abline(h=0, col="blue") 

# MSE
MSE <- mean(epep^2)
RMSE <- sqrt(MSE)
RMSE

