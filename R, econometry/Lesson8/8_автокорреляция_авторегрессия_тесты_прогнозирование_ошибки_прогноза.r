TT <- 100
# Белый шум
set.seed(123) # задаем начальное число для датчика
eps <- rnorm(TT, sd=2)
#plot(eps, type = "l") 
#plot(eps, type = "b") 
plot.ts(eps) # график для временных рядов
abline(h=0, col="blue")
acf(eps) # АКФ для белого шума, случайно болтается без какой то системы

# AR - авторегрессия
phi <- 0.8
x <- eps[1] # t=1
for (t in 2:TT) {
  x[t] <- phi * x[t-1] + eps[t]
}
#x <- x + 10 # если хотим среднее 10
plot.ts(x) # график для временных рядов
abline(h=0, col="blue")
acf(x, lag.max=90) # убывает, но случайно уходит в отриц. область

phi <- -0.8 # орицательная корреляция между соседними наблюдениями
x <- eps[1] # t=1
for (t in 2:TT) {
  x[t] <- phi * x[t-1] + eps[t]
}
plot.ts(x) # график для временных рядов
abline(h=0, col="blue")
acf(x, lag.max=30)

phi <- 1 # случайное блуждание
x <- eps[1] # t=1
for (t in 2:TT) {
  x[t] <- phi * x[t-1] + eps[t]
}
plot.ts(x) # график для временных рядов
abline(h=0, col="blue")
acf(x, lag.max=20)


# задание с курсом доллара
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv("usd_volume.txt") 
#summary(df$X.VOL.)

df <- data.frame(date=df$X.DATE., x = df$X.VOL.)
df$date <- as.Date(as.character(df$date), "%Y%m%d") # as.character преобразует в строчку числа
df <- df[df$x != 0, ]

# Лаги будем создавать внутри фрейма
TT <- nrow(df)
df <- within(df, # within - внутри датафрейма работаем.
  {x1 <- c(x[1], x[-TT]) # 1-й лаг
  x2 <- c(x1[1], x1[-TT]) # второй
  x3 <- c(x2[1], x2[-TT])
  x4 <- c(x3[1], x3[-TT])}
)
min(df$x)

plot(df$x ~ df$date, type="l")
acf(df$x) # автокорреляционная функция не угасает, медленно убывает. Похоже на нестационарность.
acf(df$x)[1] # первая автокорреляция

# Авторегрессия 1 порядка
reg1 <- lm(x ~ x1, data=df)
#plot.ts(resid(reg1)[1:50])
summary(reg1) # коэф. автокорреляции 0.688
reg2 <- lm(x ~ x1+x2, data=df)
summary(reg2)
reg3 <- lm(x ~ x1+x2+x3, data=df)
summary(reg3) # коэф. автокорреляции 0.688
reg4 <- lm(x ~ x1+x2+x3+x4, data=df)
summary(reg4)
library(lmtest)
library(sandwich) # ковар. матрица

coeftest(reg4, vcov=vcovHAC) # все коэф. значимы, все лаги нужны, укоротить нельзя.

# тест бройша-годфри
bgtest(reg4) # LM-test, 1 лаг был добавлен, p значение маленькое, 0 гипотезу отклоняем
bgtest(reg4, order=5) # жестко отклоняем

