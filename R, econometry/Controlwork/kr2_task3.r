setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 3a)
df <- read.csv("US_sp500.csv", stringsAsFactors=FALSE)
df <- df[(nrow(df):1), ]

df1 <- df[df$Date >= "1998-01-01", ]
df1 <- df1[df1$Date < "2003-01-01", ]

r <- diff(log(df1$Close)) * 100 # логарифмические доходности

plot.ts(r)
abline(h=0, col="blue")
abline(h=mean(r), col="red")
acf(r^2) # АКФ, синий - дов. интервал для белого шума 
acf(abs(r)) # имеется кластеризация волатильности, акф для доходности выше дов. интервала для белого шума
var(r)

# 3Б) Riskmetrics
a <- 0.05
s1 <- var(r) # дисперсия начальная, быстро забывается
TT <- length(r)
for (t in 2:TT)
  s1[t] <- a * r[t-1]^2 + (1-a)*s1[t-1]
vol <- sqrt(s1) # волатильность
plot.ts(vol) # в процентных пунктах

a <- 0.3
s2 <- var(r) # дисперсия начальная, быстро забывается
TT <- length(r)
for (t in 2:TT)
  s2[t] <- a * r[t-1]^2 + (1-a)*s2[t-1]
vol2 <- sqrt(s2) # волатильность
plot.ts(vol2) # в процентных пунктах

plot.ts(r)
lines(vol, col="red", lwd=2)
lines(vol2, col="blue", lwd=2)
# 3в)
L1 <- (r[1]^2)/(s1[1]) + log(s1[1])
for (t in 2:TT)
  L1[t] <- (r[t]^2)/(s1[t]) + log(s1[t])
L2 <- (r[1]^2)/(s2[1]) + log(s2[1])
for (t in 2:TT)
  L2[t] <- (r[t]^2)/(s2[t]) + log(s2[t])
mean(L2)
mean(L1)

library(lmtest)
library(sandwich)

reg11 <- lm(L2 - L1 ~ 1) # строим регрессию от const

lmtest::coeftest(reg11, vcov=vcovHAC)
