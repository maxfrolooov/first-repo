setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sandwich)
library(lmtest)

df <- read.csv("data2_13.csv")

# 2а) Найдите пары значимо коррелированных переменных. Какие из пар отрицательно, а какие положительно коррелированы?

round(cor(df), 3)
cor.test(df$z8, df$z1)
cor.test(df$z3, df$z2)
cor.test(df$z6, df$z4)
cor.test(df$z7, df$z5)

cor.test(df$z7, df$z6)
cor.test(df$z7, df$z4)
plot(df)

# 2б) Среди пар найти нелинейную зависимость. Подтвердить тестом.

write.table(round(cor(df), 3), "cor.txt", sep="\t", quote=FALSE)
plot(df$z8 ~ df$z1, data=df)
plot(df$z3 ~ df$z2, data=df) # визуально есть нелинейность
plot(df$z6 ~ df$z4, data=df)
plot(df$z7 ~ df$z5, data=df)

# построим регрессию
reg <- lm(df$z3 ~ df$z2 + I(df$z2^2), data=df)
coeftest(reg) 
coeftest(reg, vcov=vcovHC)

# 2в) Гетероскедостичность
reg <- lm(df$z7 ~ df$z5, data=df)
plot(df$z7 ~ df$z5, data=df)
abline(reg)

plot(resid(reg) ~ fitted(reg)) # resid - остатки для регрессии(e), fitted - расчетное значение (y с крышкой в качестве переменной z)
abline(h=0, col="blue")

reg2 <- lm(resid(reg)^2 ~ fitted(reg)) # квадраты остатков от расч. значения
# Бройша-Пейгана
summary(reg2) # (p-value 1.44*10^-12), отклоняем гипотезу о гомоскедостичности при уровне значимости 10^-15, гамма 1 не равна 0.
bptest(reg2) # тест Бройша-Пейгана. p-value 9.08*10^-05, нулевая гипотеза о гомоскедостичности отвергаем.

plot(fitted(reg2))
# тест на 1 -1 коэф. регрессии
coefci(reg, vcov=vcovHC)
