library(quantreg)
library(sandwich)
library(lmtest)

data(engel)
df <- data.frame(engel)

reg <- lm(foodexp ~ income, data=df)
coefci(reg, vcov=vcovHC) # 0.35-0.61 для коэффициента наклона 95% дов. интервал.
plot(foodexp ~ income, data=df)
abline(reg, col="blue") # на графике видим увеличение разброса данных по мере увеличения переменной income. То есть гомоскедостичность визуально нарушается.

plot(resid(reg) ~ fitted(reg))
abline(h=0, col="blue") # квадраты остатков увеличиваются по мере увеличения расчетного значения income. Визуально есть гетероскедостичность.

# Вспомогательная регрессия
reg2 <- lm(resid(reg)^2 ~ fitted(reg)) # квадраты остатков от расч. значения
# Бройша-Пейгана
summary(reg2) # F статистика сильно значима (p-value 2.2*10^-16), отклоняем нулевую гипотезу, гамма 1 не равна 0. Есть гетероскедостичность.
bptest(reg2) # p-value 2.2*10^-16, отклоняем нулевую гипотезу.
plot(fitted(reg2)) # видны визуальные отклонения в оценке дисперсии по мере изменений расчетного значения.

# Логарифмическая регрессия
reg <- lm(log(foodexp) ~ log(income), data=df)
plot(log(foodexp) ~ log(income), data=df)
abline(reg, col="blue") # на графике не видим увеличения разброса данных по мере увеличения переменной income. С логарифмом значительно сгладили отклонения.

plot(resid(reg) ~ fitted(reg))
abline(h=0, col="blue") # квадраты остатков не увеличиваются визуально по мере увеличения расчетного значения income. Визуально нет гетероскедостичность.

# Вспомогательная регрессия
reg2 <- lm(resid(reg)^2 ~ fitted(reg)) # квадраты остатков от расч. значения
# Бройша-Пейгана
summary(reg2) # F статистика p-value 0.00015. Нулевую гипотезу все равно отклоняем, но F статистика значительно меньше, 14 против 200.
plot(fitted(reg2)) 
bptest(reg2) # через хи квадрат.p-value 0.00355. Нулевую гипотезу отклоняем.

# взвешенная дисперсия
wt <- 1/df$income ^ 2
reg.w <- lm(foodexp ~ income, weights = wt, data=df) # исправляем гетероскедостичность. 
coefci(reg.w, vcov=vcovHC) # 0.54-0.50 - значительно улучшили точность довер. интервала.
plot(rstandard(reg.w) ~ fitted(reg.w)) # нормированные остатки от расчетных значений

reg2.w <- lm(rstandard(reg.w)^2 ~ fitted(reg.w))
bptest(reg2.w) # p-value 0.013 - для уровня значимости 0.01 нулевую гипотезу не отклоняем.
summary(reg2.w)

# Медианная регрессия (и квантильные)
qreg <- rq(foodexp ~ income, data=df) # квантильная регрессия
qreg90 <- rq(foodexp ~ income, data=df, tau=0.9)
qreg10 <- rq(foodexp ~ income, data=df, tau=0.1)
plot(foodexp ~ income, data=df)
abline(qreg)
abline(qreg90)
abline(qreg10)
