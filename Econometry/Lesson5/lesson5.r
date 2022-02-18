setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.delim("temp_wage.tsv", skip=3)
names(df)[1] <- "region" # меняем первую переменную на region

plot(wage ~ temp, data=df)
cor(df$wage, df$temp)
# plot(df$wage~df$temp)

# регрессия, получается список всяких характеристик
reg <- lm(wage ~ temp, data=df) # wage - зависимая часть, temp - обьясняющая
# lm(df$wage~df$temp) - так тоже можно
summary(reg) # intercept - константа (31,9), temp - обьясняющая пер.
# получился отрицательный наклон -1,2353
# wage = 31.9079 - 1.2353 * temp + остатки
plot(wage ~ temp, data=df)
abline(reg, col="blue", lw=2) # рисуем линию регрессии, lw - толщина линии

cor(df$wage, df$temp)^2 # 0.25906, reg - multiple R-squared = 0,2591 = 25,91%

# найдем расчетные значения и положим их на график
plot(wage ~ fitted(reg), data=df, asp=1) # по горизонтали - расчетные значения, asp - шаг по y и по x равный
abline(c(0,1), col="blue", lw=2) # добавляем линию с наклоном 1 - 100 процентов если совпадают

cor(df$wage, fitted(reg))^2 # 0.259 между y и y^

# обратная регрессия
reg.inv = lm(temp ~ wage, data=df)
summary(reg.inv) # R^2 такой же, но коэф. другие
# temp = 0.31403 - 0.20971 * wage + остаток
# wage = 0.31403 / 0.20971 - 1 / 0.20971 * temp
cf.inv <- coef(reg.inv) # получем коэф. регрессии
cf.inv

# Данные
plot(wage ~ temp, data=df)
# прямая регрессия (исходная)
abline(reg, col="blue", lw=2)
# Обратная регрессия
abline(c(-cf.inv[1]/cf.inv[2], 1/cf.inv[2]), col="red", lw=2)
# две линии средних
abline(v=mean(df$temp))
abline(h=mean(df$wage))
