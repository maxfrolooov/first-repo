install.packages("Ecdat")
library(Ecdat)
data(CRSPday) # 
df <- data.frame(CRSPday)
df[,4:7] <- df[,4:7] * 100
rm(CRSPday) # удалить данные
plot.ts(df[,4:7])  # рисуется линиями для временных рядов

# сравнение с нулем

plot.ts(df$ge)
abline(h=0, col = "red")
abline(h=mean(df$ge), col = "green", lty=2)

stats <- c(mean=mean(df$ge), sd=sd(df$ge), se.mean = sd(df$ge)/sqrt(nrow(df)-1)) # за день
stats # колебания больше чем среднее, сильные колебания. se.mean - насколько точно измеряем среднее

t.test(df$ge) # p value - p значение, здесь маленькое число, нулевую гипотезу отвергаем для 5 процентов
for (var in c("ge", "ibm", "mobil", "crsp")){
  print(var)
  print(t.test(df[var]))
}
# names(df)
colMeans((df[,4:7])) # только среднее для 4 компаний

# корреляция 0 или не 0

cor(df[,4:7])
library(PerformanceAnalytics)
chart.Correlation(df[4:7])

cor.test(df$ge, df$ibm) # тест корреляции по пирсону
t.test(scale(df$ge)*scale(df$ibm)) # произведение , scale - стандартизация сл. величины (делить на стандартную ошибку)

# сравнение средних (одно мат ожидание равно другому)

t.test(df$ge, df$ibm) # статастически разные, дисперсии там разные
t.test(df$ge, df$ibm, paired = TRUE) # paired - спаренные, за один и тот же день берутся доходности

# нормальность (доходностей)

hist(df$ge, breaks = 100, freq = FALSE) # freq = false - плотность а не в штуках
curve(dnorm(x, mean(df$ge), sd(df$ge)), add = TRUE, col = "blue")

stats <- c(asy=mean(scale(df$ge)^3), kurt=mean(scale(df$ge)^4))
stats

t.test(scale(df$ge)^3) # тест ассиметрии
t.test(scale(df$ge)^4-3) # куртозис, значимо отличается от нуля

shapiro.test(df$ge) # отклонили 0 гипотезу, там не t статистика, мощность у этого теста большая. Тест на нормальность

qqnorm(df$ge) # квантиль нормального распределения сравнивается с нашими данными 
qqline(df$ge) # данные должны быть по линии ровной с наклоном 1. Данные отклоняются из-за выбросов.
