setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv("smetana.csv", stringsAsFactors=FALSE)

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C") # сохраняем текущую и устанавливаем "C"

df$period <- as.Date(df$period, "%d %b %Y")

Sys.setlocale("LC_TIME", lct)

df <- df[order(df$period), c("period", "value")] # сортировка

p <- df$value # создаем вектор цен p на товар

#Создать вектор p цен на товар. Сделать вектор именованным, используя полученные даты, names()

names(p)=df$period
View(p)


#график
#Построить график (линии и маркеры) ряда цен от даты (команда plot(), pch=вид маркера, cex=размер маркера). Добавить линию нуля (команда abline() с опцией h=)
plot(p ~ as.Date(names(p)), type= "b", pch=10, cex=1)
lines(p ~ as.Date(names(p)))
abline(h=0, col = "blue") # цены выше нуля

#Конвертировать ряда dp во помесячный временной ряд (команда ts() с опциями frequency и start=c(год,месяц)). Построить график (команда plot() или plot.ts()).
p.ts=ts(p, frequency = 12, start=c(2017,01))
plot(p.ts)
View(p.ts)

#Создать ряды логарифмических темпов прироста (dp) и обычных темпов прироста (DP) в % в годовом исчислении (умножить на 1200 % - это значит на сколько поменяется помесячно, но в годовом исчислении). Построить аналогичный график. Были ли в цене вашего товара большие скачки?
dp=diff(log(p))*100*12
plot(dp ~ as.Date(names(dp)), type= "b", pch=10, cex=1)
lines(dp ~ as.Date(names(dp)))
abline(h=0, col = "red")

DP=((diff(p)/p[-length(p)]+1)^12-1)*100
plot(DP ~ as.Date(names(DP)), type= "b", pch=10, cex=1)
abline(h=0, col = "red")

library(psych)
describe(dp)
describe(DP)

#Построить гистограмму ряда dp, подобрав подходящую ширину интервала (команда hist(), опция breaks=. для полученя плотности, а не частоты опция freq=FALSE). Добавить «бахому» наблюдений (командой rug()). Добавить нормальную кривую с соответствующими dp параметрами mean и sd. (Для кривой функции команда curve() с опцией add=TRUE. Функция плотности нормального распределения dnorm()).
hist(dp, breaks = 25, freq = FALSE)
rug(dp)
curve(dnorm(x, mean(dp), sd(dp)), add=TRUE, col="red")

hist(DP, breaks = 25, freq = FALSE)
rug(DP)
curve(dnorm(x, mean(DP), sd(DP)), add=TRUE, col="red")

mean(scale(dp)^3) #считаем скошенность(ассиметрия)
mean(scale(DP)^3)
mean(scale(dp)^4) #курсозис #более острая вершина, длинные хвосты
mean(scale(DP)^4)

summary(dp)
summary(DP)

stats.dp=data.frame(
  mean(dp), # среднее, средняя доходность в год
  var(dp), # дисперсия
  sd(dp), # стандартное отклонение, 19 процентныз пунктов - достаточно сильные колебания 
  min(dp),
  quantile(dp, 0.25),
  median(dp),
  quantile(dp, 0.75),
  max(dp),
  mean(scale(dp)^3), # - скошенность, скошено влево
  mean(scale(dp)^4) # - куртозис, больше 3 (чем у нормального)
)

names(stats.dp) <- c("Среднее", "Дисперсия", "Среднекв. отклонение", "Минимум", 
                    "Квартиль 25%", "Медиана", "Квантиль 75%",
                    "Максимум", "Скошенность", "Куртозис")

row.names(stats.dp) = "dp"

View(stats.dp)

stats.dp=t(stats.dp)
write.table(stats.dp, "out_dp.txt", sep="\t", quote=FALSE) # quaote - убрать ковычки


stats.DP=data.frame(
  mean(DP), # среднее, средняя доходность в год
  var(DP), # дисперсия
  sd(DP), # стандартное отклонение, 19 процентныз пунктов - достаточно сильные колебания 
  min(DP),
  quantile(DP, 0.25),
  median(DP),
  quantile(DP, 0.75),
  max(DP),
  mean(scale(DP)^3), # - скошенность, скошено влево
  mean(scale(DP)^4) # - куртозис, больше 3 (чем у нормального)
)

names(stats.DP) <- c("Среднее", "Дисперсия", "Среднекв. отклонение", "Минимум", 
                     "Квартиль 25%", "Медиана", "Квантиль 75%",
                     "Максимум", "Скошенность", "Куртозис")

row.names(stats.DP) = "DP"

View(stats.DP)

stats.DP=t(stats.DP)
write.table(stats.DP, "out_dp_big.txt", sep="\t", quote=FALSE) # quaote - убрать ковычки

