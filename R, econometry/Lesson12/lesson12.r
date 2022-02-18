# ПАНЕЛЬНЫЕ ДАННЫЕ
#install.packages("plm") # линейная регрессия для панельных данных
library(plm)

# i - state, t - year. 
data("Cigar")
#class(Cigar)

sales <- split(Cigar$sales, Cigar$state) # разбиваем для каждого штата продажи (на душу населения)
sales <- data.frame(sales)
ts.plot(sales, col=1:ncol(sales)) # курить стали меньше
lines(rowMeans(sales), lwd=5)

price <- split(Cigar$price, Cigar$state) # разбиваем для каждого штата цены
price <- data.frame(price)
ts.plot(price, col=1:ncol(sales), log="y") # логарифмируем цены. в 80 видно ускоряющуюся инфляцию
lines(rowMeans(price), lwd=5)

cpi <- Cigar$cpi[Cigar$state == 1] # cpi для всех штатов одинаков
ts.plot(price/cpi, col=1:ncol(sales), log="y") # график реальных цен
lines(rowMeans(price/cpi), lwd=5)

# панельный фрейм для регрессии
dfp <- pdata.frame(Cigar, index=c("state", "year"))
class(dfp)
head(index(dfp))

dfp <- within(dfp, { # чтобы $ не писать каждый раз
  lsales <- log(sales)
  lrprice <- log(price/cpi)
  lp16 <- log(pop16/pop) # pop16 - кол-во старше 16 лет
  lrincome <- log(ndi/cpi) # реальный доход
  lrpimin <- log(pimin/cpi) # реальная минимальная цена по соседним штатам
})
# создаем лаг продаж для авторегрессии
dfp$lag.lsales <- plm::lag(dfp$lsales)
# Создаем темпы прироста продаж
dfp$dlsales <- dfp$lsales - dfp$lag.lsales
dfp <- na.omit(dfp)

# регрессия обычная
reg.pool.nt <- lm(dlsales ~ lag.lsales + lrprice + lp16 + lrincome + lrpimin, data=dfp) # без времени
summary(reg.pool.nt) # от цены отрицательная зависимость (типо эластичность спроса по цене). 

# сквозная регрессия, эффекты штатов отсутствуют
reg.pool <- lm(dlsales ~ lag.lsales + lrprice + lp16 + lrincome + lrpimin + year, data=dfp) # со временем. year берет как качественную переменную
summary(reg.pool)

# с фиксированными эффектами
reg.fe <- lm(dlsales ~ lag.lsales + lrprice + lp16 + lrincome + lrpimin + year + state, data=dfp) # со временем и штатом
summary(reg.fe)

reg.pool.nt <- plm(dlsales ~ lag.lsales + lrprice + lp16 + lrincome + lrpimin, 
                   model="pooling", data=dfp)
summary(reg.pool.nt)
reg.fe <- plm(dlsales ~ lag.lsales + lrprice + lp16 + lrincome + lrpimin, 
                   model="within", effect="twoways", data=dfp) # effect - какой хотим учесть эффект
summary(reg.fe) # plm - все переменные центрированные, зависимая переменная другая. Другой R^2



# Панельная регрессия со случайными эффектами - продолжение с предыдущего занятия.
reg.re <- plm(dlsales ~ lag.lsales + lrprice + lp16 + lrincome + lrpimin,
              model="random", data=dfp) 
summary(reg.re) # полчили оценки другие по сравнению с фиксированными эффектами
reg.fe1 <- plm(dlsales ~ lag.lsales + lrprice + lp16 + lrincome + lrpimin, 
              model="within", effect= "twoways", data=dfp) # фиксир. эффекты
phtest(reg.fe1, reg.re) 

