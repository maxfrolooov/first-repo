# 2. Метод k средних (k-means). Сгенерированные данные и квартиры в Октябрьском районе.
n <- 1000
set.seed(123)
df <- data.frame(
  grn=sample(1:3, n, replace=TRUE, prob=c(0.5, 0.3, 0.2))) # replace - с возвращением вытягиваем, prob - вероятность что будет группа
df$gr <- c("A", "B", "C")[df$grn] 
df$cx <- c(2, 4, 6)[df$grn]# центр для каждой группы. x - переменная
df$cy <- c(6, 2, 4)[df$grn]

df$x <- df$cx + rnorm(n) # генерируем переменные
df$y <- df$cy + rnorm(n)

plot(y~x, data=df, col=df$grn+1, pch=16+df$grn)
points(cy ~ cx, data=df, pch=3, cex=3, lwd=3)
text(cy ~ cx, gr, data=df, cex=3, pos=4)
plot(y~x, data=df, pch=16)

df2 <- df[, c("x", "y")]
#plot(y~x, data=df2, pch=16)
km <- kmeans(df2, 3, nstart=50) # 3- центра (кластера), nstart - количество прогонов
str(km) # краткая информация об обьекте

plot(y~x, data=df2, col=km$cluster+1, pch=16+km$cluster) # наблюдается ровная граница
points(km$centers, pch=3, cex=3, lwd=3)
text(km$centers[,1], km$centers[,2], 1:3, cex=3, pos=4)
points(cy ~ cx, data=df, pch=4, cex=3, lwd=3, col="orange")
text(cy ~ cx, gr, data=df, cex=3, pos=2)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.delim("Kvart_Okt.dat", encoding = "UTF-8", header=TRUE, na.strings="-") # квартиры в Октябрьском районе.
names(df) <- c("id", "place", "district", "street", "house.num", "price", "rooms", "area", "liv.area", "kitchen",
               "floor", "nfloors")
df <- df[df$liv.area>10,]
df <- df[df$kitchen>1,]
df <- na.omit(df)
summary(df)
plot(price~area, data=df, log="xy")
# Подготовка данных
df2 <- data.frame(
  df$rooms,
  log(df$price), log(df$area), log(df$liv.area), df$kitchen)
df2 <- df2[df2$df.rooms <= 3, ] # убираем 4 комнатные квартиры
df3 <- scale(df2[, -1]) # убираем 1 столбец
summary(df3)

# кластеры
km <- kmeans(df3, 3, nstart = 50) # по комнатам кластеризуем
# нарисуем цену и площадь
plot(log.df.price. ~ log.df.area., 
     col=km$cluster+1, pch=km$cluster+16, # разделение по 4 переменным, поэтому в кооринатах для 2 нечеткое разделение.
     data=df3)
points(km$centers[,1:2], pch=3, cex=2, lwd=3)
text(km$centers[,1], km$centers[,2], 1:3, cex=3, pos=4)

table(df2$df.rooms*10, km$cluster)
