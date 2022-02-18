setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.delim("StreetLight2020.tsv", skip=5) # actual - траффик машин, actual в тысячах штук


# Оценка плотности для actual
x <- df$actual
summary(x) # сильно не симметричное распределение, есть выбросы 
hist(x, breaks = 200, freq=FALSE)
hist(log(x), breaks = 100, freq=FALSE)

# ядерная оценка
plot(density(x, adjust=0.1)) # kernal density estimator, adjust - h полоса
#kernel = c("gaussian", "epanechnikov", "rectangular",
#           "triangular", "biweight",
#           "cosine", "optcosine")
?density
plot(density(log(x), adjust=0.3))

bounds <- c(0.1, 0.33, 1, 3.3, 10, 33, 100, 330) # посчитаем чистоты для этих интервалов. В логарифмах это примерное одномерный шаг
hi <- hist(x, breaks=bounds, plot=FALSE)
hi$counts # частоты в штуках

df2 <- data.frame(counts = hi$counts)

df2$perc <- prop.table(df2$counts) * 100
row.names(df2) <- paste0(bounds[1:7], "_", bounds[2:8])
df2$lab.perc <- paste0(round(df2$perc, 2), "%")
df2

# Пакет ggplot2
library(ggplot2)

# гистограмма 
ggplot(df, aes(x=actual, stat(density))) + 
  geom_histogram(color="blue", fill="lightblue", bins=200) +
  theme_light() # тема белая

ggplot(df, aes(x=log(actual), stat(density))) + 
  geom_histogram(color="blue", fill="lightblue", bins=200) +
  theme_bw()

# плотность
ggplot(df, aes(x=actual)) + 
  geom_density(color="blue", fill="lightblue", adjust=0.1) +
  theme_gray()

ggplot(df, aes(x=log(actual))) + 
  geom_density(color="blue", fill="lightblue", adjust=0.3) + # ядерная оценка плотности
  geom_rug() + # бахрома
  geom_vline(xintercept = mean(log(df$actual))) + # среднее
  theme_gray()

# рисуем частоты для df2
ggplot(df2, aes(x=row.names(df2), y=counts, fill=row.names(df2), label=counts)) +
  geom_col(width=0.5, color="red") +
  geom_text(nudge_y=50) + # поднимем цифры выше
  theme_bw()

ggplot(df2, aes(x=row.names(df2), y=perc, fill=row.names(df2), label=lab.perc)) +
  geom_col(width=0.5, color="red") +
  geom_text(nudge_y=2) + # поднимем цифры выше
  theme_bw()
# + coord_flip() - переворачивает ось графика

# полярные координаты
ggplot(df2, aes(x=factor(1), y=perc, fill=row.names(df2))) +
  geom_col(width=0.5, color="red") +
  geom_text(nudge_y=0.4) + # поднимем цифры выше
  coord_polar(theta ="x", direction=-1) +
  theme_void()

# точечная диаграмма для actual 
ggplot(df, aes(x=estimated, y=actual)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color="blue", size=1)

ggplot(df, aes(x=estimated, y=actual)) +
  geom_point() +
  geom_smooth(method="lm") +
  stat_ellipse(size=1.5, color="red")
