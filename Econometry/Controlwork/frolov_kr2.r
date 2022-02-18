setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# a)
df <- read.csv("1_13.csv", stringsAsFactors=FALSE)

lgt=glm(df$y~df$x1+df$x2, family = "binomial")
summary(lgt)
fitted(lgt) # расчетные
predict.glm(lgt) # предсказанные

plot(df$y ~ predict.glm(lgt), col = 'blue')
points(fitted(lgt) ~ predict.glm(lgt), pch = '--')

pROC::plot.roc(df$y, fitted(lgt))
# б)
df2 <- df[, c("x1", "x2")]
km <- kmeans(df2, 4, nstart=50)
plot(x2~x1, data=df2, col=km$cluster+1, pch=16+km$cluster) # наблюдается ровная граница
points(km$centers, pch=3, cex=3, lwd=3)
text(km$centers[,1], km$centers[,2], 1:4, cex=3, pos=4)
df$z <- factor(km$cluster)

# в)

lgt2=glm(df$y~df$x1+df$x2+df$z, family = "binomial")
summary(lgt2)
fitted(lgt2) # расчетные
predict.glm(lgt2) # предсказанные

plot(df$y ~ predict.glm(lgt2), col = 'blue')
points(fitted(lgt2) ~ predict.glm(lgt2), pch = '--')

pROC::plot.roc(df$y, fitted(lgt))
par(new=TRUE)
pROC::plot.roc(df$y, fitted(lgt2), col="blue")

# г)

summary(lgt2)

# д)
table(df$y, fitted(lgt2) > 0.5)
