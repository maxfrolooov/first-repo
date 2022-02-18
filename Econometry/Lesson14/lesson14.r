#install.packages("modeldata")
# Задача классификации и метод ближайших соседей
library(modeldata)
data("lending_club")
df <- lending_club
rm(lending_club)
summary(df)
str(df) # тип переменных

table(df$Class)
df$bad <- (df$Class == "bad") + 0
table(df$bad)

hist(df$funded_amnt) # гистограмма суммы, которую выдали
# hist(log(df$funded_amnt))

levels(df$term) # функция показывает уровни
df$term_num <- ifelse(df$term=="term_36", 36, 60) # срок, месяцы
table(df$term_num)

hist(df$int_rate) # ставка процента
table(df$sub_grade) # классы заемщиков
prop.table(table(df$sub_grade, df$Class))
table(df$sub_grade, df$Class)

#переведем в пропорции штуки 
prop.table(table(df$sub_grade, df$Class), margin=1)
plot(prop.table(table(df$sub_grade, df$Class), margin=1))

#названия колонок в цифрах
df$grade=as.numeric(df$sub_grade)
plot(prop.table(table(df$grade, df$Class), margin=1))

#перевод дохода в лог с учетом минимума в 1000
df$log_inc=log(pmax(df$annual_inc, 1000))

#округлить ставки, перейти к пропорциям
plot(prop.table(table(round(df$int_rate), df$Class), margin=1))
#чем больше ставка, тем больше вероятность, что не вернут 


#логит 
lgt=glm(df$bad~df$funded_amnt+df$term_num+df$int_rate+df$grade+df$log_inc, family = "binomial")
summary(lgt)
lgt

fitted(lgt)#расчетные вероятности 
predict.glm(lgt)
plot(df$bad~predict.glm(lgt),pch="|")
points(fitted(lgt)~predict.glm(lgt),pch="+", col="red")

#прогнозирование 
#с использованием тренировочной выборки 
n=nrow(df)
train=sample(n, 0.8*n)#80%
test=(1:n)[-train] #20%

#оцениваем туже модель, но с подмножеством
lgt1=glm(df$bad~df$funded_amnt+df$term_num+df$int_rate+df$grade+df$log_inc, family = binomial, subset = train)
pp=predict(lgt1, newdata=list(df[test, ]), type="response") #df[test,] строчки трейн, столбцы все
hist(pp)
table(df$bad[test], pp>0.4)#матрица

#кривая ROC
install.packages("pROC")
pROC::plot.roc(df$bad[test],pp)
pROC::roc(df$bad[test],pp)




# install.packages('modeldata')
library('modeldata')

data('lending_club')
df = lending_club
rm(lending_club) # удалить 
summary(df)
str(df)

# Посмотреть сколько хороших и плохих
table(df$Class)

# Перевести зависимую переменную в 0,1
df$bad = (df$Class == 'bad') + 0
table(df$bad)

# Гистограммы количественных объясняющих переменных
hist(df$funded_amnt)
hist(df$annual_inc)

# Округлить процентные ставки, перейти к пропорциям в таблице сопряженности, нарисовать
plot(prop.table(table(round(df$int_rate), df$Class), margin = 1))

# Перевести доход в логарифмы, учитывая минимум 1000
df$log_inc = log(pmax(df$annual_inc, 1000))

# Посмотреть на качественную объясняющую переменную term, перевести ее в число
levels(df$term)
df$term_num = ifelse(df$term == 'term_36', 36, 60)
table(df$term_num)

# Посмотреть таблицу сопряженности для sub_grade в штуках и в долях, перевести в числа
levels(df$sub_grade)
table(df$sub_grade, df$bad)
prop.table(table(df$sub_grade, df$Class), margin = 1)
plot(prop.table(table(df$sub_grade, df$Class), margin = 1))
df$grade = as.numeric(df$sub_grade)

# Оценим логит
lgt = glm(bad ~ funded_amnt + term_num + int_rate + grade + log_inc, data = df, family = binomial)
summary(lgt)
fitted(lgt) # расчетные
predict.glm(lgt) # предсказанные

# График наблюдений с вероятностями 
plot(df$bad ~ predict.glm(lgt), col = 'blue')
points(fitted(lgt) ~ predict.glm(lgt), pch = '--')


# Прогнозирование на тестовой
n = nrow(df)
train = sample(n, 0.8*n)
test = (1:n)[-train]
lgt = glm(bad ~ funded_amnt + term_num + int_rate + grade + log_inc, data = df, subset = train, family = binomial)
summary(lgt)
pp = predict(lgt, newdata = df[test, ], type = 'response') # предсказанные вероятности
table(df$bad[test], pp > 0.2) #матрица 

# ROC
# install.packages('pROC')
pROC::plot.roc(df$bad[test], pp)
