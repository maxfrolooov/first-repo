setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.csv("data1_13.csv")

# 1а) Укажите переменные с левой (правой) асимметрией. Обоснуйте.
# 1б) Укажите переменные с маленьким (большим) куртозисом. Обоснуйте.

for (num in c("x1", "x2", "x3", "x4", "x5")){
  print(num)
  stats <- c(asy=mean(scale(df[num])^3), kurt=mean(scale(df[num])^4-3)) 
  print(stats)
}

# 1в) Гистограммы, нужно определить моды
hist(df$x1, breaks = 250, freq = FALSE)
hist(df$x2, breaks = 250, freq = FALSE)



