library("ggpubr")
library("arulesViz")
library("dplyr")

#Se cargan los datos
datos <- read.csv('C:/Users/Will/Desktop/U/Analisis/Lab 3/bank-additional.csv', sep = ";")
#datos <- read.csv('C:/bank-additional.csv', sep = ";")

#Se discretizan las variables cuantitativas
datos.dis <- datos
datos.dis[,"age"] <- cut(datos.dis$age, breaks = c(15, 29, 59, 100), labels = c("young", "adult", "elderly"))
datos.dis[,"duration"] <- cut(datos.dis$duration, breaks = c(-1, 60, 180, 480, 600, 4000), labels = c("very.short", "short", "average", "long", "very.long"))
datos.dis[,"campaign"] <- cut(datos.dis$campaign, breaks = c(-1, 8, 16, 24, 32, 40), labels = c("less.than.eight", "nine.to.sixteen", "seventeen.to.twentyfour", "twentyfive.to.thrirtytwo", "more.than.thrirtythree"))
datos.dis[,"pdays"] <- cut(datos.dis$pdays, breaks = c(-1, 7, 14, 21, 28, 999), labels = c("less.than.seven", "eight.to.fourteen", "fiveteen.to.twentyone", "more.than.twentytwo", "never.contacted"))
datos.dis[,"emp.var.rate"] <- cut(datos.dis$emp.var.rate, breaks = c(-4.00, -3.00, 0.00, 0.18, 1.30, 2.30), labels = c("very.low", "low", "average", "hight", "very.hight"))

