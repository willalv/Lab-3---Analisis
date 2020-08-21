library("ggpubr")
library("arulesViz")
library("dplyr")

#Se cargan los datos
#datos <- read.csv('C:/Users/Will/Desktop/U/Analisis/Lab 3/bank-additional.csv', sep = ";")
datos <- read.csv('C:/bank-additional.csv', sep = ";")

#Se discretizan las variables cuantitativas
datos.dis <- datos
datos.dis[,"age"] <- cut(datos.dis$age, breaks = c(15, 29, 59, 100), labels = c("young", "adult", "elderly"))
datos.dis[,"duration"] <- cut(datos.dis$duration, breaks = c(-1, 60, 180, 480, 600, 4000), labels = c("very.short", "short", "average", "long", "very.long"))
datos.dis[,"campaign"] <- cut(datos.dis$campaign, breaks = c(-1, 8, 16, 24, 32, 40), labels = c("less.than.eight", "nine.to.sixteen", "seventeen.to.twentyfour", "twentyfive.to.thrirtytwo", "more.than.thrirtythree"))
datos.dis[,"pdays"] <- cut(datos.dis$pdays, breaks = c(-1, 7, 14, 21, 28, 999), labels = c("less.than.seven", "eight.to.fourteen", "fiveteen.to.twentyone", "more.than.twentytwo", "never.contacted"))
datos.dis[,"emp.var.rate"] <- cut(datos.dis$emp.var.rate, breaks = c(-4.00, -3.00, 0.00, 0.18, 1.30, 2.30), labels = c("very.low", "low", "average", "hight", "very.hight"))

#IPC
datos.dis[,"cons.price.idx"] <- cut(datos.dis$cons.price.idx, breaks = c(0.00, 92.00, 94.00, 120.00), labels = c("decrease", "remained", "increase"))
#ICC
datos.dis[,"cons.conf.idx"] <- cut(datos.dis$cons.conf.idx, breaks = c(-50.00, -35.00, -30.00, -20.00, -15.00, 0.00), labels = c("very.high", "high", "normal", "low", "very.low"))
#Euribor
datos.dis[,"euribor3m"] <- cut(datos.dis$euribor3m, breaks = c(-1.00, 2.00, 3.5, 5), labels = c("low", "average", "hight"))
#Tasa de empleo
datos.dis[,"nr.employed"] <- cut(datos.dis$nr.employed, breaks = c(-1.00, 54.00, 56.00, 60.00), labels = c("low", "average", "high"))
datos.dis[,"previous"] <- cut(datos.dis$previous, breaks = c(-1, 1, 2, 3, 20), labels = c("never", "occasional", "normally", "moderately"))

