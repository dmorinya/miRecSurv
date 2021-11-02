library(readxl)
library(WriteXLS)
#setwd("~/OneDrive - Universitat Autònoma de Barcelona/Investigacio/Actuals/Tesis/Gilma/Articles/3/R/results_frailty")

datos <- read_excel("Gaceta500/results1111.xls")
datos$codigo <- 1111
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "2-2"
r1111 <- datos

datos <- read_excel("Gaceta500/results1112.xls")
datos$codigo <- 1112
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "2-2"
r1112 <- datos

datos <- read_excel("Gaceta500/results1113.xls")
datos$codigo <- 1113
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "2-2"
r1113 <- datos

datos <- read_excel("Gaceta500/results1114.xls")
datos$codigo <- 1114
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "2-2"
r1114 <- datos

datos <- read_excel("Gaceta500/results1115.xls")
datos$codigo <- 1115
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "2-2"
r1115 <- datos

datos <- read_excel("Gaceta500/results1121.xls")
datos$codigo <- 1121
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "2-5"
r1121 <- datos

datos <- read_excel("Gaceta500/results1122.xls")
datos$codigo <- 1122
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "2-5"
r1122 <- datos

datos <- read_excel("Gaceta500/results1123.xls")
datos$codigo <- 1123
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "2-5"
r1123 <- datos

datos <- read_excel("Gaceta500/results1124.xls")
datos$codigo <- 1124
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "2-5"
r1124 <- datos

datos <- read_excel("Gaceta500/results1125.xls")
datos$codigo <- 1125
datos$n <- 500
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "2-5"
r1125 <- datos

datos <- read_excel("Gaceta500/results1211.xls")
datos$codigo <- 1211
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "10-2"
r1211 <- datos

datos <- read_excel("Gaceta500/results1212.xls")
datos$codigo <- 1212
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "10-2"
r1212 <- datos

datos <- read_excel("Gaceta500/results1213.xls")
datos$codigo <- 1213
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "10-2"
r1213 <- datos

datos <- read_excel("Gaceta500/results1214.xls")
datos$codigo <- 1214
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "10-2"
r1214 <- datos

datos <- read_excel("Gaceta500/results1215.xls")
datos$codigo <- 1215
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "10-2"
r1215 <- datos

datos <- read_excel("Gaceta500/results1221.xls")
datos$codigo <- 1221
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "10-5"
r1221 <- datos

datos <- read_excel("Gaceta500/results1222.xls")
datos$codigo <- 1222
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "10-5"
r1222 <- datos

datos <- read_excel("Gaceta500/results1223.xls")
datos$codigo <- 1223
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "10-5"
r1223 <- datos

datos <- read_excel("Gaceta500/results1224.xls")
datos$codigo <- 1224
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "10-5"
r1224 <- datos

datos <- read_excel("Gaceta500/results1225.xls")
datos$codigo <- 1225
datos$n <- 500
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "10-5"
r1225 <- datos

datos <- read_excel("Gaceta500/results2111.xls")
datos$codigo <- 2111
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "2-2"
r2111 <- datos

datos <- read_excel("Gaceta500/results2112.xls")
datos$codigo <- 2112
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "2-2"
r2112 <- datos

datos <- read_excel("Gaceta500/results2113.xls")
datos$codigo <- 2113
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "2-2"
r2113 <- datos

datos <- read_excel("Gaceta500/results2114.xls")
datos$codigo <- 2114
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "2-2"
r2114 <- datos

datos <- read_excel("Gaceta500/results2115.xls")
datos$codigo <- 2115
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "2-2"
r2115 <- datos

datos <- read_excel("Gaceta500/results2121.xls")
datos$codigo <- 2121
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "2-5"
r2121 <- datos

datos <- read_excel("Gaceta500/results2122.xls")
datos$codigo <- 2122
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "2-5"
r2122 <- datos

datos <- read_excel("Gaceta500/results2123.xls")
datos$codigo <- 2123
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "2-5"
r2123 <- datos

datos <- read_excel("Gaceta500/results2124.xls")
datos$codigo <- 2124
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "2-5"
r2124 <- datos

datos <- read_excel("Gaceta500/results2125.xls")
datos$codigo <- 2125
datos$n <- 500
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "2-5"
r2125 <- datos

datos <- read_excel("Gaceta500/results2211.xls")
datos$codigo <- 2211
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "10-2"
r2211 <- datos

datos <- read_excel("Gaceta500/results2212.xls")
datos$codigo <- 2212
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "10-2"
r2212 <- datos

datos <- read_excel("Gaceta500/results2213.xls")
datos$codigo <- 2213
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "10-2"
r2213 <- datos

datos <- read_excel("Gaceta500/results2214.xls")
datos$codigo <- 2214
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "10-2"
r2214 <- datos

datos <- read_excel("Gaceta500/results2215.xls")
datos$codigo <- 2215
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "10-2"
r2215 <- datos

datos <- read_excel("Gaceta500/results2221.xls")
datos$codigo <- 2221
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "10-5"
r2221 <- datos

datos <- read_excel("Gaceta500/results2222.xls")
datos$codigo <- 2222
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "10-5"
r2222 <- datos

datos <- read_excel("Gaceta500/results2223.xls")
datos$codigo <- 2223
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "10-5"
r2223 <- datos

datos <- read_excel("Gaceta500/results2224.xls")
datos$codigo <- 2224
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "10-5"
r2224 <- datos

datos <- read_excel("Gaceta500/results2225.xls")
datos$codigo <- 2225
datos$n <- 500
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "10-5"
r2225 <- datos

datos <- read_excel("Gaceta500/results3111.xls")
datos$codigo <- 3111
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "2-2"
r3111 <- datos

datos <- read_excel("Gaceta500/results3112.xls")
datos$codigo <- 3112
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "2-2"
r3112 <- datos

datos <- read_excel("Gaceta500/results3113.xls")
datos$codigo <- 3113
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "2-2"
r3113 <- datos

datos <- read_excel("Gaceta500/results3114.xls")
datos$codigo <- 3114
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "2-2"
r3114 <- datos

datos <- read_excel("Gaceta500/results3115.xls")
datos$codigo <- 3115
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "2-2"
r3115 <- datos

datos <- read_excel("Gaceta500/results3121.xls")
datos$codigo <- 3121
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "2-5"
r3121 <- datos

datos <- read_excel("Gaceta500/results3122.xls")
datos$codigo <- 3122
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "2-5"
r3122 <- datos

datos <- read_excel("Gaceta500/results3123.xls")
datos$codigo <- 3123
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "2-5"
r3123 <- datos

datos <- read_excel("Gaceta500/results3124.xls")
datos$codigo <- 3124
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "2-5"
r3124 <- datos

datos <- read_excel("Gaceta500/results3125.xls")
datos$codigo <- 3125
datos$n <- 500
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "2-5"
r3125 <- datos

datos <- read_excel("Gaceta500/results3211.xls")
datos$codigo <- 3211
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "10-2"
r3211 <- datos

datos <- read_excel("Gaceta500/results3212.xls")
datos$codigo <- 3212
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "10-2"
r3212 <- datos

datos <- read_excel("Gaceta500/results3213.xls")
datos$codigo <- 3213
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "10-2"
r3213 <- datos

datos <- read_excel("Gaceta500/results3214.xls")
datos$codigo <- 3214
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "10-2"
r3214 <- datos

datos <- read_excel("Gaceta500/results3215.xls")
datos$codigo <- 3215
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "10-2"
r3215 <- datos

datos <- read_excel("Gaceta500/results3221.xls")
datos$codigo <- 3221
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "Gaceta"
datos$of <- "10-5"
r3221 <- datos

datos <- read_excel("Gaceta500/results3222.xls")
datos$codigo <- 3222
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "Gaceta"
datos$of <- "10-5"
r3222 <- datos

datos <- read_excel("Gaceta500/results3223.xls")
datos$codigo <- 3223
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "Gaceta"
datos$of <- "10-5"
r3223 <- datos

datos <- read_excel("Gaceta500/results3224.xls")
datos$codigo <- 3224
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "Gaceta"
datos$of <- "10-5"
r3224 <- datos

datos <- read_excel("Gaceta500/results3225.xls")
datos$codigo <- 3225
datos$n <- 500
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 1
datos$Población <- "Gaceta"
datos$of <- "10-5"
r3225 <- datos

Gaceta500a <- rbind(r1111, r1112, r1113, r1114, r1115)
Gaceta500b <- rbind(r1121, r1122, r1123, r1124, r1125)
Gaceta500c <- rbind(r1211, r1212, r1213, r1214, r1215)
Gaceta500d <- rbind(r1221, r1222, r1223, r1224, r1225)
Gaceta500e <- rbind(r2111, r2112, r2113, r2114, r2115)
Gaceta500f <- rbind(r2121, r2122, r2123, r2124, r2125)
Gaceta500g <- rbind(r2211, r2212, r2213, r2214, r2215)
Gaceta500h <- rbind(r2221, r2222, r2223, r2224, r2225)
Gaceta500i <- rbind(r3111, r3112, r3113, r3114, r3115)
Gaceta500j <- rbind(r3121, r3122, r3123, r3124, r3125)
Gaceta500k <- rbind(r3211, r3212, r3213, r3214, r3215)
Gaceta500l <- rbind(r3221, r3222, r3223, r3224, r3225)

Gaceta500m <- rbind(Gaceta500a, Gaceta500b, Gaceta500c, Gaceta500d)
Gaceta500n <- rbind(Gaceta500e, Gaceta500f, Gaceta500g, Gaceta500h)
Gaceta500o <- rbind(Gaceta500i, Gaceta500j, Gaceta500k, Gaceta500l)

Gaceta500 <- rbind(Gaceta500m, Gaceta500n, Gaceta500o)






