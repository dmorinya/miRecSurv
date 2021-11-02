library(readxl)
library(WriteXLS)
setwd("~/OneDrive - Universitat Autònoma de Barcelona/Investigacio/01_Acabats/02_Tesis/10_Gilma_Hernandez/Articles/3/R/results/Type1")

results <- read_excel("SJWEH1000/results1111_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1111
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "2-2"
r1111 <- datos

results <- read_excel("SJWEH1000/results1112_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1112
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "2-2"
r1112 <- datos

results <- read_excel("SJWEH1000/results1113_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1113
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "2-2"
r1113 <- datos

results <- read_excel("SJWEH1000/results1114_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1114
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "2-2"
r1114 <- datos

results <- read_excel("SJWEH1000/results1115_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1115
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 2
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "2-2"
r1115 <- datos

results <- read_excel("SJWEH1000/results1121_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1121
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "2-5"
r1121 <- datos

results <- read_excel("SJWEH1000/results1122_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1122
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "2-5"
r1122 <- datos

results <- read_excel("SJWEH1000/results1123_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1123
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "2-5"
r1123 <- datos

results <- read_excel("SJWEH1000/results1124_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1124
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "2-5"
r1124 <- datos

results <- read_excel("SJWEH1000/results1125_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1125
datos$n <- 1000
datos$d <- "Low"
datos$o <- 2
datos$f <- 5
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "2-5"
r1125 <- datos

results <- read_excel("SJWEH1000/results1211_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1211
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "10-2"
r1211 <- datos

results <- read_excel("SJWEH1000/results1212_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1212
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "10-2"
r1212 <- datos

results <- read_excel("SJWEH1000/results1213_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1213
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "10-2"
r1213 <- datos

results <- read_excel("SJWEH1000/results1214_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1214
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "10-2"
r1214 <- datos

results <- read_excel("SJWEH1000/results1215_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1215
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 2
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "10-2"
r1215 <- datos

results <- read_excel("SJWEH1000/results1221_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1221
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "10-5"
r1221 <- datos

results <- read_excel("SJWEH1000/results1222_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1222
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "10-5"
r1222 <- datos

results <- read_excel("SJWEH1000/results1223_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1223
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "10-5"
r1223 <- datos

results <- read_excel("SJWEH1000/results1224_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1224
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "10-5"
r1224 <- datos

results <- read_excel("SJWEH1000/results1225_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 1225
datos$n <- 1000
datos$d <- "Low"
datos$o <- 10
datos$f <- 5
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "10-5"
r1225 <- datos

results <- read_excel("SJWEH1000/results2111_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2111
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "2-2"
r2111 <- datos

results <- read_excel("SJWEH1000/results2112_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2112
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "2-2"
r2112 <- datos

results <- read_excel("SJWEH1000/results2113_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2113
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "2-2"
r2113 <- datos

results <- read_excel("SJWEH1000/results2114_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2114
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "2-2"
r2114 <- datos

results <- read_excel("SJWEH1000/results2115_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2115
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 2
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "2-2"
r2115 <- datos

results <- read_excel("SJWEH1000/results2121_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2121
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "2-5"
r2121 <- datos

results <- read_excel("SJWEH1000/results2122_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2122
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "2-5"
r2122 <- datos

results <- read_excel("SJWEH1000/results2123_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2123
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "2-5"
r2123 <- datos

results <- read_excel("SJWEH1000/results2124_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2124
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "2-5"
r2124 <- datos

results <- read_excel("SJWEH1000/results2125_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2125
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 2
datos$f <- 5
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "2-5"
r2125 <- datos

results <- read_excel("SJWEH1000/results2211_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2211
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "10-2"
r2211 <- datos

results <- read_excel("SJWEH1000/results2212_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2212
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "10-2"
r2212 <- datos

results <- read_excel("SJWEH1000/results2213_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2213
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "10-2"
r2213 <- datos

results <- read_excel("SJWEH1000/results2214_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2214
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "10-2"
r2214 <- datos

results <- read_excel("SJWEH1000/results2215_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2215
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 2
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "10-2"
r2215 <- datos

results <- read_excel("SJWEH1000/results2221_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2221
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "10-5"
r2221 <- datos

results <- read_excel("SJWEH1000/results2222_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2222
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "10-5"
r2222 <- datos

results <- read_excel("SJWEH1000/results2223_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2223
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "10-5"
r2223 <- datos

results <- read_excel("SJWEH1000/results2224_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2224
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "10-5"
r2224 <- datos

results <- read_excel("SJWEH1000/results2225_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 2225
datos$n <- 1000
datos$d <- "Medium"
datos$o <- 10
datos$f <- 5
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "10-5"
r2225 <- datos

results <- read_excel("SJWEH1000/results3111_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3111
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "2-2"
r3111 <- datos

results <- read_excel("SJWEH1000/results3112_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3112
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "2-2"
r3112 <- datos

results <- read_excel("SJWEH1000/results3113_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3113
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "2-2"
r3113 <- datos

results <- read_excel("SJWEH1000/results3114_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3114
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "2-2"
r3114 <- datos

results <- read_excel("SJWEH1000/results3115_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3115
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 2
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "2-2"
r3115 <- datos

results <- read_excel("SJWEH1000/results3121_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3121
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "2-5"
r3121 <- datos

results <- read_excel("SJWEH1000/results3122_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3122
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "2-5"
r3122 <- datos

results <- read_excel("SJWEH1000/results3123_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3123
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "2-5"
r3123 <- datos

results <- read_excel("SJWEH1000/results3124_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3124
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "2-5"
r3124 <- datos

results <- read_excel("SJWEH1000/results3125_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3125
datos$n <- 1000
datos$d <- "High"
datos$o <- 2
datos$f <- 5
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "2-5"
r3125 <- datos

results <- read_excel("SJWEH1000/results3211_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3211
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "10-2"
r3211 <- datos

results <- read_excel("SJWEH1000/results3212_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3212
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "10-2"
r3212 <- datos

results <- read_excel("SJWEH1000/results3213_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3213
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "10-2"
r3213 <- datos

results <- read_excel("SJWEH1000/results3214_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3214
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "10-2"
r3214 <- datos

results <- read_excel("SJWEH1000/results3215_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3215
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 2
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "10-2"
r3215 <- datos

results <- read_excel("SJWEH1000/results3221_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3221
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.1
datos$Población <- "SJWEH"
datos$of <- "10-5"
r3221 <- datos

results <- read_excel("SJWEH1000/results3222_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3222
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.3
datos$Población <- "SJWEH"
datos$of <- "10-5"
r3222 <- datos

results <- read_excel("SJWEH1000/results3223_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3223
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.5
datos$Población <- "SJWEH"
datos$of <- "10-5"
r3223 <- datos

results <- read_excel("SJWEH1000/results3224_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3224
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 0.75
datos$Población <- "SJWEH"
datos$of <- "10-5"
r3224 <- datos

results <- read_excel("SJWEH1000/results3225_type1.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=1-mean(results$AGx_cov), COMPois=1-mean(results$ComPoisx_cov), COMPoisB=1-mean(results$ComPoisBx_cov))
datos <- data.frame(cbind(c0,c1))
colnames(datos)[1] <- "Model"
colnames(datos)[2] <- "Type1"
datos$codigo <- 3225
datos$n <- 1000
datos$d <- "High"
datos$o <- 10
datos$f <- 5
datos$bef <- 1
datos$Población <- "SJWEH"
datos$of <- "10-5"
r3225 <- datos

SJWEH1000a <- rbind(r1111, r1112, r1113, r1114, r1115)
SJWEH1000b <- rbind(r1121, r1122, r1123, r1124, r1125)
SJWEH1000c <- rbind(r1211, r1212, r1213, r1214, r1215)
SJWEH1000d <- rbind(r1221, r1222, r1223, r1224, r1225)
SJWEH1000e <- rbind(r2111, r2112, r2113, r2114, r2115)
SJWEH1000f <- rbind(r2121, r2122, r2123, r2124, r2125)
SJWEH1000g <- rbind(r2211, r2212, r2213, r2214, r2215)
SJWEH1000h <- rbind(r2221, r2222, r2223, r2224, r2225)
SJWEH1000i <- rbind(r3111, r3112, r3113, r3114, r3115)
SJWEH1000j <- rbind(r3121, r3122, r3123, r3124, r3125)
SJWEH1000k <- rbind(r3211, r3212, r3213, r3214, r3215)
SJWEH1000l <- rbind(r3221, r3222, r3223, r3224, r3225)

SJWEH1000m <- rbind(SJWEH1000a, SJWEH1000b, SJWEH1000c, SJWEH1000d)
SJWEH1000n <- rbind(SJWEH1000e, SJWEH1000f, SJWEH1000g, SJWEH1000h)
SJWEH1000o <- rbind(SJWEH1000i, SJWEH1000j, SJWEH1000k, SJWEH1000l)

SJWEH1000 <- rbind(SJWEH1000m, SJWEH1000n, SJWEH1000o)






