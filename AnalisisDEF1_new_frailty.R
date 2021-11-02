setwd("~/OneDrive - Universitat Autònoma de Barcelona/Investigacio/Actuals/Tesis/Gilma/Articles/3/R")
library(survsim)
library(data.table)
library(COMPoissonReg)
library(compoisson)
library(survival)
library(doParallel)
library(MASS)
library(WriteXLS)

nCores <- detectCores()
registerDoParallel(nCores)
#setwd("/home/dmorina/Documents/Docència/Tesis/Gilma/Articles/3 Recurrent events")
#source("scripts/genResultsDEF1_new.R")  # CLUSTER
source("scripts/genResultsDEF1_new_frailty.R") # FRAILTY

nsim <- 100


########## POBLACIONES GACETA #########

# Dependencia evento baja:
d.ev1 <- c('weibull','weibull','weibull')
b0.ev1 <- c(8.109, 7.927, 7.745) 
a.ev1 <- c(1,1,1)
d.cens1 <- c('weibull','weibull','weibull')
b0.cens1 <- c(8.909, 8.427, 7.995) 
a.cens1 <- c(1,1,1)
# Dependencia evento moderada:
d.ev2 <- c('weibull','weibull','weibull') 
b0.ev2 <- c(8.109, 7.703, 7.298) 
a.ev2 <- c(1,1,1)
d.cens2 <- c('weibull','weibull','weibull')
b0.cens2 <- c(8.909, 8.203, 7.548)
a.cens2 <- c(1,1,1)
# Dependencia evento alta:
d.ev3 <- c('weibull','weibull','weibull') 
b0.ev3 <- c(8.109, 7.193, 6.276) 
a.ev3 <- c(1,1,1)
d.cens3 <- c('weibull','weibull','weibull')
b0.cens3 <- c(8.909, 7.693, 6.526)
a.cens3 <- c(1,1,1)


########## GACETA: DEPENDENCIA BAJA (CP)

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
           genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=730, old=730, 
                          d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                          a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1111.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1111.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1111.ag, "results/Gaceta/results1111.xls")




results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=730, old=730, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1112.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1112.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1112.ag, "results/Gaceta/results1112.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=730, old=730, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1113.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1113.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1113.ag, "results/Gaceta/results1113.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=1825, old=730, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1121.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1121.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1121.ag, "results/Gaceta/results1121.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=1825, old=730, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1122.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1122.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1122.ag, "results/Gaceta/results1122.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=1825, old=730, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1123.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1123.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1123.ag, "results/Gaceta/results1123.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=730, old=3650, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1211.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1211.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1211.ag, "results/Gaceta/results1211.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=730, old=3650, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1212.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1212.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1212.ag, "results/Gaceta/results1212.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=730, old=3650, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1213.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1213.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1213.ag, "results/Gaceta/results1213.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=1825, old=3650, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1221.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1221.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1221.ag, "results/Gaceta/results1221.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=1825, old=3650, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1222.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1222.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1222.ag, "results/Gaceta/results1222.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=1825, old=3650, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)
WriteXLS(results, "results/Gaceta/res1223.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res1223.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res1223.ag, "results/Gaceta/results1223.xls")


########## GACETA: DEPENDENCIA MODERADA (CP)

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=730, old=730, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2111.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2111.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2111.ag, "results/Gaceta/results2111.xls")




results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=730, old=730, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2112.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2112.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2112.ag, "results/Gaceta/results2112.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=730, old=730, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2113.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2113.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2113.ag, "results/Gaceta/results2113.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=1825, old=730, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2121.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2121.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2121.ag, "results/Gaceta/results2121.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=1825, old=730, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2122.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2122.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2122.ag, "results/Gaceta/results2122.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=1825, old=730, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2123.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2123.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2123.ag, "results/Gaceta/results2123.xls")

source("scripts/genResultsDEF1_new_frailtyB.R") # FRAILTY
results <- foreach(k=1:101, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty_B(k, nm=1000, bef=.1, ft=730, old=3650, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2211.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2211.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2211.ag, "results/Gaceta/results2211.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=730, old=3650, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2212.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2212.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2212.ag, "results/Gaceta/results2212.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=730, old=3650, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2213.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2213.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2213.ag, "results/Gaceta/results2213.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=1825, old=3650, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2221.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2221.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2221.ag, "results/Gaceta/results2221.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=1825, old=3650, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2222.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2222.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2222.ag, "results/Gaceta/results2222.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=1825, old=3650, 
                     d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                     a.ev=a.ev2, a.cens=a.cens2, m=5)
WriteXLS(results, "results/Gaceta/res2223.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res2223.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res2223.ag, "results/Gaceta/results2223.xls")


########## GACETA: DEPENDENCIA ALTA (CP)

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=730, old=730, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3111.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3111.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3111.ag, "results/Gaceta/results3111.xls")




results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=730, old=730, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3112.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3112.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3112.ag, "results/Gaceta/results3112.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=730, old=730, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3113.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3113.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3113.ag, "results/Gaceta/results3113.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=1825, old=730, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3121.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3121.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3121.ag, "results/Gaceta/results3121.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=1825, old=730, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3122.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3122.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3122.ag, "results/Gaceta/results3122.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=1825, old=730, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3123.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3123.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3123.ag, "results/Gaceta/results3123.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=730, old=3650, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3211.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3211.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3211.ag, "results/Gaceta/results3211.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=730, old=3650, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3212.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3212.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3212.ag, "results/Gaceta/results3212.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=730, old=3650, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3213.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3213.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3213.ag, "results/Gaceta/results3213.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.1, ft=1825, old=3650, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3221.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3221.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3221.ag, "results/Gaceta/results3221.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.3, ft=1825, old=3650, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3222.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3222.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3222.ag, "results/Gaceta/results3222.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=1000, bef=.5, ft=1825, old=3650, 
                     d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                     a.ev=a.ev3, a.cens=a.cens3, m=5)
WriteXLS(results, "results/Gaceta/res3223.xls")
c0 <- rbind("AG F", "ComP (CP) F", "ComP (GT) F")
c1 <- rbind(AG=mean(results$gcoefAG.x), COMPois=mean(results$gcoefCOMPois.x), COMPoisB=mean(results$gcoefCOMPoisB.x))
c2 <- rbind(AG=mean(results$gcoefAG.x1), COMPois=mean(results$gcoefCOMPois.x1), COMPoisB=mean(results$gcoefCOMPoisB.x1))
c3 <- rbind(AG=mean(results$gcoefAG.x2), COMPois=mean(results$gcoefCOMPois.x2), COMPoisB=mean(results$gcoefCOMPoisB.x2))

bias25 <- ((c1-.25)/.25)*100
bias50 <- ((c2-.5)/.5)*100
bias75 <- ((c3-.75)/.75)*100

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

results$AGx1_ci_i<-results$gcoefAG.x1-1.96*results$gsdAG.x1
results$AGx1_ci_s<-results$gcoefAG.x1+1.96*results$gsdAG.x1
results$ComPoisx1_ci_i<-results$gcoefCOMPois.x1-1.96*results$gsdCOMPois.x1
results$ComPoisx1_ci_s<-results$gcoefCOMPois.x1+1.96*results$gsdCOMPois.x1
results$ComPoisBx1_ci_i<-results$gcoefCOMPoisB.x1-1.96*results$gsdCOMPoisB.x1
results$ComPoisBx1_ci_s<-results$gcoefCOMPoisB.x1+1.96*results$gsdCOMPoisB.x1

results$AGx2_ci_i<-results$gcoefAG.x2-1.96*results$gsdAG.x2
results$AGx2_ci_s<-results$gcoefAG.x2+1.96*results$gsdAG.x2
results$ComPoisx2_ci_i<-results$gcoefCOMPois.x2-1.96*results$gsdCOMPois.x2
results$ComPoisx2_ci_s<-results$gcoefCOMPois.x2+1.96*results$gsdCOMPois.x2
results$ComPoisBx2_ci_i<-results$gcoefCOMPoisB.x2-1.96*results$gsdCOMPoisB.x2
results$ComPoisBx2_ci_s<-results$gcoefCOMPoisB.x2+1.96*results$gsdCOMPoisB.x2

#LPI individuales
results$LPIxAG<-results$AGx_ci_s-results$AGx_ci_i
results$LPIx1AG<-results$AGx1_ci_s-results$AGx1_ci_i
results$LPIx2AG<-results$AGx2_ci_s-results$AGx2_ci_i
results$LPIxCOMPois<-results$ComPoisx_ci_s-results$ComPoisx_ci_i
results$LPIx1COMPois<-results$ComPoisx1_ci_s-results$ComPoisx1_ci_i
results$LPIx2COMPois<-results$ComPoisx2_ci_s-results$ComPoisx2_ci_i
results$LPIxCOMPoisB<-results$ComPoisBx_ci_s-results$ComPoisBx_ci_i
results$LPIx1COMPoisB<-results$ComPoisBx1_ci_s-results$ComPoisBx1_ci_i
results$LPIx2COMPoisB<-results$ComPoisBx2_ci_s-results$ComPoisBx2_ci_i


LPIx  <- rbind(AG=mean(results$LPIxAG), COMPois=mean(results$LPIxCOMPois), COMPoisB=mean(results$LPIxCOMPoisB))
LPIx1 <-rbind(AG=mean(results$LPIx1AG), COMPois=mean(results$LPIx1COMPois), COMPoisB=mean(results$LPIx1COMPoisB))
LPIx2 <-rbind(AG=mean(results$LPIx2AG), COMPois=mean(results$LPIx2COMPois), COMPoisB=mean(results$LPIx2COMPoisB))


#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0.25 & 0.25<=results$AGx_ci_s,1 ,0)
results$AGx1_cov<-ifelse(results$AGx1_ci_i<=0.5 & 0.5<=results$AGx1_ci_s,1 ,0)
results$AGx2_cov<-ifelse(results$AGx2_ci_i<=0.75 & 0.75<=results$AGx2_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0.25 & 0.25<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisx1_cov<-ifelse(results$ComPoisx1_ci_i<=0.5 & 0.5<=results$ComPoisx1_ci_s,1 ,0)
results$ComPoisx2_cov<-ifelse(results$ComPoisx2_ci_i<=0.75 & 0.75<=results$ComPoisx2_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0.25 & 0.25<=results$ComPoisBx_ci_s,1 ,0)
results$ComPoisBx1_cov<-ifelse(results$ComPoisBx1_ci_i<=0.5 & 0.5<=results$ComPoisBx1_ci_s,1 ,0)
results$ComPoisBx2_cov<-ifelse(results$ComPoisBx2_ci_i<=0.75 & 0.75<=results$ComPoisBx2_ci_s,1 ,0)

cov.x  <-rbind(AG=sum(results$AGx_cov)/nrow(results), COMPois=sum(results$ComPoisx_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx_cov)/nrow(results))
cov.x1 <-rbind(AG=sum(results$AGx1_cov)/nrow(results), COMPois=sum(results$ComPoisx1_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx1_cov)/nrow(results))
cov.x2 <- rbind(AG=sum(results$AGx2_cov)/nrow(results), COMPois=sum(results$ComPoisx2_cov)/nrow(results), COMPoisB=sum(results$ComPoisBx2_cov)/nrow(results))

mean.bias <- (abs(bias25)+abs(bias50)+abs(bias75)) / 3
mean.LPI <- (LPIx+LPIx1+LPIx2) / 3
mean.cob <- (cov.x+cov.x1+cov.x2)*100 / 3

res3223.ag <- data.frame(c0,c1,c2,c3,bias25,bias50,bias75, LPIx, LPIx1, LPIx2, cov.x, cov.x1, cov.x2, mean.bias, mean.LPI, mean.cob)
WriteXLS(res3223.ag, "results/Gaceta/results3223.xls")