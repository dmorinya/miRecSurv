# Poblaciones Gaceta con priskb = 1
setwd("/home/dmorinya/Insync/dmorina@ub.edu/OneDrive Biz/Docència/Tesis/Gilma/Articles/3 Recurrent events/BMC MRM/Rev/R scripts")
library(survsim)
library(data.table)
library(COMPoissonReg)
library(compoisson)
library(survival)
library(doParallel)
library(MASS)
library(WriteXLS)

nCores     <- detectCores()
registerDoParallel(nCores)
source("genResultsDEF1bef100_new_frailty_type1.R")  # FRAILTY

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
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=730, old=730, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)

WriteXLS(results, "results/Gaceta/n250/results1115_type1.xls")



results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=1825, old=730, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)
WriteXLS(results, "results/Gaceta/n250/results1125_type1.xls")



results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=730, old=3650, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)

WriteXLS(results, "results/Gaceta/n250/results1215_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=1825, old=3650, 
                 d.ev=d.ev1, d.cens=d.cens1, b0.ev=b0.ev1, b0.cens=b0.cens1,
                 a.ev=a.ev1, a.cens=a.cens1, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)
WriteXLS(results, "results/Gaceta/n250/results1225_type1.xls")



########## GACETA: DEPENDENCIA MODERADA (CP)

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=730, old=730, 
                 d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                 a.ev=a.ev2, a.cens=a.cens2, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)
WriteXLS(results, "results/Gaceta/n250/results2115_type1.xls")



results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=1825, old=730, 
                 d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                 a.ev=a.ev2, a.cens=a.cens2, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)
WriteXLS(results, "results/Gaceta/n250/results2125_type1.xls")



results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=730, old=3650, 
                 d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                 a.ev=a.ev2, a.cens=a.cens2, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)
WriteXLS(results, "results/Gaceta/n250/results2215_type1.xls")



results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=1825, old=3650, 
                 d.ev=d.ev2, d.cens=d.cens2, b0.ev=b0.ev2, b0.cens=b0.cens2,
                 a.ev=a.ev2, a.cens=a.cens2, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)
WriteXLS(results, "results/Gaceta/n250/results2225_type1.xls")




########## GACETA: DEPENDENCIA ALTA (CP)

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=730, old=730, 
                 d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                 a.ev=a.ev3, a.cens=a.cens3, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)
WriteXLS(results, "results/Gaceta/n250/results3115_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=1825, old=730, 
                 d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                 a.ev=a.ev3, a.cens=a.cens3, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)
WriteXLS(results, "results/Gaceta/n250/results3125_type1.xls")



results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=730, old=3650, 
                 d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                 a.ev=a.ev3, a.cens=a.cens3, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)
WriteXLS(results, "results/Gaceta/n250/results3215_type1.xls")



results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1bef100_new_frailty(k, nm=250, bef=1, ft=1825, old=3650, 
                 d.ev=d.ev3, d.cens=d.cens3, b0.ev=b0.ev3, b0.cens=b0.cens3,
                 a.ev=a.ev3, a.cens=a.cens3, m=5)

#Para las coberturas y el LPI
results$AGx_ci_i<-results$gcoefAG.x-1.96*results$gsdAG.x
results$AGx_ci_s<-results$gcoefAG.x+1.96*results$gsdAG.x
results$ComPoisx_ci_i<-results$gcoefCOMPois.x-1.96*results$gsdCOMPois.x
results$ComPoisx_ci_s<-results$gcoefCOMPois.x+1.96*results$gsdCOMPois.x
results$ComPoisBx_ci_i<-results$gcoefCOMPoisB.x-1.96*results$gsdCOMPoisB.x
results$ComPoisBx_ci_s<-results$gcoefCOMPoisB.x+1.96*results$gsdCOMPoisB.x

#coberturas
results$AGx_cov<-ifelse(results$AGx_ci_i<=0 & 0<=results$AGx_ci_s,1 ,0)
results$ComPoisx_cov<-ifelse(results$ComPoisx_ci_i<=0 & 0<=results$ComPoisx_ci_s,1 ,0)
results$ComPoisBx_cov<-ifelse(results$ComPoisBx_ci_i<=0 & 0<=results$ComPoisBx_ci_s,1 ,0)

WriteXLS(results, "results/Gaceta/n250/results3225_type1.xls")

