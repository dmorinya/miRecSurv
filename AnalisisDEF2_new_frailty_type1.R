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
source("genResultsDEF1_new_frailty_type1.R") # FRAILTY

nsim <- 100


########## POBLACIONES SJWEH #########

# Respiratorio:
d.ev4 <- c('lnorm','llogistic','weibull') 
b0.ev4 <- c(7.195, 6.583, 6.678) 
a.ev4 <- c(1.498,.924,.923)
d.cens4 <- c('weibull','weibull','weibull') 
b0.cens4 <- c(7.315, 6.975, 6.712)
a.cens4 <- c(1.272,1.218,1.341)
# Musculoesquelético:
d.ev5 <- c('llogistic','weibull','lnorm')
b0.ev5 <- c(7.974, 7.109, 5.853) 
a.ev5 <- c(.836,.758,1.989)
d.cens5 <- c('weibull','weibull','weibull') 
b0.cens5 <- c(7.283, 6.900, 6.507)
a.cens5 <- c(1.332,1.156,1.498)
# Mental:
d.ev6 <- c('lnorm','lnorm','lnorm')
b0.ev6 <- c(8.924, 6.650, 6.696) 
a.ev6 <- c(1.545,2.399,2.246)
d.cens6 <- c('weibull','weibull','weibull') 
b0.cens6 <- c(7.287, 6.530, 6.212) 
a.cens6 <- c(1.352,1.177,1.991)



########## SJWEH: DEPENDENCIA BAJA (CP)

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
           try(genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=730, old=730, 
                          d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                          a.ev=a.ev4, a.cens=a.cens4, m=5))
results <- as.data.frame(apply(results, 2, as.numeric))

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
WriteXLS(results, "results/SJWEH/n250/results1111_type1.xls")




results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=730, old=730, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1112_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=730, old=730, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1113_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=730, old=730, 
                             d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                             a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1114_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=1825, old=730, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1121_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=1825, old=730, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1122_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=1825, old=730, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1123_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=1825, old=730, 
                             d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                             a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1124_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=730, old=3650, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1211_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=730, old=3650, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1212_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=730, old=3650, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1213_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=730, old=3650, 
                             d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                             a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1214_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=1825, old=3650, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1221_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=1825, old=3650, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1222_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=1825, old=3650, 
                 d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                 a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1223_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=1825, old=3650, 
                             d.ev=d.ev4, d.cens=d.cens4, b0.ev=b0.ev4, b0.cens=b0.cens4,
                             a.ev=a.ev4, a.cens=a.cens4, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results1224_type1.xls")


########## SJWEH: DEPENDENCIA MODERADA (CP)

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=730, old=730, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2111_type1.xls")




results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=730, old=730, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2112_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=730, old=730, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2113_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=730, old=730, 
                             d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                             a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2114_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=1825, old=730, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2121_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=1825, old=730, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2122_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=1825, old=730, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2123_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=1825, old=730, 
                             d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                             a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2124_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=730, old=3650, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2211_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=730, old=3650, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2212_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=730, old=3650, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2213_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=730, old=3650, 
                             d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                             a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2214_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=1825, old=3650, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2221_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=1825, old=3650, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2222_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=1825, old=3650, 
                     d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                     a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2223_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=1825, old=3650, 
                             d.ev=d.ev5, d.cens=d.cens5, b0.ev=b0.ev5, b0.cens=b0.cens5,
                             a.ev=a.ev5, a.cens=a.cens5, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results2224_type1.xls")


########## SJWEH: DEPENDENCIA ALTA (CP)

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=730, old=730, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3111_type1.xls")




results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=730, old=730, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3112_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=730, old=730, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3113_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  try(genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=730, old=730, 
                             d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                             a.ev=a.ev6, a.cens=a.cens6, m=5))
results <- as.data.frame(apply(results, 2, as.numeric))

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
WriteXLS(results, "results/SJWEH/n250/results3114_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=1825, old=730, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3121_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=1825, old=730, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3122_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=1825, old=730, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3123_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=1825, old=730, 
                             d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                             a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3124_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=730, old=3650, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3211_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=730, old=3650, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3212_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=730, old=3650, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3213_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=730, old=3650, 
                             d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                             a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3214_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.1, ft=1825, old=3650, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3221_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.3, ft=1825, old=3650, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3222_type1.xls")


results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.5, ft=1825, old=3650, 
                     d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                     a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3223_type1.xls")

results <- foreach(k=1:nsim, .combine=rbind) %dopar% 
  genResultsDEF1_new_frailty(k, nm=250, bef=.75, ft=1825, old=3650, 
                             d.ev=d.ev6, d.cens=d.cens6, b0.ev=b0.ev6, b0.cens=b0.cens6,
                             a.ev=a.ev6, a.cens=a.cens6, m=5)

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
WriteXLS(results, "results/SJWEH/n250/results3224_type1.xls")