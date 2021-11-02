  require(xlsx)
  require(readxl)
  require(ggplot2)
  require(png)
  require(lubridate)
  require(stringr)
  require(lattice)
  require(EnvStats)
  require(gridExtra)
  require(ggridges)
  library( reshape2 )
  library(car)
  
  #setwd("~/OneDrive - Universitat Autònoma de Barcelona/Investigacio/Actuals/Tesis/Gilma/Articles/3/R")
  
  #####Gráficos Simulación modelos
  datos <- read_excel("results/all_results_graph.xls")
  names(datos)<-gsub(' ','',
                     gsub('\n','',
                          gsub('\r','',
                               gsub('[()+/+.]','',names(datos)))))
  
  names(datos)
  
  datos$d <- factor(datos$d, levels=c("Low","Medium","High"))
  datos$pop[datos$Población=="Gaceta" & datos$d=="Low"] <- "Population 1"
  datos$pop[datos$Población=="Gaceta" & datos$d=="Medium"] <- "Population 2"
  datos$pop[datos$Población=="Gaceta" & datos$d=="High"] <- "Population 3"
  datos$pop[datos$Población=="SJWEH" & datos$d=="Low"] <- "Population 4"
  datos$pop[datos$Población=="SJWEH" & datos$d=="Medium"] <- "Population 5"
  datos$pop[datos$Población=="SJWEH" & datos$d=="High"] <- "Population 6"
  
  #datos$Model[datos$Model=="F.CHM"] <- "CHFM.strata"
  #datos$Model[datos$Model=="F.SHMI.CP"] <- "SHFMI.CP"
  #datos$Model[datos$Model=="F.SHMI.GT"] <- "SHFMI.GT"
  
  # Para follow-up = 5 años:
  
  datos<-subset(datos, datos$f==5 & (datos$Model=="CHFM.strata" |
                datos$Model=="SHFMI.CP" | datos$Model=="SHFMI.GT"))
  
  datos$of[datos$of=="2-2"] <- "B=2,F=2" 
  datos$of[datos$of=="2-5"] <- "B=2,F=5" 
  datos$of[datos$of=="10-2"] <- "B=10,F=2" 
  datos$of[datos$of=="10-5"] <- "B=10,F=5" 
  
  datos$o[datos$o==2] <- "Max(t) before t0 = 2 years"
  datos$o[datos$o==10] <- "Max(t) before t0 = 10 years" 
  datos$o <- factor(datos$o, levels=c("Max(t) before t0 = 2 years",
                                      "Max(t) before t0 = 10 years"))
  
    
  datos$of <- factor(datos$of, levels=c("B=2,F=2","B=2,F=5","B=10,F=2","B=10,F=5"))
  
  #################Gráficos Por TIEMPOS old and follow, n=1000
  
  datos2<-subset(datos, datos$n==1000)
  datos2$n
  
  datos2_1<-melt(datos2[,c("Model","d","bef","meanbias","Población","o","pop")],id=c("Model","d","bef","Población","o","pop"))
  p <- ggplot(data = datos2_1, aes(x = bef,
                                   y = value,
                                   color = Model,
                                   linetype = Model,
                                   shape = Model,
                                   fill = Model)) +
    facet_wrap(o~pop, ncol = 6, scales = "free_y")+
    geom_line(size=1) +
    scale_color_manual(values = c(CHFM.strata= "azure4", SHFMI.CP="black", SHFMI.GT="black")) +
    scale_linetype_manual(values = c(CHFM.strata= "dashed", SHFMI.CP= "solid", SHFMI.GT="dotted"))+
    #scale_y_continuous(breaks = c(0,10,20,30,40,50,60))
    scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
  p<-p + labs(x="% subjects at risk before t0", y = "Mean Bias")
  p<-p+  geom_line(aes(color = Model))
  p<-p+ theme(legend.position="bottom")
  #p<-p +  geom_hline(aes(yintercept = 10), colour = 'red')
p
#Guardar gráfico como tiff Width=1120 y Height=550

datos2_2<-melt(datos2[,c("Model","d","bef","meanLPI","Población","o", "pop")],id=c("Model","d","bef","Población","o", "pop"))
p <- ggplot(data = datos2_2, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6, scales = "free_y")+
  geom_line(size=1) +
  scale_color_manual(values = c(CHFM.strata= "azure4", SHFMI.CP="black", SHFMI.GT="black")) +
  scale_linetype_manual(values = c(CHFM.strata= "dashed", SHFMI.CP= "solid", SHFMI.GT="dotted"))+
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="% subjects at risk before t0", y = "Average length of the 95% confidence interval")
p<-p+  geom_line(aes(color = Model))
p<-p+ theme(legend.position="bottom")
p
#Guardar gráfico como tiff Width=1120 y Height=550

datos2_3<-melt(datos2[,c("Model","d","bef","meancob","Población","o","pop")],id=c("Model","d","bef","Población","o","pop"))
p <- ggplot(data = datos2_3, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6, scales = "free_y")+
  geom_line(size=1) +
  scale_color_manual(values = c(CHFM.strata= "azure4", SHFMI.CP="black", SHFMI.GT="black")) +
  scale_linetype_manual(values = c(CHFM.strata= "dashed", SHFMI.CP= "solid", SHFMI.GT="dotted"))+
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="% subjects at risk before t0", y = "Coverage")
p<-p+  geom_line(aes(color = Model))
p<-p +  geom_hline(aes(yintercept = 95), colour = 'grey')
p<-p+ theme(legend.position="bottom")
p
#Guardar gráfico como tiff Width=1120 y Height=550



#################Gráficos Por TIEMPOS old and follow, n=500

datos2<-subset(datos, datos$n==500)
datos2$n

datos2_1<-melt(datos2[,c("Model","d","bef","meanbias","Población","o","pop")],id=c("Model","d","bef","Población","o","pop"))
p <- ggplot(data = datos2_1, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6, scales = "free_y")+
  geom_line(size=1) +
  scale_color_manual(values = c(CHFM.strata= "red", SHFMI.CP="green4", SHFMI.GT="green4")) +
  scale_linetype_manual(values = c(CHFM.strata= "dashed", SHFMI.CP= "solid", SHFMI.GT="dotted"))+
  #scale_y_continuous(breaks = c(0,10,20,30,40,50,60))
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="% subjects at risk before t0", y = "Mean Bias")
p<-p+  geom_line(aes(color = Model))
p<-p+ theme(legend.position="bottom")
#p<-p +  geom_hline(aes(yintercept = 10), colour = 'red')
p
#Guardar gráfico como tiff Width=1120 y Height=550

datos2_2<-melt(datos2[,c("Model","d","bef","meanLPI","Población","o", "pop")],id=c("Model","d","bef","Población","o", "pop"))
p <- ggplot(data = datos2_2, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6, scales = "free_y")+
  geom_line(size=1) +
  scale_color_manual(values = c(CHFM.strata= "red", SHFMI.CP="green4", SHFMI.GT="green4")) +
  scale_linetype_manual(values = c(CHFM.strata= "dashed", SHFMI.CP= "solid", SHFMI.GT="dotted"))+
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="% subjects at risk before t0", y = "Average length of the 95% confidence interval")
p<-p+  geom_line(aes(color = Model))
p<-p+ theme(legend.position="bottom")
p
#Guardar gráfico como tiff Width=1120 y Height=550

datos2_3<-melt(datos2[,c("Model","d","bef","meancob","Población","o","pop")],id=c("Model","d","bef","Población","o","pop"))
p <- ggplot(data = datos2_3, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6, scales = "free_y")+
  geom_line(size=1) +
  scale_color_manual(values = c(CHFM.strata= "red", SHFMI.CP="green4", SHFMI.GT="green4")) +
  scale_linetype_manual(values = c(CHFM.strata= "dashed", SHFMI.CP= "solid", SHFMI.GT="dotted"))+
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="% subjects at risk before t0", y = "Coverage")
p<-p+  geom_line(aes(color = Model))
p<-p +  geom_hline(aes(yintercept = 95), colour = 'grey')
p<-p+ theme(legend.position="bottom")
p
#Guardar gráfico como tiff Width=1120 y Height=550




#################Gráficos Por TIEMPOS old and follow, n=250

datos2<-subset(datos, datos$n==250)
datos2$n

datos2_1<-melt(datos2[,c("Model","d","bef","meanbias","Población","o","pop")],id=c("Model","d","bef","Población","o","pop"))
p <- ggplot(data = datos2_1, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6, scales = "free_y")+
  geom_line(size=1) +
  scale_color_manual(values = c(CHFM.strata= "red", SHFMI.CP="green4", SHFMI.GT="green4")) +
  scale_linetype_manual(values = c(CHFM.strata= "dashed", SHFMI.CP= "solid", SHFMI.GT="dotted"))+
  #scale_y_continuous(breaks = c(0,10,20,30,40,50,60))
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="% subjects at risk before t0", y = "Mean Bias")
p<-p+  geom_line(aes(color = Model))
p<-p+ theme(legend.position="bottom")
#p<-p +  geom_hline(aes(yintercept = 10), colour = 'red')
p
#Guardar gráfico como tiff Width=1120 y Height=550

datos2_2<-melt(datos2[,c("Model","d","bef","meanLPI","Población","o", "pop")],id=c("Model","d","bef","Población","o", "pop"))
p <- ggplot(data = datos2_2, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6, scales = "free_y")+
  geom_line(size=1) +
  scale_color_manual(values = c(CHFM.strata= "red", SHFMI.CP="green4", SHFMI.GT="green4")) +
  scale_linetype_manual(values = c(CHFM.strata= "dashed", SHFMI.CP= "solid", SHFMI.GT="dotted"))+
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="% subjects at risk before t0", y = "Average length of the 95% confidence interval")
p<-p+  geom_line(aes(color = Model))
p<-p+ theme(legend.position="bottom")
p
#Guardar gráfico como tiff Width=1120 y Height=550

datos2_3<-melt(datos2[,c("Model","d","bef","meancob","Población","o","pop")],id=c("Model","d","bef","Población","o","pop"))
p <- ggplot(data = datos2_3, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6, scales = "free_y")+
  geom_line(size=1) +
  scale_color_manual(values = c(CHFM.strata= "red", SHFMI.CP="green4", SHFMI.GT="green4")) +
  scale_linetype_manual(values = c(CHFM.strata= "dashed", SHFMI.CP= "solid", SHFMI.GT="dotted"))+
  scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="% subjects at risk before t0", y = "Coverage")
p<-p+  geom_line(aes(color = Model))
p<-p +  geom_hline(aes(yintercept = 95), colour = 'grey')
p<-p+ theme(legend.position="bottom")
p
#Guardar gráfico como tiff Width=1120 y Height=550

