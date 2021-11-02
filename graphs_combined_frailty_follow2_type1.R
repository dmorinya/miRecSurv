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
  datos <- read_excel("all_results_graph_type1.xls")
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
  
  datos$Model[datos$Model=="AG F"] <- "COMMON"
  datos$Model[datos$Model=="ComP (CP) F"] <- "SPECIFIC.CP"
  datos$Model[datos$Model=="ComP (GT) F"] <- "SPECIFIC.GT"
  
  # Para follow-up = 2 años:
  
  datos<-subset(datos, datos$f==2 & (datos$Model=="COMMON" |
                datos$Model=="SPECIFIC.CP" | datos$Model=="SPECIFIC.GT"))
  
  datos$of[datos$of=="2-2"] <- "B=2,F=2" 
  datos$of[datos$of=="2-5"] <- "B=2,F=5" 
  datos$of[datos$of=="10-2"] <- "B=10,F=2" 
  datos$of[datos$of=="10-5"] <- "B=10,F=5" 
  
  datos$o[datos$o==2] <- "Max(t) before t0 = 2 years"
  datos$o[datos$o==10] <- "Max(t) before t0 = 10 years" 
  datos$o <- factor(datos$o, levels=c("Max(t) before t0 = 2 years",
                                      "Max(t) before t0 = 10 years"))
  
    
  datos$of <- factor(datos$of, levels=c("B=2,F=2","B=2,F=5","B=10,F=2","B=10,F=5"))
  datos$Type1 <- datos$Type1*100
  
  #################Gráficos Por TIEMPOS old and follow, n=1000
  
  datos2<-datos[which(datos$n==1000),]
  datos2$n
  
  datos2_1<-melt(datos2[,c("Model","d","bef","Type1","Población","o","pop")],id=c("Model","d","bef","Población","o","pop"))
  p <- ggplot(data = datos2_1, aes(x = bef,
                                   y = value,
                                   color = Model,
                                   linetype = Model,
                                   shape = Model,
                                   fill = Model)) +
    facet_wrap(o~pop, ncol = 6)+
    geom_line(size=1) +
    scale_color_manual(values = c(COMMON= "azure4", SPECIFIC.CP="black", SPECIFIC.GT="black")) +
    scale_linetype_manual(values = c(COMMON= "dashed", SPECIFIC.CP= "solid", SPECIFIC.GT="dotted"))+
    scale_y_continuous(breaks = c(0,2,4,6,8,10))
    scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
  p<-p + labs(x="Proportion of subjects at risk before t0", y = "Type 1 error ocurrence (%)")
  p<-p+  geom_line(aes(color = Model))
  p<-p+ theme(legend.position="bottom")
  p<-p +  geom_hline(aes(yintercept = 5), colour = 'lightgrey')
p
#Guardar gráfico como tiff Width=1120 y Height=550



#################Gráficos Por TIEMPOS old and follow, n=500

datos2<-subset(datos, datos$n==500)
datos2$n

datos2_1<-melt(datos2[,c("Model","d","bef","Type1","Población","o","pop")],id=c("Model","d","bef","Población","o","pop"))
p <- ggplot(data = datos2_1, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6)+
  geom_line(size=1) +
  scale_color_manual(values = c(COMMON= "azure4", SPECIFIC.CP="black", SPECIFIC.GT="black")) +
  scale_linetype_manual(values = c(COMMON= "dashed", SPECIFIC.CP= "solid", SPECIFIC.GT="dotted"))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10))
scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="Proportion of subjects at risk before t0", y = "Type 1 error ocurrence (%)")
p<-p+  geom_line(aes(color = Model))
p<-p+ theme(legend.position="bottom")
p<-p +  geom_hline(aes(yintercept = 5), colour = 'lightgrey')
p
#Guardar gráfico como tiff Width=1120 y Height=550


#################Gráficos Por TIEMPOS old and follow, n=250

datos2<-subset(datos, datos$n==250)
datos2$n

datos2_1<-melt(datos2[,c("Model","d","bef","Type1","Población","o","pop")],id=c("Model","d","bef","Población","o","pop"))
p <- ggplot(data = datos2_1, aes(x = bef,
                                 y = value,
                                 color = Model,
                                 linetype = Model,
                                 shape = Model,
                                 fill = Model)) +
  facet_wrap(o~pop, ncol = 6)+
  geom_line(size=1) +
  scale_color_manual(values = c(COMMON= "azure4", SPECIFIC.CP="black", SPECIFIC.GT="black")) +
  scale_linetype_manual(values = c(COMMON= "dashed", SPECIFIC.CP= "solid", SPECIFIC.GT="dotted"))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10))
scale_x_continuous(breaks = c(0.1,0.3,0.5,0.75,1))
p<-p + labs(x="Proportion of subjects at risk before t0", y = "Type 1 error ocurrence (%)")
p<-p+  geom_line(aes(color = Model))
p<-p+ theme(legend.position="bottom")
p<-p +  geom_hline(aes(yintercept = 5), colour = 'lightgrey')
p
#Guardar gráfico como tiff Width=1120 y Height=550

