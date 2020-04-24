setwd('/home/alexggo/MEGA alegigo/13. Proyectos/DATOS PROYECTO QUINOA/Estadistica Alex y rawdata')

#Estadistica de rawTallos
tT<-read.csv("rawTallos.csv")
#Hay que calcular los Tukey para cada variable.
#Convertir en narrow form con columna llamada variable, y otra que sea el valor
library(tidyr)
tinytT<-gather(tT,"variable","value",c(4:13))
variable.list<-as.factor(unique(tinytT$variable))
#Subsetear por variable
list.tT<-list()
for (i in 1:10){
  list.tT[[i]]<-tinytT[tinytT$variable==variable.list[i],]
}
names(list.tT)<-variable.list

#Realizar un modelo
anovatT<-list()
for (i in 1:10){
  anovatT[[i]]<-lm(list.tT[[i]]$value~list.tT[[i]]$LOC*list.tT[[i]]$Variedad,na.action = na.exclude)
}

library(emmeans)
marginal<-list()

a<-anovatT[[1]]
emmeans(a,~LOC:Variedad)


for (i in 1:10){
  marginal[[i]] <- emmeans(anovatT[[i]], ~LOC:Variedad)
}

pairslist<-list()
for (i in 1:10){
  pairslist[[i]]<-pairs(marginal[[i]],adjust="tukey")
}

cldlist<-list()
for (i in 1:10){
  clslist[[i]]<-cld(marginal[[i]],alpha=0.05,Letters=letters,adjust="tukey")
}
names(clslist)<-variable.list

