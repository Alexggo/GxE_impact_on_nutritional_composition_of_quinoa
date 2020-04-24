
###############  AMINOACIDS  ##################

setwd('C:/Users/alexg/OneDrive - Stony Brook University/13. Proyectos/DATOS PROYECTO QUINOA/Estadistica Alex y rawdata')

###AMINOACIDS###
rawamino<-read.csv(file='rawamino.csv')
rawamino$VARLOC<-factor(rawamino$VARLOC,
                         levels=c('Regalona-Chile','Salcedo-Chile',
                                  'Titicaca-Chile','Regalona-Spain',
                                  'Salcedo-Spain','Titicaca-Spain',
                                  'Salcedo-Peru'))

#Vector aminoacidos
aminovet<-levels(rawamino$AMINOACID)



#Subset por AMINOACID
subrawamino<-list()
for (i in 1:19){
  subrawamino[[i]]<-rawamino[rawamino$AMINOACID==aminovet[i],]
}
names(subrawamino)<-aminovet

#ANOVA 2 FACTORES CON INTERACCION. VAR*LOC
aov2amino<-list()
for (i in 1:19){
  aov2amino[[i]]<-aov(subrawamino[[i]]$CONCENTRATION~subrawamino[[i]]$VAR*subrawamino[[i]]$LOC)
}
names(aov2amino)<-aminovet

aov2aminosum<-list()
for (i in 1:19){
  aov2aminosum[[i]]<-summary(aov2amino[[i]])
}
names(aov2aminosum)<-aminovet

##Resultados del analisis ANOVA-2.
##Significativos por VAR AC.ASPARTICO,ARGININA,FENILALANINA,LISINA,METIONINA,PROLINA,TRIPTOFANO
##Significativos por LOC AC.ASPARTICO,ARGININA,ISOLEUCINA,LISINA,METIONINA,SERINA,TRIPTOFANO,VALINA
##Significativos por LOC y VAR AC.ASPARTICO,ALANINA,FENILALANINA,ISOLEUCINA,LEUCINA,LISINA,VALINA



##Analisis Post-Hoc. Tukey HSD
tukeyamino<-list()
for (i in 1:19){
  tukeyamino[[i]]<-TukeyHSD(aov2amino[[i]])
}
names(tukeyamino)<-aminovet

##Convertir en listas de dataframes los resultados de Tukey
dfVAR<-list()
for (i in 1:19){
  dfVAR[[i]]<-as.data.frame(tukeyamino[[i]][1])
  dfVAR[[i]]$name<-rownames(dfVAR[[i]])
  names(dfVAR[[i]])<-c('diff','lwr','upr','p.adj','name')
}
names(dfVAR)<-aminovet

dfLOC<-list()
for (i in 1:19){
  dfLOC[[i]]<-as.data.frame(tukeyamino[[i]][2])
  dfLOC[[i]]$name<-rownames(dfLOC[[i]])
  names(dfLOC[[i]])<-c('diff','lwr','upr','p.adj','name')
}
names(dfLOC)<-aminovet


dfVARLOC<-list()
for (i in 1:19){
  dfVARLOC[[i]]<-as.data.frame(tukeyamino[[i]][3])
  names(dfVARLOC[[i]])<-c('diff','lwr','upr','p.adj')
  dfVARLOC[[i]]<-na.omit(dfVARLOC[[i]])
  dfVARLOC[[i]]$name<-rownames(dfVARLOC[[i]])
  dfVARLOC[[i]]$AMINOACID<-rep(aminovet[i],21)
}
names(dfVARLOC)<-aminovet

##DF VAR
dfVAR<-rbind(dfVAR[[1]],dfVAR[[2]],dfVAR[[3]],dfVAR[[4]],dfVAR[[5]],
             dfVAR[[6]],dfVAR[[7]],dfVAR[[8]],dfVAR[[9]],dfVAR[[10]],
             dfVAR[[11]],dfVAR[[12]],dfVAR[[13]],dfVAR[[14]],dfVAR[[15]],
             dfVAR[[16]],dfVAR[[17]],dfVAR[[18]],dfVAR[[19]])
aminovetVAR<-vector()
for (i in 1:19){
  aminovetVAR<-c(aminovetVAR,rep(aminovet[i],3))
}

dfVAR$AMINOACID<-aminovetVAR

##DF LOC
dfLOC<-rbind(dfLOC[[1]],dfLOC[[2]],dfLOC[[3]],dfLOC[[4]],dfLOC[[5]],
              dfLOC[[6]],dfLOC[[7]],dfLOC[[8]],dfLOC[[9]],dfLOC[[10]],
              dfLOC[[11]],dfLOC[[12]],dfLOC[[13]],dfLOC[[14]],dfLOC[[15]],
              dfLOC[[16]],dfLOC[[17]],dfLOC[[18]],dfLOC[[19]])

aminovetLOC<-vector()
for (i in 1:19){
  aminovetLOC<-c(aminovetLOC,rep(aminovet[i],3))
}
dfLOC$AMINOACID<-aminovetLOC

##DF VARLOC
dfVARLOC<-rbind(dfVARLOC[[1]],dfVARLOC[[2]],dfVARLOC[[3]],dfVARLOC[[4]],dfVARLOC[[5]],
                 dfLOC[[6]],dfVARLOC[[7]],dfVARLOC[[8]],dfVARLOC[[9]],dfVARLOC[[10]],
                 dfVARLOC[[11]],dfVARLOC[[12]],dfVARLOC[[13]],dfVARLOC[[14]],dfVARLOC[[15]],
                 dfVARLOC[[16]],dfVARLOC[[17]],dfVARLOC[[18]],dfVARLOC[[19]])



###Summary de datos
library(Rmisc)
sumamino.ALL<-summarySE(rawamino,measurevar ='CONCENTRATION',groupvars = c('AMINOACID','VARLOC','VAR','LOC'))
sumamino.LOC<-summarySE(rawamino,measurevar ='CONCENTRATION',groupvars = c('AMINOACID','LOC'))
sumamino.VAR<-summarySE(rawamino,measurevar ='CONCENTRATION',groupvars = c('AMINOACID','VAR'))


#LISTAS, SOLO PARA VER PARA CADA AMINOACID EN UNA GRAFICA
#sumamino.LOC.list<-list()
#sumamino.VAR.list<-list()
#sumamino.VARLOC.list<-list()

#for (i in 1:19){
#  sumamino.VARLOC.list[[i]]<-summarySE(rawamino[rawamino$AMINOACID==aminovet[i],],measurevar ='CONCENTRATION',groupvars = c('AMINOACID','VARLOC','VAR','LOC'))
#  sumamino.VAR.list[[i]]<-summarySE(rawamino[rawamino$AMINOACID==aminovet[i],],measurevar ='CONCENTRATION',groupvars = c('AMINOACID','VAR'))
#  sumamino.LOC.list[[i]]<-summarySE(rawamino[rawamino$AMINOACID==aminovet[i],],measurevar ='CONCENTRATION',groupvars = c('AMINOACID','LOC'))
#}

#names(sumamino.LOC.list)<-aminovet
#names(sumamino.VAR.list)<-aminovet
#names(sumamino.VARLOC.list)<-aminovet

#############      GRAFICOS         ################
###Grafico general de puntos y barras de error para los dos factores
library(ggplot2)
pdf()
ggplot(sumamino.ALL,aes(x=VAR,y=CONCENTRATION,group=LOC,linetype=LOC))+
  geom_point()+
  geom_errorbar(aes(ymin=CONCENTRATION-se, ymax=CONCENTRATION+se), width=.1)+
  geom_line()+
  facet_wrap(~AMINOACID,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g)')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))

ggplot(sumamino.ALL,aes(x=LOC,y=CONCENTRATION,group=VAR,linetype=VAR))+
  geom_point()+
  geom_errorbar(aes(ymin=CONCENTRATION-se, ymax=CONCENTRATION+se), width=.1)+
  geom_line()+
  facet_wrap(~AMINOACID,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g)')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))

###Graficos de barras de CONCENTRATIONes
ggplot(sumamino.LOC,aes(x=LOC,y=CONCENTRATION,fill=LOC))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=CONCENTRATION-se, ymax=CONCENTRATION+se),width=0.1)+
  facet_wrap(~AMINOACID,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g)')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))


ggplot(sumamino.VAR,aes(x=VAR,y=CONCENTRATION,fill=VAR))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=CONCENTRATION-se, ymax=CONCENTRATION+se),width=0.1)+
  facet_wrap(~AMINOACID,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g)')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))


ggplot(sumamino.ALL,aes(x=VARLOC,y=CONCENTRATION,fill=VARLOC))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=CONCENTRATION-se, ymax=CONCENTRATION+se),width=0.1)+
  facet_wrap(~AMINOACID,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g)')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(axis.text.x=element_text(angle = 40, vjust = 0.5))

###Graficos de barras de diferencias de CONCENTRATIONes (Tukeys)
ggplot(dfLOC,aes(x=name,y=diff))+
  facet_wrap(~AMINOACID,scale='free')+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 25, vjust = 0.5))

ggplot(dfVAR,aes(x=name,y=diff))+
  facet_wrap(~AMINOACID,scale='free')+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 25, vjust = 0.5))
  
ggplot(dfVARLOC,aes(x=name,y=diff))+
  facet_wrap(~AMINOACID,scale='free')+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=45, vjust=0.5,hjust=0.5))


dev.off()






library(tidyr)
library(ggbiplot)
x<-rawamino
x<-spread(x,'AMINOACID','CONCENTRATION')
x<-na.omit(x)
mat<-x[,-c(1:4)]

mat.pca <- prcomp(mat, scale. = TRUE)

pdf()
class<-x$LOC
ggbiplot(mat.pca, obs.scale = 1, var.scale = 1, groups = class, ellipse = TRUE, circle = TRUE)+
  theme_classic()

class<-x$VAR
ggbiplot(mat.pca, obs.scale = 1, var.scale = 1, groups = class, ellipse = TRUE, circle = TRUE)+
  theme_classic()

class<-x$VARLOC
ggbiplot(mat.pca, obs.scale = 1, var.scale = 1, groups = class, ellipse = TRUE, circle = TRUE)+
  theme_classic()
dev.off()

