
###############  ELEMENTS CUANTITATIVOS  ##################

setwd('C:/Users/alexg/OneDrive - Stony Brook University/13. Proyectos/DATOS PROYECTO QUINOA/Estadistica Alex y rawdata')

###ELEMENTS###
rawelem<-read.csv(file='rawelem.csv')
rawelem$VARLOC<-factor(rawelem$VARLOC,
                         levels=c('Regalona-Chile','Salcedo-Chile',
                                  'Titicaca-Chile','Regalona-Spain',
                                  'Salcedo-Spain','Titicaca-Spain',
                                  'Salcedo-Peru'))

#Vector ELEMENTs
elemvet<-levels(rawelem$ELEMENT)

#Subset por ELEMENT
subrawelem<-list()
for (i in 1:7){
  subrawelem[[i]]<-rawelem[rawelem$ELEMENT==elemvet[i],]
}
names(subrawelem)<-elemvet

#ANOVA 2 FACTORES CON INTERACCION. VAR*LOC
aov2elem<-list()
for (i in 1:7){
  aov2elem[[i]]<-aov(subrawelem[[i]]$CONCENTRATION~subrawelem[[i]]$VAR*subrawelem[[i]]$LOC)
}
names(aov2elem)<-elemvet

aov2elemsum<-list()
for (i in 1:7){
  aov2elemsum[[i]]<-summary(aov2elem[[i]])
}
names(aov2elemsum)<-elemvet
##Resultados del analisis ANOVA-2.
#Diferencias de medias de CONCENTRATION significativas
#Por VAR, todos son (p-valor<0.0001), pero el Zinc (p-valor=0.03901)
#Por LOC, todos son (p-valor<0.0001),pero el potasio no (p-valor=0.152)
#Por Var:LOC, todos son (p-valor<0.0001), pero el zinc (p-valor=0.00541)


##Analisis Post-Hoc. Tukey HSD
tukeyelem<-list()
for (i in 1:7){
  tukeyelem[[i]]<-TukeyHSD(aov2elem[[i]])
}
names(tukeyelem)<-elemvet

##Convertir en listas de dataframes los resultados de Tukey
dfVAR<-list()
for (i in 1:7){
  dfVAR[[i]]<-as.data.frame(tukeyelem[[i]][1])
  dfVAR[[i]]$name<-rownames(dfVAR[[i]])
  names(dfVAR[[i]])<-c('diff','lwr','upr','p.adj','name')
}
names(dfVAR)<-elemvet

dfLOC<-list()
for (i in 1:7){
  dfLOC[[i]]<-as.data.frame(tukeyelem[[i]][2])
  dfLOC[[i]]$name<-rownames(dfLOC[[i]])
  names(dfLOC[[i]])<-c('diff','lwr','upr','p.adj','name')
}
names(dfLOC)<-elemvet

dfVARLOC<-list()
for (i in 1:7){
  dfVARLOC[[i]]<-as.data.frame(tukeyelem[[i]][3])
  names(dfVARLOC[[i]])<-c('diff','lwr','upr','p.adj')
  dfVARLOC[[i]]<-na.omit(dfVARLOC[[i]])
}
names(dfVARLOC)<-elemvet

namesVARLOC<-rownames(dfVARLOC[[1]])

for (i in 1:7){
  dfVARLOC[[i]]$name<-namesVARLOC
}


##DF VAR
dfVAR<-rbind(dfVAR[[1]],dfVAR[[2]],dfVAR[[3]],dfVAR[[4]],dfVAR[[5]],
                  dfVAR[[6]],dfVAR[[7]])
elemvetVAR<-vector()
for (i in 1:7){
  elemvetVAR<-c(elemvetVAR,rep(elemvet[i],3))
}
dfVAR$ELEMENT<-elemvetVAR

##DF LOC
dfLOC<-rbind(dfLOC[[1]],dfLOC[[2]],dfLOC[[3]],dfLOC[[4]],dfLOC[[5]],
                  dfLOC[[6]],dfLOC[[7]])

elemvetLOC<-vector()
for (i in 1:7){
  elemvetLOC<-c(elemvetLOC,rep(elemvet[i],3))
}
dfLOC$ELEMENT<-elemvetLOC

##DF VARLOC
dfVARLOC<-rbind(dfVARLOC[[1]],dfVARLOC[[2]],dfVARLOC[[3]],dfVARLOC[[4]],dfVARLOC[[5]],
      dfVARLOC[[6]],dfVARLOC[[7]])

elemvetVARLOC<-vector()
for (i in 1:7){
  elemvetVARLOC<-c(elemvetVARLOC,rep(elemvet[i],21))
}
dfVARLOC$ELEMENT<-elemvetVARLOC



###Summary de datos
library(Rmisc)
sumelem.ALL<-summarySE(rawelem,measurevar ='CONCENTRATION',groupvars = c('ELEMENT','VARLOC','VAR','LOC'))
sumelem.LOC<-summarySE(rawelem,measurevar ='CONCENTRATION',groupvars = c('ELEMENT','LOC'))
sumelem.VAR<-summarySE(rawelem,measurevar ='CONCENTRATION',groupvars = c('ELEMENT','VAR'))


#LISTAS, SOLO PARA VER PARA CADA ELEMENT EN UNA GRAFICA
#sumelem.LOC.list<-list()
#sumelem.VAR.list<-list()
#sumelem.VARLOC.list<-list()

#for (i in 1:7){
#  sumelem.VARLOC.list[[i]]<-summarySE(rawelem[rawelem$ELEMENT==elemvet[i],],measurevar ='CONCENTRATION',groupvars = c('ELEMENT','VARLOC','VAR','LOC'))
#  sumelem.VAR.list[[i]]<-summarySE(rawelem[rawelem$ELEMENT==elemvet[i],],measurevar ='CONCENTRATION',groupvars = c('ELEMENT','VAR'))
#  sumelem.LOC.list[[i]]<-summarySE(rawelem[rawelem$ELEMENT==elemvet[i],],measurevar ='CONCENTRATION',groupvars = c('ELEMENT','LOC'))
#}

#names(sumelem.LOC.list)<-elemvet
#names(sumelem.VAR.list)<-elemvet
#names(sumelem.VARLOC.list)<-elemvet

#############      GRAFICOS         ################
###Grafico general de puntos y barras de error para los dos factores
library(ggplot2)
pdf()
ggplot(sumelem.ALL,aes(x=VAR,y=CONCENTRATION,group=LOC,linetype=LOC))+
  geom_point()+
  geom_errorbar(aes(ymin=CONCENTRATION-se, ymax=CONCENTRATION+se), width=.1)+
  geom_line()+
  facet_wrap(~ELEMENT,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g')

ggplot(sumelem.ALL,aes(x=LOC,y=CONCENTRATION,group=VAR,linetype=VAR))+
  geom_point()+
  geom_errorbar(aes(ymin=CONCENTRATION-se, ymax=CONCENTRATION+se), width=.1)+
  geom_line()+
  facet_wrap(~ELEMENT,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g')

###Graficos de barras de CONCENTRATIONes
ggplot(sumelem.LOC,aes(x=LOC,y=CONCENTRATION,fill=LOC))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=CONCENTRATION-ci, ymax=CONCENTRATION+ci),width=0.1)+
  facet_wrap(~ELEMENT,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g)')+
  scale_fill_grey()+
  theme(legend.position='bottom')

ggplot(sumelem.VAR,aes(x=VAR,y=CONCENTRATION,fill=VAR))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=CONCENTRATION-ci, ymax=CONCENTRATION+ci),width=0.1)+
  facet_wrap(~ELEMENT,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g)')+
  scale_fill_grey()+
  theme(legend.position='bottom')

ggplot(sumelem.ALL,aes(x=VARLOC,y=CONCENTRATION,fill=VARLOC))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=CONCENTRATION-ci, ymax=CONCENTRATION+ci),width=0.1)+
  facet_wrap(~ELEMENT,scales='free')+
  theme_classic()+
  ylab('CONCENTRATION (ug/g)')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

###Graficos de barras de diferencias de CONCENTRATIONes (Tukeys)
ggplot(dfLOC,aes(x=name,y=diff))+
  facet_wrap(~ELEMENT,scale='free')+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 25, vjust = 0.5))

ggplot(dfVAR,aes(x=name,y=diff))+
  facet_wrap(~ELEMENT,scale='free')+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 25, vjust = 0.5))
  
ggplot(dfVARLOC,aes(x=name,y=diff))+
  facet_wrap(~ELEMENT,scale='free')+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=40, vjust=0.5,hjust=0.5))
dev.off()

library(tidyr)
library(ggbiplot)
x<-rawelem[,-7]
x<-spread(x,'ELEMENT','CONCENTRATION')
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




# Library
library(fmsb)
library(tidyr)

x<-sumelem.ALL[,c(1,2,3,4,6)]
x<-spread(x,'ELEMENT','CONCENTRATION')


#Maximos y minimos
max1<-vector()
min1<-vector()
for (i in 4:10){
  max1[i-3]<-max(x[,c(i)])
  min1[i-3]<-min(x[,c(i)])
}

rownames(x)<-x$VARLOC
y<-x[,c(4:10)]
z<-rbind(max1,min1,y)

#==================
# Plot 1: Default radar chart proposed by the library:
radarchart(z)


##OUTPUT
pdf()

#Hacemos tres radar chart para cada LOC

z.chile<-z[c(1:5),]
z.peru<-z[c(1,2,9),]
z.esp<-z[c(1,2,6,7,8),]

#Negro es Regalona, Rojo Salcedo, Titicaca es Verde
radarchart(z.chile,title = 'Chile')

#Rojo Salcedo
radarchart(z.peru, title = 'Peru',pcol='red')

#Negro es Regalona, Rojo Salcedo, Titicaca Verde
radarchart(z.esp, title = 'Spain')

par(mfrow=c(1,1))

#Hacemos tres radar chart para cada LOC
z.reg<-z[c(1,2,3,6),]
z.sal<-z[c(1,2,4,7,9),]
z.tit<-z[c(1,2,5,8),]

#Rojo Chile, verde Spain
radarchart(z.reg,title = 'Regalona',pcol=c('red','green'))

#Negro Chile, verde Spain, rojo Peru
radarchart(z.sal, title = 'Salcedo',pcol=c('black','green','red'))

#Verde Spain, Rojo Chile
radarchart(z.tit, title = 'Titicaca',pcol=c('red','green'))

dev.off()






