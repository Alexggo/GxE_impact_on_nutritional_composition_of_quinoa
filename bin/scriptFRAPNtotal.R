setwd('C:/Users/alexg/OneDrive - Stony Brook University/13. Proyectos/DATOS PROYECTO QUINOA/Estadistica Alex y rawdata')
rawFRAPN<-read.csv('rawFRAPN.csv',dec = ",")
rawFRAPN$VARLOC<-factor(rawFRAPN$VARLOC,
                        levels=c('Regalona-Chile','Salcedo-Chile',
                                 'Titicaca-Chile','Regalona-Spain',
                                 'Salcedo-Spain','Titicaca-Spain',
                                 'Salcedo-Peru'))

for (i in 5:13){
  rawFRAPN[,i]<-as.numeric(rawFRAPN[,i])
}


rawFRAPN1<-rawFRAPN[,c(2,3,4,7,13)]
colnames(rawFRAPN1)<-c('VAR','LOC','VARLOC','PROT.PERCENT','FRAP')

rawNtotal<-rawFRAPN1[,c(1,2,3,4)]  #N Total
rawFRAP<-rawFRAPN1[,c(1,2,3,5)]    #FRAP
######################################################
##Fin de preparacion de datos

#Anova2factores (VAR*LOC)
anovaNtotal<-aov(rawNtotal$PROT.PERCENT~rawNtotal$VAR*rawNtotal$LOC)
anovaFRAP<-aov(rawFRAP$FRAP~rawFRAP$VAR*rawFRAP$LOC)

summary(anovaNtotal) #La VAR y el LOC tienen un efecto pero no la interaccion
summary(anovaFRAP) #La VAR, el LOC y la interaccion tienen un efecto

#tukeys
TuNtotal<-TukeyHSD(anovaNtotal)
TuFRAP<-TukeyHSD(anovaFRAP)

#DF tukeys Ntotal
dfN.VAR<-as.data.frame(TuNtotal$`rawNtotal$VAR`)
dfN.VAR$name<-rownames(dfN.VAR)
dfN.LOC<- as.data.frame(TuNtotal$`rawNtotal$LOC`)
dfN.LOC$name<-rownames(dfN.LOC)
dfN.VARLOC<- as.data.frame(na.omit(TuNtotal$`rawNtotal$VAR:rawNtotal$LOC`))
dfN.VARLOC$name<-rownames(dfN.VARLOC)

#DF tukeys FRAP
dfFRAP.VAR<- as.data.frame(TuFRAP$`rawFRAP$VAR`)
dfFRAP.VAR$name<-rownames(dfFRAP.VAR)
dfFRAP.LOC<- as.data.frame(TuFRAP$`rawFRAP$LOC`)
dfFRAP.LOC$name<-rownames(dfFRAP.LOC)
dfFRAP.VARLOC<- as.data.frame(na.omit(TuFRAP$`rawFRAP$VAR:rawFRAP$LOC`))
dfFRAP.VARLOC$name<-rownames(dfFRAP.VARLOC)


####Resumenes de datos, medias, sd, se, ci
library(Rmisc)
sumN.VARLOC<-summarySE(rawNtotal,measurevar = 'PROT.PERCENT',groupvars = c('VARLOC','VAR','LOC'))
sumN.LOC<-summarySE(rawNtotal,measurevar = 'PROT.PERCENT',groupvars = 'LOC')
sumN.VAR<-summarySE(rawNtotal,measurevar = 'PROT.PERCENT',groupvars = 'VAR')

sumFRAP.VARLOC<-summarySE(rawFRAP,measurevar = 'FRAP',groupvars = c('VARLOC','VAR','LOC'))
sumFRAP.LOC<-summarySE(rawFRAP,measurevar = 'FRAP',groupvars = 'LOC')
sumFRAP.VAR<-summarySE(rawFRAP,measurevar = 'FRAP',groupvars = 'VAR')

#############      GRAFICOS         ################
###Grafico general de puntos y barras de error para los dos factores
#Unidades de la variable FRAP mmol. ET100g 

library(ggplot2)
pdf()
ggplot(sumN.VARLOC,aes(x=VAR,y=PROT.PERCENT,group=LOC,linetype=LOC))+
  geom_point()+
  geom_errorbar(aes(ymin=PROT.PERCENT-se, ymax=PROT.PERCENT+se), width=.1)+
  geom_line()+
  theme_classic()+
  ylab('Prot percent')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))

ggplot(sumFRAP.VARLOC,aes(x=VAR,y=FRAP,group=LOC,linetype=LOC))+
  geom_point()+
  geom_errorbar(aes(ymin=FRAP-se, ymax=FRAP+se), width=.1)+
  geom_line()+
  theme_classic()+
  ylab('FRAP')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))



ggplot(sumN.VARLOC,aes(x=LOC,y=PROT.PERCENT,group=VAR,linetype=VAR))+
  geom_point()+
  geom_errorbar(aes(ymin=PROT.PERCENT-se, ymax=PROT.PERCENT+se), width=.1)+
  geom_line()+
  theme_classic()+
  ylab('Prot percent')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))

ggplot(sumFRAP.VARLOC,aes(x=LOC,y=FRAP,group=VAR,linetype=VAR))+
  geom_point()+
  geom_errorbar(aes(ymin=FRAP-se, ymax=FRAP+se), width=.1)+
  geom_line()+
  theme_classic()+
  ylab('FRAP')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))




###Graficos de barras de concentraciones
ggplot(sumN.LOC ,aes(x=LOC,y=PROT.PERCENT,fill=LOC))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=PROT.PERCENT-ci, ymax=PROT.PERCENT+ci),width=0.1)+
  theme_classic()+
  ylab('PROT.PERCENT')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))

ggplot(sumFRAP.LOC ,aes(x=LOC,y=FRAP,fill=LOC))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=FRAP-ci, ymax=FRAP+ci),width=0.1)+
  theme_classic()+
  ylab('FRAP')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))


ggplot(sumN.VAR,aes(x=VAR,y=PROT.PERCENT,fill=VAR))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=PROT.PERCENT-ci, ymax=PROT.PERCENT+ci),width=0.1)+
  theme_classic()+
  ylab('Prot Percent')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))

ggplot(sumFRAP.VAR,aes(x=VAR,y=FRAP,fill=VAR))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=FRAP-ci, ymax=FRAP+ci),width=0.1)+
  theme_classic()+
  ylab('FRAP')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=25, vjust=0.5,hjust=0.5))


ggplot(sumN.VARLOC,aes(x=VARLOC,y=PROT.PERCENT,fill=VARLOC))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=PROT.PERCENT-ci, ymax=PROT.PERCENT+ci),width=0.1)+
  theme_classic()+
  ylab('PROT.PERCENT')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(axis.text.x=element_text(angle = 40, vjust = 0.5))

ggplot(sumFRAP.VARLOC,aes(x=VARLOC,y=FRAP,fill=VARLOC))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=FRAP-ci, ymax=FRAP+ci),width=0.1)+
  theme_classic()+
  ylab('FRAP')+
  scale_fill_grey()+
  theme(legend.position='bottom')+
  theme(axis.text.x=element_text(angle = 40, vjust = 0.5))

###Graficos de barras de diferencias de concentraciones (Tukeys)
ggplot(dfN.VARLOC,aes(x=name,y=diff))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 25, vjust = 0.5))+
  ggtitle('Ntotal')

ggplot(dfN.VAR,aes(x=name,y=diff))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 25, vjust = 0.5))+
  ggtitle('Ntotal')


ggplot(dfN.LOC,aes(x=name,y=diff))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=45, vjust=0.5,hjust=0.5))+
  ggtitle('Ntotal')




ggplot(dfFRAP.VARLOC,aes(x=name,y=diff))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 25, vjust = 0.5))+
  ggtitle('FRAP')


ggplot(dfFRAP.VAR,aes(x=name,y=diff))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 25, vjust = 0.5))+
  ggtitle('FRAP')

ggplot(dfFRAP.LOC,aes(x=name,y=diff))+
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=lwr, ymax=upr),width=0.1)+
  theme_classic()+
  theme(text = element_text(size=8),axis.text.x = element_text(angle=45, vjust=0.5,hjust=0.5))+
  ggtitle('FRAP')
dev.off()

