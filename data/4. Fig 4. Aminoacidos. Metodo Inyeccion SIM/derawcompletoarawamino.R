####De rawamino completo a rawamino recalculado
setwd('C:/Users/alexg/OneDrive - Stony Brook University/13. Proyectos/DATOS PROYECTO QUINOA/Estadistica Alex y rawdata/Metodo Inyeccion SIM')

###AMINOACIDOS###
rawamino<-read.csv(file='rawaminocompleto.csv')

#Eliminar las ultimas filas (peso no conocido)
rawamino<-rawamino[-c(46:54),]

#Separar varibles text y numero
rawamino.text<-rawamino[,c(1:5)]
rawamino.num<-rawamino[,c(7:26)]

#Recalcular las variables numericas (ppm)*0.15*5/peso
newamino<-rawamino.num*0.15*5/rawamino$PESO

#Union de las variables numericas y texto
rawamino1<-cbind(rawamino.text,newamino)

#En forma narrow
library(tidyr)
ex<-gather(rawamino1,'AMINOACIDO','CONCENTRACION',LISINA:TRIPTÓFANO)

#Eliminacion de outliers por filas
#Maria elimina la muestra 8, 3,18,5,14
#Son las filas 3,4,13,14,24,25,26,27,28,29,42,43,44
library(Rmisc)
tab<-summarySE(ex,measurevar = 'CONCENTRACION',groupvar=c('MUESTRA','VARIEDAD','PAIS','VAR.PAIS','AMINOACIDO'))

tab$MUESTRA<-factor(tab$MUESTRA,levels=c(1,8,15,2,9,16,3,10,
                                         17,4,11,18,5,12,19,
                                         6,13,20,7,14,21))

tab2<-tab[order(tab$MUESTRA),]
tab2<-tab2[,c(1:5,7)]

tab3<-spread(tab2,'AMINOACIDO','CONCENTRACION')

tab3[c(2,7,12,13,20),]<-NA

tab3[4,8]<-NA

tab3[6,c(5,7,13,20)]<-NA

tab3<-tab3[-c(2,7,12,13,20),]

tab4<-gather(tab3,'AMINOACIDO','CONCENTRACION',AC.ASPÁRTICO:VALINA)

tab5<-na.omit(tab4)

tab6<-tab5[tab5$AMINOACIDO!='CISTEÍNA',]

write.csv(tab6,'rawamino.csv')

