setwd('/home/alexggo/MEGA/13. Proyectos/DATOS PROYECTO QUINOA/Estadistica Alex y rawdata')

rawTallos<-read.csv("rawTallos.csv")

library('Rmisc')

names<-colnames(rawTallos)
names<-names[-c(1:4)]

listvar<-list()
for (i in 1:10){
  listvar[[i]]<-summarySE(rawTallos,measurevar = names[i],groupvars = c("VAR","LOC","VARLOC"))
  listvar[[i]]$variable<-names[i]
  colnames(listvar[[i]])<-c("LOC","VAR","VARLOC","N","mean",
                            "sd","se","ci","variable")
   }
names(listvar)<-names

df<-data.frame()
for (i in 1:10){
  df<-rbind(df,listvar[[i]])
}

