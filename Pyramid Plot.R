#install.packages(c("plotrix","tidyverse") #should be ran only once
library(plotrix)
library(tidyverse)

incidence= read.delim("incidence.txt" , stringsAsFactors = FALSE, na.strings = "n/a")
death=read.delim("death.txt",stringsAsFactors = FALSE, na.strings = "n/a")
incidence=incidence[,-2]
death= death[,-2]
incidence[is.na(incidence)]=0
death[is.na(death)]=0
colnames(incidence)= c("type","female","male")
colnames(death)= c("type","female","male")

data= inner_join(incidence, death, by=c("type"))
colnames(data)= c("type", "in.female","in.male","de.female","de.male")
data= mutate(data, in.f= in.female/1000,
             in.m = in.male/1000,
             d.f=de.female/1000,
             d.m= de.male/1000)

types= c("Breast","Esophagus","Kidney","Leukemia","Liver","Lung","Lymphoma","Ovary","Pancreas","Prostate")
pyramid.plot(data$in.f,data$in.m,
             laxlab= c(0,50,100,150,200,250),
             raxlab=c(0,50,100,150,200),
             top.labels=c("Female","Types of Cancer","Male"),labels=types,
             gap  =25, labelcex = .8, unit="$ in 000's",lxcol="#edf8e9", rxcol="#f2f0f7")

pyramid.plot(data$d.f,data$d.m,
             laxlab= c(0,50,100,150,200,250),
             raxlab=c(0,50,100,150,200),
             top.labels=c("Female","","Male"),labels=types,space= 0.4,
             gap  =25, labelcex = 1, unit="",lxcol="#74c476", rxcol="#9e9ac8", add=TRUE)

