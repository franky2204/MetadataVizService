library(tidyverse)

df<-data.frame(
  ID_patient=sample(paste("HAL78_",abs(.Random.seed)[1:278],sep=""),500,replace = TRUE),
  tissue=sample(c("rectum","colon","left","right"),500,replace = TRUE,prob=c(1,2,3,4)),
  hospital=sample(c("Torino1","Torino2","Milano","Alessandria","Genova"),500,replace = TRUE),
  grade=sample(c("I","II","III"),500,replace = TRUE,prob=c(2,2,1)),
  recidive=sample(c("TRUE","FALSE"),500,replace = TRUE,prob=c(2,1)),
  sex=sample(c("F","M"),500,replace = TRUE),
  therapy=sample(c("chemotherapy", "immunotherapy", "endocrine", "targeted"),500,replace = TRUE),
  time=sample(c("DIAG","FU3","M1","M2"),500,replace = TRUE),
  WBCellCount=rnorm(500,5,2),
  Hematocrit=rnorm(500,45,5),
  PlateletCount=rnorm(500,250,30))

df<-df%>%
  mutate(WBCellCount=ifelse(recidive,WBCellCount+2,WBCellCount),
         Hematocrit=ifelse(sex=="M",Hematocrit+5,Hematocrit),
         PlateletCount=ifelse(sex=="M",PlateletCount,PlateletCount+20),
         Prova_corr=WBCellCount*Hematocrit)

variables<-c("ID_patient",
             "tissue",
             "hospital",
             "grade",
             "recidive",
             "sex",
             "therapy",
             "time",
             "WBCellCount",
             "Hematocrit",
             "PlateletCount",
             "Prova_corr")
classes<-c("character",
           "factor",
           "factor",
           "factor",
           "logic",
           "logic",
           "factor",
           "factor",
           "numeric",
           "numeric",
           "numeric",
           "numeric")

