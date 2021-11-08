readAndFormat<-function(FileMovements="./Data/Movements.RData", FileTournees="./Tournees.RData", fileHerdDesc='HD_2017-2019.csv'){

  library(sf)
  library(flux)
  library(bbmle)
  library(doParallel)
  library(plyr); library(dplyr)
  library(foreach)
  
  load(FileMovements)
  df$date=as.Date(df$date)
  df$days<-as.numeric(df$date)-as.numeric(df$date)[1]+2
  as.character(df$from)->df$from
  as.character(df$to)->df$to
  if (any(df$siteTo %in% c("ST","EQ","PA", "SL","CR","AG","AU","CR","IN","LAB","NE","PE","SA"))){
    df<-df[-which(df$siteTo %in% c("EQ","PA", "SL","CR","AG","AU","CR","IN","LAB","NE","PE","SA")),]
    }
  if (any(df$siteFrom %in% c("ST","EQ","PA", "SL","CR","AG","AU","CR","IN","LAB","NE","PE","SA"))){
   df<-df[-which(df$siteFrom %in% c("ST","EQ","PA","SL","CR","AG","AU","CR","IN","LAB","NE","PE","SA")),]
  }
  
  
  load(FileTournees)
  Tournees$date=as.Date(Tournees$date)
  Tournees$days<-as.numeric(Tournees$date)-as.numeric(Tournees$date)[1]+2
  as.character(Tournees$from)->Tournees$from
  as.character(Tournees$to)->Tournees$to
  if (any(Tournees$siteTo %in% c("ST","EQ","PA", "SL","CR","AG","AU","CR","IN","LAB","NE","PE","SA"))){
    Tournees<-Tournees[-which(Tournees$siteTo %in% c("ST","EQ","PA", "SL","CR","AG","AU","CR","IN","LAB","NE","PE","SA")),]
  }
  if (any(Tournees$siteFrom %in% c("ST","EQ","PA", "SL","CR","AG","AU","CR","IN","LAB","NE","PE","SA"))){
    Tournees<-Tournees[-which(Tournees$siteFrom %in% c("ST","EQ","PA","SL","CR","AG","AU","CR","IN","LAB","NE","PE","SA")),]
  }

  load("./Data/herdDesc.RData")# read.delim(fileHerdDesc,sep=";",header=TRUE,dec=",")
  herdDesc<-herdDesc[nchar(as.character(herdDesc$lieu))<6,]
  # load("./Data/herdDesc.RData")
  # herdDesc<-herdDesc[,-c(1,ncol(herdDesc))]
  herdDesc$routeInf<-NA
  herdDesc$detectionDate<-NA
  herdDesc$isDetected<-FALSE
  herdDesc$probInfection<-NA
  herdDesc$DistFromSource=0
  

  # herdDesc<-herdDesc[-which(herdDesc$type %in% c("AU","IN")),]
  herdDesc$site=as.factor(herdDesc$site)
  levels(herdDesc$site)[c(5,6)]="Breeders"
  levels(herdDesc$site)[c(1,4,6)]="Farrowers"
  levels(herdDesc$site)[c(2,3,5)]="Finishers"
  factor(herdDesc$site,sort(levels(herdDesc$site)))->herdDesc$site
  
  data=list(df,Tournees,herdDesc)
}