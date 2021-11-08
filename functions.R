library(dplyr)

# Initiate the infectious process - Introduction in Index herd-----------------
# HerdDesc : Herd characteristics dataframe
# simDay : Current day of simulation
# origine : index herd ID

Initialize<-function(herdDesc,origine,day){
  
  herdDesc<-data.frame(herdDesc)
  herdDesc$status="S"
  herdDesc$isDetected<-FALSE
  herdDesc$detectionDate<-NA
  herdDesc$S=herdDesc$size# All animals are susceptible
  herdDesc$I=0
  herdDesc$E=0
  herdDesc$R=0
  herdDesc$infDate=NA
  herdDesc$source=NA
  herdDesc$RDate=NA
  herdDesc$surveillance=NA
  herdDesc$routeInf="None"
  which(rowSums(herdDesc[,c("S","E","I","R")])==0)->noSizes
  herdDesc[noSizes,"S"]=floor(runif(length(noSizes),10,100))
  herdDesc$herdSize=rowSums(herdDesc[,c("S","E","I","R")])
  herdDesc$SurvDate=NA
  
  InitInf=which(herdDesc$lieu %in% origine)
  herdDesc$status[InitInf]="I" #Herd status For Index Herd
  herdDesc$E[InitInf]=min(herdDesc$herdSize[InitInf],5) # 5 initial Exposed animal in Index herd
  herdDesc$S[InitInf]=herdDesc$herdSize[InitInf]-herdDesc$E[InitInf] #Update S pop in Index herd
  herdDesc$infDate[InitInf]=day #Introduction time
  
  
  herdDesc$col="green"
  herdDesc$col[InitInf]="red"
  herdDesc$Vsize=2
  return(herdDesc)
}






# Within-herd Transmission function ---------------------------------------
# HerdDesc : Herd characteristics dataframe
# simDay : Current day of simulation
# Beta : Within-herd transmission rate
# delta : average duration of the latency period
# theta :average duration of the infectious period

WithinHerdProcess<-function(herdDesc,simDay,Beta=0.6,delta=4, # 
                            theta=7,biosecurity=FALSE){
  
  infHerds=which(herdDesc$status %in% c("E","I"))

  if (length(infHerds)>0){#s'il y a des infectés, on fait évoluer SEIR
    lambda=Beta*herdDesc[infHerds,"I"]/(herdDesc[infHerds,"S"]+ herdDesc[infHerds,"E"]+
                                             herdDesc[infHerds,"I"])

    if(biosecurity) lambda=lambda/herdDesc[infHerds,"biosecurity"]
    
    
    
    Pinf=1-exp(-lambda) #probabilité d'infection
    infection=rbinom(length(infHerds),herdDesc[infHerds,"S"],Pinf) # New Exposed
    infectious=rbinom(length(infHerds),herdDesc[infHerds,"E"],1/delta)# New Infecctious
    resistance=rbinom(length(infHerds),herdDesc[infHerds,"I"],1/theta) # New Removed / Dead
    
    herdDesc[infHerds,"S"]=herdDesc[infHerds,"S"] - infection
    herdDesc[infHerds,"E"]=herdDesc[infHerds,"E"] + infection - infectious
    herdDesc[infHerds,"I"]=herdDesc[infHerds,"I"] + infectious - resistance
    herdDesc[infHerds,"R"]= herdDesc[infHerds,"R"] + resistance

  }#fin évolution SEIR intra
  
  return(herdDesc)
}



# Between-herd tranmission function ---------------------------------------
# HerdDesc : Herd characteristics dataframe
# daydfInf : extaction of Movement dataFrame for current day
# dayTourInf : extaction of Round dataFrame for current day
# simDay : Current day of simulation
# probInfTransit: Probability of infection through transit (Rounds)


BetweenHerdProcess<-function(herdDesc,daydfInf,dayTourInf,simDay,probInfTransit){

  if (nrow(daydfInf)>0){ #s'il y en a :
    
    
    # check for transit movement infections ----------------------------------------------
      transit <- anti_join(dayTourInf, daydfInf) 
      indexDest=which(herdDesc$lieu %in% transit$to & herdDesc$status=="S")
      if (length(indexDest)>0){
        (runif(length(indexDest))<probInfTransit)->tmp
        if (any(tmp)){
          herdDesc$status[indexDest[tmp]]="I"
          herdDesc$col[indexDest[tmp]]="purple"
          herdDesc$source[indexDest[tmp]]=as.character(transit$from[match(herdDesc$lieu[indexDest[tmp]],transit$to)])
          herdDesc$infDate[indexDest[tmp]]=simDay
          herdDesc$S[indexDest[tmp]]=herdDesc$S[indexDest[tmp]]-1
          herdDesc$E[indexDest[tmp]]=herdDesc$E[indexDest[tmp]]+1
          herdDesc$routeInf[indexDest[tmp]]="Transit"
       }
    }  
    
    ## Check for animal movement -----------------------------------------------
    
    
    for (a in 1:nrow(daydfInf)){#pour chaque envoie
      nMov=as.numeric(as.character(daydfInf[a,"nAnim"]))#nb animaux bougés
      indexSource=which(herdDesc$lieu==as.character(daydfInf[a,1]))
      indexDest=which(herdDesc$lieu==as.character(daydfInf[a,2]))
      if (length(indexDest)>0){
        if (nMov>sum(herdDesc[indexSource,c("S","E","I","R")])){
          herdDesc$S[indexSource]=herdDesc$S[indexSource]+nMov
          herdDesc$herdSize[indexSource]=sum(herdDesc[indexSource,c("S","E","I","R")])
        }
        
        if (nMov>sum(herdDesc[indexDest,c("S","E","I","R")])){
          herdDesc$S[indexDest]=herdDesc$S[indexDest]+nMov
          herdDesc$herdSize[indexDest]=sum(herdDesc[indexDest,c("S","E","I","R")])
        }
        
        res=sample(c(rep("S",herdDesc$S[indexSource]),
                     rep("E",herdDesc$E[indexSource]),
                     rep("I",herdDesc$I[indexSource]),
                     rep("R",herdDesc$R[indexSource])), nMov)
        events <- data.frame(type = factor(res, c('S','E','I','R')),quantity = 1)
        res=table(events$type)
        
        
        resDest=sample(c(rep("S",herdDesc$S[indexDest]),
                         rep("E",herdDesc$E[indexDest]),
                         rep("I",herdDesc$I[indexDest]),
                         rep("R",herdDesc$R[indexDest])), nMov)
        events <- data.frame(type = factor(resDest, c('S','E','I','R')),quantity = 1)
        resDest=table(events$type)
        
        #on enlève les nMov animaux de SEIR
        herdDesc$S[indexSource]=herdDesc$S[indexSource]-res["S"]
        herdDesc$E[indexSource]=herdDesc$E[indexSource]-res["E"]
        herdDesc$I[indexSource]=herdDesc$I[indexSource]-res["I"]
        herdDesc$R[indexSource]=herdDesc$R[indexSource]-res["R"]
        herdDesc$S[indexSource]=herdDesc$S[indexSource]+nMov
        
        
        
        ## Reaction-diffusion ------------------------------------------------------
        ## we replace individuals in Destination farms by individuals moved from source
        herdDesc$S[indexDest]=herdDesc$S[indexDest]-resDest["S"]+res["S"]
        herdDesc$E[indexDest]=herdDesc$E[indexDest]-resDest["E"]+res["E"]
        herdDesc$I[indexDest]=herdDesc$I[indexDest]-resDest["I"]+res["I"]
        herdDesc$R[indexDest]=herdDesc$R[indexDest]-resDest["R"]+res["R"]
        
        herdDesc$herdSize[indexDest]=rowSums(herdDesc[indexDest,c("S","E","I","R")])
        
        
        #élevage devient infecté si ce n'est pas déjà le cas
        if (herdDesc$status[indexDest]!="I" & (herdDesc$I[indexDest]>0 |herdDesc$E[indexDest]>0)) { #si le statu correspondant au lieu a,2 vaut s alors
          herdDesc$status[indexDest]="I"# si infection: rouge, source, date d'infection
          herdDesc$col[indexDest]="red"
          herdDesc$source[indexDest]=as.character(daydfInf[a,1])
          herdDesc$infDate[indexDest]=simDay
          herdDesc$routeInf[indexDest]="Direct"
        }
      }
    }
  }
  return(herdDesc)
}


# local transmission function ---------------------------------------------
# sources: ID of infected herds
# beta   : Maximum transmission rate at 0 distance
# Radius : Sd of the ngaussian Kernel
# HerdDesc : Herd characteristics dataframe
# simDay : Current day of simulation 

localTransmission<-function(sources,beta,Radius,herdDesc,simDay){
  
  HD=herdDesc
  coordinates(HD)<-~Lon+Lat
  proj4string(HD)=CRS("+init=epsg:4326")
  HD<-spTransform(HD,CRS("+proj=utm +zone=30"))
  H=st_as_sf(HD)
  
  for (source in sources){
    a=st_buffer(H$geometry[source],Radius)
    as.numeric(st_intersects(H,a))->x
    index=which(x==1)
    for (i in index) {
      if (herdDesc[i,]$status=="S") {
        probLocal<-1-exp(-dnorm(gDistance(HD[i,],HD[source,]),0,Radius/4)*(sqrt(2*pi)*(Radius/4))*beta*herdDesc[source,"I"])
        # print(paste("transmission locale",probLocal,";S=",herdDesc$S[i]))
        herdDesc$E[i]=rbinom(1,herdDesc$S[i],probLocal)
        herdDesc$S[i]=herdDesc$S[i]-herdDesc$E[i]
        if (herdDesc$E[i]>0){
          herdDesc$probInfection[i]=probLocal
          herdDesc$status[i]="I"
          herdDesc$col[i]="orange"
          herdDesc$source[i]=as.character(herdDesc$lieu[source])
          herdDesc$DistFromSource[i]=gDistance(HD[i,],HD[source,])/1000
          herdDesc$infDate[i]=as.Date(simDay,origin = "1970-01-01")
          herdDesc$routeInf[i]="local"
        }
      }
    }
  }
  return(herdDesc)
}


# Surveillance function------------------------------------------------------------
# HerdDesc : Herd characteristics dataframe
# df   : extaction of Movement dataFrame for current day
# Tour : extaction of Round dataFrame for current day
# day : Current day of simulation
# TrackDuration : Tracing time-interval


HerdSurveillance<-function(herdDesc,day,df,Tournees,TrackDuration=30){
  if (any(herdDesc$isDetected & herdDesc$detectionDate==day)){
    sources<-which(herdDesc$isDetected & herdDesc$detectionDate==day)  
    HD=herdDesc
    coordinates(HD)<-~Lon+Lat
    proj4string(HD)=CRS("+init=epsg:4326")
    HD<-spTransform(HD,CRS("+proj=utm +zone=30"))
    H=st_as_sf(HD)
    for (source in sources){
      
      
      # Surveillance zone -------------------------------------------------------
      
      
      BuffSurv=st_buffer(H$geometry[source],10000)
      as.numeric(st_intersects(H,BuffSurv))->x
      indexSurv=which(x==1)
      indexSurv=indexSurv[-which(indexSurv==source)]
      herdDesc$surveillance[indexSurv]="Surveillance Zone"
      herdDesc$SurvDate[indexSurv]=day
      
      
      # Protection zone ---------------------------------------------------------
      
      
      BuffProt=st_buffer(H$geometry[source],3000)
      as.numeric(st_intersects(H,BuffProt))->x
      indexProt=which(x==1)
      indexProt=indexProt[-which(indexProt==source)]
      herdDesc$surveillance[indexProt]="Protection Zone"
      herdDesc$SurvDate[indexProt]=day
      
      # tracing - to source -----------------------------------------------------
      
      df[df$to==herdDesc$lieu[source],]->MoveToSource 
      if(nrow(MoveToSource)>0){
        MoveToSource[(as.Date(MoveToSource$date)>=(as.Date(day-TrackDuration))) & 
                       (as.Date(MoveToSource$date)<= day),]->MoveToSource
        herdDesc$surveillance[herdDesc$lieu %in% MoveToSource$from]="Upward tracing"
        herdDesc$SurvDate[herdDesc$lieu %in% MoveToSource$from]=day
      }
      Tournees[Tournees$to==herdDesc$lieu[source],]->RoundToSource 
      if(nrow(RoundToSource)>0){
        RoundToSource[(as.Date(RoundToSource$date)>=(as.Date(day-TrackDuration))) & 
                        (as.Date(RoundToSource$date)<= day),]->RoundToSource
        if (nrow(anti_join(RoundToSource[,c("from","to","date")],
                           MoveToSource[,c("from","to","date")])->transit)>0)
          herdDesc$surveillance[herdDesc$lieu %in% transit$from]="Upward transit tracing"
        herdDesc$SurvDate[herdDesc$lieu %in% transit$from]=day
        
      }
      
      # tracing - from source ----------------------------------------------------
      
      df[df$from==herdDesc$lieu[source],]->MovefromSource         
      if(nrow(MovefromSource)>0){
        MovefromSource[(as.Date(MovefromSource$date)>=(as.Date(day-TrackDuration))) & 
                         (as.Date(MovefromSource$date)<= day),]->MovefromSource
        herdDesc$surveillance[herdDesc$lieu %in% MovefromSource$to]="Downward tracing"
        herdDesc$SurvDate[herdDesc$lieu %in% MovefromSource$to]=day
      }
      
      Tournees[Tournees$from==herdDesc$lieu[source],]->RoundFromSource 
      if(nrow(RoundFromSource)>0){
        RoundFromSource[(as.Date(RoundFromSource$date)>=(as.Date(day-TrackDuration))) & 
                          (as.Date(RoundFromSource$date)<= day),]->RoundFromSource
        if (nrow(anti_join(RoundFromSource[,c("from","to","date")],
                           MovefromSource[,c("from","to","date")])->transit)>0)
          herdDesc$surveillance[herdDesc$lieu %in% transit$from]="Downward transit tracing"
        herdDesc$SurvDate[herdDesc$lieu %in% transit$from]=day
      }
    }
  }
  herdDesc$survDate[!is.na(herdDesc$surveillance)]<-day
  
  
  herdDesc
}

# Detection Function --------------------------------------------------------
# HerdDesc : Herd characteristics dataframe
# simDay : Current day of simulation 
# PrevThreshold: Prevalence threshold for detection

Detection<-function(herdDesc,simDay,PrevThreshold,NumDeaths){
  
  rowSums(prop.table(as.matrix(herdDesc[,c("S","E","I","R")]),1)[,3:4])->PrevalenceI
  detected<-which(PrevalenceI>=PrevThreshold & 
                    herdDesc$R>=NumDeaths & 
                    !herdDesc$isDetected)
  herdDesc$detectionDate[detected]=simDay
  herdDesc$status[detected]="D"
  herdDesc$I[detected]=0
  herdDesc$isDetected[detected]=TRUE
  herdDesc$col[detected]="black"
  return(herdDesc)
}  


RemoveLinks<-function(dframe,criteria,D){
if (any((dframe$from %in% dframe$criteria | dframe$to %in% criteria) & dframe$date>=D))  
  dframe<- dframe[-which((dframe$from %in% criteria | dframe$to %in% criteria) & dframe$date>=D),]
dframe
}

