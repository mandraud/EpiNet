HerdSurveillance<-function(herdDesc,day,df,Tournees,durDownWard=30){
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
           MoveToSource[(as.Date(MoveToSource$date)>=(as.Date(day-durDownWard))) & 
                 (as.Date(MoveToSource$date)<= day),]->MoveToSource
           herdDesc$surveillance[herdDesc$lieu %in% MoveToSource$from]="Upward tracing"
           herdDesc$SurvDate[herdDesc$lieu %in% MoveToSource$from]=day
           }
         Tournees[Tournees$to==herdDesc$lieu[source],]->RoundToSource 
         if(nrow(RoundToSource)>0){
         RoundToSource[(as.Date(RoundToSource$date)>=(as.Date(day-durDownWard))) & 
                        (as.Date(RoundToSource$date)<= day),]->RoundToSource
         if (nrow(anti_join(RoundToSource[,c("from","to","date")],
                            MoveToSource[,c("from","to","date")])->transit)>0)
         herdDesc$surveillance[herdDesc$lieu %in% transit$from]="Upward transit tracing"
         herdDesc$SurvDate[herdDesc$lieu %in% transit$from]=day
         
         }
         
# tracing - from source ----------------------------------------------------

         df[df$from==herdDesc$lieu[source],]->MovefromSource         
         if(nrow(MovefromSource)>0){
            MovefromSource[(as.Date(MovefromSource$date)>=(as.Date(day-durDownWard))) & 
                        (as.Date(MovefromSource$date)<= day),]->MovefromSource
            herdDesc$surveillance[herdDesc$lieu %in% MovefromSource$to]="Downward tracing"
            herdDesc$SurvDate[herdDesc$lieu %in% MovefromSource$to]=day
         }
         
         Tournees[Tournees$from==herdDesc$lieu[source],]->RoundFromSource 
         if(nrow(RoundFromSource)>0){
           RoundFromSource[(as.Date(RoundFromSource$date)>=(as.Date(day-durDownWard))) & 
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