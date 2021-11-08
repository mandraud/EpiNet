RunModel<-function(P,data){
# Running parameters 
  # P defines the scenario: P= "Scen.ID",
  #                            "Detection T/F",
  #                            "Prevalence Threshold",
  #                            "death threshold",
  #                            "Control in protection zone",
  #                            "Control in surveillance zone",   
  #                            "Control traced herds",   
  #                            "Number of simulations"
  as.numeric(P[8])->sim
  detection<-as.logical(P[2])
  prevThreshold<-as.numeric(P[3])
  numDeaths<-as.numeric(P[4])
  ControlProt<-as.logical(P[5])
  ControlSurv<-as.logical(P[6])
  ControlTrace<-as.logical(P[7])
  # data
  dfInit<-data[[1]]          #Animal Movements
  TourneesInit<-data[[2]]    #Truck Transit
  herdDesc<-data[[3]]        #herd Descriptions
  HT<-levels(herdDesc$site)  #herd types to select The index herds
  
  #Epidemiological parameters
  Radius<-500          # Local transmission radius
  Duration<-40         # Simulation duration
  beta1<-0.6           # Within-herd direct transmission rate
  betaLoc<-0.0001      # Local transmission rate
  probInfTransit<-0.05 # Probability of transmission though transit trucks
  
  cl<-rep(10,3)
  cl0 <- makeCluster(3)
  registerDoParallel(cl0)
 
  # Loop on index herd type 
    Outputs=foreach(Type=1:3,.packages =c("sf","rgeos","rgdal",
                                          "tidyverse","doParallel","foreach",
                                          "tidyverse",'tibble'
                                          )) %dopar% 
      { 
       
   # Loop on simulations   
      registerDoParallel(cl[Type])
      infectedList<-foreach (isim=1:sim, .packages =c("sf","rgeos","rgdal",
                                                     "tidyverse","tibble","dplyr")) %dopar% 
        {
          
    #sourcing the transmission functions      
      source('functions.R')
          
    #initialise simulations
        df<-data.frame(dfInit)
        Tournees<-data.frame(TourneesInit)

    
        # herdDesc$biosecurity<-sample(1:3,nrow(herdDesc),replace = TRUE)
    
    # Set the introduction date and location        
        day=as.Date(sample(df$date,1))
        dayEnd=day+Duration
        as.character(sample(herdDesc$lieu[herdDesc$site %in% HT[Type] & 
                                            herdDesc$lieu %in% df$from[(df$date>day & df$date<dayEnd)]],1))->origine
        herdDesc<-Initialize(herdDesc,origine,day)
    
    
    # Loop on simulation days 
      # simulations end when the duration is reached or when 
      # no exposed/infectious indivifduals are present in the system. 
    while (day<=dayEnd & any(herdDesc$status %in% c("E","I"))){
      
      #decrease of  the detection threshold after the first detection
      if(any(herdDesc$isDetected)){
        numDeaths<-1
        prevThreshold<-0.01
      }
      
      #stop movements from detected herd
      if (detection){
        herdDesc <- Detection(herdDesc,day,prevThreshold,numDeaths)
        detHerds <- select(herdDesc %>% filter(isDetected & detectionDate==day),"lieu")
        
        if(StopAll & nrow(detHerds)>0){
          df       <- RemoveLinks(df,detHerds,day) 
          Tournees <- RemoveLinks(Tournees,detHerds,day) 
        }
        if(StopRounds & nrow(detHerds)>0){
          Tournees <- RemoveLinks(Tournees,detHerds,day)  
          }
      } 
        
      
      
      # implement surveillance protocols: 
        # onset of protection and surveillance zones around detected herds
        # Tracing of contact herds. 
      herdDesc<-HerdSurveillance(herdDesc,day,df,Tournees)
      
      #Protection zone movement control
      ProtHerd=select(herdDesc %>% 
                        filter(surveillance== "Protection Zone" & SurvDate==day),
                      "lieu")
      
        if (ControlProt & nrow(ProtHerd)>0){
          df       <- RemoveLinks(df,ProtHerd,day)  
          Tournees <- RemoveLinks(Tournees,ProtHerd,day)  
        }
      
      #Surveillance zone movement control
      SurvHerd=select(herdDesc %>% 
                        filter(surveillance== "Surveillance Zone" & SurvDate==day),
                      "lieu")
        if (ControlSurv & nrow(SurvHerd)>0){
          df       <- RemoveLinks(df,SurvHerd,day)  
          Tournees <- RemoveLinks(Tournees,SurvHerd,day)
        }
      
      #Tracing movement control
      TraceHerd=select(
                       herdDesc %>% 
                        filter(surveillance %in% c("Upward transit tracing",
                                                   "Upward tracing",
                                                   "Downward transit tracing",
                                                   "Downward tracing") & 
                               SurvDate==day),
                      "lieu"
                      )
        if (ControlTrace & nrow(TraceHerd)>0){
          df       <- RemoveLinks(df,TraceHerd,day)  
          Tournees <- RemoveLinks(Tournees,TraceHerd,day)
        }
 

# Transmission Processes ----------------------------------------------

      #keep  movements on day D 
      daydf<-df[df$date==day,]
      if (nrow(Tournees)>0){
        dayTour<-Tournees[Tournees$date==day,]
      }else{dayTour=NULL}
    
    #within-herd Dynamics  
    herdDesc<-WithinHerdProcess(herdDesc,simDay=day,Beta = beta1, biosecurity=FALSE)
    
    herdDesc %>%
      mutate(RDate=replace(RDate, status=="I" & I==0 & E==0, as.Date(day,origin = "1970-01-01"))) %>%
      mutate(S=replace(S, status=="I" & I==0 & E==0,select(herdDesc %>% filter(status=="I" & I==0 & E==0),"herdSize"))) %>%
      mutate(R=replace(R, status=="I" & I==0 & E==0, 0))%>%
      mutate(status=replace(status, status=="I" & I==0 & E==0, "R")) 
    
    # Local Transmission
    if (any(herdDesc$status=="I")){
      sources=which(herdDesc$status=="I")
      herdDesc<-localTransmission(sources,betaLoc,Radius,herdDesc,day)
    }
    
    if (nrow(daydf)>0){ #s'il y en a :
      if (any(herdDesc$status[herdDesc$lieu %in% daydf$from]=="I")){ #si il y a des infectÃ©s
        daydfInf<-daydf[which(daydf$from %in% herdDesc$lieu[herdDesc$status=="I"]),,]
        if (nrow(dayTour)>0){
        dayTourInf<-dayTour[which(dayTour$from %in% herdDesc$lieu[herdDesc$status=="I"]),,]
        }
    
    # Between-herd transmission
        herdDesc<-BetweenHerdProcess(herdDesc,daydfInf,dayTourInf,day,probInfTransit)
        herdDesc %>%
          mutate(RDate=replace(RDate, status=="I" & I==0 & E==0, as.Date(day,origin = "1970-01-01"))) %>%
          mutate(S=replace(S, status=="I" & I==0 & E==0,select(herdDesc %>% filter(status=="I" & I==0 & E==0),"herdSize"))) %>%
          mutate(R=replace(R, status=="I" & I==0 & E==0, 0))%>%
          mutate(status=replace(status, status=="I" & I==0 & E==0, "R")) 
      }
    }
    day=day+1
    }#end Day loop
    herdDesc$infDate<-as.Date(herdDesc$infDate,origin = "1970-01-01")
    to.infectedList<- herdDesc#[herdDesc$status %in% c("I","R","D"),]
    
    }#end simulation loop
    
    to.outputs=infectedList
} #end index type loop
stopCluster(cl0)
gc()

# record outputs with the simulation ID (P[1])
save(Outputs,file=paste0("./OutPut/output",as.numeric(P[1]),".RData"))
}

