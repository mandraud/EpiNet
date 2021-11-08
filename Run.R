rm(list=ls())

library(doParallel)
library(foreach)
source("ReadAndFormatData.R")
source("FinalModel.R")
params<-read.delim("Scenarios.csv",header=TRUE,sep=";")[c(1,4,10,13),]
params$sim=10

##read Movement Data and herd Characteristics
#The function returns a list of 3 elements:
  # Animal movements
  # Transit movements
  # Herd descriptions.
data <- readAndFormat(FileMovements = "./Data/MovementDataFrame.RData",
                      FileTournees = "./Data/Tournees.RData",
                      fileHerdDesc = "./Data/herdDesc.RData")  

# Runs sequentially of the model simulations occros a table of parameters governng the scenarios
params<-read.delim("Scenarios.csv",header=TRUE,sep=";")[c(1,4,10,13),]
params$sim=10

apply(params,1,RunModel,data)

 

