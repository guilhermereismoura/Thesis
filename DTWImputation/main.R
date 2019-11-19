rm(list=ls())
library(DTWBI)
source("getQuery.R")
source("applyDTW.R")
source("derivative.R")
source("preparaDados.R")
source("fazMissings.R")
source("createWindowsToSearch.R")
library(openxlsx)

startTime=Sys.time()


IDAnalisar= "ID 74"
windowSize=5
carateristica= "X1"
missing= 0
missingVector= c(13, 16, 21, 24, 35, 38)

foundVector= c()
IDVector= c()
bestPositionVector= c()

dataBase = preparaDados("excelFiles\\ECG.csv")


#simular um NA

tempResultMissings= fazMissings(dataBase, IDAnalisar, carateristica, missing, missingVector)

dataBase= tempResultMissings[[1]]
missingVector= tempResultMissings[[2]]
valueVector= tempResultMissings[[3]]


#################################

while(length(which(is.na(dataBase[[IDAnalisar]][[carateristica]])))>= 1){


  querySize = getQuery(dataBase[[IDAnalisar]][[carateristica]], windowSize)
  
  print("missing position")
  print(querySize[[3]])
  
  
  windowToSearch= createWindowsToSearch(querySize, windowSize)
  
  
  #results= applyDTW(dataDTWBI$query, windowSize, windowToSearch, original$query, missingPosition)
  
  contador=0
  
  bestScore=100000
  
  
  for(i in ls(dataBase)){
  
    if(i == IDAnalisar){
      
     next;
      
    }
    
    contador= contador+1;
    #print(i)
  
    results= applyDTW(dataBase[[IDAnalisar]][[carateristica]], windowSize, windowToSearch, dataBase[[i]][[carateristica]], querySize[[3]])
    
    if(results[[1]] < bestScore && !is.na(results[[3]])){
      
      bestScore= results[[1]]
      bestIndex= results[[2]]
      bestValue= results[[3]]
      bestId= i
      
    }
    
  
  }
  
  print(bestValue)
  dataBase[[IDAnalisar]][[carateristica]][querySize[[3]]] = bestValue
  
  
  foundVector= c(foundVector, bestValue)
  IDVector= c(IDVector, bestId)
  bestPositionVector= c(bestPositionVector, bestIndex)
  
  #print(bestScore)
  #print(bestIndex)
  #print(bestValue)
  #print(bestId)
  
  objectToInsert= list(c(paste("Score", bestScore, sep = ";", collapse = NULL), paste("Best ID", bestId, sep= ";"), paste("ID a ser analizado", IDAnalisar, sep=";"), paste("Carateristica a ser analizada", carateristica, sep=";"), paste("Missing", missing, sep=";")), c("Valores encontrados por ordem", foundVector) , c("Posições Missings", missingVector), c("Valores originais", valueVector), c("Indices encontrados por ordem", bestPositionVector), c("ID encontrado por ordem", IDVector))
  
  
  write.xlsx(objectToInsert, file = "Results\\Teste4.xlsx")
  
  
}

endTime= Sys.time()
#print(windowToSearch)

print(endTime-startTime)





