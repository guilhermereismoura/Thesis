fazMissings <- function(dataBase, IDAnalisar, carateristica, missing, missingVector){
  

  
  if(missing>=1){
    #faz apenas um missing na posição definida
    
    valueVector= c(dataBase[[IDAnalisar]][[carateristica]][missing])
    
    dataBase[[IDAnalisar]][[carateristica]][missing] = NA
    
    missingVector= c(missing)
    
    
    
  }else if(missing == 0){
    #Com missing a 0 usa-se um vetor pré-determinado no missingVector
    
    
    valueVector= c()
    
    for (item in missingVector){
      
      valueVector= c(valueVector, dataBase[[IDAnalisar]][[carateristica]][item])
      dataBase[[IDAnalisar]][[carateristica]][item] = NA

    }
    
  }else{
    #Foi dada uma percentagem de missings
    
    numberOfMissings= missing * length(dataBase[[IDAnalisar]][[carateristica]])
    
    numberOfMissings= round(numberOfMissings, digits=0)
    
    missingVector = sample(length(dataBase[[IDAnalisar]][[carateristica]]), numberOfMissings)
    
    valueVector= c()
    
    
    for (item in missingVector){
      
      valueVector= c(valueVector, dataBase[[IDAnalisar]][[carateristica]][item])
      dataBase[[IDAnalisar]][[carateristica]][item] = NA
      
    }
  }
  
  return(list(dataBase, missingVector, valueVector))
  
}