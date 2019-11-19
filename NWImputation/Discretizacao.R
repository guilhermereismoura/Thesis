Discretizacao <- function(dataBase, carateristicaAAnalisar, nBins, IDAAnalisar){
  
  
  
  
  for(i in ls(dataBase)){
    

    dataBase[[i]][[carateristicaAAnalisar]] = as.numeric(dataBase[[i]][[carateristicaAAnalisar]])
    
    
    dataBase[[i]][carateristicaAAnalisar] = dataDiscretize(dataBase[[i]][[carateristicaAAnalisar]], classBoundaries = nBins, classStates=letters[1:nBins], method= "equal")
    #resultado tem discreteData, classBoundaries, midValues, e classStates
    

    
    dataBase[[i]][[carateristicaAAnalisar]]$discreteData[which(is.na(dataBase[[i]][[carateristicaAAnalisar]]$discreteData))] = '?'
    

    
  }
  
  
  missingSequence= dataBase[[IDAAnalisar]][[carateristicaAAnalisar]]
  #print(dataBase[[IDAAnalisar]][[carateristicaAAnalisar]]$classBoundaries)
  classBound= dataBase[[IDAAnalisar]][[carateristicaAAnalisar]]$classBoundaries
  missingString= paste(missingSequence$discreteData, collapse = "")
  
  
  
  return(list(dataBase, missingString, classBound))
  
  
  
  
}