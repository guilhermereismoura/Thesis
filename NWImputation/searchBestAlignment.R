searchBestAlignment <- function(dataBase, carateristica, missingString, mat, gapOpeningV, gapExtensionV){

  
  #init for search
  bestScore= -10000000;
  bestID= "ID"
  
  
  
  for(i in ls(dataBase)){
    
    resultString= paste(dataBase[[i]][[carateristica]]$discreteData, collapse = "")
    
    needleResult= pairwiseAlignment(missingString, resultString, type= "global", substitutionMatrix= mat, gapOpening= gapOpeningV, gapExtension= gapExtensionV)
    
    
    #Para encontrar o valor encontrado
    
    loc= regexpr("[?]", toString(needleResult@pattern))[1]
    
    
    #encontrar o valor para substituir
    letter= substr(toString(needleResult@subject), loc, loc)
    
    
    #Está a retirar resultados que não encontrem matches
    
    if(bestScore < needleResult@score && (letter != "-" && letter != "?") && grepl("?", toString(needleResult@pattern), fixed = TRUE)){
    #if(bestScore < needleResult@score){
      
      bestScore = needleResult@score
      bestID= i
      bestPattern= needleResult@pattern
      bestSubject= needleResult@subject
      bestNeedleResult= needleResult
      
    }
    
  }
  
  
  
  
  
  
  
  
  
  return(list(bestScore, bestID, bestPattern, bestSubject, bestNeedleResult))
  
  
}