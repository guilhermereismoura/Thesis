createWindowsToSearch <- function(querySize, windowSize){
  
  leftSize= querySize[[1]]
  rightSize= querySize[[2]]
  missingPosition= querySize[[3]]
  
  tempWindow= 0 
  
  nJanelas= leftSize + rightSize -(windowSize-1)
  
  auxRightSize=0
  
  #descobrir 1ª coordenada do lado direito da janela
  while((leftSize + auxRightSize)<windowSize){
    
    auxRightSize= auxRightSize + 1
  }
  
  auxLeftSize= leftSize
  
  windowToSearch= list()
  contador =1
  
  for(i in 1:nJanelas){
    
    
    
    #lado esquerdo da janela
    if(auxLeftSize>0){
      
      tempWindow= (missingPosition - auxLeftSize):(missingPosition - 1)
      
    }
    

    #lado direito da janela
    if(length(tempWindow)== windowSize){
      
    }else{
      
      if(auxLeftSize>0){
        tempWindow= c(tempWindow, (1+ missingPosition):(missingPosition + auxRightSize))
        
      }else{
        
        tempWindow= (1+ missingPosition):(missingPosition + auxRightSize)
        
      }
    }
    
    windowToSearch[[contador]] = tempWindow
    
    auxLeftSize= auxLeftSize-1
    auxRightSize= auxRightSize+1
    
    contador= contador + 1
    tempWindow= 0    #reiniciar tempWindow
    

  }
  
  return(windowToSearch)
  
}