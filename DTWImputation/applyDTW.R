applyDTW <- function(dataWithMissing, window_Size, windowsToSearch, dataToCompare, missingPosition){
  
  source("derivative.R")

  
  

  library(dtw)
  listDerivativeAnswer= c()
  listResults= c()
  listResultsIndex = c()

  #ciclo para percorrer as diferentes janelas encontradas anteriormente
  for(i in windowsToSearch){
    
    #1.1
    
    #######################################
    #Criar a searchingWindow com a derivada
    
    indiceElementos=0
    searchingWindow= c()
    
    completeFirstValue= FALSE
    completeFirstValueMissing= FALSE
    completeLastValue= FALSE
    completeLastValueMissing= FALSE
    
    #percorre cada posição da janela a ser analisada  --> Derivada
    for(elements in i){
      
      
      #indiceElementos é usado para ver o elemento dentro da janela
      indiceElementos= indiceElementos + 1
      
      #se estiver a analisar o 1º elemento da janela
      if(indiceElementos== 1){
        

        
        #se o 1º elemento for o 1º elemento da série a ser analisada (não tem valores para trás) ou se o elemento anterior a este não exitir
        if(i[1]==1 || is.na(dataWithMissing[[i[1]-1]])){
          
          completeFirstValueMissing= TRUE #usar variável para aplicar formula de derivada com 1º e 2º ponto apenas
          
        }else{
          #é o 1º valor
          completeFirstValue= TRUE
        }
      }
      
      
      #se for o último elemento
      
      
      
      if(elements == i[length(i)]){

        if((i[length(i)]+1 > length(dataWithMissing)) || is.na(dataWithMissing[[i[length(i)]+1]])){
          
          completeLastValueMissing= TRUE
          # print("Último Índice de todos")
          
          
        }else{
          
          completeLastValue= TRUE
          
        }
      }
      
      

      if(completeFirstValue){
        
        #print(dataWithMissing)
        #print(i[indiceElementos]-1)
        #print(elements)
        #print(i[indiceElementos+1])
        valorAAcrescentar= derivative(dataWithMissing, i[indiceElementos]-1, elements, i[indiceElementos+1])
        
        # print(i[indiceElementos]-1)
        # print(elements)
        # print(i[indiceElementos+1])
        # print("1º valor")
        # print(valorAAcrescentar)
        # 
        
      }

      if(completeFirstValueMissing){
        
        #derivada com 2 pontos (faço assim ou assumo valor 0? ou assumo o valor da derivada do ponto seguinte?)
        valorAAcrescentar= (dataWithMissing[[i[2]]]-dataWithMissing[[i[1]]])/(i[2]-i[1])
        
        # print("1º valor missing")
        # print(valorAAcrescentar)
        
        
      }

      if(completeLastValue){
        
        # print("Último Índice")
        valorAAcrescentar= derivative(dataWithMissing, i[indiceElementos-1], elements, i[indiceElementos]+1)
        # print(i[indiceElementos-1])
        # print(elements)
        # print(i[indiceElementos]+1)
        # print(valorAAcrescentar)
    
        
      }

      if(completeLastValueMissing){
        
        valorAAcrescentar= (dataWithMissing[[elements]]-dataWithMissing[[i[indiceElementos-1]]])/(i[indiceElementos]-i[indiceElementos-1])
        
        # print("Último valor missing")
        # print(valorAAcrescentar)
        
      }

      # print(completeFirstValue)
      # print(completeFirstValueMissing)
      # print(completeLastValue)
      # print(completeLastValueMissing)
      
      if(!completeFirstValue && !completeFirstValueMissing && !completeLastValue && !completeLastValueMissing){
        
        
        # print("Novo valor")
        # print(i[indiceElementos-1])
        # print(elements)
        # print(i[indiceElementos+1])
        
        valorAAcrescentar= derivative(dataWithMissing, i[indiceElementos-1], elements, i[indiceElementos+1])
        
      }
      
      #Reinicializar as variáveis
      
      
      completeFirstValue= FALSE
      completeFirstValueMissing= FALSE
      completeLastValue= FALSE
      completeLastValueMissing= FALSE
      
      searchingWindow= c(searchingWindow, valorAAcrescentar)
      
      }
    #searchingWindow= dataWithMissing[i]

    
    
    
    indexSearch= i-i[1]
    
    
    missingPositionAux= missingPosition - i[1]
    
    
    

    #ciclo para percorrer os dados
    for(j in 1:(length(dataToCompare)-window_Size+1)){
      
      indexSearch= indexSearch + 1
      missingPositionAux= missingPositionAux + 1
      

      if(missingPositionAux<=0){
        
        next;
      }
      

      if(missingPositionAux>length(dataToCompare)){
        break;
        
      }
      

      if(anyNA(dataToCompare[indexSearch]) || is.na(dataToCompare[[missingPositionAux]]) || indexSearch[length(indexSearch)] > length(dataToCompare)){
        #print("NA found"); print(missingPositionAux)}
        next;
      
      }
      
      ##################################
      #Transformar em DDTW
      indiceElementos=0
      searchingWindowCompare= c()
      completeFirstValue= FALSE
      completeFirstValueMissing= FALSE
      completeLastValue= FALSE
      completeLastValueMissing= FALSE
      
      for(elements in indexSearch){
        

        indiceElementos= indiceElementos + 1
        
        if(indiceElementos == 1){
    
          
          if(indexSearch[1]==1 || is.na(dataToCompare[[indexSearch[1]-1]])){
            
            completeFirstValueMissing= TRUE #usar variável para aplicar formula de derivada com 1º e 2º ponto apenas
            
          }else{
            
            completeFirstValue= TRUE
            
          }
          
          
        }

        if(elements == indexSearch[length(indexSearch)]){
          
          if((indexSearch[length(indexSearch)]+1) > length(dataToCompare) || is.na(dataToCompare[[indexSearch[length(indexSearch)]+1]])){
            
            completeLastValueMissing= TRUE
            # print("Último Índice de todos")
            
            
          }else{
            completeLastValue= TRUE
          }
          
        }
        
        
        if(completeFirstValue){
          
          
          valorAAcrescentar= derivative(dataToCompare, indexSearch[indiceElementos]-1, elements, indexSearch[indiceElementos+1])
          
          # print(i[indiceElementos]-1)
          # print(elements)
          # print(i[indiceElementos+1])
          # print("1º valor")
          # print(valorAAcrescentar)
          # 
          
        } 

        if(completeFirstValueMissing){
          
          #derivada com 2 pontos (faço assim ou assumo valor 0? ou assumo o valor da derivada do ponto seguinte?)
          valorAAcrescentar= (dataToCompare[[indexSearch[2]]]-dataToCompare[[indexSearch[1]]])/(indexSearch[2]-indexSearch[1])
        
          # print("1º valor missing")
          # print(valorAAcrescentar)
          
          
        }

        if(completeLastValue){
          
          # print("Último Índice")
          valorAAcrescentar= derivative(dataToCompare, indexSearch[indiceElementos-1], elements, indexSearch[indiceElementos]+1)
          # print(i[indiceElementos-1])
          # print(elements)
          # print(i[indiceElementos]+1)
          # print(valorAAcrescentar)
          
        }

        if(completeLastValueMissing){
          
          valorAAcrescentar= (dataToCompare[[elements]]-dataToCompare[[indexSearch[indiceElementos-1]]])/(elements-indexSearch[indiceElementos-1])
          
          # print("Último valor missing")
          # print(valorAAcrescentar)
          
        }

        if(!completeFirstValue && !completeFirstValueMissing && !completeLastValue && !completeLastValueMissing){
          
          
          # print("Novo valor")
          # print(i[indiceElementos-1])
          # print(elements)
          # print(i[indiceElementos+1])
          
          valorAAcrescentar= derivative(dataToCompare, indexSearch[indiceElementos-1], elements, indexSearch[indiceElementos+1])
          
        }

        completeFirstValue= FALSE
        completeFirstValueMissing= FALSE
        completeLastValue= FALSE
        completeLastValueMissing= FALSE
        
        searchingWindowCompare= c(searchingWindowCompare, valorAAcrescentar)
        
           
      }
      
      
      # print("New Searching window is...")
      # print(searchingWindow)
      # print("New Searching windowCompare is...")
      # print(searchingWindowCompare)

      
       
      #####################################
      #dtwResult= dtw(dataWithMissing[i], dataToCompare[indexSearch], keep.internals = TRUE)
      
      
      dtwResult= dtw(searchingWindow, searchingWindowCompare, keep.internals = TRUE)

      
      
      
      
      #aqui ter atenção com as excepções!!!!!! missing antes e depois!
      

      if((missingPositionAux+1)>length(dataToCompare)){
        
        derivativeAnswer= (dataToCompare[[missingPositionAux]]-dataToCompare[[missingPositionAux-1]])/(missingPositionAux - (missingPositionAux-1))
        
        
      }else if((missingPositionAux-1) < 1){
        
        derivativeAnswer= (dataToCompare[[missingPositionAux+1]]-dataToCompare[[missingPositionAux]])/((missingPositionAux+1) - missingPositionAux)
        
      }else{
        
      derivativeAnswer= derivative(dataToCompare, missingPositionAux-1, missingPositionAux, missingPositionAux+1)
      
      }
      #print(dtwResult$normalizedDistance)
      
      listResults= c(listResults, dtwResult$normalizedDistance)
      listResultsIndex= c(listResultsIndex, missingPositionAux)
      listDerivativeAnswer= c(listDerivativeAnswer, derivativeAnswer)
      
    }
    
  } 

  #print(min(listResults))
  #print(listResultsIndex[which(listResults == min(listResults))])
  
  indexMinimo= listResultsIndex[which(listResults == min(listResults))]
  derivadaMinimo= listDerivativeAnswer[which(listResults == min(listResults))]
  
  #print(derivadaMinimo)

  
  #print("Indíce encontrado")
  #print(indexMinimo[1])
  #obter o valor esperado
  
  if(missingPosition == 1 || is.na(dataWithMissing[[missingPosition-1]])){
    
    valorFinal= dataWithMissing[[missingPosition+1]] - derivadaMinimo[1]*((missingPosition+1) - missingPosition)
    
  }else if(missingPosition == length(dataWithMissing) || is.na(dataWithMissing[[missingPosition+1]])){
    
    valorFinal= dataWithMissing[[missingPosition-1]] + derivadaMinimo[1]*((missingPosition+1) - missingPosition)
    
  }else{
  
  valorFinal= 2*derivadaMinimo[1] + dataWithMissing[[missingPosition + 1]]/2 + dataWithMissing[[missingPosition -1]]/2
  
  }
  
  
  
  
  #print("Valor a imputar")
  #print(valorFinal)
  #print(dataToCompare[indexMinimo])

  return(list(min(listResults), indexMinimo[1], valorFinal))
  
}
