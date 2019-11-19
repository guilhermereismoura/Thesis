
preparaDados <- function(fileName){
  
  
  library(hash)
  
  dicionarioBD= hash()
  listVariables= list()
  
  dadosExcel= read.csv(file= fileName, header=TRUE, sep=",")
  
  dadosExcel["X1__1"]
  
  
  nDados= length(dadosExcel$subject_id)
  
  
  for (item in names(dadosExcel)){
    
    splitArray= strsplit(item, "__")
    
    if(length(splitArray[[1]])>1 & is.na(match(splitArray[[1]][1], listVariables))){
      
      listVariables = append(listVariables,splitArray[[1]][1])
      
      print(item)
      
    }
      
    
  }
  
  
  
  
  for(id in (1:nDados)){
    
    dicionarioBD[paste("ID", id)] = hash()
    
    for(name in listVariables){
      
      dicionarioBD[[paste("ID", id)]][name] = list()
      
    }
    
  }
  
  
  
  
  #dicionarioBD[["ID 1"]][["X1"]] = c(dicionarioBD[["ID 1"]]["X1"], "abc")
  
  #list= as.list(dicionarioBD)
  
  
  for(item in names(dadosExcel)){
    
    contador= 0
    
  
    
    nomeVariavelTemp = strsplit(item, "__")[[1]][1]
    
    if(nomeVariavelTemp %in% listVariables){
      for(element in dadosExcel[[item]]){
      
  
        
        contador= contador + 1
        #dicionarioBD[[paste("ID", contador)]][[nomeVariavelTemp]] =
        dicionarioBD[[paste("ID", contador)]][[nomeVariavelTemp]] = c(dicionarioBD[[paste("ID", contador)]][[nomeVariavelTemp]], element)
  
      }
      
    }
    
  
  }
  
  return (dicionarioBD)

}
