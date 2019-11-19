library(openxlsx)

#objectToInsert= list(finalResult[[1]], finalResult[[2]], finalResult[[3]], finalResult[[4]], finalResult[[5]], IDAnalisar, carateristica, 152, classBound)

objectToInsert= list(c(paste("Score", finalResult[1], sep = ";", collapse = NULL), paste("Best ID", finalResult[2], sep= ";"), paste("ID a ser analizado", IDAnalisar, sep=";"), paste("Carateristica a ser analizada", carateristica, sep=";"), paste("Missing", missing, sep=";") ) , strsplit(toString(finalResult[[3]]), ""), strsplit(toString(finalResult[[4]]), ""), c("Posições Missings", missingVector), c("Discretização",classBound), c("Valores originais", valueVector))


write.xlsx(objectToInsert, file = "Results\\Teste1.xlsx")
