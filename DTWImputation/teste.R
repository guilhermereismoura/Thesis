library(openxlsx)

#objectToInsert= list(finalResult[[1]], finalResult[[2]], finalResult[[3]], finalResult[[4]], finalResult[[5]], IDAnalisar, carateristica, 152, classBound)

objectToInsert= list(c(paste("Score", bestScore, sep = ";", collapse = NULL), paste("Best ID", bestId, sep= ";"), paste("ID a ser analizado", IDAnalisar, sep=";"), paste("Carateristica a ser analizada", carateristica, sep=";"), paste("Missing", missing, sep=";")), c("Valores encontrados por ordem", foundVector) , c("Posições Missings", missingVector), c("Valores originais", valueVector), c("Indices encontrados por ordem", bestPositionVector), c("ID encontrado por ordem", IDVector))


write.xlsx(objectToInsert, file = "Results\\Teste.xlsx")


