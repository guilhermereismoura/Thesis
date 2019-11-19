rm(list=ls())



library(bnspatial)
library(NameNeedle)
library(Biostrings)
source("preparaDados.R")
source("Discretizacao.R")
source("buildScoreMatrix.R")
source("searchBestAlignment.R")
source("fazMissings.R")


#dadosExcel= read.csv(file= "D:\\guilh\\Documents\\tese\\codigo\\meu\\data.csv", header=TRUE, sep=";")
#original= read.csv(file= "D:\\guilh\\Documents\\tese\\codigo\\meu\\data2.csv", header=TRUE, sep=";")
#original2= read.csv(file= "D:\\guilh\\Documents\\tese\\codigo\\meu\\data.csv", header=TRUE, sep=";")




#####################################################Escolha de parâmetros###################################################
IDAnalisar= "ID 74"
carateristica= "X1"

gapOpening= -0.8
gapExtension= -0.3

missing= 0

#missing = 0
#missing <1
#missing >=1

missingVector= c( 37)


#Missing value
dataBase = preparaDados("excelFiles\\ECG.csv")


tempResultMissings= fazMissings(dataBase, IDAnalisar, carateristica, missing, missingVector)

dataBase= tempResultMissings[[1]]
missingVector= tempResultMissings[[2]]
valueVector= tempResultMissings[[3]]


#dataBase[[IDAnalisar]][[carateristica]][missing] = NA





####################################################Preparar dados###########################################################



#Número de Bins para discretização (limitado ao alfabeto)
nBins= floor(sqrt(length(dataBase[[IDAnalisar]][[carateristica]])))




#####################################################Discretização
#testeData$value[which(!is.na(testeData$value))]


resultDiscrete = Discretizacao(dataBase, carateristica, nBins, IDAnalisar)

missingString= resultDiscrete[[2]]

classBound= resultDiscrete[[3]]



#####################################Formar a matriz
#Mismatch score / Match score / Missing mismatch score

mat= buildScoreMatrix(nBins, -1, 1, -0.5)


#####################################


#remove a sequência que se está a procurar
del(IDAnalisar, dataBase)



finalResult= searchBestAlignment(dataBase, carateristica, missingString, mat, gapOpening, gapExtension)