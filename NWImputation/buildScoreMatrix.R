buildScoreMatrix <- function(nBins, missmatchScore, matchScore, missingMatch){
  
  
  mat <- matrix(missmatchScore, nrow = nBins + 1, ncol = nBins + 1)
  rownames(mat) = c(letters[1:nBins], '?')
  colnames(mat) = c(letters[1:nBins], '?')
  
  for (i in seq_len(nBins + 1)) mat[i, i] <- matchScore
  
  for (i in seq_len(nrow(mat))) mat[i, ncol(mat)] <- missingMatch
  
  for (i in seq_len(ncol(mat))) mat[nrow(mat), i] <- missingMatch
  
  mat[nrow(mat), ncol(mat)] = missmatchScore
  
  return(mat)
  
  
}