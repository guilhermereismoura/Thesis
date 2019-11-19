derivative <- function(serie, pastValue, presentValue, futureValue){
 
  
  output= (serie[[presentValue]] - serie[[futureValue]] + (serie[[futureValue]] - serie[[pastValue]])/2)/2;
  
  return(output)
   
}