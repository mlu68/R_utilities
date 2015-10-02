## Miscellaneous R functions ##

####################################################################################
## Keep only most frequent modalities
frequentModalities <- function(col, nmax) {
  
  S <- data.frame(unlist(table(col)))
  
  S <- S[order(S$Freq, decreasing = TRUE),]
  
  S <- S[1:nmax,]
  
  freqMod <- as.character(S$col)
  
  col[!(col %in%  freqMod)] <- 'OTHER'
  
  assign('col', col, envir=.GlobalEnv)
  
}

####################################################################################
## NA replace
NaReplace <- function(x,valnew){
  
  if(is.vector(x))
    
    x[is.na(x)] <- rep(valnew,length(x[is.na(x)]))
  
  else
    
    for(i in  1:ncol(x))
      
      x[is.na(x[,i]),i] <- rep(valnew,length(x[is.na(x[,i]),i]))
    
    return(x)
}