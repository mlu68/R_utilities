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

####################################################################################
## Lag function
lagpad <- function(x, k=1) {
  i<-is.vector(x)
  if(is.vector(x)) x<-matrix(x) else x<-matrix(x,nrow(x))
  if(k>0) {
    x <- rbind(matrix(rep(NA, k*ncol(x)),ncol=ncol(x)), matrix(x[1:(nrow(x)-k),], ncol=ncol(x)))
  }
  else {
    x <- rbind(matrix(x[(-k+1):(nrow(x)),], ncol=ncol(x)),matrix(rep(NA, -k*ncol(x)),ncol=ncol(x)))
  }
  if(i) x[1:length(x)] else x
}

####################################################################################
## Last observation moved forward function for NA replacement
na.lomf <- function(x) {
  
  na.lomf.0 <- function(x) {
    non.na.idx <- which(!is.na(x))
    if (is.na(x[1L])) {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  }
  
  dim.len <- length(dim(x))
  
  if (dim.len == 0L) {
    na.lomf.0(x)
  } else {
    apply(x, dim.len, na.lomf.0)
  }
}