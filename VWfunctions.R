## R functions for VW ##

####################################################################################
## Function 1 df => VW
df2vwBrut <- function(df , target){
  
  lineHolders <- df[,target]
  
  dfvar <- df[, names(df) !=target]
  
  v <- paste0(rep(names(dfvar), each = nrow(dfvar)) , "_", unlist(dfvar))
  
  ma <- matrix(v , ncol = ncol(dfvar) , nrow = nrow(df) , byrow = F)
  
  ma <- apply(ma , 1 , paste , collapse = " ")
  
  v2 <- paste0(rep(names(dfvar), each = nrow(dfvar)) , ":", unlist(dfvar))
  
  ma2 <- matrix(v2 , ncol = ncol(dfvar) , nrow = nrow(df) , byrow = F)
  
  ma2 <- apply(ma2 , 1 , paste , collapse = " ")
  
  nlines <- 1:nrow(df)
  
  DF <- paste(lineHolders, " ", nlines, "|f", " ", ma, " ", ma2, sep = '')
  
  DF
}

####################################################################################
## Function 2 df => VW
## Version avec traitement adapté numériques/caractères
cols2Lines <- function(df, sep) {
  
  v <- paste0(rep(names(df), each = nrow(df)) , sep, unlist(df))
  
  ma <- matrix(v , ncol = ncol(df) , nrow = nrow(df) , byrow = F)
  
  ma <- apply(ma , 1 , paste , collapse = " ")
  
  ma
  
}

df2vwNumCar <- function(df , target){
  
  lineHolders <- df[,target]
  
  dfvar <- df[, names(df) !=target]
  
  numericVar <- dfvar[,sapply(dfvar, is.numeric)]
  
  characterVar <- dfvar[,!sapply(dfvar, is.numeric)]
  
  maNum <- cols2Lines(numericVar, ':')
  
  maCar <- cols2Lines(characterVar, '_')
  
  nlines <- 1:nrow(df)
  
  DF <- paste(lineHolders, " ", nlines, "|f", " ", maCar, " ", maNum, sep = '')
  
  DF
}
