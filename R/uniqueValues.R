uniqueValues<-function(df, group=NULL){
  require(reshape2)
  #calculate unique values
  f<-function(t){
  p<-rapply(t,function(x)length(unique(x)))
  q<-melt(p)
  names(q) <- c("unique")
  
  a<-rapply(df, function(x)any(is.na(x)))
  b <-as.data.frame(melt(a))
  b [b == TRUE] <- 1 #NA=TRUE is equal to 1
  b [b == FALSE] <- 0# NA=FALSE is equal to 0
  
  n<- q$unique 
  m<- q$unique - b$value
  Df <- data.frame(names(df),m,n) #as data.frame
  names(Df) <- c("variable", "uniqueValid","unique") #change name
  print(Df) #print matrix
  }
  if(is.null(group)==TRUE){
    f(df)
  }
  else{if(is.null(group)==FALSE){
    by(df, group, f)
    
  }}
}
