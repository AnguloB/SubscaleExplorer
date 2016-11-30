uniqueValues <-
function(df)
{
  require(reshape2)
  #calculate unique values
  
  p<-rapply(df,function(x)length(unique(x)))
  q<-melt(p)
  names(q) <- c("unique")
  
  #calculate NA in order to calculate uniquevalid
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
