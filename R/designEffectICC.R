designEffectICC <-
function(df, cluster){
  require(multilevel)  
  require (nlme)
  require (reshape2)
  v<-melt(table(cluster))
  n<-mean(v$value)
  icc12<-mult.icc(df, cluster)
  icc1<-icc12$ICC1
  ICC11<-round(icc1,3)
  effect <-1+(n-1)*icc1
  Df <- data.frame(icc12$Variable, ICC11, effect)
  names(Df) <- c("variable", "ICC1", "design_effect")
  print("n ( mean cluster size  when groups are different size)")
  print(n)
  print(Df)
  return(Df)
}
