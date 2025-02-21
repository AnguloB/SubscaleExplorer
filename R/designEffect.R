designEffect <-function(data=df,  cluster=NULL, round1=3)
  {
  df <- data
  require(multilevel)
  require(reshape2)
  if (sum(count(unique(cluster))[2])<=19){
    warning("There are less than 20 clusters")
  }
  if(is.null(cluster)==TRUE){
    warning("There is no cluster variable")
  }
  else{if(is.null(cluster)==FALSE){
    v <- melt(table(cluster))
    n <- mean(v$value)
    icc1<-function(x, group1=cluster, data1=data){
      out1<-aov(x ~as.factor(group1),data=data1)
      output<-ICC1(out1)
    }
    a<-as.matrix(apply(df,2,icc1))
    
    effect1 <- 1 + (n - 1) * a

    results<-round(data.frame(a, effect1),3)
    names(results)<- c( "ICC1","designEffect" )
    print("n (mean cluster size  when groups are different size)")
    print(n)
    return(results)
  }}
  }
