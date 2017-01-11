designEffect <-function(df, cluster=cluster, data=df, round1=3)
  {
  require(multilevel)
  require(reshape2)
    v <- melt(table(cluster))
    n <- mean(v$value)
    icc1<-function(x, group1=cluster, data1=data){
      out1<-aov(x ~as.factor(group1),data=data1)
      output<-ICC1(out1)
    }
    a<-as.matrix(apply(df,2,icc1))
    rec<-a
    change<-a
    rec[a<=0]<-0
    change[change<=0]<- "**"
    change[change>0] <-""
    
    effect1 <- 1 + (n - 1) * a
    effect2 <- 1 + (n - 1) * rec
    
    results<-round(data.frame(a,rec, effect1, effect2),3)
    results<-data.frame(results, change)
    names(results)<- c( "ICC1","ICC1corrected" ,"designEffect", "designEffectCorrected", "" )
    print("n ( mean cluster size  when groups are different size)")
    print(n)
    return(results)
  }
