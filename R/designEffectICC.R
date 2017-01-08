designEffectICC <-function(df, group=group, data=df, round1=3)
  {
    v <- melt(table(group))
    n <- mean(v$value)
    icc1<-function(x, group1=group, data1=data){
      out1<-aov(x ~as.factor(group1),data=data1)
      output<-ICC1(out1)
    }
    a<-as.matrix(apply(df,2,icc1))
    effect <- 1 + (n - 1) * a
    results<-round(data.frame(a, effect), round1)
    names(results)<- c( "ICC1", "designEffect")
    print("n ( mean cluster size  when groups are different size)")
    print(n)
    return(results)
  }


