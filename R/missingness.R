missingness<-
function(df, group=NULL, pattern=FALSE,showNA=FALSE){
    require(mice)
    table1<-function(f, pattern1=pattern, casesAllMissing=showNA){
      l<-c("-------")
      l1 <- c("-------")
      bnn<-f 
      t<-sum(complete.cases(f)) # complete cases count
      u<-sum(!complete.cases(f)) # incomplete cases count 
      v<-nrow(f) #total cases
      v1<-ncol(f) #total variables
      v2<-v*v1 #total cases x total variables (total cell)
      p33<-sum(is.na(f)) #totalmissings per row and column
      p1 <- round(p33/v2, 5) #calculate Proportion of missingness
      g<- sum(!rowSums(!is.na(f))) #Complete missing data 
      b <- t+u-g #Some or all items answered (complete cases+incompletecases - complete missing data)
      t1<- round(t/v,2) # n complete cases/ total cases
      u1<- round(u/v,2) #n incomplete cases /total cases
      b1<- round(b/v,2)  #some or all items answered /total cases
      aad<- c("")
      
      y<-round(t(data.frame(t,u,b,g, p1, v, v1, v, p33, v2, p1)),2)
      yy1<-data.frame(aad,l, t,u,l,b,g,l1, aad,v1,v, p33, v2,aad) #final
      yy2<-t(yy1)
      y2<-round(data.frame(t1, u1, b1,g,p1,v,v1,v, p33, v2, p1),2)
      y3<-data.frame(aad,l,y2$t1, y2$u1,l, y2$b1,round(y2$g/v,2),l1,aad, aad,aad,aad, aad, p1) #final
      y4<-t(y3)
      title <-c("CASE MISSINGNESS"," ", "Cases with all items answered", "Cases with incomplete data", " ","Cases with some or all items answered", 
                "Cases with completely missing data","-----------------------", "CELL MISSINGNESS", "n variables"," n cases",  
                "n missing cells",  "Total cases x Total variables", "Proportion of cell missingness")
      j1<-data.frame(title , yy2, y4)
      names(j1)<- c( "Description",  "Freq", "Prop")
      row.names(j1)<- NULL
      a<-as.data.frame(which(!rowSums(!is.na(f))))
      names(a)<- ("Rows with completely missing data")
      row.names(a)<- NULL
      kl<-apply(f,2,function(x)(sum(is.na(x))))
      kl1<-as.data.frame(kl)
      names(kl1)<-("na")
      gh<-apply(kl1,1, function(x)(x/v))
      gh1<-as.data.frame(gh)
      names(gh1)<-c("prop.na")
      gg<-data.frame(names(f),kl1$na, gh1$prop.na)
      names(gg)<-c("variable", "na", "prop.na")
      gg$prop.na<-round(gg$prop.na, 3)
    
      mdp<- md.pattern(f)
      if(nrow(mdp)>=3){
      nam <-rownames(mdp)
      row.names(mdp)<- NULL
      nam <- nam[-length(nam)]
      g<-nrow(mdp)-1
      n<-c(nam,"NA summary")
      pat<-data.frame(n,mdp)
      vv1 <- pat[-length(pat)]
      fvv<-nrow(pat)
      u1<- matrix(c(rep.int("",length(fvv))),nrow=fvv,ncol=1)
      hfg<-data.frame(vv1,u1,pat[,ncol(pat)])
      hfg1<-names(hfg)
      bb2 <- hfg1[-length(hfg1)]
      bb3 <- bb2[-length(bb2)]
      bb1 <- c("")
      f<-c(bb3,bb1)
      names(hfg)<-f
      falta<-tail(hfg, n=1)
      n<-dim(hfg)[1]
      hfg11<-hfg[1:(n-1),]
      temprow <- matrix(c(rep.int("--",length(hfg11))),nrow=1,ncol=length(hfg11))
      newrow <- data.frame(temprow)
      colnames(newrow) <- colnames(hfg11)
      hfg2 <- rbind(hfg11,newrow)
      mdp1<-rbind(hfg2,falta)
      }
      
      if(nrow(mdp)<=2){
      mdp1<- c("All items have been answered")}
      
      print("-------------------------------------------------------")
      print("Missingness summary")
      print(j1)
      if(casesAllMissing==TRUE) {
             if(nrow(a)>=1){
                 print(a)}
               else{
                 if(nrow(a)==0){
                   print("No rows with completely missing data")
                 }}}
      print("Total missing variable")
      print(gg)
      if(pattern1==TRUE){
         print("Missing Data Pattern")
         print(mdp1)
      }
      print("-------------------------------------------------------")
      return(j1)
    }
    if (is.null(group)) {
      
      answer<-table1(df)
      return(answer)
    }
    else {
      if (!is.data.frame(group) && !is.list(group) && (length(group) <                            +                                                        NROW(df))) 
        group <- df[, group]
      table1(df)
      answer <- by(df, group, table1)
      return(answer)
    }
  }
