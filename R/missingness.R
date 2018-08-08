missingness<-
  function(df, group=NULL, pattern=FALSE,showNA=FALSE){
    require(mice)
    table1<-function(f, pattern1=pattern,group=NULL,  casesAllMissing=showNA){
      l<-c("-------")
      l1 <- c("-------")
      
      propMi <- round(sum(is.na(f))/(nrow(f)*ncol(f)), 5) #calculate Proportion of missingness
      #antic g--> compMi
      compMi<- sum(!rowSums(!is.na(f))) #Complete missing data 
      b <- sum(complete.cases(f))+sum(!complete.cases(f))-compMi #Some or all items answered (complete cases+incompletecases - complete missing data)
      t1<- round(sum(complete.cases(f))/nrow(f),2) # n complete cases/ total cases
      u1<- round(sum(!complete.cases(f))/nrow(f),2) #n incomplete cases /total cases
      b1<- round(b/nrow(f),2)  #some or all items answered /total cases
      aad<- c("")
      
      y<-round(t(data.frame(sum(complete.cases(f)),sum(!complete.cases(f)),b,compMi, propMi, nrow(f), ncol(f), nrow(f), sum(is.na(f)), nrow(f)*ncol(f), propMi)),2)
      yy1<-data.frame(aad,l, sum(complete.cases(f)),sum(!complete.cases(f)),l,b,compMi,l1, aad,ncol(f),nrow(f), sum(is.na(f)), nrow(f)*ncol(f),aad) #final
      yy2<-t(yy1)
      y2<-round(data.frame(t1, u1, b1,compMi,propMi,nrow(f),ncol(f),nrow(f), sum(is.na(f)), nrow(f)*ncol(f), propMi),2)
      y3<-data.frame(aad,l,y2$t1, y2$u1,l, y2$b1,round(y2$compMi/nrow(f),2),l1,aad, aad,aad,aad, aad, propMi) #final
      y4<-t(y3)
      title <-c("CASE MISSINGNESS"," ", "Cases with all items answered", "Cases with incomplete data", " ","Cases with some or all items answered", 
                "Cases with completely missing data","-----------------------", "CELL MISSINGNESS", "n variables"," n cases",  
                "n missing cells",  "Total cases x Total variables", "Proportion of cell missingness")
      j1<-data.frame(title , yy2, y4)
      names(j1)<- c( "Description",  "Freq", "Prop")
      row.names(j1)<- NULL
      a<-as.data.frame(which(!rowSums(!is.na(f))))
      names(a)<- (" id case with completely missing data")
      row.names(a)<- NULL
      kl<-apply(f,2,function(x)(sum(is.na(x))))
      kl1<-as.data.frame(kl)
      names(kl1)<-("na")
      gh<-apply(kl1,1, function(x)(x/nrow(f)))
      gh1<-as.data.frame(gh)
      names(gh1)<-c("prop.na")
      gg<-data.frame(names(f),kl1$na, gh1$prop.na)
      names(gg)<-c("variable", "na", "prop.na")
      gg$prop.na<-round(gg$prop.na, 3)
      
      mdp<- md.pattern(f, plot=FALSE)
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
      output1<-list(j1, gg)
      names(output1)<- c("General Summary", "Variable summary")
      
      if(casesAllMissing==TRUE) {
        if(nrow(a)>=1){
          output1<-list(j1,gg,  a)
          names(output1)<- c("General Summary", "Variable summary", "cases with completely missing data")
        }
        else{
          if(nrow(a)==0){
            output1<-list(j1,gg,  c("No rows with completely missing data"))
            names(output1)<- c("General Summary", "Variable summary", "cases with completely missing data")
          }}}
      if(pattern1==TRUE){
        output1<-list(j1, gg, mdp1)
        names(output1)<- c("General Summary", "Variable summary",  "Pattern")
        
      }
      return(output1)   }
    if (is.null(group)) {
      
      answer<-table1(df)
      return(answer)
    }
    else {
      if (!is.data.frame(group) && !is.list(group) && (length(group) <                            +                                                        NROW(df))) 
        group <- df[, group]
      table1(df)
      answer <- by(df, group, table1)
      names(answer)<- names(table(group))
      return(answer)
    }
  }
