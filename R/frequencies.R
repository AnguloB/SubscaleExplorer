frequencies <-
function(df, group=NULL, missing=TRUE , prop=FALSE, round1=2){
  require(reshape2)
  table1<-function(df, missing1=missing, prop1=prop){
    if (missing1==TRUE) {
      if (prop1==TRUE) {
        t <-melt(df, na.rm=FALSE)
        u<-table(t, exclude=NULL)
        answer1<-round(prop.table(u,1), round1)}
      else {if(prop1==FALSE){
        t <-melt(df, na.rm=FALSE)
        answer1<-table(t, exclude=NULL)}}}
    else  { if (missing1==FALSE) {
      if (prop1==TRUE) {
        t <-melt(df)
        u<-table(t)
        answer3<-round(prop.table(u,1), round1)}
      else {if(prop1==FALSE){
        t <-melt(df)
        answer4<-table(t)}}}
    }}
  
  table2<- function(df, missing1=missing, prop1=prop){
    
    if(missing==TRUE){
      if(prop==FALSE){
       table(df, useNA="always")}
      else{if(prop==TRUE){
        answer<-round(prop.table(table(df, useNA="always")), round1)
      }}}
    else{ if(missing==FALSE){
      if(prop==FALSE){
        table(df)}
      else{if(prop==TRUE){
        answer<-round(prop.table(table(df)), round1)
      }}}}
  }
  
  if (is.null(group)) {
    if(is.vector(df)==FALSE){
    answer<-table1(df)
    print(answer)}
    else{if(is.vector(df)==TRUE){
      answer<-table2(df)
      print(answer)
    }}
  }
  else {
    if (!is.data.frame(group) && !is.list(group) && (length(group) < 
                                                     +                                                        NROW(df))) 
      group <- df[, group]
    if(is.vector(df)==FALSE){
      answer <- by(df, group, table1)
      print(answer)}
    else{if(is.vector(df)==TRUE){
      answer <- by(df, group, table2)
      print(answer)
    }}
  }}
