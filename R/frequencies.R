frequencies <-
function(df, group=NULL, missing=TRUE , prop=FALSE, round1=2){
  require(reshape2)
  table1<-function(df, missing1=missing, prop1=prop){
    if (missing1==TRUE) {
      if (prop1==TRUE) {
        t <-melt(df, na.rm=FALSE)
        u<-table(t, exclude=NULL)
        w<-round(prop.table(u,1), round1)
        n<-dim(w)[1]
        answer1<-w[1:(n-1),]}
      else {if(prop1==FALSE){
        t <-melt(df, na.rm=FALSE)
        w<-table(t, exclude=NULL)
        n<-dim(w)[1]
        answer1<-w[1:(n-1),]}}}
    else  { if (missing1==FALSE) {
      if (prop1==TRUE) {
        t <-melt(df)
        u<-table(t)
        answer3<-round(prop.table(u,1), round1)}
      else {if(prop1==FALSE){
        t <-melt(df)
        answer4<-table(t)}}}
    }}
  
  if (is.null(group)) {
    answer<-table1(df)
    print(answer)
  }
  else {
    if (!is.data.frame(group) && !is.list(group) && (length(group) < 
                                                     +                                                        NROW(df))) 
      group <- df[, group]
    answer <- by(df, group, table1)
    print(answer)
  }}
