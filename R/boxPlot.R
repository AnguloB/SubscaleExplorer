boxPlot <-
  function(df, group=NULL, out="default", missing=TRUE, color="#F4A460", labList=TRUE, legendLab="Group", title1="", xOrder=TRUE){
    require(ggplot2)
    require(reshape2)
    require(plyr)
    require(gridExtra)
    theme_nogrid <- function (base_size = 12, base_family = "")
    {theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(panel.grid = element_blank(), plot.title=element_text(hjust=0.5))+
        theme(axis.text.x =element_text(size = base_size * 0.8 , lineheight = 0.9,
                                        vjust = 0.5, hjust=1, angle=45))}
    
    if(is.vector(df)==FALSE){
      if(xOrder==TRUE){
        df11<-df
        df1<- as.data.frame(df11[,order(colnames(df11))])}
      else{if(xOrder==FALSE){
        df1<-df
      }}
    }
    else{if(is.vector(df)==TRUE){df1<-df}}
    
    if(labList==TRUE){
      labList1<-levels(factor(group))
    }
    else{
      if(is.vector(labList)==TRUE)
      {labList1<-labList}}
    
    if(missing==TRUE)
    {
      if(is.vector(df1)==TRUE)
      {
        df3<-data.frame(replace(1:length(df1),1:length(df1),1),df1)
        names(df3)<- c("variable", "value")
      }
      else{ if(is.vector(df1)==FALSE){
        df3<-data.frame(melt(df1))
      }}
    }
    else
      if(missing==FALSE) {
        if(is.vector(df1)==TRUE)
        {
          df3<-data.frame(replace(1:length(df1),1:length(df1),1),df1)
          names(df3)<- c("variable", "value")
        } 
        else{ if(is.vector(df1)==FALSE){
          df2<-data.frame(melt(df1))
          df3 <- df2[!is.na(df2$value), ]
        }}
      }
    
    if (is.null(group)) {
      k<-ggplot(df3, aes(x=variable, y=value))+geom_boxplot(fill=color, colour="black", alpha=0.3)+
        scale_y_continuous(breaks=as.numeric(names(table(df3$value))))+xlab("")+
        labs(list(title = title1))+
        theme_nogrid()
      warning("no grouping variable requested")
      return(k)
    }
    else {
      if (!is.data.frame(group) && !is.list(group) && (length(group) <                                            +                                                        NROW(df1)))
        group <- df1[, group]
      g<-data.frame(group,df1)
      g$group<-factor(g$group,labels = labList1)
      if(is.vector(df1)==TRUE)
      {
        z1<-data.frame(replace(1:nrow(g),1:nrow(g),1),g)
        names(z1)<- c("variable", "group", "value")
        y<-z1
      }
      else{ if(is.vector(df1)==FALSE){
        y<-melt(g)
        
      }}
      if(missing==TRUE)
      {
        y1<-y}
      else
        if(missing==FALSE) {
          y1 <- na.omit(y)}
      if(out=="default"){
        p<-ggplot(y1, aes(variable, value)) + geom_boxplot(aes(fill = group))+ xlab("")+
          scale_y_continuous(as.numeric(names(table(y1$value))))+
          scale_fill_discrete(name=legendLab)+
          labs(list(title = title1))+
          theme_nogrid()
        return(p)}
      else

      {if(out=="rearrange"){
        plot1 <- function(df, color1=color){
          plot1<-ggplot(df, aes(x=variable, y=value))+geom_boxplot(fill=color1, colour="black", alpha=0.3)+ggtitle(df$group)+
            scale_y_continuous(as.numeric(names(table(df$value))))+ xlab("")+theme_nogrid()
          }
        {
          plotGroup<-by(y1,y1$group, plot1)
          n <- length(plotGroup)
          nCol <- floor(sqrt(n))
          do.call("grid.arrange", c(plotGroup, ncol=nCol))          }
      }
      }
    }
  }
