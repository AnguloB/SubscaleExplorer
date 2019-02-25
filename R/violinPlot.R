violinPlot <-
  function(df, group=NULL, out="default", missing=TRUE, color="#F4A460", 
           colorBox="Spectral",xOrder=TRUE,labList=TRUE, legendLab="Group", 
           title1="",y.lab= " ", alpha=.3,numBreaks=10){
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
    if (is.null(group)) { #plot when there is no group
      
      if( length(as.numeric(names(table(df3$value))))>=15)
      {
        breaks1<-round(seq(min(as.numeric(names(table(df3$value)))),max(as.numeric(names(table(df3$value)))), length.out = numBreaks),1)
      }
      
      else{ breaks1<-  length(as.numeric(names(table(df3$value))))}
      k<-ggplot(df3, aes(x=variable, y=value))+geom_violin(fill=color, trim=FALSE,colour="black", alpha=alpha)+ 
        scale_y_continuous(name= y.lab, breaks=breaks1)+xlab("") +geom_boxplot(width=.1)+
        labs(list(title = title1))+     
        theme_nogrid()
      print(k)
      
    }
    else {     #for groups
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
      
      if( length(as.numeric(names(table(y1$value))))>=15)
      {
        breaks1<-round(seq(min(as.numeric(names(table(y1$value)))),max(as.numeric(names(table(y1$value)))), length.out = numBreaks),1)
      }
      
      else{ breaks1<-  length(as.numeric(names(table(y1$value))))}
      
      if(out=="default"){ #all groups in the same graph
        dodge <- position_dodge(width = 0.8)
        p<-ggplot(y1, aes(x=variable, y=value, fill=group)) + geom_violin (aes(fill = group), position=dodge)+xlab("")+
          scale_y_continuous(name= y.lab, breaks1)+geom_boxplot(width=.1, position=dodge)+
          scale_fill_brewer(name=legendLab,palette=colorBox)+
          labs(list(title = title1))+     
          theme_nogrid()
        return(p)}
      else
      {if(out=="rearrange"){
        plot1 <- function(df, color1=color){
          
          if( length(as.numeric(names(table(df$value))))>=15)
          {
            breaks1<-round(seq(min(as.numeric(names(table(df$value)))),max(as.numeric(names(table(df$value)))), length.out = numBreaks),1)
          }
          
          plot1 <- ggplot(df, aes(x = variable, y = value)) + 
            geom_violin(fill = color, colour = "black", alpha = alpha) + 
            ggtitle(df$group) + scale_y_continuous(name=y.lab,breaks1) + 
            xlab("") + geom_boxplot(width = 0.1) + theme_nogrid()
          return(plot1)}
        {
          plotGroup<-by(y1,y1$group, plot1)
          n <- length(plotGroup)
          nCol <- floor(sqrt(n))
          do.call("grid.arrange", c(plotGroup, ncol=nCol))          }
      }}
    }
  }
