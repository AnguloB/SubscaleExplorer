violinPlot1 <-
  function(df, group=NULL, same=TRUE, missing=TRUE, color="#F4A460",labList=NULL, legendLab="Group", title1="Title"){
    theme_nogrid <- function (base_size = 12, base_family = "")
    {theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(panel.grid = element_blank(), plot.title=element_text(hjust=0.5))+
        theme(axis.text.x =element_text(size = base_size * 0.8 , lineheight = 0.9,
                                        vjust = 0.5, hjust=1, angle=45))}
    if(is.null(labList)){
      labList1<-levels(factor(group))
    }else
    {labList1<-labList}

    if(missing==TRUE)
    { 
      df3<<-data.frame(melt(df))} #dataframe with missings to be ploted
    else 
      if(missing==FALSE) { #dataframe without missings to be ploted
        df2<-melt(df)
        df3 <- df2[!is.na(df2$value), ]}
    
    if (is.null(group)) { #plot when there is no group
      k<-ggplot(df3, aes(x=variable, y=value))+geom_violin(fill=color, colour="black", alpha=0.3)+ 
        scale_y_continuous(breaks=as.numeric(names(table(df3$value))))+xlab("")+geom_boxplot(width=.1)+
        labs(list(title = title1))+     
        theme_nogrid()
      print(k)
      warning("no grouping variable requested")
      
    }
    else {     #for groups
      if (!is.data.frame(group) && !is.list(group) && (length(group) <                                            +                                                        NROW(df))) 
        group <- df[, group]
      g<-data.frame(group,df)
      g$group<-factor(g$group,labels = labList1)
      y<-melt(g)
      if(missing==TRUE)   #data with missing when there is a group
      { 
        y1<-y}
      
      else 
        if(missing==FALSE) { #data without missing when there is  a group
          y1 <- na.omit(y)}
      
      if(same==TRUE){ #all groups in the same graph
        dodge <- position_dodge(width = 0.8)
        p<-ggplot(y1, aes(x=variable, y=value, fill=group)) + geom_violin (aes(fill = group), position=dodge)+xlab("")+
          scale_y_continuous(as.numeric(names(table(y1$value))))+geom_boxplot(width=.1, position=dodge)+
          scale_fill_discrete(name=legendLab)+
          labs(list(title = title1))+     
          theme_nogrid()
        print(p)}
      else{if(same==FALSE){ #produces a pdf with a page per group
        plot1 <- function(df){
          p<-ggplot(df, aes(x=variable, y=value))+geom_violin(fill=color, colour="black", alpha=0.3)+ggtitle(df$group)+ 
            scale_y_continuous(as.numeric(names(table(y1$value))))+ xlab("")+geom_boxplot(width=.1)+theme_nogrid()
          print(p)}
        {pdf("violinJ3.pdf")   #pdf name file
          d_ply(y1, .(group),plot1) 
          dev.off()}
      }} 
    }
  }
