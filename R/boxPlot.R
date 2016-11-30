
boxPlot <-
  function(df, group=NULL, same=TRUE, missing=TRUE, color="#F4A460", group1="Group1", group2="Group2", legendLab="Group", title1=""){
    require(ggplot2)
    require(reshape2)
    require(plyr)
    theme_nogrid <- function (base_size = 12, base_family = "")
    {theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(panel.grid = element_blank(), plot.title=element_text(hjust=0.5))+
        theme(axis.text.x =element_text(size = base_size * 0.8 , lineheight = 0.9,
                                        vjust = 0.5, hjust=1, angle=45))}
    if(missing==TRUE)
    {
      df3<-data.frame(melt(df))}
    else
      if(missing==FALSE) {
        df2<-melt(df)
        df3 <- df2[!is.na(df2$value), ]}
    if (is.null(group)) {
      k<-ggplot(df3, aes(x=variable, y=value))+geom_boxplot(fill=color, colour="black", alpha=0.3)+
        scale_y_continuous(breaks=as.numeric(names(table(df3$value))))+xlab("")+
        labs(list(title = title1))+
        theme_nogrid()
      warning("no grouping variable requested")
      print(k)
    }
    else {
      if (!is.data.frame(group) && !is.list(group) && (length(group) <                                            +                                                        NROW(df)))
        group <- df[, group]
      g<-data.frame(group,df)
      g$group<-factor(g$group,labels = c(group1, group2))
      
      y<-melt(g)
      if(missing==TRUE)
      {
        y1<-y}
      else
        if(missing==FALSE) {
          y1 <- na.omit(y)}
      if(same==TRUE){
        p<-ggplot(y1, aes(variable, value)) + geom_boxplot(aes(fill = group))+ xlab("")+
          scale_y_continuous(as.numeric(names(table(df$value))))+
          scale_fill_discrete(name=legendLab)+
          labs(list(title = title1))+
          theme_nogrid()
        print(p)}
      else{if(same==FALSE){
        plot1 <- function(df){
          p<-ggplot(df, aes(x=variable, y=value))+geom_boxplot(fill="#666699", colour="black", alpha=0.3)+ggtitle(df$group)+
            scale_y_continuous(as.numeric(names(table(df$value))))+ xlab("")+theme_nogrid()
          print(p)}
        {pdf("boxplotJ3.pdf")
          d_ply(y1, .(group),plot1)
          dev.off()}
      }}
    }
  }
