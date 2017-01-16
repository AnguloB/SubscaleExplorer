freqbubble<-function (df, group = FALSE, color = "#666699", title = "", x.lab = "", 
                      y.lab = "Response", angle = 45, alpha = 0.3, lsize = 12) 
{
  require(ggplot2)
  require(plyr)
  require(gridExtra)
  
  
  
  table1<- function(df1, title1=title){
    z <- melt(df1, na.rm = TRUE)
    names(z) <- c("variable", "value", "Freq")
    if (any(is.na(z$value))) {
      g <- as.character(factor(unique(z$value)))
      g1 <- c(g, "NA")
      g2 <- as.character(na.omit(g1))
    }
    else {
      if (!any(is.na(z$value))) {
        g2 <- unique(z$value)
      }
    }
    if (is.integer(z$Freq)) {
      dd <- ("Frequencies")
    }
    else {
      if (is.numeric(z$Freq)) {
        dd <- c("Proportions")
      }
    }
    z$variable <- factor(z$variable, levels = unique(z$variable))
    z$value <- as.character(z$value)
    g <- as.character(factor(unique(z$value)))
    g1 <- c(g, "NA")
    g2 <- as.character(na.omit(g1))
    theme_nogrid <- function(base_size = lsize, base_family = "") {
      theme_bw(base_size = lsize, base_family = base_family) %+replace% 
        theme(panel.grid = element_blank()) + theme(axis.text.x = element_text(size = base_size * 
                                                                                 0.8, lineheight = 0.9, vjust = 0.5, hjust = 1, 
                                                                               angle = angle))
    }
    {
      plot1 <- ggplot(data = z, aes(x = variable, y = value, 
                                    size = Freq)) + geom_point(aes(size = Freq, stat = "identity", 
                                                                   position = "identity"), shape = 20, color = color, 
                                                               alpha = alpha) + scale_size_continuous(name = (dd), 
                                                                                                      range = c(2, 30)) + scale_y_discrete(labels = (g2)) + 
        xlab(x.lab) + ylab(y.lab) + ggtitle(title1) + 
        theme_nogrid()
      warning("no grouping variable requested")
      return(plot1)
    }
  }
  
  if(is.table(df)){
    table1(df)
  }
  else{
    if(is.list(df)){
      plotGroup<-lapply(df,table1)
      n <- length(plotGroup)
      nCol <- floor(sqrt(n))
      do.call("grid.arrange", c(plotGroup, ncol=nCol))  
    }
  }
  
}
