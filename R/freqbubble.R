freqbubble<-function (df, group = FALSE, numbers=TRUE,color = "#666699", title = "", x.lab = "", 
                      y.lab = "Response", angle = 45, alpha = 0.3, lsize = 12, 
                      xOrder1 = TRUE, bubbleSize = c(2, 30), legend= TRUE, numberSize=3) 
{
  require(ggplot2)
  require(plyr)
  require(gridExtra)
  if (group == FALSE) {
    warning("no grouping variable requested")
  }
  table1 <- function(df1, title1 = title, xOrder=xOrder1) {
    
    if (xOrder == TRUE) {
      df1 <- df1[sort(rownames(df1)), ]
    }
    else {
      if (xOrder == FALSE) {
        df1 <- df1
      }
    }
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
    z$value <- factor(z$value)
    g <- as.character(factor(unique(z$value)))
    g1 <- c(g, "NA")
    g2 <- as.character(na.omit(g1))
    
    
    theme_nogrid <- function(base_size = lsize, base_family = "") {
      theme_bw(base_size = lsize, base_family = base_family) %+replace% 
        theme(panel.grid = element_blank()) + 
        theme(axis.text.x = element_text(size = base_size * 0.8, lineheight = 0.9, vjust = 0.5, hjust = 1,  angle = angle))
    }
    {
      plot1 <- ggplot(data = z, aes(x = variable, y = value, size = Freq, label=Freq)) + 
        geom_point(aes(size = Freq, stat = "identity", position = "identity"), shape = 20, color = color, alpha = alpha) + 
        scale_size_continuous(name = (dd),range = bubbleSize) + xlab(x.lab) + ylab(y.lab) + 
        ggtitle(title1) + scale_y_discrete()+
        theme_nogrid()
      if(numbers ==TRUE){
        plot1<-plot1 + geom_text(color="black", size= numberSize)}
      
      if(legend==FALSE){ 
        plot1<-plot1+ theme(legend.position="none")
      }
      return(plot1)
    }
  }
  if(is.table(df)){
    table1(df)
  }
  else{
    if (is.list(df)) {
      plotGroup <- lapply(df, table1)
      n <- length(plotGroup)
      nCol <- floor(sqrt(n))
      do.call("grid.arrange", c(plotGroup, ncol = nCol))
    }}
}
