freqbubble<-function (df, group=FALSE,color = "#666699", title = "", x.lab = "", y.lab = "Response", 
                       angle = 45, alpha = 0.3, lsize = 12,  color1 = "black", color2 = "#9999CC", group1 = "Group1", group2 = "Group2") 
  
{
  require(ggplot2)
  require(plyr)
  if (group=FALSE) {
    z <<- melt(df, na.rm = TRUE)
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
                                                                                 0.8, lineheight = 0.9, vjust = 0.5, hjust = 1, angle = angle))
    }
    {
      plot1 <- ggplot(data = z, aes(x = variable, y = value, 
                                    size = Freq)) + geom_point(aes(size = Freq, stat = "identity", 
                                                                   position = "identity"), shape = 20, color = color, 
                                                               alpha = alpha) + scale_size_continuous(name = (dd), 
                                                                                                      range = c(2, 30)) + scale_y_discrete(labels = (g2)) + 
        xlab(x.lab) + ylab(y.lab) + ggtitle(title) + theme_nogrid()
      warning("no grouping variable requested")
      print(plot1)
    }}
  
  else{
    if(group==TRUE)
    {
      df1<-df[1]
      df2<-df[2]
      z1 <- melt(df1, na.rm = TRUE)
      z1<<-data.frame(z1)
      z2 <- melt(df2, na.rm = TRUE)
      z2<<-data.frame(z2)
      
      dat1 <<- rbind.fill(z1, z2)
      dat1[is.na(dat1)] <- "NA"   
      
      names(dat1)<- c("variable", "value", "f1", "Type")
      
      dat3<-dat1
      
      dat3$value[is.na(dat3$value)] <- "NA"
      if (is.integer(dat1$f1)) {
        dd <- ("Frequencies")
      }
      else {
        if (is.numeric(dat1$f1)) {
          dd <- c("Proportions")
        }
      }
      if (any(is.na(dat1$value))) {
        g <- as.character(factor(unique(dat1$value)))
        g1 <- c(g, "NA")
        g2 <- as.character(na.omit(g1))
      }
      else {
        if (!any(is.na(dat1$value))) {
          g2 <- unique(dat1$value)
        }
      }
    }
    theme_nogrid <- function(base_size = 12, base_family = "") {
      theme_bw(base_size = 12, base_family = base_family) %+replace% 
        theme(panel.grid = element_blank()) + theme(axis.text.x = element_text(size = base_size * 
                                                                                 0.8, lineheight = 0.9, vjust = 0.5, hjust = 1, angle = 45))
    }
    {
      plot1 <- ggplot(dat3, aes(x = variable, y = value, size = f1, color = Type)) + 
        geom_point(aes(size = f1, color = Type), shape = 19, alpha = 0.5) + 
        scale_size_continuous(name = (dd), range = c(2, 30)) + 
        scale_y_discrete(labels = (g2)) + 
        scale_colour_manual(values = c(color1, color2), label = c(group1,group2)) + 
        theme_nogrid()
      print(plot1)
    }  
  }}
