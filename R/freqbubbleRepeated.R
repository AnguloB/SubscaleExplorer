freqbubbleRepeated<-function (df1, df2, color1 = "black", color2 = "#9999CC", Wave1 = "Wave1", 
                               Wave2 = "Wave2", sizeBubble =c(2,30)) 
{
  {
    require(ggplot2)
    require(reshape2)
    z1 <- melt(df1, na.rm = TRUE)
    z2 <- melt(df2, na.rm = TRUE)
    dat1 <- data.frame(z1, z2)
    dat1 <- within(dat1, {
      variable.1 <- NULL
    })
    dat1 <- within(dat1, {
      value.2 <- NULL
    })
    names(dat1) <- c("variable", "value", "f1", "f2")
    dat1$h1 <- 1:nrow(dat1)
    dat1$h1[dat1$h1 > 0.1] <- "P1"
    dat1$h2 <- 1:nrow(dat1)
    dat1$h2[dat1$h2 > 0.1] <- "P2"
    predat3.1 <- subset(dat1, select = c(variable, value, 
                                         f1, h1))
    names(predat3.1) <- c("variable", "value", "f1", "Type")
    predat3.2 <- subset(dat1, select = c(variable, value, 
                                         f2, h2))
    names(predat3.2) <- c("variable", "value", "f1", "Type")
    dat3 <- rbind(predat3.1, predat3.2)
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
    plot1 <- ggplot(dat3, aes(x = variable, y = value, size = 1, 
                              color = Type)) + geom_point(aes(size = f1, color = Type), 
                                                          shape = 19, alpha = 0.5) + scale_size_continuous(name = (dd), 
                                                                                                           range = sizeBubble) + scale_y_discrete(labels = (g2)) + 
      scale_colour_manual(values = c(color1, color2), label = c(Wave1, 
                                                                Wave2)) + theme_nogrid()
    print(plot1)
  }
}