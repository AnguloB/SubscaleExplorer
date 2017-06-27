\name{boxPlot}
\alias{boxPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produce a quick boxplot }
\description{
Produce a quick boxplot from a data frame or matrix and if wanted a vector with group membership.}
\usage{
boxPlot(df, group=NULL, out="default", missing=TRUE, color="#F4A460", labList=TRUE, legendLab="Group", title1="", xOrder=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
vector, data frame or matrix}
  \item{group}{
A vector of the same length as df (p.ex df$group) Default (NULL)
}
  \item{out}{
For groups. If "default" a boxplot with all groups is printed. If "rearranged" one graph with a plot for each group is printed
}
  \item{missing}{
If FALSE, na.omit is used before ploting for variable group. Default  TRUE
}
\item{labList}{
default NULL uses factor levels as group membership label.Else list (p.eg c("group1", "group2"))
}
\item{color}{
color when group is NULL (default "#F4A460")
}
 \item{colorBox}{
boxplot color for group (default = "Spectral" from RColorBrewer)}
\item{alpha}{
transparency, defaults .3
}
\item{legendLab}{
Legend title label (default="Group")}
\item{title1}{
Plot title (default="")
}
\item{xOrder}{
x labels ordered alphabetically (default=TRUE)
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{plot }{Single plot }
\item{pdf }{One plot per each group (same=FALSE)}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ariadna Angulo-Brunet (ariadna.angulo@uab.cat), Carme Viladrich}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{violinPlot}},  \code{\link{ggplot2}}
}
\examples{
#generate some random data
x<-1:5
y<-1:2
v1<-sample(x,80,replace=TRUE)
v2<-sample(x,80,replace=TRUE)
v3<-sample(x,80,replace=TRUE)
v4<-sample(x,80,replace=TRUE)
gender<- sample(y, 80, replace=TRUE)
df <- data.frame(v1, v2,v3,v4)


#Run boxplot
boxPlot(df)

#Run boxplot by group 
boxPlot(df, group=gender, labList=c("Male", "Female"), legendLab="Gender", title1="Boxplot by gender")

#Run a boxplot for each group
boxPlot(df, group=gender, out="rearrange")


}


\keyword{ boxplot }
