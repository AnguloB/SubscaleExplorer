\name{boxPlot}
\alias{boxPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produce a quick boxplot }
\description{
Produce a quick boxplot from a data frame or matrix and if wanted a vector with group membership.}
\usage{
boxPlot(df, group=NULL, same=TRUE, missing=TRUE, color="#F4A460", 
labList, legendLab="Group", title1="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Data frame or matrix}
  \item{group}{
A vector of the same length as df (p.ex df$group) Default (NULL)
}
  \item{same}{
Should the plot be printed (default TRUE) or should appear a pdf in working directory (each page presents a single group)
}
  \item{missing}{
If FALSE, na.omit is used before ploting. Default  TRUE
}
\item{labList}{
default NULL uses factor levels as group membership label.Else list (p.eg c("group1", "group2"))
}
\item{color}{
color when group is NULL (default "#F4A460")
}
\item{legendLab}{
Legend title label (default="Group")}
\item{title1}{
Plot title (default=NULL)
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

#Run a boxplot per group (pdf)
boxPlot(df, group=gender, same=FALSE)


}


\keyword{ boxplot }
