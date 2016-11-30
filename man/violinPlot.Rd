\name{violinPlot}
\alias{violinPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Violin plot}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
violinPlot(df, group=NULL, same=TRUE, missing=TRUE, color="#F4A460", group1="Group1",
group2="Group2", legendLab="Group", title1="Title")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Data frame or matrix}
  \item{group}{
variable group  (df$variable)}
  \item{same}{
If you want one plot with all groups set to TRUE, otherwise FALSE}
 \item{color}{
color when group is NULL (default "#F4A460")
}
\item{group1}{
Label for group1 (default="Group1")
}
\item{group2}{
Label for group2 (default="Group2")
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

\item{plot }{}

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
 \code{\link{boxPlot}},  \code{\link{ggplot2}}}
 
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

#default plot
violinPlot(df)

#change color 
violinPlot(df, color="green")}

