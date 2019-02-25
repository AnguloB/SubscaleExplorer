\name{violinPlot}
\alias{violinPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Violin plot}
\description{
Produce a quick violin plot with a boxplot inside from a data frame or matrix and if wanted a vector with group membership}
\usage{
violinPlot(df, group=NULL, same=TRUE, missing=TRUE, color="#F4A460", group1="Group1",
group2="Group2", legendLab="Group", title1="Title", numBreaks=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
vector, data frame or matrix}
  \item{group}{
variable group  (df$variable)}
  \item{same}{
If you want one plot with all groups set to TRUE, otherwise FALSE}
 \item{color}{
color when group is NULL (default "#F4A460")
}
 \item{colorBox}{
boxplot color for group (default = "Spectral" from RColorBrewer)}
\item{labList}{
default NULL uses factor levels as group membership label.Else list (p.eg c("group1", "group2"))
}
\item{legendLab}{
Legend title label (default="Group")}
\item{title1}{
Plot title (default=NULL)
}
\item{numBreaks}{
when there are more than 15 response values breaks are modified for better visualization (default=10)
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
y<-c("Female", "Male")
v1<-sample(x,80,replace=TRUE)
v2<-sample(x,80,replace=TRUE)
v3<-sample(x,80,replace=TRUE)
v4<-sample(x,80,replace=TRUE)
gender<- sample(y, 80, replace=TRUE)
df <- data.frame(v1, v2,v3,v4)

#default plot
violinPlot(df)

#change color 
violinPlot(df, color="green")

#by group 
violinPlot(df, gender)

#a plot for each group 
violinPlot(df, gender, out="rearrange")

}

