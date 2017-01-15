\name{freqbubble}
\alias{freqbubble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Freqbubble for frequencies and proportions
}
\description{
Freqbubble using frequencies object }
\usage{
freqbubble(df, group=NULL,color = "#666699", title = "", x.lab = "", y.lab = "Response", 
                       angle = 45, alpha = 0.3, lsize = 12,  color1 = "black", color2 = "#9999CC",
                       group1 = "Group1", group2 = "Group2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
object created by function (\code{\link{frequencies}})}
 \item{group}{
 "TRUE"" if df is an object with 2 groups (default=NULL)
}
  \item{color}{
bubble color when there is no group (default = "#666699") }
  \item{title}{
default empty, character}
  \item{x.lab}{
default empty}
  \item{y.lab}{
default "response"}
  \item{angle}{
x labels angle (default=45)}
  \item{alpha}{
transparence (default 0.3)}
  \item{lsize}{
letter size (default 12)}
 \item{color1}{
color for group 1 in group plot (default="black")}
 \item{color2}{
color for group 2 in group plot (default="#9999CC")}
 \item{group1}{
 label for group1 in legend (default ="Group1")
}
 \item{group2}{
  label for group2 in legend (default ="Group2")
}
}

\value{
plot
}

\details{
Only two groups are allowed in plot by group.It is recommended to use proportions in order to compare groups.}
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
 \code{\link{frequencies}},  \code{\link{freqbubbleRepeated}}, \code{\link{table}}}
 
 
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
#Execute default function (frequencies with missing data included)
obj<-frequencies(df)
freqbubble(obj)

#group=NULL
obj<-frequencies(df, gender, prop=TRUE)
freqbubble(obj, group="TRUE", group1="Female", group2="Male")

}
 

