\name{freqbubble}
\alias{freqbubble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Freqbubble for frequencies and proportions
}
\description{
Freqbubble using frequencies object }
\usage{
freqbubble(df, number=TRUE,color = "#666699",
          title = "", x.lab = "", y.lab = "Response",
          angle = 45, alpha = 0.3, lsize = 12,  xOrder1 = TRUE, 
          sizeBubble = c(2, 30), legend= TRUE, numberSize=3) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
object created by function (\code{\link{frequencies}})}
  \item{color}{
bubble color when there is no group (default = "#666699") }
\item{number}{
label proportions (default = TRUE) }
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
  \item{xOrder}{
x labels should be alphabetically ordered? (default TRUE)}
\item{bubbleSize}{
bubble size range, default = c(2, 30)}
\item{legend}{
legend should be plot?, default = TRUE}
\item{numberSize}{
number size, default= 3}
}

\value{
plot
}

\details{
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

#object with group
objGroup<-frequencies(df, gender, prop=TRUE)
freqbubble(objGroup)

}
 

