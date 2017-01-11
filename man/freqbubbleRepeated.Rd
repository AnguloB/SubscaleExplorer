\name{freqbubbleRepeated}
\alias{freqbubbleRepeated}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Freqbubble for multiple groups}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
freqbubbleRepeated(df1, df2, color1 = "black", color2 = "#9999CC", group1 = "Group1", group2 = "Group2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df1}{
object created by function "frequencies" for group1
}
  \item{df2}{
object created by function "frequencies" for group2
}
  \item{color1}{
bubble color group 1, default = "black"}
  \item{color2}{
bubble color group 2, default = "#9999CC"}

  \item{group1}{
label for group1}
  \item{group2}{
label for group2}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
plot
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
 \code{\link{frequencies}},  \code{\link{freqbubble}}
 }
 
\examples{
#generate some random data
x<-1:5
y<-c("t1", "t2")
v1<-sample(x,80,replace=TRUE)
v2<-sample(x,80,replace=TRUE)
v3<-sample(x,80,replace=TRUE)
v4<-sample(x,80,replace=TRUE)
time<- sample(y, 80, replace=TRUE)
df <- data.frame(v1, v2,v3,v4)
obj<-frequencies(df, group=time)
freqbubbleRepeated(obj$t1, obj$t2, color1 = "black", color2 = "#9999CC", group1 = "Group1", group2 = "Group2")}
