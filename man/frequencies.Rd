\name{frequencies}
\alias{frequencies}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Quick frequencies and proportions table}

\description{This function generates a quick frequencies or proportions table. Useful for freqbubble function  
}
\usage{
frequencies(df, group = NULL, missing = TRUE, prop = FALSE, round1 = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
    vector, dataframe or matrix
}
  \item{group}{
A vector same length as df (e.g, df$group) Default (NULL)} 
  \item{missing}{
Logical. TRUE (default) missing displayed}
  \item{prop}{
Logical. If TRUE proportions are displayed instead of frequencies}
  \item{round1 }{
Rounds values. default=2}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Matrix table
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
 \code{\link{table}}}
 
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
frequencies(df)
#Execute proportions without missings
frequencies(df, missing=FALSE, prop=TRUE)

#Execute frequencies by group
frequencies(df, group=gender)
#Execute proportions by group 
frequencies(df, group=gender, prop=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ frequencies }
