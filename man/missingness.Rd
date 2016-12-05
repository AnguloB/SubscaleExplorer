\name{missingness}
\alias{missingness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Missing Analysis}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
missingness(df, group = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
dataframe or matrix}
  \item{group}{
group (p.eg gender)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Frequencies and proportions for case missingness
cases with all items answered
cases with incomplete data
cases with some or all items answered
cases with completely missing data
Summary cell missingness
total cases
proportion of cell missingness

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ariadna Angulo-Brunet (ariadna.angulo@uab.cat), Carme Viladrich}


\seealso{
 \code{\link{mice}}}
 

\examples{
x<-c(1:5, NA)
y<-1:2
v1<-sample(x,80,replace=TRUE)
v2<-sample(x,80,replace=TRUE)
v3<-sample(x,80,replace=TRUE)
v4<-sample(x,80,replace=TRUE)
gender<- sample(y, 80, replace=TRUE)
df <- data.frame(v1, v2,v3,v4)

missingness(df)

#by group
missingness(df, group=gender)
}

