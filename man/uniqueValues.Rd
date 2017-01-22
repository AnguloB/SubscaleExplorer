\name{uniqueValues}
\alias{uniqueValues}
\title{
Unique values per variable}
\description{
Unique values (with or without missings) per variable from a dataframe or matrix
}
\usage{
uniqueValues(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
dataframe or matrix}
\item{group}{
vector same length to df}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

 \item{table }{Variable names in first column (variable), counts of unique values without considering missings in second column (uniqueValid). Unique values +  missing in third column (unique). For instance, if there are 4 endorsed values in a variable uniqueValid=4. If there are missing data unique=5.}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ariadna Angulo-Brunet (ariadna.angulo@uab.cat), Carme Viladrich}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#generate some random data
x<-1:5
y<-1:2
v1<-sample(x,80,replace=TRUE)
v2<-sample(x,80,replace=TRUE)
v3<-sample(x,80,replace=TRUE)
v4<-sample(x,80,replace=TRUE)
df <- data.frame(v1, v2,v3,v4)
gender<- sample(y, 80, replace=TRUE)

uniqueValues(df)
#by group 

uniqueValues(df, group=gender)

}



