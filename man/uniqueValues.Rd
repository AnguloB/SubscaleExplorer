\name{uniqueValues}
\alias{uniqueValues}
\title{
Unique values per variable}
\description{
Unique values (with or without missings) per variable giving a dataframe or matrix
}
\usage{
uniqueValues(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
dataframe or matrix}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

 \item{table }{First column has variable names, second column represents unique values without considering missings. Third column represents unique values +  missing. For instance, if there are 4 possible values in a variable, first column is going to have a 4. If there is missing data second column is going to have n+1 = 5 }
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ariadna Angulo-Brunet (ariadna.angulo@uab.cat), Carme Viladrich}
\note{
You can use function "by" to get results by group}

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
by(df, gender, uniqueValues)
}



