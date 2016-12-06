\name{designEffectICC}
\alias{designEffectICC}

\title{
Design effect and Intraclass Correlation Coeficient for Multilevel Analysis
}
\description{
Calculate design effect and ICC for multilevel analysis}
\usage{
designEffectICC(df, cluster)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
dataframe or matrix 
}
  \item{cluster}{
vector same length as df (e.g Data$team)
}
}
\details{
ICC1 corresponds to the intraclass correlation coefficient 1  (see more details in ICC1 {multilevel}). "ICC1 represents the amount of individual-level variance that can be explained by group membership". Design effect is computed based in design effect formula in Hox (2010).

If cluster size is not the same across groups, sample size mean is used instead.}
\value{

  \item{n}{Cluster size or mean cluster size when groups are different size}
 \item{table }{three columns, first indicates variable name, second ICC1 and third indicates design effect }

}
\references{
Hox, J. (2010). Multilevel analysis - Techniques and applications. Multilevel analysis techniques and applications. (2nd ed.). New York: NY: Routledge.}

\author{
Ariadna Angulo-Brunet (ariadna.angulo@uab.cat), Carme Viladrich}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{ICC1}}, \code{\link{mult.icc}, \code{\link{designEffectICC}}
}
\examples{
#generate  some random data
x<-1:5
y<-1:10
v1<-sample(x,800,replace=TRUE)
v2<-sample(x,800,replace=TRUE)
v3<-sample(x,800,replace=TRUE)
v4<-sample(x,800,replace=TRUE)
team<- sample(y, 80, replace=TRUE)
df <- data.frame(v1, v2,v3,v4)

designEffectICC(df, team)
}
