\name{getscale}
\alias{getscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get scales and subscales giving a dataframe or matrix and a reference sheet from excel
%%  ~~function to do ... ~~
}
\description{
}
\usage{
getscale(df, file, column)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Data frame or matrix}
  \item{file}{
excel file (e.g "example.xlsx")}
  \item{column}{
Column name for scale in excel (e.g "Scale")}
 \item{sheet}{
sheet number}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#reference is an excel sheet with a colum "VariablesInput" which contain variable name of  the data frame
getscale(df, "reference.xlsx", column="Scales")
a<-unlist(output)
a<-data.frame((vapply(output,unlist,unlist(output[[1]]))))

for(i in 1:ncol(a)){
  assign(names(output)[i], a[[i]])
#remove innecessary objects
#rm(a,i, output)
}
}
