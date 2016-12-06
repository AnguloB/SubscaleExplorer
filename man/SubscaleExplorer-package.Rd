\name{SubscaleExplorer-package}
\alias{SubscaleExplorer-package}
\alias{SubscaleExplorer}
\docType{package}
\title{SubscaleExplorer}
\description{This package is a quick tool to explore subscales. All functions can perform analysis by group.}

\details{
Sometimes when working with many variables it is necessary to deal with groupings of variables to obtain results. This work is sometimes hard and requires having many subsets in our environment. This package proposes   \code{\link{getscale}} function that returns the subscales specified in an Excel sheet.

For the description of quantitative data we first recommend the use of the psych package to obtain descriptive statistics (\code{\link{describeBy}}), the mahalanobis distance (\code{\link{mahalanobis}}) and mardia’s test for multivariate normality. With \code{\link{boxPlot}} and \code{\link{violinPlot}} a graphical description can be obtained. The violinPlot also includes a boxplot inside. 

For ordinal or categorical data first \code{\link{frecuencies}} should be used. This function does frequencies or proportions, with or withour missing values. With this object we can obtain a plot with frebubble function. 

\code{\link{uniqueValues}} function can be used in quantittive and ordinal data. If used with quantitative data can be difficult to interpret by group. If there are different unique_values across groups can make \code{\link{designEffectICC}} fail.

}
\author{
Ariadna Angulo-Brunet (ariadna.angulo@uab.cat), Carme Viladrich}

\references{}

\keyword{ package }
\seealso{
\code{\link{psych}}, \code{\link{ggplot2}}, \code{\link{mice}}}
}
\examples{
}
