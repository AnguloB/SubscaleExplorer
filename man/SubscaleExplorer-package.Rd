\name{SubscaleExplorer-package}
\alias{SubscaleExplorer-package}
\alias{SubscaleExplorer}
\docType{package}
\title{SubscaleExplorer}
\description{This package is a quick tool to explore subscales. All functions can perform analyses by group.}

\details{
Sometimes when working with many variables such as psychometric subscales it is necessary to deal with variable sets to obtain results. This task can be hard. This package proposes \code{\link{getscale}} function that returns the variables grouped in the subscales specified in an Excel spreadsheet.

For quantitative data description we first recommend the use of the psych package to obtain descriptive statistics (\code{\link{describeBy}}), the mahalanobis distance (\code{\link{outlier}}) and mardia's test (\code{\link{mardia}}) for multivariate normality. With \code{\link{boxPlot}} and \code{\link{violinPlot}} a graphical description can be obtained. The violinPlot also includes a boxplot inside. 

For ordinal or categorical data, descriptive statistics can be obtained using \code{\link{frequencies}}. This function computes frequencies or proportions, with or without missing values. This object can be plotted  with \code{\link{freqbubble}}. For independent groups use \code{\link{freqbubble}} and for two waves measures use \code{\link{freqbubbleRepeated}}. 

\code{\link{uniqueValues}} function is best suited for ordinal or categorical data If used with quantitative data can be difficult to interpret.

}
\author{
Ariadna Angulo-Brunet (ariadna.angulo@uab.cat), Carme Viladrich}

\references{}

\keyword{ package }
\seealso{
\code{\link{psych}}, \code{\link{ggplot2}}, \code{\link{mice}}}
\examples{
}
