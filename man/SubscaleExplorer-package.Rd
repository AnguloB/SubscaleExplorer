\name{SubscaleExplorer-package}
\alias{SubscaleExplorer-package}
\alias{SubscaleExplorer}
\docType{package}
\title{SubscaleExplorer}
\description{This package provides  quick tools to explore subscales. All functions can perform analyses by group.}

\details{
Sometimes when working with many variables such as psychometric subscales it is necessary to deal with variable groups to obtain meaningful results. This task is usually hard. This package proposes \code{\link{getscale}}, a function that returns a vector that allows doing quick subsets from information specified in an Excel spreadsheet.

First, for quantitative data description we recommend the use of the psych package to obtain descriptive statistics (\code{\link{describeBy}}), the mahalanobis distance (\code{\link{outlier}}) and mardia's test (\code{\link{mardia}}) for multivariate normality. With \code{\link{boxPlot}} or \code{\link{violinPlot}} a graphical description can be obtained. The violinPlot also includes a boxplot inside. 

For ordinal or categorical data, descriptive statistics can be obtained using \code{\link{frequencies}}. This function computes frequencies or proportions, with or without missing values. This object can be plotted  with \code{\link{freqbubble}}. For independent groups use \code{\link{freqbubble}} and for two waves measures (or groups) use \code{\link{freqbubbleRepeated}}. 

\code{\link{uniqueValues}} function is best suited for ordinal or categorical data. If used with quantitative data can be difficult to interpret.

}
\author{
Ariadna Angulo-Brunet (ariadna.angulo@uab.cat), Carme Viladrich}

\references{}

\keyword{ package }
\seealso{
\code{\link{psych}}, \code{\link{ggplot2}}, \code{\link{mice}}}
\examples{
}
