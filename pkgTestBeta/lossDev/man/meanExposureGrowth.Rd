\name{meanExposureGrowth}
\alias{meanExposureGrowth}
\title{A generic function to plot and/or return the posterior of the mean exposure growth for models in lossDev.}
\description{A generic function to plot and/or return the posterior of the mean exposure growth for models in \pkg{lossDev}.}
\details{exposure growth is currently modeled in a way that is not intended for sophisticated forecasting.  Each year's rate of growth is assumed to be a realization from a normal distribution with mean equal to this parameter.}
\value{Mainly called for the side effect of plotting.}
\docType{genericFunction}
\seealso{\code{\link[=meanExposureGrowth,AnnualAggLossDevModelOutput-method]{meanExposureGrowth("AnnualAggLossDevModelOutput")}}}
\arguments{\item{object}{The object from which to plot and/or return the mean exposure growth.}
\item{plotDensity}{A logical value. If \code{TRUE}, the density is plotted. If \code{plotTrace} is also \code{TRUE}, then two plots are generated.  If they are both \code{FALSE}, then only the statistics are returned.}
\item{plotTrace}{A logical value. If \code{TRUE}, the trace is plotted. If \code{plotDensity} is also \code{TRUE}, then two plots are generated.  If they are both \code{FALSE}, then only the statistics are returned.}}
