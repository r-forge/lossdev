\name{finalCumulativeDiff,AnnualAggLossDevModelOutputWithZeros-method}
\alias{finalCumulativeDiff,AnnualAggLossDevModelOutputWithZeros-method}
\title{A method to plot and/or return the difference between final actual and predicted cumulative payments.}
\description{A method to plot and/or return the difference between final actual and predicted cumulative payments.}
\details{The relative difference (x/y - 1) between the final observed cumulative payment and the corresponding predicted cumulative payment is plotted for each exposure year.
The horizontal lines of each box represent (starting from the top) the 90th, 75th, 50th, 20th, and 10th percentiles.  Exposure years in which all cumulative payments are \code{NA} are omitted.

This method accounts for zero payments.}
\value{Mainly called for the side effect of plotting the difference between final actual and predicted cumulative payments by exposure year.  Also returns a named array for the percentiles in the plot.  Returned invisibly.}
\docType{methods}
\seealso{\code{\link{finalCumulativeDiff}}}
\arguments{\item{object}{The object of type \code{AnnualAggLossDevModelOuputWithZeros} from which to plot and/or return the difference between final actual and predicted cumulative payments.}
\item{plot}{A logical value. If \code{TRUE}, the plot is generated and the statistics are returned; otherwise only the statistics are returned.}}
