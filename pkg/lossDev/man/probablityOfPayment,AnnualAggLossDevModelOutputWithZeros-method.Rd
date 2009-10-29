\name{probablityOfPayment,AnnualAggLossDevModelOutputWithZeros-method}
\alias{probablityOfPayment,AnnualAggLossDevModelOutputWithZeros-method}
\title{A method to plot predicted vs actual payments for models from the lossDev package.}
\description{A method to plot predicted vs actual payments for models from the \pkg{lossDev} package.}
\details{Because the model is Bayesian, each estimated payment comes as a distribution.
The median of this distribution is used as a point estimate when plotting and/or returning values.
Note: One cannot calculate the estimated incremental payments from the estimated cumulative payments (and vice versa) since the median of sums need not be equal to the sum of medians.}
\value{Mainly called for the side effect of plotting.  Also returns a named array (with the same structure as the input triangle) containing the predicted log incremental payments.  Returned invisibly.}
\docType{methods}
\seealso{\code{\link{predictedPayments}}}
\arguments{\item{object}{The object of type \code{AnnualAggLossDevModelOutput} from which to plot predicted vs actual payments and return predicted payments.}
\item{plot}{A logical value. If \code{TRUE}, then the plot is generated and the statistics are returned; otherwise only the statistics are returned.}}
