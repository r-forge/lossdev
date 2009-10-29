##################################################################################################
##                                                                                              ##
##    lossDev is an R-package.                                                                  ##
##    It is a Bayesian time series model of loss development.                                   ##
##    Features include skewed Student-t distribution with time-varying scale parameters,        ##
##    an expert prior for the calendar year effect,                                             ##
##    and accommodation for structural breaks in the consumption path of services.              ##
##                                                                                              ##
##    Copyright © 2009, National Council On Compensation Insurance Inc.,                        ##
##                                                                                              ##
##    This file is part of lossDev.                                                             ##
##                                                                                              ##
##    lossDev is free software: you can redistribute it and/or modify                           ##
##    it under the terms of the GNU General Public License as published by                      ##
##    the Free Software Foundation, either version 3 of the License, or                         ##
##    (at your option) any later version.                                                       ##
##                                                                                              ##
##    This program is distributed in the hope that it will be useful,                           ##
##    but WITHOUT ANY WARRANTY; without even the implied warranty of                            ##
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                             ##
##    GNU General Public License for more details.                                              ##
##                                                                                              ##
##    You should have received a copy of the GNU General Public License                         ##
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.                     ##
##                                                                                              ##
##################################################################################################


##' @include zzz.R
##' @include AnnualAggLossDevModelOutput.R
##' @include StandardAnnualAggLossDevModelOutput.R
##' @include BreakAnnualAggLossDevModelOutput.R
NULL

##' The class to handle incremental payments of zero.
##'
##' \code{StandardAnnualAggLossDevModelOutputWithZeros} is a special class designed to be merged with aggregate annual model objects.
##'
##' @name StandardAnnualAggLossDevModelOutputWithZeros-class
##' @docType class
##' @seealso \code{\linkS4class{LossDevModelOutput}}
setClass(
         'StandardAnnualAggLossDevModelOutputWithZeros',
         representation(
                        priorsForProbOfPayment='numeric',
                        prob.of.non.zero.payment='NodeOutput'),
         contains=c('StandardAnnualAggLossDevModelOutput'))

##' The class to handle incremental payments of zero.
##'
##' \code{BreakAnnualAggLossDevModelOutputWithZeros} is a special class designed to be merged with aggregate annual model objects.
##'
##' @name BreakAnnualAggLossDevModelOutputWithZeros-class
##' @docType class
##' @seealso \code{\linkS4class{LossDevModelOutput}}
setClass(
         'BreakAnnualAggLossDevModelOutputWithZeros',
         representation(
                        priorsForProbOfPayment='numeric',
                        prob.of.non.zero.payment='NodeOutput'),
         contains=c('BreakAnnualAggLossDevModelOutput'))

setClassUnion('AnnualAggLossDevModelOutputWithZeros', c('StandardAnnualAggLossDevModelOutputWithZeros', 'BreakAnnualAggLossDevModelOutputWithZeros'))

gompertz <- function(x, scale, fifty.fifty)
{
    b <- scale
    c <- -log(-log(0.5)) / b - fifty.fifty

    return(exp(-exp(-b * (x + c))))
}

##' @export
accountForZeroPayments <- function(object)
{
    time.begin <- Sys.time()

    if(is(object, 'StandardAnnualAggLossDevModelOutput'))
    {
        ans <- new('StandardAnnualAggLossDevModelOutputWithZeros')
    } else if(is(object, 'BreakAnnualAggLossDevModelOutput'))
    {
         ans <- new('BreakAnnualAggLossDevModelOutputWithZeros')
    } else {
        stop('Current type of "object" is unsupported')
    }

    for(s in slotNames(object))
        slot(ans, s) <- slot(object, s)

    rm(object)

    u <- getPaymentNoPaymentMatrix(ans@input)

    p.emp <- calculateProbOfPayment(u)

    if(all(p.emp == 1))
        stop('This function can only be called on triangles with "zero" payments.')

    ans@priorsForProbOfPayment <- estimate.priors(p.emp)

    jags.data <- getJagsData(ans@input)
    if(dim(u)[1] != jags.data$K)
        stop('error "dim(u)[1] != jags.data$K"')

    jags.data$u <- u
    jags.data$scale.prior <- ans@priorsForProbOfPayment['scale']
    jags.data$fifty.fifty.prior <- ans@priorsForProbOfPayment['fifty.fifty']




    parameters.to.save. <- 'prob.of.non.zero.payment'
    ##print(parameters.to.save.)


    ##We will NOT count the addaptive phase as part of the burnin.
    ##ans@burnIn <- as.integer(burnIn)
    ##ans@sampleSize <- as.integer(sampleSize)
    ##ans@thin <- as.integer(thin)
    ##ans@nChains <- as.integer(nChains)

    warning('figure out how to set "nAddapt", "burnIn," and "thin"')
    nAddapt <- 1000
    nChains <- dim(ans@inc.pred@value)['chain']
    burnIn <- 1000
    sampleSize <- dim(ans@inc.pred@value)['iteration']
    thin <- 1


    message(paste('Preparing Jags Model\nadapting for', nAddapt, 'iterations\n\n'))
    jm <- jags.model(file=file.path(myLibPath(), myPkgName(), 'models', 'probOfPayment.model.txt'),
                     data=jags.data,
                     n.chains=nChains,
                     n.adapt=nAddapt)


    message(paste('Burning-In Jags Model for', burnIn, 'iterations\n', 'Total Burn-In = ', burnIn))
    update(jm, burnIn)

    message(paste('Sampling Jags Model for', sampleSize, 'iterations Thin =', thin,'\n', 'This will result in ~', sampleSize / thin, 'Samples'))
    output <- jags.samples(jm, parameters.to.save., sampleSize, thin)



    for(i in parameters.to.save.)
        slot(ans,i) <- newNodeOutput(output[[i]])
    ##slot(ans,i) <- new('NodeOutput', value=new('safe.mcmc', value=output[[i]]))

    if(!validObject(ans))
        stop('A valid output could not be created')

          print(paste('Update took', Sys.time() - time.begin))

    return(invisible(ans))


}

getPaymentNoPaymentMatrix <- function(object)
{
    if(!is(object, 'AnnualAggLossDevModelInput'))
        stop('this function currently only supports objects of type "AnnualAggLossDevModelInput"')

    inc <- object@incrementals

    ans <- array(NA, dim(inc))

    f <- function(x)
    {
        if(is.na(x)) {
            NA
        } else if(x > 0) {
            1
        } else if (x == 0) {
            0
        } else {
            NA
        }
    }
    ans <- apply(inc, c(1,2), f)
}

calculateProbOfPayment <- function(x)
{
    K <- dim(x)[2]

    ans <- numeric(K)
    for(i in 1:K)
        ans[i] <- sum(x[,i] == 1, na.rm=TRUE) / sum(x[,i] %in% c(0,1), na.rm=TRUE)

    ans[which(is.na(ans))] <- NA

    return(ans)

}

estimate.priors <- function(p)
{
    if(all(p == 1))
        stop('This function can only be called on p vectors with some probably of a "zero" payment.')

    K <- length(p)

    scale <- 1
    fifty.fifty <- mean(min(which(!is.na(p) & p != 1)), K)

    f <- function(x)
    {
        p.hat <- 1 - gompertz(1:K, x[1], x[2])
        sse <- sum((p.hat - p)^2)
        return(sse)
    }

    lse <- nlm(f, c(scale, fifty.fifty))

    if(! lse$code %in% c(1, 2, 3))
        stop('unable to find proper priors')
    ans <- lse$estimate

    names(ans) <- c('scale', 'fifty.fifty')

    return(ans)


}


##' A method to plot and/or return the difference between final actual and predicted cumulative payments.
##'
##' The relative difference (x/y - 1) between the final observed cumulative payment and the corresponding predicted cumulative payment is plotted for each exposure year.
##' The horizontal lines of each box represent (starting from the top) the 90th, 75th, 50th, 20th, and 10th percentiles.  Exposure years in which all cumulative payments are \code{NA} are omitted.
##'
##' This method accounts for zero payments.
##'
##' @name finalCumulativeDiff,AnnualAggLossDevModelOutputWithZeros-method
##' @param object The object of type \code{AnnualAggLossDevModelOuputWithZeros} from which to plot and/or return the difference between final actual and predicted cumulative payments.
##' @param plot A logical value. If \code{TRUE}, the plot is generated and the statistics are returned; otherwise only the statistics are returned.
##' @return Mainly called for the side effect of plotting the difference between final actual and predicted cumulative payments by exposure year.  Also returns a named array for the percentiles in the plot.  Returned invisibly.
##' @docType methods
##' @seealso \code{\link{finalCumulativeDiff}}
setMethod('finalCumulativeDiff',
          signature(object='AnnualAggLossDevModelOutputWithZeros'),
          function(object, plot)
      {

          tmp <- object@inc.pred@value * object@prob.of.non.zero.payment@value
          object@inc.pred <- newNodeOutput(tmp)

          current.class <- class(object)
          i <- match(current.class, is(object))
          f <- selectMethod('finalCumulativeDiff', is(object)[i+1])
          return(f(object, plot))

      })

##' A method to plot and/or return the predicted tail factors for a specific attachment point.
##'
##' The tail factor is the ratio of the estimated ultimate loss to cumulative loss at some point in development time.
##' This is a method to allow for the retrieval and illustration of the tail factor by exposure year.
##'
##' Because the model is Bayesian, each tail factor comes as a distribution.  To ease graphical interpretation, only the median for each factor is plotted/returned.
##' See for more details \code{\link{tailFactor}}.
##'
##' For comparison purposes, the function returns three separated tail factors for three scenarios.  Theses three tail factors are returned as a list with the following names and meanings:
##' \describe{
##'   \item{\dQuote{Actual}}{
##'     These are the tail factors estimated when taking the break into consideration.
##'   }
##'   \item{\dQuote{AsIfPostBreak}}{
##'     These are the tail factors estimated when assuming all years where in the post-break regime.
##'   }
##'   \item{\dQuote{AsIfPreBreak}}{
##'     These are the tail factors estimated when assuming all years where in the pre-break regime.
##'   }
##' }
##'
##' @name tailFactor,BreakAnnualAggLossDevModelOutputWithZeros-method
##' @param object The object from which to plot the predicted tail factors and return tail factors for \emph{all} attachment points.
##' @param attachment An integer value specifying the attachment point for the tail.  Must be at least 1. See Details for more info.
##' @param useObservedValues A logical value.  If \code{TRUE}, observed values are substituted for predicted values whenever possible in the calculation.  If \code{FALSE}, only predicted values are used.
##' @param firstIsHalfReport A logical value or \code{NA}.  See Details for more information.
##' @param finalAttachment An integer value must be at least 1 default value is \code{attachment}.  A call to \code{tailFactor} returns (invisibly) a matrix of tail factors through this value.
##' @param plot A logical value. If \code{TRUE}, the plot is generated and the statistics are returned; otherwise only the statistics are returned.
##' @return Mainly called for the side effect of plotting.  Also returns tail factors for \emph{all} attachment points through \code{finalAttachment}.  See Details. Returned invisibly.
##' @docType methods
##' @seealso \code{\link{tailFactor}}
##' @seealso \code{\link[=tailFactor,StandardAnnualAggLossDevModelOutput-method]{tailFactor("StandardAnnualAggLossDevModelOutput")}}
setMethod('tailFactor',
          signature(object='BreakAnnualAggLossDevModelOutputWithZeros'),
          function(object, attachment, useObservedValues, firstIsHalfReport, finalAttachment, plot)
      {


          tmp <- object@inc.pred@value * object@prob.of.non.zero.payment@value
          object@inc.pred <- newNodeOutput(tmp)

          tmp <- object@inc.brk@value
          for(i in 1:2)
              tmp[,,i,,] <- tmp[,,i,,] * object@prob.of.non.zero.payment@value

          object@inc.brk <- newNodeOutput(tmp)

          current.class <- class(object)
          i <- match(current.class, is(object))
          f <- selectMethod('tailFactor', is(object)[i+1])
          return(f(object, attachment, useObservedValues, firstIsHalfReport, finalAttachment, plot))

      })

##' A method to plot and/or return the predicted tail factors for a specific attachment point.
##'
##' The tail factor is the ratio of the estimated ultimate loss to cumulative loss at some point in development time.
##' This is a method to allow for the retrieval and illustration of the tail factor by exposure year.
##'
##' Because the model is Bayesian, each tail factor comes as a distribution.  To ease graphical interpretation, only the median for each factor is plotted/returned.
##' See for more details \code{\link{tailFactor}}.
##'
##' For comparison purposes, the function returns three separated tail factors for three scenarios.  Theses three tail factors are returned as a list with the following names and meanings:
##' \describe{
##'   \item{\dQuote{Actual}}{
##'     These are the tail factors estimated when taking the break into consideration.
##'   }
##'   \item{\dQuote{AsIfPostBreak}}{
##'     These are the tail factors estimated when assuming all years where in the post-break regime.
##'   }
##'   \item{\dQuote{AsIfPreBreak}}{
##'     These are the tail factors estimated when assuming all years where in the pre-break regime.
##'   }
##' }
##'
##' @name tailFactor,StandardAnnualAggLossDevModelOutputWithZeros-method
##' @param object The object from which to plot the predicted tail factors and return tail factors for \emph{all} attachment points.
##' @param attachment An integer value specifying the attachment point for the tail.  Must be at least 1. See Details for more info.
##' @param useObservedValues A logical value.  If \code{TRUE}, observed values are substituted for predicted values whenever possible in the calculation.  If \code{FALSE}, only predicted values are used.
##' @param firstIsHalfReport A logical value or \code{NA}.  See Details for more information.
##' @param finalAttachment An integer value must be at least 1 default value is \code{attachment}.  A call to \code{tailFactor} returns (invisibly) a matrix of tail factors through this value.
##' @param plot A logical value. If \code{TRUE}, the plot is generated and the statistics are returned; otherwise only the statistics are returned.
##' @return Mainly called for the side effect of plotting.  Also returns tail factors for \emph{all} attachment points through \code{finalAttachment}.  See Details. Returned invisibly.
##' @docType methods
##' @seealso \code{\link{tailFactor}}
##' @seealso \code{\link[=tailFactor,StandardAnnualAggLossDevModelOutput-method]{tailFactor("StandardAnnualAggLossDevModelOutput")}}
setMethod('tailFactor',
          signature(object='StandardAnnualAggLossDevModelOutputWithZeros'),
          function(object, attachment, useObservedValues, firstIsHalfReport, finalAttachment, plot)
      {

          tmp <- object@inc.pred@value * object@prob.of.non.zero.payment@value
          object@inc.pred <- newNodeOutput(tmp)

          current.class <- class(object)
          i <- match(current.class, is(object))
          f <- selectMethod('tailFactor', is(object)[i+1])
          return(f(object, attachment, useObservedValues, firstIsHalfReport, finalAttachment, plot))

      })

##' A method to plot predicted vs actual payments for models from the \pkg{lossDev} package.
##'
##' Because the model is Bayesian, each estimated payment comes as a distribution.
##' The median of this distribution is used as a point estimate when plotting and/or returning values.
##' Note: One cannot calculate the estimated incremental payments from the estimated cumulative payments (and vice versa) since the median of sums need not be equal to the sum of medians.
##'
##' @name predictedPayments,AnnualAggLossDevModelOutputWithZeros-method
##' @param object The object of type \code{AnnualAggLossDevModelOutput} from which to plot predicted vs actual payments and return predicted payments.
##' @param type A singe character value specifying whether to plot/return the predicted incremental or cumulative payments. Valid values are "incremental" or "cumulative."  See details as to why these may not match up.
##' @param logScale A logical value.  If \code{TRUE}, then values are plotted on a log scale.
##' @param mergePredictedWithObserved A logical value.  If \code{TRUE}, then the returned values treat observed incremental payments at "face value"; otherwise predicted values are used in place of observed values.
##' @param plotObservedValues A logical value.  If \code{FALSE}, then only the predicted values are plotted.
##' @param plotPredictedOnlyWhereObserved A logical value.  If \code{TRUE}, then only the predicted incremental payments with valid corresponding observed (log) incremental payment are plotted. Ignored for \code{type="cumulative"}.
##' @param quantiles A vector of quantiles for the predicted payments to return.  Usefull for constructing credible intervals.
##' @param plot A logical value. If \code{TRUE}, then the plot is generated and the statistics are returned; otherwise only the statistics are returned.
##' @return Mainly called for the side effect of plotting.  Also returns a named array (with the same structure as the input triangle) containing the predicted log incremental payments.  Returned invisibly.
##' @docType methods
##' @seealso \code{\link{predictedPayments}}
setMethod('predictedPayments',
          signature(object='AnnualAggLossDevModelOutputWithZeros'),
          f <- function(object, type, logScale, mergePredictedWithObserved, plotObservedValues, plotPredictedOnlyWhereObserved, quantiles, plot)
      {

          current.class <- class(object)
          i <- match(current.class, is(object))
          f <- selectMethod('predictedPayments', is(object)[i+1])

          type <- match.arg(type)
          if(type == 'incremental' && plotPredictedOnlyWhereObserved && plotObservedValues)
          {
              f(object, type, logScale, mergePredictedWithObserved, plotObservedValues, plotPredictedOnlyWhereObserved, quantiles, plot)
              plot <- FALSE
          }
           tmp <- object@inc.pred@value * object@prob.of.non.zero.payment@value
           object@inc.pred <- newNodeOutput(tmp)


           return(f(object, type, logScale, mergePredictedWithObserved, plotObservedValues, plotPredictedOnlyWhereObserved, quantiles, plot))

      })



