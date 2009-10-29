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
NULL


setOldClass('mcarray')

##' A class to hold \acronym{JAGS} output.  This class is only used internally.  No user-level function should return a class of this type.
##'
##' \code{NodeOutput} is a wrapper class for \code{mcarray}.
##' It is used to provide easy access to summary statistics.
##' Current slots are:
##' \describe{
##'   \item{\code{value}}{
##'     An S3 object of type \code{mcarray}.
##'   }
##'   \item{\code{mean}}{
##'     An array that is the marginalized mean of slot \code{value}.
##'   }
##'   \item{\code{median}}{
##'     An array that is the marginalized median of slot \code{value}.
##'   }
##'   \item{\code{sd}}{
##'     An array that is the marginalized standard deviation of slot \code{value}.
##'   }
##' }
##' @name NodeOutput-class
##' @docType class
##' @seealso \code{\link{newNodeOutput}}.
setClass(
         'NodeOutput',
         representation(
                        value='mcarray',
                        mean='array',
                        median='array',
                        sd='array'
                        ))

##' A method to construct new object of type \code{NodeOutput}. Intended for internal use only.
##'
##' This method will return a valid NodeOutput object.
##' @param mcarray An S3 object of type \code{mcarray}.
##' @return An object of class \code{NodeOutput}.
##' @seealso \code{\linkS4class{NodeOutput}}
newNodeOutput <- function(mcarray)
{
    ans <- new('NodeOutput')

    ans@mean <- as.array(summary(mcarray, mean)[[1]])
    ans@median <- as.array(summary(mcarray, median)[[1]])
    my.sd <- function(x) sd(as.vector(x))[[1]]
    ans@sd <- as.array(summary(mcarray, my.sd)[[1]])
    slot(ans, 'value') <- mcarray

    if(!validObject(ans))
        stop("could not create a valid")
    return(ans)
}

