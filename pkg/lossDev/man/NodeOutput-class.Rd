\name{NodeOutput-class}
\alias{NodeOutput-class}
\title{A class to hold JAGS output.}
\description{A class to hold \acronym{JAGS} output.  This class is only used internally.  No user-level function should return a class of this type.}
\details{\code{NodeOutput} is a wrapper class for \code{mcarray}.
It is used to provide easy access to summary statistics.
Current slots are:
\describe{
\item{\code{get.value.end}}{
An environment containing a parameterless function called "get.value" which when called will return the mcarray for the node.
}
\item{\code{mean}}{
An array that is the marginalized mean of slot \code{value}.
}
\item{\code{median}}{
An array that is the marginalized median of slot \code{value}.
}
\item{\code{sd}}{
An array that is the marginalized standard deviation of slot \code{value}.
}
}}
\docType{class}
\seealso{\code{\link{newNodeOutput}}.}
