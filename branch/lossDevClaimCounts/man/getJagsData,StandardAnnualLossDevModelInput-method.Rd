\name{getJagsData,StandardAnnualLossDevModelInput-method}
\alias{getJagsData,StandardAnnualLossDevModelInput-method}
\title{A method to collect all the needed model input specific to the standard model.}
\description{A method to collect all the needed model input specific to the standard model. Intended for internal use only.}
\details{There are currently two types of \code{AnnualAggLossDevModel}s (break and standard).  These models have many data elements in common.
This method appends only the elements specific to the standard model onto the list created by a call to \code{NextMethod()}.

The following elements are appended:
\describe{
\item{\code{x.0}}{A single value. The lower bound for the location of knots.}
\item{\code{x.r}}{A single value. The upper bound for the location of knots.}
\item{\code{beta.prior}}{A vector giving the prior for the location of knots.}
}}
\value{A named list of the specific model elements.  See details for more info.}
\docType{methods}
\seealso{\code{\link{getJagsData}} \code{\link[getJagsData,AnnualAggLossDevModelInput-method]{getJagsData("AnnualAggLossDevModelInput")}}}
\seealso{\code{\link[getJagsData,BreakAnnualAggLossDevModelInput-method]{getJagsData("BreakAnnualAggLossDevModelInput")}}}
\arguments{\item{object}{An object of type \code{StandardAnnualAggLossDevModelInput} from which to collect the needed model input.}}
