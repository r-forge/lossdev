\name{getPaymentNoPaymentMatrix}
\alias{getPaymentNoPaymentMatrix}
\title{A function to turn a matrix of incremental payments into zero or ones depending upon whether a payment is positive.}
\usage{getPaymentNoPaymentMatrix(object)}
\description{A function to turn a matrix of incremental payments into zero or ones depending upon whether a payment is positive.}
\details{The conversion rule is as follows.  If \code{NA}, then \code{NA}. If \code{1}, then}
\value{A matrix of zero or one (or \code{NA}) matching the structure of in input matrix.}
\arguments{\item{object}{The matrix of incremental payments.}}
