\name{dot-lossDevOptions}
\title{Options for lossDev.}
\usage{.lossDevOptions(...)}
\description{Options for \pkg{lossDev}.}
\details{Currently the only option is \code{keepCodaOnDisk}.
If \code{TRUE} (the default), then \pkg{filehash} will be used to store the coda for every node in a temporary file.
This reduces the required memory and can improve copying performance.
Since copied objects refer to the same file, copy time can be greatly reduced.
If \code{FALSE}, then coda's will be kept in memory.
Changing the value will only be taking into account on a "going forward" basis.}
\value{The current (or altered) list of option settings is returned.}
\alias{.lossDevOptions}
\arguments{\item{\dots}{named values to set.  If empty, only the current list of option settings is returned.}}
