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

## This file contains the functions needed for package startup.
## Values assigned by .onLoad are also included here.
#' @import methods
NULL

##' The Packages Mutable State.
##'
##'
##' @name mutableState
##' @keywords internal
mutableState <- new.env(parent=emptyenv())


##' Current Name of the Package. Intended for internal use only.
##'
##' Set by \code{.onLoad}.
##'
##'
##' @return The current name of the package including version number if the package was installed as such. (i.e. \samp{lossDev})
##' @seealso \code{\link{dot-onLoad-lossDev}}
myPkgName <- function() return(get('myPkgName', env=mutableState, inherits=FALSE))

##' Installation Library of the Package.   Intended for internal use only.
##'
##' @return The installation library path.
##' Set by \code{.onLoad}.
##'
##'
##' @seealso \code{\link{dot-onLoad-lossDev}}
myLibPath <- function() return(get('myLibPath', env=mutableState, inherits=FALSE))


##' Intialize the Namespace.  Intended for internal use only.
##'
##' Currently only sets correct functions for \code{myPkgName} and \code{myLibPath} and loads the \acronym{JAGS} module.
##
##'
##' @name dot-onLoad-lossDev
##' @seealso \code{\link{.onLoad}}
##' @import rjags
##' @import filehash
.onLoad <- function(libname, pkgname)
{
    ##Create functions to return the required values. Lexical scoping ensures the correct values are returned.
    myPkgName <- pkgname
    myLibPath <- libname

    ##Assign the values to the package namespace and lock the bindings for safty.
    assign('myPkgName', myPkgName, env=mutableState)
    lockBinding('myPkgName', env=mutableState)
    assign('myLibPath', myLibPath, env=mutableState)
    lockBinding('myLibPath', env=mutableState)

    ##Load the JAGS module.
    jags.module('lossDev', normalizePath(file.path(myLibPath(), myPkgName(), 'libs')))

    wd <- getwd()
    on.exit(setwd(wd))

    db.folder <- tempfile()
    dir.create(db.folder)
    setwd(db.folder)

    dbCreate('lossDev.filehash', type='RDS')
    mutableState$fileHashDBForCodas <- dbInit('lossDev.filehash', type='RDS')
    mutableState$CounterForCreatedCodas <- 0

    mutableState$lossDevOptions <- list()
    mutableState$lossDevOptions[['keepCodaOnDisk']] <- TRUE

}






##' A Safe Version of \code{setGeneric}. Intended for internal use only.
##'
##' \code{setGeneric} will overwrite existing generic functions.  This will result in the loss of all methods already associated with that generic.
##' \code{setGenericVerif} only sets the generic if it is not already a generic.
##' If a generic by the name of \code{name} already exists, a warning is issued and NULL is returned.  Otherwise \code{setGeneric} is called and its value returned.
##'
##' @param name The character string name of the generic function.
##' @param \dots Additional arguments to pass to \code{setGeneric}.
##' @return \code{setGenericVerif} really exists for its side effect; but returns the value returned by \code{setGeneric} or NULL.
##' @seealso \code{\link{setGeneric}}
setGenericVerif <- function(name, ...)
  {
    if(!isGeneric(name))
        return(setGeneric(name, ...))
    else
    {
        warning('Tried to overwrite an exisiting generic function')
        return(NULL)
    }
  }

##' Options for \pkg{lossDev}.
##'
##' Currently the only option is \code{keepCodaOnDisk}.
##' If \code{TRUE} (the default), then \pkg{filehash} will be used to store the coda for every node in a temporary file.
##' This reduces the required memory and can improve copying performance.
##' Since copied objects refer to the same file, copy time can be greatly reduced.
##' If \code{FALSE}, then coda's will be kept in memory.
##' Changing the value will only be taking into account on a "going forward" basis.
##'
##' @param \dots named values to set.  If empty, only the current list of option settings is returned.
##' @return The current (or altered) list of option settings is returned.
##' @export
##' @name dot-lossDevOptions
##' @aliases .lossDevOptions
.lossDevOptions <- function(...)
{

    args <- list(...)
    n <- names(args)

    if(length(n) == 0)
        return(mutableState$lossDevOptions)

    if(length(n) != 1 || n[1] != 'keepCodaOnDisk')
        stop('The only current option is "keepCodaOnDisk"')


    for(i in n)
    {
        if(i == 'keepCodaOnDisk')
            if(!is.logical(args[[i]]))
                stop('"keepCodaOnDisk" must be a logical value.  Reverting to previous setting.')
        mutableState$lossDevOptions[[i]] <- args[[i]]
    }

    return(mutableState$lossDevOptions)

}

