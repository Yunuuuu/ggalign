% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scheme-.R
\name{scheme_update}
\alias{scheme_update}
\title{Update the scheme}
\usage{
scheme_update(e1, e2, ...)
}
\arguments{
\item{e1}{The original \code{\link{Scheme}} object.}

\item{e2}{The new \code{\link{Scheme}} object. Usually should have the same \code{key} as
\code{e1}.}

\item{...}{Additional arguments passed to methods.}
}
\value{
A new \code{\link{Scheme}} object, resulting from merging \code{e1} and \code{e2}.
}
\description{
\code{scheme_update()} is used by developers to define how two \code{\link{Scheme}} objects
with the same key should be merged. This typically happens when adding or
updating a \code{\link{Scheme}} in a collection.
}
\keyword{internal}
