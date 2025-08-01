% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scheme-.R
\name{scheme_init}
\alias{scheme_init}
\title{Initialize a scheme object}
\usage{
scheme_init(scheme)
}
\arguments{
\item{scheme}{A \code{\link{Scheme}} object to initialize.}
}
\value{
The initialized \code{\link{Scheme}} object.
}
\description{
\code{scheme_init()} is a developer-facing generic used to define the initial
state of a \code{\link{Scheme}} object. It is typically called during rendering to
initialize layout schemes, allowing plots to inherit layout behavior from the
scheme.
}
\keyword{internal}
