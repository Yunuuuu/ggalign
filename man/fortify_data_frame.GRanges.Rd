% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fortify-data-frame-.R
\name{fortify_data_frame.GRanges}
\alias{fortify_data_frame.GRanges}
\title{Build a data frame}
\usage{
\method{fortify_data_frame}{GRanges}(data, ..., data_arg = NULL, call = NULL)
}
\arguments{
\item{data}{An object to be converted to a data frame.}

\item{...}{Arguments passed to methods.}

\item{data_arg}{The argument name for \code{data}. Developers can use it to
improve messages. Not used by the user.}

\item{call}{The execution environment where \code{data} and other arguments for
the method are collected. Developers can use it to improve messages. Not
used by the user.}
}
\value{
A data frame with at least following columns:
\itemize{
\item \code{seqnames}: The sequence (e.g., chromosome) names.
\item \code{start}: The start positions of the ranges.
\item \code{end}: The end positions of the ranges.
\item \code{width}: The width of each range.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This function converts various objects to a data frame.
}
\seealso{
Other \code{\link[=fortify_data_frame]{fortify_data_frame()}} methods:
\code{\link{fortify_data_frame.character}()},
\code{\link{fortify_data_frame.default}()},
\code{\link{fortify_data_frame.dendrogram}()},
\code{\link{fortify_data_frame.matrix}()},
\code{\link{fortify_data_frame.phylo}()}
}
\concept{fortify_data_frame}
