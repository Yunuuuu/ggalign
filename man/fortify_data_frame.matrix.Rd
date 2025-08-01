% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fortify-data-frame-.R
\name{fortify_data_frame.matrix}
\alias{fortify_data_frame.matrix}
\alias{fortify_data_frame.DelayedMatrix}
\alias{fortify_data_frame.Matrix}
\title{Build a data frame}
\usage{
\method{fortify_data_frame}{matrix}(data, lvls = NULL, ..., data_arg = NULL, call = NULL)

\method{fortify_data_frame}{DelayedMatrix}(data, ...)

\method{fortify_data_frame}{Matrix}(data, ...)
}
\arguments{
\item{data}{A matrix-like object.}

\item{lvls}{A logical value indicating whether to restore factor levels using
those stored in \code{\link[=ggalign_lvls]{ggalign_lvls()}}, or a character vector specifying custom
levels for the \code{value} column. If levels are provided or restored, the
\code{value} column will be returned as a factor.}

\item{...}{These dots are for future extensions and must be empty.}

\item{data_arg}{The argument name for \code{data}. Developers can use it to
improve messages. Not used by the user.}

\item{call}{The execution environment where \code{data} and other arguments for
the method are collected. Developers can use it to improve messages. Not
used by the user.}
}
\value{
Matrix will be transformed into a long-form data frame, where each row
represents a unique combination of matrix indices and their corresponding
values. The resulting data frame will contain the following columns:
\itemize{
\item \code{.row_names} and \code{.row_index}: the row names (only applicable when names
exist) and an integer representing the row index of the original matrix.
\item \code{.column_names} and \code{.column_index}: the column names (only applicable
when names exist) and column index of the original matrix.
\item \code{value}: the matrix value, returned as a factor if levels are specified or
restored.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This function converts various objects to a data frame.
}
\seealso{
Other \code{\link[=fortify_data_frame]{fortify_data_frame()}} methods:
\code{\link{fortify_data_frame.GRanges}()},
\code{\link{fortify_data_frame.character}()},
\code{\link{fortify_data_frame.default}()},
\code{\link{fortify_data_frame.dendrogram}()},
\code{\link{fortify_data_frame.phylo}()}
}
\concept{fortify_data_frame}
