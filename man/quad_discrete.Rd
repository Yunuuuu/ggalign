% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout-quad-.R
\name{quad_discrete}
\alias{quad_discrete}
\alias{quad_alignb}
\title{Arrange plots in the quad-side of a main plot by aligning discrete axis}
\usage{
quad_discrete(
  data = waiver(),
  mapping = aes(),
  ...,
  theme = NULL,
  active = NULL,
  width = NA,
  height = NA
)

quad_alignb(
  data = waiver(),
  mapping = aes(),
  ...,
  theme = NULL,
  active = NULL,
  width = NA,
  height = NA
)
}
\arguments{
\item{data}{Default dataset to use for the layout. If not specified, it must be supplied in each plot added to the layout. By default, it will try to inherit from
parent layout. \code{\link[=fortify_matrix]{fortify_matrix()}} will be used to convert data to a
matrix.}

\item{mapping}{Default list of aesthetic mappings to use for main plot in the
layout. If not specified, must be supplied in each layer added to the main
plot.}

\item{...}{Additional arguments passed to \code{\link[=fortify_matrix]{fortify_matrix()}}.}

\item{theme}{A \code{\link[ggplot2:theme]{theme()}} object used to customize various
elements of the layout, including \code{guides}, \code{title}, \code{subtitle}, \code{caption},
\code{margins}, \code{panel.border}, and \code{background}. By default, the theme will
inherit from the parent \code{layout}. It also controls the panel spacing for all
plots in the layout.}

\item{active}{A \code{\link[=active]{active()}} object that defines the context settings when
added to a layout.}

\item{width, height}{The relative width/height of the main plot, can be a
\code{\link[grid:unit]{unit}} object.}
}
\value{
A \code{QuadLayout} object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

The \code{quad_discrete} function arranges plots in the quad-side of a main plot
by aligning discrete variables along two axes. \code{quad_alignb} is an alias for
\code{quad_discrete} for historical reasons.
}
\section{ggplot2 specification}{

The data in the underlying main plot contains following columns:
\itemize{
\item \code{.panel_x} and \code{.panel_y}: the column and row panel groups.
\item \code{.x} and \code{.y}: an integer index of \code{x} and \code{y} coordinates
\item \code{.discrete_x} and \code{.discrete_y}: a factor of the data labels (only
applicable when \code{.row_names} and \code{.column_names} exists).
\item \code{.row_names} and \code{.column_names}: A character of the row and column names
of the original matrix (only applicable when names exist).
\item \code{.row_index} and \code{.column_index}: the row and column index of the original
matrix.
\item \code{value}: the actual matrix value.
}
}

