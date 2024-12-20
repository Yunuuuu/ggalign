% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/align-gg.R
\name{align_gg}
\alias{align_gg}
\alias{ggalign}
\title{Create ggplot object}
\usage{
align_gg(
  data = waiver(),
  mapping = aes(),
  size = NULL,
  limits = TRUE,
  facet = TRUE,
  no_axes = NULL,
  active = NULL,
  set_context = deprecated(),
  order = deprecated(),
  name = deprecated(),
  free_guides = deprecated(),
  free_spaces = deprecated(),
  plot_data = deprecated(),
  theme = deprecated(),
  free_labs = deprecated()
)
}
\arguments{
\item{data}{A flexible input that specifies the data to be used
\itemize{
\item \code{NULL}: No data is set.
\item \code{\link[ggplot2:waiver]{waiver()}}: Uses the layout matrix.
\item A \code{function} (including purrr-like lambda syntax) that is applied to the
layout matrix, and must return a matrix. If you want to transform the final
plot data, please use \code{\link[=scheme_data]{scheme_data()}}.
\item A \code{matrix}, \verb{data frame}, or atomic vector.
}}

\item{mapping}{Default list of aesthetic mappings to use for plot.
If not specified, must be supplied in each layer added to the plot.}

\item{size}{The relative size of the plot, can be specified as a
\code{\link[grid:unit]{unit}}.}

\item{limits}{Logical; if \code{TRUE}, sets layout limits for the plot.}

\item{facet}{Logical; if \code{TRUE}, applies facets to the layout. If \code{FALSE},
\code{limits} will also be set to \code{FALSE}.}

\item{no_axes}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} Logical; if \code{TRUE},
removes axes elements for the alignment axis using \code{\link[=theme_no_axes]{theme_no_axes()}}. By
default, will use the option-
\code{"ggalign.align_no_axes"}.}

\item{active}{A \code{\link[=active]{active()}} object that defines the context settings when
added to a layout.}

\item{set_context}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Please use \code{active}
argument instead.}

\item{order}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Please use \code{active} argument
instead.}

\item{name}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Please use \code{active} argument
instead.}

\item{free_guides}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}} Please use
\code{\link[=scheme_align]{scheme_align()}} function instead.}

\item{free_spaces}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Please use
\code{\link[=scheme_align]{scheme_align()}} function instead.}

\item{plot_data}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Please use
\code{\link[=scheme_data]{scheme_data()}} function instead.}

\item{theme}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Please use
\code{\link[=scheme_theme]{scheme_theme()}} function instead.}

\item{free_labs}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Please use
\code{\link[=scheme_align]{scheme_align()}} function instead.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

\code{align_gg()} is similar to \code{ggplot} in that it initializes a \code{ggplot} data
and \code{mapping}. Same with other \verb{align_*} functions. \code{align_gg()} allowing you
to provide data in various formats, including matrices, data frames, or
simple vectors. By default, it will inherit from the layout. If a function,
it will apply with the layout matrix.  \code{ggalign} is an alias of \code{align_gg}.
}
\section{ggplot2 specification}{

\code{align_gg} initializes a ggplot \code{data} and \code{mapping}.

\code{align_gg()} always applies a default mapping for the axis of the data index
in the layout. This mapping is \code{aes(y = .data$.y)} for horizontal stack
layout (including left and right annotation) and \code{aes(x = .data$.x)}
for vertical stack layout (including top and bottom annotation).

The data in the underlying \code{ggplot} object will contain following columns:
\itemize{
\item \code{.panel}: the panel for the aligned axis. It means \code{x-axis} for vertical
stack layout (including top and bottom annotation), \code{y-axis} for
horizontal stack layout (including left and right annotation).
\item \code{.x}/\code{y} and \code{.discrete_x}/\code{.discrete_y}: an integer index of \code{x}/\code{y}
coordinates and a factor of the data labels (only applicable when names
exists).
\item \code{.names} (\code{\link[vctrs:vec_names]{vec_names()}}) and \code{.index}
(\code{\link[vctrs:vec_size]{vec_size()}}/\code{\link[=NROW]{NROW()}}): a character names (only
applicable when names exists) and an integer index of the original data.
\item \code{.row_names} and \code{.row_index}: the row names and an integer of
row index of the original matrix (only applicable if \code{data} is a
\code{matrix}).
\item \code{.column_names} and \code{.column_index}: the column names and column index of
the original matrix (only applicable if \code{data} is a \code{matrix}).
\item \code{value}: the actual value (only applicable if \code{data} is a \code{matrix} or
atomic vector).
}

\code{matrix} input will be automatically melted into a long foramted data frame.

Atomic vector will be put in the \code{value} column of the data frame.

In the case where the input data is already a data frame, following columns
(\code{.panel}, \code{.index}, \code{.names}, \code{.x}/\code{.y}, \code{.discrete_x}/\code{.discrete_y}) are
added to the data frame.

It is recommended to use \code{.x}/\code{.y}, or \code{.discrete_x}/\code{.discrete_y} as the
\code{x}/\code{y} mapping.

If the data inherits from \code{\link[=quad_layout]{quad_layout()}}/\code{\link[=ggheatmap]{ggheatmap()}}, an additional
column will be added.
\itemize{
\item \code{.extra_panel}: the panel information for column (left or right
annotation) or row (top or bottom annotation).
}
}

\section{Axis Alignment for Observations}{

It is important to note that we consider rows as observations, meaning
\code{vec_size(data)}/\code{NROW(data)} must match the number of observations along the
axis used for alignment (x-axis for a vertical stack layout, y-axis for a
horizontal stack layout).
}

\examples{
ggheatmap(matrix(rnorm(81), nrow = 9)) +
    anno_top() +
    ggalign() +
    geom_point(aes(y = value))

# if data is `NULL`, a data frame with following column will be created
# (`.panel`, `.index`, `.names`, `.x`/`.y`, `.discrete_x`/`.discrete_y`)
ggheatmap(matrix(rnorm(81), nrow = 9)) +
    anno_top(size = 0.5) +
    align_dendro(k = 3L) +
    ggalign(data = NULL, size = 0.2) +
    geom_tile(aes(y = 1L, fill = .panel))

}
