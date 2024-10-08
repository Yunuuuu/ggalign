% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-draw.R
\name{geom_draw}
\alias{geom_draw}
\title{Layer with Grid or Function}
\usage{
geom_draw(
  draw = zeroGrob(),
  ...,
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  inherit.aes = TRUE
)
}
\arguments{
\item{draw}{Either a function (can be purrr-style) which accepts two
arguments (\code{data} and \code{coords}) and returns a \link[grid:grid.grob]{grob}
or any objects which can be converted to \link[grid:grid.grob]{grob} by \code{\link[=patch]{patch()}}.

when \code{draw} is a function, it is used as the \code{draw_group} function
in a \link[ggplot2:ggplot2-ggproto]{Geom} \code{ggproto} object.}

\item{...}{<\link[rlang:dyn-dots]{dyn-dots}> Additional arguments passed to
function specified in argument \code{draw} or \code{\link[=patch]{patch()}} if \code{draw} is not a
function.}

\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this layer.
When using a \verb{geom_*()} function to construct a layer, the \code{stat}
argument can be used the override the default coupling between geoms and
stats. The \code{stat} argument accepts the following:
\itemize{
\item A \code{Stat} ggproto subclass, for example \code{StatCount}.
\item A string naming the stat. To give the stat as a string, strip the
function name of the \code{stat_} prefix. For example, to use \code{stat_count()},
give the stat as \code{"count"}.
\item For more information and other ways to specify the stat, see the
\link[ggplot2:layer_stats]{layer stat} documentation.
}}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A ggplot2 layer.
}
\description{
Draw ggplot2 layer with a grod or function.
}
\details{
If you want to combine the functionality of multiple geoms it can
usually be achieved by preparing the data for each of the geoms inside the
\verb{draw_*()} call and send it off to the different geoms, collecting the
output in a \code{\link[grid:grid.grob]{grid::gList}} (a list of grobs) if the call is \code{draw_group()}
or a \code{\link[grid:grid.grob]{grid::gTree}} (a grob containing multiple children grobs) if the
call is \code{draw_panel()}.
}
\examples{
ggdraw_text <- grid::textGrob(
    "ggdraw",
    x = c(0, 0, 0.5, 1, 1),
    y = c(0, 1, 0.5, 0, 1),
    hjust = c(0, 0, 0.5, 1, 1),
    vjust = c(0, 1, 0.5, 0, 1)
)
ggplot2::ggplot(data.frame(x = 1, y = 2)) +
    geom_draw(ggdraw_text)
}
\seealso{
\url{https://ggplot2.tidyverse.org/reference/ggplot2-ggproto.html}
}
