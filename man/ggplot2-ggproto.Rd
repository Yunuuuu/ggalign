% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-draw.R
\docType{data}
\name{GeomDraw}
\alias{GeomDraw}
\alias{ggplot2-ggproto}
\title{Base ggproto classes for ggplot2}
\description{
Base ggproto classes for ggplot2
}
\section{Geoms}{



All \verb{geom_*()} functions (like \code{geom_point()}) return a layer that
contains a \verb{Geom*} object (like \code{GeomPoint}). The \verb{Geom*}
object is responsible for rendering the data in the plot.

Each of the \verb{Geom*} objects is a \code{\link[ggplot2:ggproto]{ggproto()}} object, descended
from the top-level \code{Geom}, and each implements various methods and
fields.

Compared to \code{Stat} and \code{Position}, \code{Geom} is a little
different because the execution of the setup and compute functions is
split up. \code{setup_data} runs before position adjustments, and
\code{draw_layer()} is not run until render time, much later.

To create a new type of Geom object, you typically will want to
override one or more of the following:
\itemize{
\item Either \code{draw_panel(self, data, panel_params, coord)} or
\code{draw_group(self, data, panel_params, coord)}. \code{draw_panel} is
called once per panel, \code{draw_group} is called once per group.

Use \code{draw_panel} if each row in the data represents a
single element. Use \code{draw_group} if each group represents
an element (e.g. a smooth, a violin).

\code{data} is a data frame of scaled aesthetics.

\code{panel_params} is a set of per-panel parameters for the
\code{coord}. Generally, you should consider \code{panel_params}
to be an opaque data structure that you pass along whenever you call
a coord method.

You must always call \code{coord$transform(data, panel_params)} to
get the (position) scaled data for plotting. To work with
non-linear coordinate systems, you typically need to convert into a
primitive geom (e.g. point, path or polygon), and then pass on to the
corresponding draw method for munching.

Must return a grob. Use \code{\link[ggplot2:zeroGrob]{zeroGrob()}} if there's nothing to
draw.
\item \code{draw_key}: Renders a single legend key.
\item \code{required_aes}: A character vector of aesthetics needed to
render the geom.
\item \code{default_aes}: A list (generated by \code{\link[ggplot2:aes]{aes()}} of
default values for aesthetics.
\item \code{setup_data}: Converts width and height to xmin and xmax,
and ymin and ymax values. It can potentially set other values as well.
}

See also the \href{https://ggplot2-book.org/extensions#sec-new-geoms}{new geoms section} of the online ggplot2 book.

}

\section{Coordinate systems}{



All \verb{coord_*()} functions (like \code{coord_trans()}) return a \verb{Coord*}
object (like \code{CoordTrans}).

Each of the \verb{Coord*} objects is a \code{\link[ggplot2:ggproto]{ggproto()}} object,
descended from the top-level \code{Coord}.  To create a new type of Coord
object, you typically will want to implement one or more of the following:
\itemize{
\item \code{aspect}: Returns the desired aspect ratio for the plot.
\item \code{labels}: Returns a list containing labels for x and y.
\item \code{render_fg}: Renders foreground elements.
\item \code{render_bg}: Renders background elements.
\item \code{render_axis_h}: Renders the horizontal axes.
\item \code{render_axis_v}: Renders the vertical axes.
\item \code{backtransform_range(panel_params)}: Extracts the panel range provided
in \code{panel_params} (created by \code{setup_panel_params()}, see below) and
back-transforms to data coordinates. This back-transformation can be needed
for coords such as \code{coord_trans()} where the range in the transformed
coordinates differs from the range in the untransformed coordinates. Returns
a list of two ranges, \code{x} and \code{y}, and these correspond to the variables
mapped to the \code{x} and \code{y} aesthetics, even for coords such as \code{coord_flip()}
where the \code{x} aesthetic is shown along the y direction and vice versa.
\item \code{range(panel_params)}: Extracts the panel range provided
in \code{panel_params} (created by \code{setup_panel_params()}, see below) and
returns it. Unlike \code{backtransform_range()}, this function does not perform
any back-transformation and instead returns final transformed coordinates. Returns
a list of two ranges, \code{x} and \code{y}, and these correspond to the variables
mapped to the \code{x} and \code{y} aesthetics, even for coords such as \code{coord_flip()}
where the \code{x} aesthetic is shown along the y direction and vice versa.
\item \code{transform}: Transforms x and y coordinates.
\item \code{distance}: Calculates distance.
\item \code{is_linear}: Returns \code{TRUE} if the coordinate system is
linear; \code{FALSE} otherwise.
\item \code{is_free}: Returns \code{TRUE} if the coordinate system supports free
positional scales; \code{FALSE} otherwise.
\item \code{setup_panel_params(scale_x, scale_y, params)}: Determines the appropriate
x and y ranges for each panel, and also calculates anything else needed to
render the panel and axes, such as tick positions and labels for major
and minor ticks. Returns all this information in a named list.
\item \code{setup_data(data, params)}: Allows the coordinate system to
manipulate the plot data. Should return list of data frames.
\item \code{setup_layout(layout, params)}: Allows the coordinate
system to manipulate the \code{layout} data frame which assigns
data to panels and scales.
}

See also the \href{https://ggplot2-book.org/extensions#sec-new-coords}{new coords section} of the online ggplot2 book.

}

\seealso{
ggproto
}
\keyword{internal}
