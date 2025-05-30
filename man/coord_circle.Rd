% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot-coord-circle.R
\name{coord_circle}
\alias{coord_circle}
\title{Polar Coordinates with Enhanced Controls}
\usage{
coord_circle(
  theta = "x",
  start = 0,
  end = NULL,
  thetalim = NULL,
  rlim = NULL,
  expand = FALSE,
  direction = 1,
  clip = "off",
  r.axis.inside = NULL,
  rotate.angle = FALSE,
  inner.radius = 0,
  outer.radius = 0.95
)
}
\arguments{
\item{theta}{variable to map angle to (\code{x} or \code{y})}

\item{start}{Offset of starting point from 12 o'clock in radians. Offset
is applied clockwise or anticlockwise depending on value of \code{direction}.}

\item{end}{Position from 12 o'clock in radians where plot ends, to allow
for partial polar coordinates. The default, \code{NULL}, is set to
\code{start + 2 * pi}.}

\item{thetalim, rlim}{Limits for the \code{theta} and \code{r} axes.}

\item{expand}{If \code{TRUE}, the default, adds a small expansion factor the
the limits to prevent overlap between data and axes. If \code{FALSE}, limits
are taken directly from the scale.}

\item{direction}{1, clockwise; -1, anticlockwise}

\item{clip}{Should drawing be clipped to the extent of the plot panel? A
setting of \code{"on"} (the default) means yes, and a setting of \code{"off"}
means no. For details, please see \code{\link[ggplot2:coord_cartesian]{coord_cartesian()}}.}

\item{r.axis.inside}{If \code{TRUE}, places the radius axis inside the
panel. If \code{FALSE}, places the radius axis next to the panel. The default,
\code{NULL}, places the radius axis outside if the \code{start} and \code{end} arguments
form a full circle.}

\item{rotate.angle}{If \code{TRUE}, transforms the \code{angle} aesthetic in data
in accordance with the computed \code{theta} position. If \code{FALSE} (default),
no such transformation is performed. Can be useful to rotate text geoms in
alignment with the coordinates.}

\item{inner.radius}{A numeric in \verb{[0, 1)} indicates the inner radius.}

\item{outer.radius}{A numeric in \verb{(0, 1]} indicates the outer radius.
\code{\link[ggplot2:coord_polar]{coord_radial()}} by default uses \code{0.8}.}
}
\description{
An extended version of \code{\link[ggplot2:coord_polar]{coord_radial()}}, providing
additional customization options.
}
\examples{
ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    coord_circle(
        start = -0.4 * pi, end = 0.4 * pi,
        inner.radius = 0.3, outer.radius = 1
    )
ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    coord_circle(
        start = -0.4 * pi, end = 0.4 * pi,
        inner.radius = 0.3, outer.radius = 0.5
    )
}
