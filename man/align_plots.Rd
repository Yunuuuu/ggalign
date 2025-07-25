% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alignpatch-align_plots.R
\name{align_plots}
\alias{align_plots}
\title{Arrange multiple plots into a grid}
\usage{
align_plots(
  ...,
  ncol = NULL,
  nrow = NULL,
  byrow = TRUE,
  widths = NA,
  heights = NA,
  area = NULL,
  guides = waiver(),
  theme = NULL,
  design = NULL
)
}
\arguments{
\item{...}{<\link[rlang:dyn-dots]{dyn-dots}> A list of plots, ususally the
ggplot object. Use \code{NULL} to indicate an empty spacer.}

\item{ncol, nrow}{The dimensions of the grid to create - if both are \code{NULL} it
will use the same logic as \code{\link[ggplot2:facet_wrap]{facet_wrap()}} to set the
dimensions}

\item{byrow}{If \code{FALSE} the plots will be filled in in column-major order.}

\item{widths, heights}{The relative widths and heights of each column and row
in the grid. Will get repeated to match the dimensions of the grid. The
special value of \code{NA} will behave as \verb{1null} unit unless a fixed aspect plot
is inserted in which case it will allow the dimension to expand or contract
to match the aspect ratio of the content.}

\item{area}{Specification of the location of areas in the layout. Can
either be specified as a text string or by concatenating calls to
\code{\link[=area]{area()}} together.}

\item{guides}{A string with one or more of \code{"t"}, \code{"l"}, \code{"b"}, \code{"r"}, and \code{"i"}
indicating which side of guide legends should be collected. Defaults to
\code{\link[ggplot2:waiver]{waiver()}}, which inherits from the parent layout. If
there is no parent layout, or if \code{NULL} is provided, no guides will be
collected.}

\item{theme}{A \code{\link[ggplot2:theme]{theme()}} object used to customize various
elements of the layout. By default, the theme will inherit from the parent
\code{layout}.}

\item{design}{An alias for \code{area}, retained for backward compatibility.}
}
\value{
An \code{AlignPatches} object.
}
\description{
Arrange multiple plots into a grid
}
\examples{
# directly copied from patchwork
p1 <- ggplot(mtcars) +
    geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) +
    geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) +
    geom_bar(aes(gear)) +
    facet_wrap(~cyl)
p4 <- ggplot(mtcars) +
    geom_bar(aes(carb))
p5 <- ggplot(mtcars) +
    geom_violin(aes(cyl, mpg, group = cyl))

# Either add the plots as single arguments
align_plots(p1, p2, p3, p4, p5)

# Or use bang-bang-bang to add a list
align_plots(!!!list(p1, p2, p3), p4, p5)

# Match plots to areas by name
area <- "#BB
          AA#"
align_plots(B = p1, A = p2, area = area)

# Compare to not using named plot arguments
align_plots(p1, p2, area = area)
}
\seealso{
\itemize{
\item \code{\link[=layout_design]{layout_design()}}
\item \code{\link[=layout_title]{layout_title()}}
\item \code{\link[=layout_theme]{layout_theme()}}
}
}
