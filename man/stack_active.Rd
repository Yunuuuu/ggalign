% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/activate.R
\name{stack_active}
\alias{stack_active}
\title{Determine the active context of stack layout}
\usage{
stack_active(
  guides = NA,
  free_spaces = NA,
  plot_data = NA,
  theme = NA,
  free_labs = NA,
  what = NULL,
  sizes = NULL
)
}
\arguments{
\item{guides}{A string containing one or more of \code{"t"}, \code{"l"}, \code{"b"}, and \code{"r"} indicates which side of guide legends should be collected. If \code{NULL}, no guide legends will be collected. If \code{\link[ggplot2:waiver]{waiver()}}, it will inherit from the parent layout}

\item{free_spaces}{A string with one or more of \code{"t"}, \code{"l"}, \code{"b"}, and \code{"r"}
indicating which border spaces should be removed. Defaults to
\code{\link[ggplot2:waiver]{waiver()}}, which inherits from the parent layout. If no
parent, the default is \code{NULL}, meaning no spaces are removed.}

\item{plot_data}{A function to transform plot data before rendering. Defaults
to \code{\link[ggplot2:waiver]{waiver()}}, which inherits from the parent layout. If
no parent layout, the default is \code{NULL}, meaning the data won't be modified.

Used to modify the data after layout has been created, which should be a data
frame, but before the data is handled of to the ggplot2 for rendering. Use
this hook if the you needs change the default data for all \code{geoms}.}

\item{theme}{Default theme for the plot in the layout. One of:
\itemize{
\item \code{\link[ggplot2:waiver]{waiver()}}: will inherit from the parent layout.
\item \code{NULL}: Use the \link[=theme_ggalign]{default theme}.
\item \code{\link[ggplot2:theme]{theme()}}: will be added with the parent layout theme.
}}

\item{free_labs}{A string with one or more of \code{"t"}, \code{"l"}, \code{"b"}, and \code{"r"} indicating
which axis titles should be free from alignment. Defaults to
\code{\link[ggplot2:waiver]{waiver()}}, which inherits from the parent layout. If no
parent layout, no axis titles will be aligned. If \code{NULL}, all axis titles
will be aligned.}

\item{what}{What should get activated for the stack layout?
Options include::
\itemize{
\item A single number or string of the plot elements in the stack layout.
\item \code{NULL}: remove any active context, this is useful when the active context is a
\code{\link[=heatmap_layout]{heatmap_layout()}} object, where any \verb{align_*()} will be added into the
heatmap. By removing the active context, we can add \verb{align_*()} into the
\code{\link[=stack_layout]{stack_layout()}}.
}}

\item{sizes}{A numeric or \link[grid:unit]{unit} object of length \code{3} indicates
the relative widths (\code{direction = "horizontal"}) / heights (\code{direction = "vertical"}).}
}
\value{
A \code{stack_active} object which can be added into
\link[=stack_layout]{StackLayout}.
}
\description{
Determine the active context of stack layout
}
\examples{
ggstack(matrix(1:9, nrow = 3L)) +
    ggheatmap() +
    # ggheamtap will set the active context, directing following addition
    # into the heatmap plot area. To remove the heatmap active context,
    # we can use `stack_active()` which will direct subsequent addition into
    # the stack
    stack_active() +
    # here we add a dendrogram to the stack.
    align_dendro()
}
