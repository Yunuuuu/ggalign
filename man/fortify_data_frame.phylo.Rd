% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/craft-align-phylo.R
\name{fortify_data_frame.phylo}
\alias{fortify_data_frame.phylo}
\title{Build a data frame}
\usage{
\method{fortify_data_frame}{phylo}(
  data,
  ...,
  type = "rectangle",
  center = FALSE,
  tree_type = NULL,
  tip_pos = NULL,
  data_arg = NULL,
  call = NULL
)
}
\arguments{
\item{data}{A \code{\link[stats:hclust]{hclust}} or a
\code{\link[stats:dendrogram]{dendrogram}} object.}

\item{...}{These dots are for future extensions and must be empty.}

\item{type}{A string indicates the plot type, \code{"rectangle"} or \code{"triangle"}.}

\item{center}{A boolean value. if \code{TRUE}, nodes are plotted centered with
respect to all leaves/tips in the branch. Otherwise (default), plot them in
the middle of the direct child nodes.}

\item{tree_type}{A single string, one of
\code{"phylogram"} or \code{"cladogram"}, indicating the type of tree.
\itemize{
\item \code{phylogram}: Represents a phylogenetic tree where branch lengths indicate
evolutionary distance or time.
\item \code{cladogram}: Represents a tree where branch lengths are not used, or the
branches do not reflect evolutionary time.
}

Usually, you don't need to modify this.}

\item{tip_pos}{The x-coordinates of the tip. Must be the same length
of the number of tips in \code{tree}.}

\item{data_arg}{The argument name for \code{data}. Developers can use it to
improve messages. Not used by the user.}

\item{call}{The execution environment where \code{data} and other arguments for
the method are collected. Developers can use it to improve messages. Not
used by the user.}
}
\value{
A \verb{data frame} with the node coordinates:
\itemize{
\item \code{.index}: the original index in the tree for the the tip/node.
\item \code{label}: the tip/node label text.
\item \code{x} and \code{y}: x-axis and y-axis coordinates for the tip/node.
\item \code{tip}: A logical value indicates whether current node is a tip.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This function converts various objects to a data frame.
}
\section{ggalign attributes}{

\code{edge}: A \verb{data frame} for edge coordinates:
\itemize{
\item \code{x} and \code{y}: x-axis and y-axis coordinates for the start node of the edge.
\item \code{xend} and \code{yend}: the x-axis and y-axis coordinates of the terminal node
for edge.
}
}

\seealso{
Other \code{\link[=fortify_data_frame]{fortify_data_frame()}} methods:
\code{\link{fortify_data_frame.GRanges}()},
\code{\link{fortify_data_frame.character}()},
\code{\link{fortify_data_frame.default}()},
\code{\link{fortify_data_frame.dendrogram}()},
\code{\link{fortify_data_frame.matrix}()}
}
\concept{fortify_data_frame}
