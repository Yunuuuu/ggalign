# Build a data frame

**\[stable\]**

This function converts various objects to a data frame.

## Usage

``` r
# S3 method for class 'phylo'
fortify_data_frame(
  data,
  ...,
  priority = "right",
  center = FALSE,
  type = "rectangle",
  tree_type = NULL,
  tip_pos = NULL,
  tip_clades = NULL,
  reorder_clades = TRUE,
  clade_gap = NULL,
  root = NULL,
  double = TRUE,
  data_arg = NULL,
  call = NULL
)
```

## Arguments

- data:

  A [`hclust`](https://rdrr.io/r/stats/hclust.html) or a
  [`dendrogram`](https://rdrr.io/r/stats/dendrogram.html) object.

- ...:

  These dots are for future extensions and must be empty.

- priority:

  A string of "left" or "right". if we draw from `right` to `left`, the
  left will override the right, so we take the `"left"` as the priority.
  If we draw from `left` to `right`, the right will override the left,
  so we take the `"right"` as priority. This is used by
  [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  to provide support of facet operation in ggplot2.

- center:

  A boolean value. if `TRUE`, nodes are plotted centered with respect to
  all leaves/tips in the branch. Otherwise (default), plot them in the
  middle of the direct child nodes.

- type:

  A string indicates the plot type, `"rectangle"` or `"triangle"`.

- tree_type:

  A single string, one of `"phylogram"` or `"cladogram"`, indicating the
  type of tree.

  - `phylogram`: Represents a phylogenetic tree where branch lengths
    indicate evolutionary distance or time.

  - `cladogram`: Represents a tree where branch lengths are not used, or
    the branches do not reflect evolutionary time.

  Usually, you don't need to modify this.

- tip_pos:

  The x-coordinates of the tip. Must be the same length of the number of
  tips in `tree`.

- tip_clades:

  Clades of the tips. Must be the same length of the number of tips in
  `data`.

- reorder_clades:

  A single boolean value, indicates whether reorder the provided
  `tip_clades` based on the actual ordering index.

- clade_gap:

  A single numeric value indicates the gap between different clades.

- root:

  A length one string or numeric indicates the root branch.

- double:

  A single logical value indicating whether horizontal lines should be
  doubled when segments span multiple branches. If `TRUE`, the
  horizontal lines will be repeated for each branch that the segment
  spans. If `FALSE`, only one horizontal line will be drawn. This is
  used by
  [`align_dendro()`](https://yunuuuu.github.io/ggalign/reference/align_dendro.md)
  to provide support of facet operation in ggplot2.

- data_arg:

  The argument name for `data`. Developers can use it to improve
  messages. Not used by the user.

- call:

  The execution environment where `data` and other arguments for the
  method are collected. Developers can use it to improve messages. Not
  used by the user.

## Value

A `data frame` with the node coordinates:

- `.panel`: Similar with `panel` column, but always give the correct
  panel for usage of the ggplot facet.

- `.index`: the original index in the tree for the the tip/node.

- `label`: the tip/node label text.

- `x` and `y`: x-axis and y-axis coordinates for the tip/node.

- `clade`: which clade the node is. You can use this column to color
  different clades.

- `panel`: which panel the node is, if we split the plot into panel
  using
  [facet_grid](https://ggplot2.tidyverse.org/reference/facet_grid.html),
  this column will show which panel the node is from. Note: some nodes
  may fall outside panel (between two panels), so there are possible
  `NA` values in this column.

- `tip`: A logical value indicates whether current node is a tip.

## ggalign attributes

`edge`: A `data frame` for edge coordinates:

- `.panel`: Similar with `panel` column, but always give the correct
  panel for usage of the ggplot facet.

- `x` and `y`: x-axis and y-axis coordinates for the start node of the
  edge.

- `xend` and `yend`: the x-axis and y-axis coordinates of the terminal
  node for edge.

- `clade`: which panel the edge is. You can use this column to color
  different groups.

- `panel1` and `panel2`: The panel1 and panel2 columns have the same
  functionality as `panel`, but they are specifically for the `edge`
  data and correspond to both nodes of each edge.

## See also

Other
[`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
methods:
[`fortify_data_frame.GRanges()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.GRanges.md),
[`fortify_data_frame.character()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.character.md),
[`fortify_data_frame.default()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.default.md),
[`fortify_data_frame.dendrogram()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.dendrogram.md),
[`fortify_data_frame.matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.matrix.md)
