# Plot Phylogenetics tree

Plot Phylogenetics tree

## Usage

``` r
align_phylo(
  phylo,
  ...,
  mapping = aes(),
  split = FALSE,
  ladderize = NULL,
  type = "rectangle",
  center = FALSE,
  tree_type = NULL,
  root = NULL,
  active = NULL,
  size = NULL,
  no_axes = deprecated()
)
```

## Arguments

- phylo:

  A `phylo` object.

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to
  [`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html).

- mapping:

  Default list of aesthetic mappings to use for plot. If not specified,
  must be supplied in each layer added to the plot.

- split:

  A logical scalar indicating whether to split the phylogenetic tree
  into separate subtrees when multiple panel groups are present.

- ladderize:

  A single string of `"left"` or `"right"`, indicating whether to
  ladderize the tree. Ladderizing arranges the tree so that the smallest
  clade is positioned on the `"right"` or the `"left"`. By default,
  `NULL` means the tree will not be ladderized.

- type:

  A string indicates the plot type, `"rectangle"` or `"triangle"`.

- center:

  A boolean value. if `TRUE`, nodes are plotted centered with respect to
  all leaves/tips in the branch. Otherwise (default), plot them in the
  middle of the direct child nodes.

- tree_type:

  A single string, one of `"phylogram"` or `"cladogram"`, indicating the
  type of tree.

  - `phylogram`: Represents a phylogenetic tree where branch lengths
    indicate evolutionary distance or time.

  - `cladogram`: Represents a tree where branch lengths are not used, or
    the branches do not reflect evolutionary time.

  Usually, you don't need to modify this.

- root:

  A length one string or numeric indicates the root branch.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

- size:

  The relative size of the plot, can be specified as a
  [`unit()`](https://rdrr.io/r/grid/unit.html). Note that for
  [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md),
  all size values will be interpreted as relative sizes, as this layout
  type adjusts based on the available space in the circular arrangement.

- no_axes:

  **\[deprecated\]** Please add
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  directly to the ggplot instead.
