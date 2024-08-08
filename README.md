
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggalign

This package extends ggplot2 and provides numerous benefits for
organizing and arranging plots. It is specifically designed to align a
specific axis of multiple ggplot objects in a consistent order. This
functionality is particularly useful for plots that require manipulation
of data order. A common plot combination that can be effectively
organized using this package includes a dendrogram and a heatmap.

## Installation

You can install the development version of `ggalign` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Yunuuuu/ggalign")
```

This package now depends on some un-merged features of patchwork. Please
see <https://github.com/thomasp85/patchwork/pull/373>. If you want to
try it, you should install patchwork with
`pak::pkg_install(Yunuuuu/patchwork@align_axis_title)`.

The documentation for `ggalign` is currently in progress.

## Features

`ggalign` provides two layout to arrange ggplot objects:

- `layout_heatmap()`/`ggheamtap()`: Arrange ggplot into a Heatmap. See
  `vignette("layout-heatmap")` for details.

- `layout_stack()`/`ggstack()`: Arrange ggplot vertically or
  horizontally. See `vignette("layout-stack")` for details.

`ggalign` provides `Align` objects to reorder or set panels for layout:

- `align_group()`: Group layout axis into panel
- `align_kmeans()`: Group layout observations by kmeans
- `align_reorder()`: Reorder layout observations
- `align_dendro()`: Reorder or Group layout based on Hierarchical
  Clustering

In addition, `Align` objects can also add plot into the layout:

- `align_gg()`/`ggalign()`: Create ggplot object in the layout
- `align_panel()`/`ggpanel()`: Create ggplot object based on the layout
  panel data.

See `vignette("align")` for details.

## Examples

## Compare with ComplexHeamtap
