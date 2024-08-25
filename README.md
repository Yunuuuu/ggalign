
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggalign

This package extends ggplot2, enabling consistent axis alignment across
multiple plots. It’s particularly useful for plots where data order
needs to be manipulated, such as aligning a dendrogram with a heatmap.

## Installation

You can install the development version of `ggalign` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Yunuuuu/ggalign")
```

## Features

`ggalign` pacakge provides two layout to arrange ggplot objects:

- `layout_heatmap()`/`ggheamtap()`: Arrange ggplot into a Heatmap
  layout. See `vignette("layout-heatmap")` for details.

- `layout_stack()`/`ggstack()`: Arrange ggplot vertically or
  horizontally. See `vignette("layout-stack")` for details.

To further customize these layouts, the `ggalign` package offers several
`Align` objects:

- `align_group()`: Group layout axis into panel with a group variable.
- `align_kmeans()`: Group layout axis into panel by kmeans
- `align_reorder()`: Reorders layout observations based on weights or
  summary statistics.
- `align_dendro()`: Reorder or Group layout based on hierarchical
  clustering

For more detailed instructions on customizing layouts, see the vignette:
`vignette("align-layout")`.

Additionally, the following `Align` objects can be used to add plots
directly into the layout:

- `align_gg()`/`ggalign()`: Create ggplot object with a customized data.
- `align_panel()`/`ggpanel()`: Create ggplot object with the layout
  panel data.

For more information on adding plots, refer to the vignette:
`vignette("align-plot")`.

## Documentation

- [Heatmap
  Layout](https://yunuuuu.github.io/ggalign/articles/layout-heatmap.html)
- [Layout
  Customization](https://yunuuuu.github.io/ggalign/articles/align-layout.html)
- [Plot
  Integration](https://yunuuuu.github.io/ggalign/articles/align-plot.html)
- [Stack
  Layout](https://yunuuuu.github.io/ggalign/articles/layout-stack.html)

## Examples

![](https://yunuuuu.github.io/ggalign/articles/more-examples_files/figure-html/unnamed-chunk-3-1.png)
![](https://yunuuuu.github.io/ggalign/articles/more-examples_files/figure-html/unnamed-chunk-2-1.png)

## Compare with ComplexHeamtap
