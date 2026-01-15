# Create a heatmap

**\[stable\]**

`heatmap_layout` is a specialized version of
[`quad_discrete()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md),
which simplifies the creation of heatmap plots by integrating essential
elements for a standard heatmap layout, ensuring that the appropriate
data mapping and visualization layers are automatically applied.
`ggheatmap` is an alias for `heatmap_layout`.

## Usage

``` r
heatmap_layout(
  data = NULL,
  mapping = aes(),
  ...,
  width = NA,
  height = NA,
  filling = waiver(),
  theme = NULL,
  active = NULL
)
```

## Arguments

- data:

  Default dataset to use for the layout. If not specified, it must be
  supplied in each plot added to the layout. By default, it will try to
  inherit from parent layout.
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  will be used to convert data to a matrix.

- mapping:

  Default list of aesthetic mappings to use for main plot in the layout.
  If not specified, must be supplied in each layer added to the main
  plot.

- ...:

  Additional arguments passed to
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md).

- width, height:

  The relative width/height of the main plot, can be a
  [`unit`](https://rdrr.io/r/grid/unit.html) object.

- filling:

  A single string of `"raster"` or `"tile"` to indicate the filling
  style. By default, `waiver()` is used, which means that if the input
  matrix has more than 20,000 cells (`nrow * ncol > 20000`),
  [`geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  will be used for performance efficiency; for smaller matrices,
  [`geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  will be used. To customize the filling style, set this to `NULL`.

  For backward compatibility, a single boolean value is acceptable:
  `TRUE` means `waiver()`, and `FALSE` means `NULL`.

  By default, the classic heatmap color scheme
  `scale_fill_gradient2(low = "blue", high = "red")` is utilized for
  continuous values.

  You can use the options `"ggalign.heatmap_continuous_fill"` or
  `"ggalign.heatmap_discrete_fill"` to modify the default heatmap body
  filling color scale. See
  [`scale_fill_continuous()`](https://ggplot2.tidyverse.org/reference/scale_colour_continuous.html)
  or
  [`scale_fill_discrete()`](https://ggplot2.tidyverse.org/reference/scale_colour_discrete.html)
  for details on option settings.

- theme:

  A [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  object used to customize various elements of the layout, including
  `guides`, `title`, `subtitle`, `caption`, `margins`, `panel.border`,
  and `background`. By default, the theme will inherit from the parent
  `layout`. It also controls the panel spacing for all plots in the
  layout.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

## Value

A `HeatmapLayout` object.

## ggplot2 specification

The data input will be converted to a matrix using
[`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md),
and the data in the underlying main plot will contain the following
columns:

- `.panel_x` and `.panel_y`: the column and row panel groups.

- `.x` and `.y`: an integer index of `x` and `y` coordinates

- `.discrete_x` and `.discrete_y`: a factor of the data labels (only
  applicable when `.row_names` and `.column_names` exists).

- `.row_names` and `.column_names`: A character of the row and column
  names of the original matrix (only applicable when names exist).

- `.row_index` and `.column_index`: the row and column index of the
  original matrix.

- `value`: the actual matrix value.

## Examples

``` r
ggheatmap(1:10)
#> → heatmap built with `geom_tile()`

ggheatmap(letters)
#> → heatmap built with `geom_tile()`

ggheatmap(matrix(rnorm(81), nrow = 9L))
#> → heatmap built with `geom_tile()`
```
