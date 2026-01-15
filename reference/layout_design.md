# Define the grid to compose plots in

To control how different plots are laid out, you need to add a layout
design specification. If you are nesting grids, the layout is scoped to
the current nesting level.

## Usage

``` r
layout_design(
  ncol = NA_real_,
  nrow = NA_real_,
  byrow = NA,
  widths = NULL,
  heights = NULL,
  area = waiver(),
  guides = NA_character_
)
```

## Arguments

- ncol, nrow:

  The number of columns and rows in the grid. Defaults to `NULL`. If
  both are `NULL`, the layout dimensions are determined automatically
  using the same logic as
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- byrow:

  A logical value indicating whether plots should be filled in row-major
  order (`TRUE`) or column-major order (`FALSE`). Defaults to `TRUE`.

- widths, heights:

  The relative widths and heights of each column and row in the grid.
  These values are recycled to match the grid dimensions. The special
  value `NA` is treated as a unit of `1null`, unless a fixed-aspect plot
  is included — in that case, the affected dimension will expand or
  contract to maintain the aspect ratio of the plot. Defaults to `NA`.

- area:

  A specification of the area layout. Can be defined either as a
  character string or as a combination of calls to
  [`area()`](https://yunuuuu.github.io/ggalign/reference/area.md).
  Defaults to `NULL`.

- guides:

  A string with one or more of `"t"`, `"l"`, `"b"`, `"r"`, and `"i"`
  indicating which side of guide legends should be collected. Defaults
  to [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html),
  which inherits from the parent layout. If there is no parent layout,
  or if `NULL` is provided, no guides will be collected.

## Value

A `layout_design` object.

## Examples

``` r
p1 <- ggplot(mtcars) +
    geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) +
    geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) +
    geom_bar(aes(gear)) +
    facet_wrap(~cyl)
align_plots(p1, p2, p3) +
    layout_design(nrow = 1L)

align_plots(p1, p2, p3) +
    layout_design(ncol = 1L)
```
