# Arrange multiple plots into a grid

An internal S7 class that represents a collection of aligned plots along
with their layout configuration, titles, tags, and theme.

## Usage

``` r
alignpatches(
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
```

## Arguments

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\> A list
  of plots, ususally the ggplot object. Use `NULL` to indicate an empty
  spacer. Each input must implement the
  [`patch()`](https://yunuuuu.github.io/ggalign/reference/patch.md)
  method.

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

- theme:

  A [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  object used to customize various elements of the layout. By default,
  the theme will inherit from the parent `layout`.

- design:

  An alias for `area`, retained for backward compatibility.

## Value

An `alignpatches` object.

## Properties

- **plots**: A list of plot objects.

- **layout**: A list specifying layout options, including:

  - `ncol`, `nrow`, `byrow`: grid layout parameters.

  - `widths`, `heights`: relative dimensions of rows/columns.

  - `area`: custom area specification.

  - `guides`: guide handling.

- **titles**: A list specifying title options (`title`, `subtitle`,
  `caption`).

- **tags**: A list specifying tag options (`tags`, `sep`, `prefix`,
  `suffix`).

- **theme**: A theme configuration object.

## See also

- [`layout_design()`](https://yunuuuu.github.io/ggalign/reference/layout_design.md)

- [`layout_title()`](https://yunuuuu.github.io/ggalign/reference/layout_title.md)

- [`layout_theme()`](https://yunuuuu.github.io/ggalign/reference/layout_theme.md)

- [`layout_tags()`](https://yunuuuu.github.io/ggalign/reference/layout_tags.md)

## Examples

``` r
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

```
