# Add a plot to annotate observations

Add a plot to annotate observations

## Usage

``` r
cross_mark(
  mark,
  data = waiver(),
  ...,
  obs_size = 1,
  inherit_index = NULL,
  inherit_panel = NULL,
  inherit_nobs = NULL,
  size = NULL,
  active = NULL
)
```

## Arguments

- mark:

  A
  [`mark_draw()`](https://yunuuuu.github.io/ggalign/reference/mark_draw.md)
  object to define how to draw the links. Like
  [`mark_line()`](https://yunuuuu.github.io/ggalign/reference/mark_line.md),
  [`mark_tetragon()`](https://yunuuuu.github.io/ggalign/reference/mark_tetragon.md).
  Note the names of the pair links will be used to define the panel
  names so must be unique.

- data:

  The dataset to use for the layout. By default,
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  will convert the data to a matrix. This argument allows you to change
  the layout data. If not specified, the original data will be used.

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md).

- obs_size:

  A single numeric value that indicates the size of a single
  observation, ranging from `(0, 1]`.

- inherit_index:

  A boolean value indicating whether to inherit the ordering index. If
  `TRUE`, will match the layout ordering index with the data names.

- inherit_panel:

  A boolean value indicating whether to inherit the panel group. If
  `TRUE`, will match the layout panel with the data names.

- inherit_nobs:

  A boolean value indicating whether to inherit the number of
  observations (nobs). If `TRUE`, the `data` input must be compatible
  with the layout data.

- size:

  The relative size of the plot, can be specified as a
  [`unit()`](https://rdrr.io/r/grid/unit.html). Note that for
  [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md),
  all size values will be interpreted as relative sizes, as this layout
  type adjusts based on the available space in the circular arrangement.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

## ggplot2 Specification

The `cross_mark` function initializes a `ggplot` object. The underlying
data contains following columns:

- `.panel`: the panel for the aligned axis. It means `x-axis` for
  vertical stack layout (including top and bottom annotation), `y-axis`
  for horizontal stack layout (including left and right annotation).

- `.names`
  ([`vec_names()`](https://vctrs.r-lib.org/reference/vec_names.html))
  and `.index`
  ([`vec_size()`](https://vctrs.r-lib.org/reference/vec_size.html)/[`NROW()`](https://rdrr.io/r/base/nrow.html)):
  a character names (only applicable when names exists) and an integer
  of index of the original data.

- `.hand`: A factor with levels `c("left", "right")` for horizontal
  stack layouts, or `c("top", "bottom")` for vertical stack layouts,
  indicating the position of the linked observations.

You can use
[`scheme_data()`](https://yunuuuu.github.io/ggalign/reference/scheme_data.md)
to modify the internal data if needed.
