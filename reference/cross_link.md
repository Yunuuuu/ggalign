# Add a plot to connect selected observations

Add a plot to connect selected observations

## Usage

``` r
cross_link(
  link,
  data = waiver(),
  ...,
  on_top = TRUE,
  obs_size = 1,
  inherit_index = NULL,
  inherit_panel = NULL,
  inherit_nobs = NULL,
  size = NULL,
  active = NULL
)
```

## Arguments

- link:

  A
  [`link_draw()`](https://yunuuuu.github.io/ggalign/reference/link_draw.md)
  object that defines how to draw the links, such as
  [`link_line()`](https://yunuuuu.github.io/ggalign/reference/link_line.md).

- data:

  The dataset to use for the layout. By default,
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  will convert the data to a matrix. This argument allows you to change
  the layout data. If not specified, the original data will be used.

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md).

- on_top:

  A boolean value indicating whether to draw the link on top of the plot
  panel (`TRUE`) or below (`FALSE`).

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

The `cross_link` function initializes a `ggplot` object but does not
initialize any data. Using
[`scheme_data()`](https://yunuuuu.github.io/ggalign/reference/scheme_data.md)
to change the internal data if needed.
