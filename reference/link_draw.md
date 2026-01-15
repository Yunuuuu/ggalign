# Define the links to connect a pair of observations

This function allows users to define links between a pair of
observations, facilitating the visualization of connections between
related data points.

## Usage

``` r
link_draw(.draw, ...)
```

## Arguments

- .draw:

  A function used to draw the links. The function must return a
  [`grob()`](https://rdrr.io/r/grid/grid-defunct.html) object. If the
  function does not return a valid `grob`, no drawing will occur. The
  input data for the function must contain two arguments: a data frame
  for the left hand coordinates and a data frame for the right hand
  observation coordinates.

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\> A list
  of formulas, where each side of the formula should be an `integer` or
  `character` index of the original data, or a
  [`range_link()`](https://yunuuuu.github.io/ggalign/reference/pair_links.md)
  object defining the linked observations. Use `NULL` to indicate no
  link on that side. You can also combine these by wrapping them into a
  single [`list()`](https://rdrr.io/r/base/list.html). If only the
  left-hand side of the formula exists, you can input it directly. For
  integer indices, wrap them with
  [`I()`](https://rdrr.io/r/base/AsIs.html) to use the ordering from the
  layout. You can also use
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html) to
  inherit values from the other group.

## See also

- [`link_line()`](https://yunuuuu.github.io/ggalign/reference/link_line.md)

- [`.link_draw()`](https://yunuuuu.github.io/ggalign/reference/dot-link_draw.md)
