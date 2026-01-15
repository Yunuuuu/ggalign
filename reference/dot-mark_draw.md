# Define the links to connect the marked observations

A base version of
[`mark_draw`](https://yunuuuu.github.io/ggalign/reference/mark_draw.md),
designed for performance optimization. This function is used to build
other `mark_*` functions that manage the drawing of links between marked
observations.

## Usage

``` r
.mark_draw(.draw, ...)
```

## Arguments

- .draw:

  A function used to draw the links. The function must return a
  [`grob()`](https://rdrr.io/r/grid/grid-defunct.html) object. If the
  function does not return a valid `grob`, nothing will be drawn. The
  input data for the function contains a list, where each item is a list
  of two data frames: one for the panel side coordinates (`"panel"`) and
  one for the marked observations coordinates (`"link"`).

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

[`mark_draw()`](https://yunuuuu.github.io/ggalign/reference/mark_draw.md)
