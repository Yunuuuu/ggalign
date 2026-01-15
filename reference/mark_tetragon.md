# Link the observations and the panel with a quadrilateral

Link the observations and the panel with a quadrilateral

## Usage

``` r
mark_tetragon(..., .element = NULL)
```

## Arguments

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

- .element:

  A
  [`element_polygon()`](https://ggplot2.tidyverse.org/reference/element.html)
  object. Vectorized fields will be recycled to match the total number
  of groups, or you can wrap the element with
  [`I()`](https://rdrr.io/r/base/AsIs.html) to recycle to match the
  drawing groups. The drawing groups are usually the same as the defined
  groups, but they will differ when the defined group of observations is
  separated and cannot be linked with a single quadrilateral. In such
  cases, the number of drawing groups will be larger than the number of
  defined groups.
