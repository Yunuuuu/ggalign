# Set Expansion for the Layout

To align axes, it is important to keep the expansion consistent across
all plots in the layout. You can add a `layout_expand` object to the
layout. For the
[`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
function, you must specify `x` and `y` arguments. For other layouts, you
can pass the expansion values using `...` directly.

## Usage

``` r
layout_expand(..., x = waiver(), y = waiver())
```

## Arguments

- ...:

  A list of range expansion constants, used to add padding around the
  data to ensure they are placed some distance away from the axes. Use
  the convenience function
  [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to generate the values.

- x, y:

  Same as `...`, but specifically for
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md).
