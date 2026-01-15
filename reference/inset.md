# Create a ggplot inset

Create a ggplot inset

## Usage

``` r
inset(plot, ..., align = "panel", on_top = TRUE, clip = TRUE, vp = NULL)
```

## Arguments

- plot:

  Any graphic that can be converted into a
  [`grob`](https://rdrr.io/r/grid/grid-defunct.html) using
  [`as_grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.md).

- ...:

  Additional arguments passed to the
  [`as_grob()`](https://yunuuuu.github.io/ggalign/reference/as_grob.md)
  method.

- align:

  A string specifying the area to place the plot: `"full"` for the full
  area, `"plot"` for the full plotting area (including the axis label),
  or `"panel"` for only the actual area where data is drawn.

- on_top:

  A single boolean value indicates whether the graphic plot should be
  put frontmost. Note: the graphic plot will always put above the
  background.

- clip:

  A single boolean value indicating whether the grob should be clipped
  if they expand outside their designated area.

- vp:

  A [`viewport`](https://rdrr.io/r/grid/viewport.html) object, you can
  use this to define the plot area.

## Value

An `inset` object that can be added to any object implementing the
[`patch()`](https://yunuuuu.github.io/ggalign/reference/patch.md)
method.

## Examples

``` r
library(grid)
p1 <- ggplot(mtcars) +
    geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) +
    geom_boxplot(aes(gear, disp, group = gear))
p1 + inset(p2, vp = viewport(0.6, 0.6,
    just = c(0, 0), width = 0.4, height = 0.4
))
```
